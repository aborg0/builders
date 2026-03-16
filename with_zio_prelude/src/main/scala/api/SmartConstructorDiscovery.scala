package api

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.time.Instant
import scala.quoted.*
import api.ValidatorInfo.*

object MacroDebugger {
  private val logFilePath = "macro_debug.log"
  // Use append mode
  private val writer = new PrintWriter(new BufferedWriter(new FileWriter(logFilePath, true)))

  def log(msg: String): Unit = synchronized {
    writer.println(s"[${Instant.now}] $msg")
    writer.flush()
  }
}

/**
 * Discovers smart constructors for types at compile time.
 * 
 * Discovery priority:
 * 1. companion.apply returning Validation[_, T]
 * 2. companion.make returning Validation[_, T]
 * 3. companion.apply returning Either[_, T]
 * 4. No validation needed (identity)
 * 
 * Note: These are regular functions used during macro expansion, not inline defs.
 */
object SmartConstructorDiscovery {
  
    /**
   * Discovers how to validate a field type.
   * Returns ValidatorInfo describing the validation strategy.
   */
  def discoverValidator(using Quotes)(
    fieldName: String,
    fieldType: quotes.reflect.TypeRepr
  ): ValidatorInfo = {
    import quotes.reflect.*
    
    val typeSymbol = fieldType.typeSymbol
    
    MacroDebugger.log(s"Discovering validator for field '$fieldName' of type ${fieldType.show}")
    MacroDebugger.log(s"  Type symbol: ${typeSymbol.fullName}")

    // Find the companion object. This is tricky for opaque types.
    val companionOpt: Option[Symbol] =
      if (typeSymbol.companionModule.exists && typeSymbol.companionModule != Symbol.noSymbol) {
        MacroDebugger.log(s"  Found standard companion: ${typeSymbol.companionModule.fullName}")
        Some(typeSymbol.companionModule)
      } else {
        MacroDebugger.log(s"  Standard companion not found for ${typeSymbol.fullName}. Looking for sibling module with name '${typeSymbol.name}'.")
        val owner = typeSymbol.owner

        // Direct sibling lookup (works for regular classes/objects)
        val directSibling = owner.declarations.find(s => (s.name.init == typeSymbol.name || s.name == typeSymbol.name) && s.flags.is(Flags.Module))
        directSibling match {
          case some @ Some(s) =>
            MacroDebugger.log(s"  Found sibling companion module: ${s.fullName}")
            some
          case None =>
            // Fallbacks to support zio.prelude Subtype/NewType patterns where the exposed field
            // type may be an inner alias `X.Type` or simply `Type` whose companion is the enclosing
            // module object (e.g. SequenceNumber.Type -> SequenceNumber object).

            // Helper: try searching upward from a symbol for a module with the given base name
            def searchUpForModule(sym: Symbol, baseName: String): Option[Symbol] =
              if (sym == Symbol.noSymbol) None
              else
                sym.declarations.find(d => d.flags.is(Flags.Module) && (d.name == baseName || d.name == baseName.init)) match {
                  case some @ Some(_) => some
                  case None => searchUpForModule(sym.owner, baseName)
                }

            // If the type's simple name is `Type` (or ends with `Type`), try to use the owner name
            // as the candidate module name (e.g. `SequenceNumber.Type` -> module `SequenceNumber`).
            val typeBasedCandidate: Option[Symbol] =
              if (typeSymbol.name == "Type" || typeSymbol.name.endsWith("Type")) {
                val base = typeSymbol.owner.name
                MacroDebugger.log(s"  Attempting subtype/newtype resolution using owner name: $base")
                searchUpForModule(owner, base)
              } else None

            typeBasedCandidate match {
              case some @ Some(s) =>
                MacroDebugger.log(s"  Found subtype/newtype companion module: ${s.fullName}")
                some
              case None =>
                // Last-resort global search: strip common suffixes and look upward for a matching module
                val stripped = typeSymbol.name.stripSuffix("$Type").stripSuffix("Type")
                MacroDebugger.log(s"  Attempting global search for module matching stripped name: $stripped")
                searchUpForModule(owner, stripped) match {
                  case some @ Some(s) =>
                    MacroDebugger.log(s"  Found global companion: ${s.fullName}")
                    some
                  case None =>
                    MacroDebugger.log(s"  No companion module found for ${typeSymbol.fullName}")
                    None
                }
            }
        }
      }

    companionOpt match {
      case Some(companion) =>
        findValidationMethod(companion, fieldType, fieldName) match {
          case Some(info) =>
            MacroDebugger.log(s"  Found validation for $fieldName: ${info.methodName}")
            info
          case None =>
            MacroDebugger.log(s"  No validation method found in companion '${companion.fullName}' for $fieldName")
            ValidatorInfo.NoValidation(fieldName, fieldType)
        }
      case None =>
        MacroDebugger.log(s"  No companion object could be resolved for $fieldName.")
        ValidatorInfo.NoValidation(fieldName, fieldType)
    }
  }
  
  /**
   * Tries to find a suitable validation method in the companion object.
   */
  private def findValidationMethod(using Quotes)(
    companion: quotes.reflect.Symbol,
    targetType: quotes.reflect.TypeRepr,
    fieldName: String
  ): Option[ValidatorInfo.NeedsValidation] = {
    import quotes.reflect.*
    
    // Try apply method first, then make
    findMethodValidation(companion, "apply", targetType, fieldName)
      .orElse(findMethodValidation(companion, "make", targetType, fieldName))
  }
  
  /**
   * Looks for a method (apply or make) returning Validation or Either.
   */
  private def findMethodValidation(using Quotes)(
    companion: quotes.reflect.Symbol,
    methodName: String,
    targetType: quotes.reflect.TypeRepr,
    fieldName: String
  ): Option[ValidatorInfo.NeedsValidation] = {
    import quotes.reflect.*
    
    val methods = companion.declaredMethods.filter(_.name == methodName)
    MacroDebugger.log(s"    Looking for method '$methodName' in ${companion.fullName}. Candidates: ${methods.map(_.toString).mkString(", ")}")
    
    // Collect all candidate validators (method, primitiveType, errorType, validationKind)
    val candidates = methods.flatMap { method =>
      val companionRef = Ref(companion)
      val methodType = companionRef.select(method).tpe.widen
      MacroDebugger.log(s"      Analyzing method: ${method.toString}")
      MacroDebugger.log(s"        Method type (widened): ${methodType.show}")

      methodType match {
        case MethodType(paramNames, paramTypes, returnType) if paramNames.length == 1 =>
          MacroDebugger.log(s"        Method has one param: ${paramNames.head} of type ${paramTypes.head.show}")
          MacroDebugger.log(s"        Return type: ${returnType.show}")
          analyzeReturnType(returnType, targetType) match {
            case Some((errorType, validationKind)) =>
              Some((method, paramTypes.head, errorType, validationKind))
            case None =>
              MacroDebugger.log(s"        Analyzed return type for ${method.name}, but no matching validation type found.")
              None
          }
        case _ =>
          MacroDebugger.log(s"        Method ${method.name} does not have a single parameter, skipping.")
          None
      }
    }

    // Prefer a candidate whose primitive param type is different from the wrapped target type
    // or is a scala primitive (Int, String, etc.). This helps pick Subtype/NewType helpers
    // that accept raw primitives (e.g. Int) instead of the inner Type alias.
    def isScalaPrimitive(t: TypeRepr): Boolean =
      t.typeSymbol.fullName.startsWith("scala.") || t.typeSymbol.isNoSymbol

    val chosen = candidates.sortBy { case (_, prim, _, _) =>
      val primIsTarget = prim.simplified =:= targetType.simplified
      val score = (if primIsTarget then 1 else 0) + (if isScalaPrimitive(prim) then -1 else 0)
      score
    }.headOption

    chosen.map { case (method, primitiveType, errorType, validationKind) =>
      ValidatorInfo.NeedsValidation(
        fieldName = fieldName,
        primitiveType = primitiveType,
        wrappedType = targetType,
        errorType = errorType,
        companionSymbol = companion,
        methodName = method.name,
        validationKind = validationKind
      )
    }
    .orElse {
      // No candidate found — try to recognize zio.prelude.Subtype / NewType companions.
      // For objects that extend Subtype[Int] the `make` method is provided by the trait.
      // Inspect the companion's ClassDef parents for a reference to Subtype or NewType.
      try {
        companion.tree match {
          case cd: quotes.reflect.ClassDef =>
            val parentNames = cd.parents.map(_.show)
            if (parentNames.exists(p => p.contains("Subtype") || p.contains("NewType"))) {
              import quotes.reflect.*
              MacroDebugger.log(s"    Companion ${companion.fullName} appears to extend Subtype/NewType; synthesizing validator using 'make'")
              Some(ValidatorInfo.NeedsValidation(
                fieldName = fieldName,
                primitiveType = TypeRepr.of[Int],
                wrappedType = targetType,
                errorType = TypeRepr.of[String],
                companionSymbol = companion,
                methodName = "make",
                validationKind = ValidationKind.FromValidation
              ))
            } else None
          case _ => None
        }
      } catch {
        case _: Throwable => None
      }
    }
  }
  
  /**
   * Analyzes a return type to see if it's Validation[E, T] or Either[E, T].
   * Returns Some((errorType, validationKind)) if valid, None otherwise.
   * 
   * Note: Validation[E, T] is a type alias for ZValidation[Nothing, E, T]
   */
  private def analyzeReturnType(using Quotes)(
    returnType: quotes.reflect.TypeRepr,
    expectedWrappedType: quotes.reflect.TypeRepr
  ): Option[(quotes.reflect.TypeRepr, ValidationKind)] = {
    import quotes.reflect.*
    
    MacroDebugger.log(s"          Analyzing return type: ${returnType.show}")
    MacroDebugger.log(s"          Expected wrapped type: ${expectedWrappedType.show}")
    
    returnType match {
      // Match ZValidation[W, E, T] or Validation[E, T] alias (which is ZValidation[Nothing, E, T])
      case AppliedType(tycon, List(logType, errorType, wrappedType)) 
        if tycon.typeSymbol.fullName == "zio.prelude.ZValidation" || tycon.typeSymbol.fullName == "zio.prelude.Validation" =>
        MacroDebugger.log(s"            Return type is ZValidation/Validation. Wrapped: ${wrappedType.show}")
        // Check if wrappedType matches our expected type (allowing for opaque type equality)
        if (wrappedType.simplified =:= expectedWrappedType.simplified) {
          MacroDebugger.log(s"            SUCCESS: Found Validation with matching simplified wrapped type: ${wrappedType.simplified.show}")
          Some((errorType, ValidationKind.FromValidation))
        } else {
          MacroDebugger.log(s"            FAILURE: ZValidation wrapped type ${wrappedType.show} does not match expected ${expectedWrappedType.show}")
          MacroDebugger.log(s"            ... simplified wrapped: ${wrappedType.simplified.show} (fullName: ${if wrappedType.simplified.typeSymbol.isNoSymbol then "NoSymbol" else wrappedType.simplified.typeSymbol.fullName})")
          MacroDebugger.log(s"            ... simplified expected: ${expectedWrappedType.simplified.show} (fullName: ${expectedWrappedType.simplified.typeSymbol.fullName})")
          None
        }
      
      // Match Either[E, T]
      case AppliedType(tycon, List(errorType, wrappedType))
        if tycon.typeSymbol.fullName == "scala.util.Either" =>
        MacroDebugger.log(s"            Return type is Either. Wrapped: ${wrappedType.show}")
        if (wrappedType.simplified =:= expectedWrappedType.simplified) {
          MacroDebugger.log(s"            SUCCESS: Found Either with matching simplified wrapped type: ${wrappedType.simplified.show}")
          Some((errorType, ValidationKind.FromEither))
        } else {
          MacroDebugger.log(s"            FAILURE: Either wrapped type ${wrappedType.show} does not match expected ${expectedWrappedType.show}")
          MacroDebugger.log(s"            ... simplified wrapped: ${wrappedType.simplified.show} (fullName: ${if wrappedType.simplified.typeSymbol.isNoSymbol then "NoSymbol" else wrappedType.simplified.typeSymbol.fullName})")
          MacroDebugger.log(s"            ... simplified expected: ${expectedWrappedType.simplified.show} (fullName: ${expectedWrappedType.simplified.typeSymbol.fullName})")
          None
        }
      
      case other => 
        MacroDebugger.log(s"          Return type is not a known validation type: ${other.show}")
        None
    }
  }
  
  /**
   * Creates a Term that calls the validator on a given argument Term.
   * This generates the actual validation call at macro expansion time.
   */
  def createValidationCall(using Quotes)(
    validatorInfo: ValidatorInfo.NeedsValidation,
    argumentTerm: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    
    val companionSymbol = validatorInfo.companionSymbol.asInstanceOf[Symbol]
    val companionRef    = Ref(companionSymbol)
    // Try to retrieve a declared method symbol; if not present (inherited methods such as
    // those provided by zio.prelude.Subtype), fall back to a Select.unique call which will
    // resolve members by name (including inherited ones).
    val methodCall = try {
      val maybeMethod = companionSymbol.declaredMethod(validatorInfo.methodName).headOption
      maybeMethod match {
        case Some(ms) => companionRef.select(ms).appliedTo(argumentTerm)
        case None =>
          // Fall back to Select.unique to invoke methods inherited from traits (e.g. make)
          Apply(Select.unique(companionRef, validatorInfo.methodName), List(argumentTerm))
      }
    } catch {
      case _: Throwable =>
        // defensive fallback
        Apply(Select.unique(companionRef, validatorInfo.methodName), List(argumentTerm))
    }

    validatorInfo.validationKind match {
      case ValidationKind.FromValidation =>
        // Smart constructor already returns Validation[E, A]
        // which is a type alias for ZValidation[Nothing, E, A]. Use as-is.
        methodCall
        
      case ValidationKind.FromEither =>
        // Smart constructor returns Either[E, A]; convert to Validation[E, A]
        // Validation[E, A] is an alias for ZValidation[Nothing, E, A]
        val validationType   = TypeRepr.of[zio.prelude.Validation.type]
        val validationComp   = validationType.typeSymbol.companionModule
        val validationRef    = Ref(validationComp)
        val fromEitherOpt    = validationComp.declaredMethods.find(_.name == "fromEither")
        val fromEitherMethod = fromEitherOpt.getOrElse {
          report.errorAndAbort("Could not find zio.prelude.Validation.fromEither")
        }

        // Extract E and A from Either[E, A]
        val eitherType = methodCall.tpe.widen.dealias
        val (errorType, resultType) = eitherType match {
          case AppliedType(_, List(e, a)) => (e, a)
          case other =>
            report.errorAndAbort(
              s"Expected Either[E, A] return type for validation method, but found: ${other.show}"
            )
        }

        // Call Validation.fromEither[E, A](eitherValue): Validation[E, A]
        validationRef
          .select(fromEitherMethod)
          .appliedToTypes(List(errorType, resultType))
          .appliedTo(methodCall)
    }
  }
  
  /**
   * Creates a Term that wraps a plain value in ZValidation.succeed.
   * This produces ZValidation[Nothing, String, T] by widening the error channel
   */
  def createSucceedCall(using Quotes)(
    argumentTerm: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    
    // Access ZValidation companion and call succeed
    val zvalidationModule = TypeRepr.of[zio.prelude.ZValidation.type].typeSymbol.companionModule
    val zvalidationRef = Ref(zvalidationModule)
    
    // Get the succeed method
    val succeedMethods = zvalidationModule.declaredMethods.filter(_.name == "succeed")
    val succeedMethod = succeedMethods.head
    
    // Create ZValidation.succeed[A](value) which gives ZValidation[Nothing, Nothing, A]
    val argType = argumentTerm.tpe.widen
    val succeedCall = zvalidationRef
      .select(succeedMethod)
      .appliedToType(argType)
      .appliedTo(argumentTerm)
    
    // Widen the error type from Nothing to String
    // We can call .mapError on the validation to widen the error channel
    // Or use identity function that widens the type
    // Actually, we need to use mapError with an identity-like function
    // Let's use: validation.asInstanceOf[ZValidation[Nothing, String, A]]
    // But that's not safe. Better: use mapError(_ => throw new Exception())
    // which will never be called but widens the type
    
    // Best approach: just use Typed to ascribe the wider type
    val widenedType = TypeRepr.of[zio.prelude.ZValidation]
      .appliedTo(List(TypeRepr.of[Nothing], TypeRepr.of[String], argType))
    
    Typed(
      succeedCall,
      Inferred(widenedType)
    )
  }
}
