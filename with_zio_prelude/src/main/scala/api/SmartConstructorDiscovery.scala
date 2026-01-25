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
        owner.declarations.find(s => (s.name.init == typeSymbol.name || s.name == typeSymbol.name) && s.flags.is(Flags.Module)) match {
          case Some(sibling) => 
            MacroDebugger.log(s"  Found sibling companion module: ${sibling.fullName}")
            Some(sibling)
          case None =>
            MacroDebugger.log(s"  No sibling companion module found in owner ${owner.fullName}.")
            None
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
    
    methods.collectFirst {
      case method =>
        val companionRef = Ref(companion)
        val methodType = companionRef.select(method).tpe.widen
        MacroDebugger.log(s"      Analyzing method: ${method.toString}")
        MacroDebugger.log(s"        Method type (widened): ${methodType.show}")
        
        methodType match {
          case MethodType(paramNames, paramTypes, returnType) if paramNames.length == 1 =>
            MacroDebugger.log(s"        Method has one param: ${paramNames.head} of type ${paramTypes.head.show}")
            MacroDebugger.log(s"        Return type: ${returnType.show}")
            // Check if return type is Validation[E, T] or Either[E, T]
            analyzeReturnType(returnType, targetType) match {
              case Some((errorType, validationKind)) =>
                val primitiveType = paramTypes.head
                Some(ValidatorInfo.NeedsValidation(
                  fieldName = fieldName,
                  primitiveType = primitiveType,
                  wrappedType = targetType,
                  errorType = errorType,
                  companionSymbol = companion,
                  methodName = methodName,
                  validationKind = validationKind
                ))
              case None =>
                MacroDebugger.log(s"        Analyzed return type for ${method.name}, but no matching validation type found.")
                None
            }
          case _ => 
            MacroDebugger.log(s"        Method ${method.name} does not have a single parameter, skipping.")
            None
        }
    }.flatten
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
    val methodSymbol    = companionSymbol.declaredMethod(validatorInfo.methodName).head
    val methodCall      = companionRef.select(methodSymbol).appliedTo(argumentTerm)

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
