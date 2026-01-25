package api

import api.ValidatorInfo.ValidationKind.{FromEither, FromValidation}

import scala.quoted.*
import zio.prelude.ZValidation

/**
 * Generates validated builder for a case class.
 *
 * Usage:
 * ```
 * case class MyClass(field1: Type1, field2: Type2)
 * object MyClass extends ValidatedBuilderGenerator[MyClass]
 * ```
 *
 * This will generate a builder that returns ZValidation[Nothing, String, MyClass]
 */
trait ValidatedBuilderGenerator[T] {
  /**
   * The apply method that returns the builder function.
   * This needs to be called with () to get the curried function chain.
   */
  def apply(): Any
}

object ValidatedBuilderGenerator {

  /**
   * Macro entry point - this needs inline to trigger macro expansion.
   * We're explicit about the return type here.
   */
  inline def derived[T]: ValidatedBuilderGenerator[T] = ${ derivedImpl[T] }

  /**
   * The macro implementation that generates the validated builder.
   */
  def derivedImpl[T: Type](using Quotes): Expr[ValidatedBuilderGenerator[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Ensure it's a case class
    if (!typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${tpe.show} must be a case class")
    }

    // Get the primary constructor
    val constructor = typeSymbol.primaryConstructor
    val constructorType = constructor.tree match {
      case DefDef(_, params, _, _) =>
        params.headOption.getOrElse {
          report.errorAndAbort(s"${tpe.show} must have at least one parameter")
        }
      case _ =>
        report.errorAndAbort(s"Could not extract constructor for ${tpe.show}")
    }

    // Extract field information
    val fields = extractFields(typeSymbol, tpe)

    if (fields.isEmpty) {
      report.errorAndAbort(s"${tpe.show} must have at least one field")
    }

    // Discover validators for each field
    val validatorInfos: List[ValidatorInfo] = fields.map { case (name, fieldType) =>
      SmartConstructorDiscovery.discoverValidator(name, fieldType)
    }

    // Generate the builder function
    val builderFunction = generateBuilderFunction(tpe, typeSymbol, validatorInfos)

    // Create the ValidatedBuilderGenerator instance
    '{
      new ValidatedBuilderGenerator[T] {
        def apply(): Any = ${ builderFunction }
      }
    }
  }

  /**
   * Extracts field names and types from a case class.
   */
  private def extractFields(using Quotes)(
    typeSymbol: quotes.reflect.Symbol,
    tpe: quotes.reflect.TypeRepr
  ): List[(String, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*

    typeSymbol.primaryConstructor.paramSymss.flatten.map { param =>
      val fieldName = param.name
      val fieldType = tpe.memberType(param)
      (fieldName, fieldType)
    }
  }

  /**
   * Generates the curried builder function.
   *
   * Pragmatic approach: Generate a single uncurried function, then curry it at runtime.
   * This avoids all the scoping issues with nested lambda generation.
   */
  private def generateBuilderFunction(using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo]
  ): Expr[Any] = {
    import quotes.reflect.*

    // Generate an uncurried function: (p1, p2, ..., pn) => validateWith(...)
    val primitiveTypes = validatorInfos.map {
      case nv: ValidatorInfo.NeedsValidation /* if nv.validationKind == FromValidation */ => nv.primitiveType.asInstanceOf[TypeRepr]
      // case nv: ValidatorInfo.NeedsValidation if nv.validationKind == FromEither => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    val fieldNames = validatorInfos.map(_.fieldName)

    // Create the uncurried function
    val uncurriedType = MethodType(fieldNames)(
      _ => primitiveTypes,
      _ => TypeRepr.of[ZValidation].appliedTo(List(
        TypeRepr.of[Nothing],
        TypeRepr.of[String],
        targetType
      ))
    )

    val uncurriedLambda = Lambda(
      owner = Symbol.spliceOwner,
      tpe = uncurriedType,
      rhsFn = (sym, params) => {
        val termParams = params.map(_.asInstanceOf[Term])
        val paramSymbols = sym.paramSymss.head
        val symbolTermPairs = paramSymbols.zip(termParams)
        generateValidateWithCall(targetType, targetSymbol, validatorInfos, symbolTermPairs)
      }
    )

    // Now wrap it in a runtime curry helper
    // We'll use Function2.curried, Function3.curried, etc.
    val arity = validatorInfos.length

    if (arity == 1) {
      uncurriedLambda.asExprOf[Any]
    } else if (arity == 2) {
      // Use .curried from Function2
      val uncurriedExpr = uncurriedLambda.asExprOf[Function2[?, ?, ?]]
      '{ $uncurriedExpr.curried }
    } else if (arity == 3) {
      val uncurriedExpr = uncurriedLambda.asExprOf[Function3[?, ?, ?, ?]]
      '{ $uncurriedExpr.curried }
    } else if (arity == 4) {
      val uncurriedExpr = uncurriedLambda.asExprOf[Function4[?, ?, ?, ?, ?]]
      '{ $uncurriedExpr.curried }
    } else {
      report.errorAndAbort(s"Currently only supporting up to 4 parameters, got $arity")
    }
  }

  /**
   * Generates the ZValidation.validateWith call.
   */
  private def generateValidateWithCall(using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo],
    params: List[(quotes.reflect.Symbol, quotes.reflect.Term)]
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // Create validation calls for each parameter
    val validationTerms = validatorInfos.zip(params).map { case (info, (paramSym, paramTerm)) =>
      info match {
        case nv: ValidatorInfo.NeedsValidation =>
          SmartConstructorDiscovery.createValidationCall(nv, paramTerm)
        case _: ValidatorInfo.NoValidation =>
          SmartConstructorDiscovery.createSucceedCall(paramTerm)
      }
    }

    // Create the constructor lambda (vi, vop, vv, vs) => TripleVal(vi, vop, vv, vs)
    val constructorLambda = createConstructorLambda(targetType, targetSymbol, validatorInfos.length)

    // Call ZValidation.validateWith
    val zvalidationModule = TypeRepr.of[zio.prelude.ZValidation.type].typeSymbol.companionModule
    val zvalidationRef = Ref(zvalidationModule)

    // Find validateWith method with matching arity
    val validateWithMethods = zvalidationModule.declaredMethods
      .filter(_.name == "validateWith")

    // For now, use the version that takes up to 4 parameters
    // TODO: Handle more parameters
    if (validationTerms.length > 4) {
      report.errorAndAbort(s"Currently only supporting up to 4 fields, got ${validationTerms.length}")
    }

    // validateWith methods have 3 parameter lists:
    // [type params][validation params][function param]
    // For 2 validations: (5, 2, 1) means 5 type params, 2 validation args, 1 function arg
    val validateWithMethod = validateWithMethods.find { method =>
      method.paramSymss match {
        case typeParams :: validationParams :: functionParam :: Nil =>
          validationParams.length == validationTerms.length && functionParam.length == 1
        case _ => false
      }
    }.getOrElse {
      // Debug: print available methods
      val available = validateWithMethods.map { m =>
        s"validateWith with ${m.paramSymss.map(_.length).mkString(", ")} params"
      }.mkString("; ")
      report.errorAndAbort(s"Could not find validateWith method for ${validationTerms.length} validation parameters. Available: $available")
    }

    // Apply the validation terms and the constructor lambda in separate argument lists
    // validateWith[TypeParams...](v1, v2, ...)(constructorFunction)
    // The method has 3 parameter lists: [type params][validation params][function param]

    // Select the method
    val methodSelect = zvalidationRef.select(validateWithMethod)

        // Apply type parameters - we need to infer these from the validation terms
    // For 2 validations returning ZValidation[W, E, A1] and ZValidation[W, E, A2],
    // validateWith needs [W, E, A1, A2, Z] where Z is the result type.
    // Normalise aliases (e.g. Validation[E, A]) to ZValidation before matching.
    val validationTypes = validationTerms.map(_.tpe.widen.dealias)

    // Extract W, E, and A types from each validation; ensure we really have
    // ZValidation[W, E, A] so we don't accidentally treat some other 3-arg
    // type constructor as a validation.
        val typeTuples = validationTypes.map {
      case AppliedType(tycon, List(w, e, a))
          if tycon.typeSymbol.fullName == "zio.prelude.ZValidation" =>
        (w, e, a)
      case other =>
        report.errorAndAbort(
          s"Expected ZValidation[W, E, A] for validation term, but found: ${other.show}"
        )
    }


    // Use String as the unified error type (all our validators should use String)
    val wType = TypeRepr.of[Nothing]
    val eType = TypeRepr.of[String]

    // Extract just the A types
    val aTypes = typeTuples.map(_._3)

    // Build the full type parameter list: [W, E, A1, A2, ..., Result]
    val allTypeParams = wType :: eType :: (aTypes :+ targetType)


    // Apply all parameter lists
    methodSelect
      .appliedToTypes(allTypeParams)
      .appliedToArgs(validationTerms)
      .appliedToArgs(List(constructorLambda))
  }

  /**
   * Creates the constructor lambda: (v1, v2, ..., vn) => CaseClass(v1, v2, ..., vn)
   */
  private def createConstructorLambda(using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    paramCount: Int
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // Get the validated types (the wrapped types)
    val wrappedTypes = targetSymbol.primaryConstructor.paramSymss.flatten.map { param =>
      targetType.memberType(param)
    }

    // Create parameter names
    val paramNames = (1 to paramCount).map(i => s"v$i").toList

    // Create the lambda type
    val lambdaType = MethodType(paramNames)(
      _ => wrappedTypes,
      _ => targetType
    )

    // Create the lambda
    Lambda(
      owner = Symbol.spliceOwner,
      tpe = lambdaType,
      rhsFn = (sym, params) => {
        val termParams = params.map(_.asInstanceOf[Term])
        val companionRef = Ref(targetSymbol.companionModule)
        val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
        companionRef.select(applyMethod).appliedToArgs(termParams)
      }
    )
  }
}
