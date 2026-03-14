package api

import api.ValidatorInfo.ValidationKind.{FromEither, FromValidation}

import scala.quoted.*
import zio.prelude.ZValidation
import zio.prelude.AssociativeBothCovariantOps

/**
 * Generates validated builder for a case class using named tuples.
 *
 * Usage:
 * ```
 * case class MyClass(field1: Type1, field2: Type2)
 * object MyClass:
 *   val validator = ValidatedBuilderGenerator.derived[MyClass]
 * ```
 *
 * Then use as:
 * ```
 * MyClass.validator().field1(value1).field2(value2)
 * // Returns: ZValidation[Nothing, String, MyClass]
 * ```
 * 
 * The builder uses primitive/unwrapped types for parameters and calls smart constructors internally.
 * For example, if field2 has type Op with a smart constructor Op.apply(String): Either[String, Op],
 * the builder will accept String and call the smart constructor.
 */
trait ValidatedBuilderGenerator[T] {
  type Builder
  def apply(): Builder
}

object ValidatedBuilderGenerator {
  import scala.NamedTuple
  import scala.NamedTuple.*
  import scala.NamedTuple.Split
  import zio.prelude.ZValidation
  import api.ValidatorInfo.ValidationKind

  type Tup[T] = NamedTuple.From[T]

  import scala.quoted.*
  import scala.deriving.Mirror

  /**
   * Match-type for the validated builder named-tuple.
   * Mirrors BuilderGenerator.BuilderFor but returns functions that produce
   * `ZValidation[Nothing, String, T]` as the final result.
   */
  // Generic ValidatedBuilderFor parameterised by the error channel type E
  type ValidatedBuilder[T, E] = ValidatedBuilderFor[
    Tuple.Head[Split[Tup[T], 1]],
    Tuple.Last[Split[Tup[T], 1]],
    T,
    E
  ]

  type ValidatedBuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T, E] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
        NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => ZValidation[Nothing, E, T]]]
      case h *: t =>
        NamedTuple[
          NamedTuple.Names[H],
          Tuple1[
            Tuple.Head[NamedTuple.DropNames[H]] => ValidatedBuilderFor[
              Tuple.Head[Split[R, 1]],
              Tuple.Last[Split[R, 1]],
              T,
              E
            ]
          ]
        ]
    }

  /**
   * Macro entry point - this needs inline to trigger macro expansion.
   * Returns an object with an apply() method that returns the nested Tuple1 builder.
   */
  inline def derived[T]: ValidatedBuilderGenerator[T] = ${ derivedImpl[T] }

  /**
   * Returns the NamedTuple-based validated builder directly for `T`.
   * The precise named-tuple shape is supplied by the macro so callers can
   * use field access like `.i(...).op(...)` without manual casts.
   */
  transparent inline def builder[T] = ${ ValidatedBuilderGenerator.builderImpl[T] }

  
  /**
   * The macro implementation that generates the validated builder.
   * Returns an object whose apply method yields the nested Tuple1 builder.
   */
  def derivedImpl[T: Type](using Quotes): Expr[ValidatedBuilderGenerator[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    // Ensure it's a case class
    if (!typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${tpe.show} must be a case class")
    }

    // Determine arity from the primary constructor term parameters
    val ctorSym = typeSymbol.primaryConstructor
    val termParamLists = ctorSym.paramSymss.map(_.filter(_.isTerm)).filter(_.nonEmpty)
    val arity: Int = termParamLists.headOption.map(_.length).getOrElse {
      report.errorAndAbort(s"Could not determine constructor arity for ${tpe.show}")
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

    // (builder expression is generated later after we bind the unified error type EU)
    // Generate the builder expression based on arity
    // val builderExpr = generateBuilderExpression[T](tpe, typeSymbol, validatorInfos, arity)

    // Compute the unified error type (union of field validators' error types)
    val errorTypeReprs: List[TypeRepr] = validatorInfos.collect {
      case nv: ValidatorInfo.NeedsValidation => nv.errorType.asInstanceOf[TypeRepr]
    }.distinct

    val unifiedErrorType: quotes.reflect.TypeRepr = errorTypeReprs match {
      case Nil => TypeRepr.of[Nothing]
      case h :: Nil => h
      case hs => hs.reduce((a, b) => OrType(a, b))
    }

    // Now bind T and EU in scope and generate the builder expression that uses EU
    TypeRepr.of[T].asType match {
      case '[t] =>
        unifiedErrorType.asType match {
          case '[eu] =>
            // builderExpr will be created once EU is bound here using the concrete eu TypeRepr
            val builderExpr = generateBuilderWithErrorType(unifiedErrorType, tpe, typeSymbol, validatorInfos, arity)
             // Build NamedTuple wrapper same as before, using the generated builderExpr
             arity match {
               case 1 =>
                 val prim0 = validatorInfos(0) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 prim0.asType match {
                   case '[p0] =>
                    '{
                      type PrimTypes = p0 *: EmptyTuple
                      type PrimNT = NamedTuple[NamedTuple.Names[Tup[t]], PrimTypes]
                      new ValidatedBuilderGenerator[t] {
                        type Builder = ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t, eu]
                        def apply(): Builder = ${ builderExpr }.asInstanceOf[Builder]
                      }
                    }.asExprOf[ValidatedBuilderGenerator[T]]
                   case _ =>
                     report.errorAndAbort("Could not compute primitive type for field 0 when deriving builder for " + Type.show[t])
                 }

               case 2 =>
                 val prim0 = validatorInfos(0) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 val prim1 = validatorInfos(1) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 (prim0.asType, prim1.asType) match {
                   case ('[p0], '[p1]) =>
                    '{
                      type PrimTypes = p0 *: p1 *: EmptyTuple
                      type PrimNT = NamedTuple[NamedTuple.Names[Tup[t]], PrimTypes]
                      new ValidatedBuilderGenerator[t] {
                        type Builder = ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t, eu]
                        def apply(): Builder = ${ builderExpr }.asInstanceOf[Builder]
                      }
                    }.asExprOf[ValidatedBuilderGenerator[T]]
                   case _ =>
                     report.errorAndAbort("Could not compute primitive types for fields 0,1 when deriving builder for " + Type.show[t])
                 }

               case 3 =>
                 val prim0 = validatorInfos(0) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 val prim1 = validatorInfos(1) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 val prim2 = validatorInfos(2) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 (prim0.asType, prim1.asType, prim2.asType) match {
                   case ('[p0], '[p1], '[p2]) =>
                    '{
                      type PrimTypes = p0 *: p1 *: p2 *: EmptyTuple
                      type PrimNT = NamedTuple[NamedTuple.Names[Tup[t]], PrimTypes]
                      new ValidatedBuilderGenerator[t] {
                        type Builder = ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t, eu]
                        def apply(): Builder = ${ builderExpr }.asInstanceOf[Builder]
                      }
                    }.asExprOf[ValidatedBuilderGenerator[T]]
                   case _ =>
                     report.errorAndAbort("Could not compute primitive types for fields 0,1,2 when deriving builder for " + Type.show[t])
                 }

               case n =>
                 report.errorAndAbort(s"Unsupported arity $n for ${tpe.show}. Extend generateBuilderExpression / caseclassN handling.")
             }
          case _ =>
            report.errorAndAbort("Could not compute builder type for T")
        }
      case _ =>
        report.errorAndAbort("Could not compute builder type for T")
    }
  }

  // Legacy helper kept for debugging; not used by inline builder anymore
  def builderImplAny[T: Type](using Quotes): Expr[scala.NamedTuple.AnyNamedTuple] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    if (!typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${tpe.show} must be a case class")
    }

    val ctorSym = typeSymbol.primaryConstructor
    val termParamLists = ctorSym.paramSymss.map(_.filter(_.isTerm)).filter(_.nonEmpty)
    val arity: Int = termParamLists.headOption.map(_.length).getOrElse {
      report.errorAndAbort(s"Could not determine constructor arity for ${tpe.show}")
    }

    val fields = extractFields(typeSymbol, tpe)
    if (fields.isEmpty) {
      report.errorAndAbort(s"${tpe.show} must have at least one field")
    }

    val validatorInfos: List[ValidatorInfo] = fields.map { case (name, fieldType) =>
      SmartConstructorDiscovery.discoverValidator(name, fieldType)
    }

    val builderExpr = generateBuilderExpression[String, T](tpe, typeSymbol, validatorInfos, arity)

    TypeRepr.of[T].asType match {
      case '[t] =>
        '{ ${ builderExpr } .asInstanceOf[scala.NamedTuple.AnyNamedTuple] }
      case _ =>
        report.errorAndAbort("Could not compute NamedTuple-based builder type for T")
    }
  }

  // Pragmatic builder: we return AnyNamedTuple at the type level, but
  // internally cast to the precise NamedTuple/ValidatedBuilderFor shape
  // parameterised by the primitive input types discovered for each field.
  def builderImpl[T: Type](using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    if (!typeSymbol.flags.is(Flags.Case)) {
      report.errorAndAbort(s"${tpe.show} must be a case class")
    }

    val ctorSym = typeSymbol.primaryConstructor
    val termParamLists = ctorSym.paramSymss.map(_.filter(_.isTerm)).filter(_.nonEmpty)
    val arity: Int = termParamLists.headOption.map(_.length).getOrElse {
      report.errorAndAbort(s"Could not determine constructor arity for ${tpe.show}")
    }

    val fields = extractFields(typeSymbol, tpe)
    if (fields.isEmpty) {
      report.errorAndAbort(s"${tpe.show} must have at least one field")
    }

    val validatorInfos: List[ValidatorInfo] = fields.map { case (name, fieldType) =>
      SmartConstructorDiscovery.discoverValidator(name, fieldType)
    }

    // Generate the builder expression based on arity
    // val builderExpr = generateBuilderExpression[T](tpe, typeSymbol, validatorInfos, arity)

    // Inlined builderImpl path: compute unified error type and bind eu
    val errorTypeReprs2: List[TypeRepr] = validatorInfos.collect {
      case nv: ValidatorInfo.NeedsValidation => nv.errorType.asInstanceOf[TypeRepr]
    }.distinct

    val unifiedErrorType2: quotes.reflect.TypeRepr = errorTypeReprs2 match {
      case Nil => TypeRepr.of[Nothing]
      case h :: Nil => h
      case hs => hs.reduce((a, b) => OrType(a, b))
    }

    TypeRepr.of[T].asType match {
      case '[t] =>
        unifiedErrorType2.asType match {
          case '[eu] =>
            val builderExpr2 = generateBuilderWithErrorType(unifiedErrorType2, tpe, typeSymbol, validatorInfos, arity)
            arity match {
               case 1 =>
                 val prim0 = validatorInfos(0) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 prim0.asType match {
                   case '[p0] =>
                    '{
                      type PrimTypes = p0 *: EmptyTuple
                      type PrimNT    = NamedTuple[NamedTuple.Names[Tup[t]], PrimTypes]
                      ${ builderExpr2 }.asInstanceOf[
                        ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t, eu]
                      ]
                    }
                   case _ =>
                     report.errorAndAbort("Could not compute primitive type for field 0 when building builder for " + Type.show[t])
                 }

               case 2 =>
                 val prim0 = validatorInfos(0) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 val prim1 = validatorInfos(1) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 (prim0.asType, prim1.asType) match {
                   case ('[p0], '[p1]) =>
                    '{
                      type PrimTypes = p0 *: p1 *: EmptyTuple
                      type PrimNT    = NamedTuple[NamedTuple.Names[Tup[t]], PrimTypes]
                      ${ builderExpr2 }.asInstanceOf[
                        ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t, eu]
                      ]
                    }
                   case _ =>
                     report.errorAndAbort("Could not compute primitive types for fields 0,1 when building builder for " + Type.show[t])
                 }

               case 3 =>
                 val prim0 = validatorInfos(0) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 val prim1 = validatorInfos(1) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 val prim2 = validatorInfos(2) match {
                   case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
                   case nv: ValidatorInfo.NoValidation   => nv.plainType.asInstanceOf[TypeRepr]
                 }
                 (prim0.asType, prim1.asType, prim2.asType) match {
                   case ('[p0], '[p1], '[p2]) =>
                    '{
                      type PrimTypes = p0 *: p1 *: p2 *: EmptyTuple
                      type PrimNT    = NamedTuple[NamedTuple.Names[Tup[t]], PrimTypes]
                      ${ builderExpr2 }.asInstanceOf[
                        ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t, eu]
                      ]
                    }
                   case _ =>
                     report.errorAndAbort("Could not compute primitive types for fields 0,1,2 when building builder for " + Type.show[t])
                 }

               case n =>
                 quotes.reflect.report.errorAndAbort(s"Unsupported arity $n when building typed validated builder for ${Type.show[T]}")
             }
          case _ =>
            quotes.reflect.report.errorAndAbort("Could not compute builder type for T")
        }
      case _ =>
        quotes.reflect.report.errorAndAbort("Could not compute builder type for T")
    }
  }

  /**
   * Generate the builder expression based on arity.
   */
  private def generateBuilderExpression[EU: Type, T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo],
    arity: Int
  ): Expr[Any] = {
    import quotes.reflect.*

    arity match {
      case 1 =>
        val info = validatorInfos.head
        generateArity1Builder[EU, T](targetType, targetSymbol, info)

      case 2 =>
        val (info0, info1) = (validatorInfos(0), validatorInfos(1))
        generateArity2Builder[EU, T](targetType, targetSymbol, info0, info1)

      case 3 =>
        val (info0, info1, info2) = (validatorInfos(0), validatorInfos(1), validatorInfos(2))
        generateArity3Builder[EU, T](targetType, targetSymbol, info0, info1, info2)

      case n =>
        report.errorAndAbort(
          s"Unsupported arity $n for ${targetType.show}. Extend generateBuilderExpression / caseclassN handling."
        )
    }
  }
  /**
   * Helper functions to wrap curried functions in Tuple1 for named tuple access.
   * These match the pattern from BuilderGenerator.scala
   */
  def caseclass1[T, T0, E](transform: T0 => ZValidation[Nothing, E, T]): Tuple1[T0 => ZValidation[Nothing, E, T]] = Tuple1(transform)
  def caseclass2[T, T0, T1, E](transform: (T0, T1) => ZValidation[Nothing, E, T]) =
    Tuple1(transform.curried.andThen(a => Tuple1(a)))
  def caseclass3[T, T0, T1, T2, E](transform: (T0, T1, T2) => ZValidation[Nothing, E, T]) =
    Tuple1(transform.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply))))
  def caseclass4[T, T0, T1, T2, T3, E](transform: (T0, T1, T2, T3) => ZValidation[Nothing, E, T]) =
    Tuple1(transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply))))))
  def caseclass5[T, T0, T1, T2, T3, T4, E](transform: (T0, T1, T2, T3, T4) => ZValidation[Nothing, E, T]) =
    Tuple1(transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply))))))))

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
   * Generate builder for arity 1.
   */
  private def generateArity1Builder[EU: Type, T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    info: ValidatorInfo
  ): Expr[Any] = {
    import quotes.reflect.*
    generateBuilderWithErrorType(TypeRepr.of[EU], targetType, targetSymbol, List(info), 1)
  }

  /** 
   * Generate builder for arity 2.
   */
  private def generateArity2Builder[EU: Type, T: Type](using Quotes)(
     targetType: quotes.reflect.TypeRepr,
     targetSymbol: quotes.reflect.Symbol,
     info0: ValidatorInfo,
     info1: ValidatorInfo
   ): Expr[Any] = {
     import quotes.reflect.*
     generateBuilderWithErrorType(TypeRepr.of[EU], targetType, targetSymbol, List(info0, info1), 2)
   }

  /**
   * Generate builder for arity 3.
   */
  private def generateArity3Builder[EU: Type, T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    info0: ValidatorInfo,
    info1: ValidatorInfo,
    info2: ValidatorInfo
  ): Expr[Any] = {
    import quotes.reflect.*
    generateBuilderWithErrorType(TypeRepr.of[EU], targetType, targetSymbol, List(info0, info1, info2), 3)
  }

  /**
   * Generate validation expression for a field.
   */
  private def generateValidationExpr[EU: Type, W: Type](using Quotes)(
    info: ValidatorInfo,
    paramExpr: Expr[Any]
  ): Expr[ZValidation[Nothing, EU, W]] = {
    import quotes.reflect.*

    info match {
      case nv: ValidatorInfo.NeedsValidation =>
        nv.primitiveType.asInstanceOf[TypeRepr].asType match {
          case '[p] =>
            nv.wrappedType.asInstanceOf[TypeRepr].asType match {
              case '[w] =>
                val typedParam = paramExpr.asExprOf[p]
                val companionSymbol = nv.companionSymbol.asInstanceOf[Symbol]
                val companionRef = Ref(companionSymbol)
                val methodSymbol = companionSymbol.declaredMethod(nv.methodName).head

                nv.validationKind match {
                  case FromEither =>
                    val eitherExpr = Apply(Select(companionRef, methodSymbol), List(typedParam.asTerm)).asExprOf[Either[Any, w]]
                    val base = '{ ZValidation.fromEither($eitherExpr) }
                    // Map/convert error type to EU and narrow result to W
                    '{ $base.mapError((e: Any) => e.asInstanceOf[EU]).asInstanceOf[ZValidation[Nothing, EU, w]] }.asInstanceOf[Expr[ZValidation[Nothing, EU, W]]]

                  case FromValidation =>
                    val call = Apply(Select(companionRef, methodSymbol), List(typedParam.asTerm))
                    val base = call.asExprOf[ZValidation[Nothing, Any, w]]
                    '{ $base.mapError((e: Any) => e.asInstanceOf[EU]).asInstanceOf[ZValidation[Nothing, EU, w]] }.asInstanceOf[Expr[ZValidation[Nothing, EU, W]]]
                }
              case _ =>
                report.errorAndAbort("Could not match wrapped type")
            }
          case _ =>
            report.errorAndAbort("Could not match primitive type")
        }

      case nv: ValidatorInfo.NoValidation =>
        '{ ZValidation.succeed($paramExpr).asInstanceOf[ZValidation[Nothing, EU, W]] }
    }
  }

  /**
   * Create an instance of the case class with validated values.
   * This method is removed since we're generating the constructor calls inline.
   */

  /**
   * Generates the ZValidation.validateWith call from a list of parameter Terms.
   * Recreated here because it was accidentally removed by earlier edits.
   */
  private def generateValidateWithCallFromTerms(using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo],
    paramTerms: List[quotes.reflect.Term]
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    // Create validation calls for each parameter
    val validationTerms = validatorInfos.zip(paramTerms).map { case (info, paramTerm) =>
      info match {
        case nv: ValidatorInfo.NeedsValidation =>
          SmartConstructorDiscovery.createValidationCall(nv, paramTerm)
        case _: ValidatorInfo.NoValidation =>
          SmartConstructorDiscovery.createSucceedCall(paramTerm)
      }
    }

    // Create the constructor lambda (v1, v2, ..., vn) => CaseClass(v1, v2, ..., vn)
    val constructorLambda = createConstructorLambda(targetType, targetSymbol, validatorInfos.length)

    // Call ZValidation.validateWith
    val zvalidationModule = TypeRepr.of[zio.prelude.ZValidation.type].typeSymbol.companionModule
    val zvalidationRef = Ref(zvalidationModule)

    // Find validateWith method with matching arity
    val validateWithMethods = zvalidationModule.declaredMethods.filter(_.name == "validateWith")

    if (validationTerms.length > 4) {
      report.errorAndAbort(s"Currently only supporting up to 4 fields, got ${validationTerms.length}")
    }

    val validateWithMethod = validateWithMethods.find { method =>
      method.paramSymss match {
        case typeParams :: validationParams :: functionParam :: Nil =>
          validationParams.length == validationTerms.length && functionParam.length == 1
        case _ => false
      }
    }.getOrElse {
      val available = validateWithMethods.map { m => s"validateWith with ${m.paramSymss.map(_.length).mkString(", ")} params" }.mkString("; ")
      report.errorAndAbort(s"Could not find validateWith method for ${validationTerms.length} validation parameters. Available: $available")
    }

    val methodSelect = zvalidationRef.select(validateWithMethod)

    // Infer type params: W, E, A1, A2, ..., Result
    val validationTypes = validationTerms.map(_.tpe.widen.dealias)
    val typeTuples = validationTypes.map {
      case AppliedType(tycon, List(w, e, a)) if tycon.typeSymbol.fullName == "zio.prelude.ZValidation" => (w, e, a)
      case other => report.errorAndAbort(s"Expected ZValidation[W, E, A] for validation term, but found: ${other.show}")
    }

    val wType = TypeRepr.of[Nothing]
    val eType = TypeRepr.of[Nothing]
    val aTypes = typeTuples.map(_._3)
    val allTypeParams = wType :: eType :: (aTypes :+ targetType)

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

  /**
   * NOTE: The concrete implementation that generated builder expressions
   * with a bound error type was implemented originally further down.
   * For the purposes of this change we'll rely on the minimal working
   * implementations above and the helper generateValidationExpr to
   * compose the validations. If further specialization is needed
   * (e.g., using companion declaredMethod directly), it can be added
   * carefully with proper term/expr conversions.
   */
  /**
   * Generate concrete builder expression with a bound unified error type `eu`.
   * Supports arities 1..3 for the tests.
   */
  private def generateBuilderWithErrorType(using Quotes)(
    euRepr: quotes.reflect.TypeRepr,
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo],
    arity: Int
  ): Expr[Any] = {
    import quotes.reflect.*

    euRepr.asType match {
      case '[eu] =>
        targetType.asType match {
          case '[t] =>
            arity match {
              case 1 =>
                val info = validatorInfos.head
                val prim = info match { case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                prim.asType match {
                  case '[p0] =>
                    val fieldType = info match { case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                    fieldType.asType match {
                      case '[f0] =>
                        // constructor lambda term -> Expr
                        val companionRef = Ref(targetSymbol.companionModule)
                        val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
                        val constructorTerm = Lambda(owner = Symbol.spliceOwner, tpe = MethodType(List("x"))(_ => List(fieldType), _ => targetType), rhsFn = (sym, params) => companionRef.select(applyMethod).appliedToArgs(params.asInstanceOf[List[Term]]))
                        val constructorExpr = constructorTerm.asExprOf[f0 => t]
                        '{
                          val raw = (a0: p0) => {
                            val v0 = ${ generateValidationExpr[eu, f0](info, '{ a0 }) }
                            v0.map(${ constructorExpr })
                          }
                          caseclass1(raw)
                        }
                      case _ => report.errorAndAbort("Could not match field type for arity=1")
                    }
                  case _ => report.errorAndAbort("Could not compute primitive type for field 0")
                }

              case 2 =>
                val info0 = validatorInfos(0); val info1 = validatorInfos(1)
                val prim0 = info0 match { case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                val prim1 = info1 match { case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                (prim0.asType, prim1.asType) match {
                  case ('[p0], '[p1]) =>
                    val field0 = info0 match { case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                    val field1 = info1 match { case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                    (field0.asType, field1.asType) match {
                      case ('[f0], '[f1]) =>
                        val companionRef = Ref(targetSymbol.companionModule)
                        val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
                        val constructorTerm = Lambda(owner = Symbol.spliceOwner, tpe = MethodType(List("x","y"))(_ => List(field0, field1), _ => targetType), rhsFn = (sym, params) => companionRef.select(applyMethod).appliedToArgs(params.asInstanceOf[List[Term]]))
                        val constructorExpr = constructorTerm.asExprOf[(f0, f1) => t]
                        '{
                          val raw = (a0: p0, a1: p1) => {
                            val v0 = ${ generateValidationExpr[eu, f0](info0, '{ a0 }) }
                            val v1 = ${ generateValidationExpr[eu, f1](info1, '{ a1 }) }
                            v0.zipWith(v1)(${ constructorExpr })
                          }
                          caseclass2(raw)
                        }
                      case _ => report.errorAndAbort("Could not match field types for arity=2")
                    }
                  case _ => report.errorAndAbort("Could not compute primitive types for fields in arity=2")
                }

              case 3 =>
                val info0 = validatorInfos(0); val info1 = validatorInfos(1); val info2 = validatorInfos(2)
                val prim0 = info0 match { case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                val prim1 = info1 match { case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                val prim2 = info2 match { case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                (prim0.asType, prim1.asType, prim2.asType) match {
                  case ('[p0], '[p1], '[p2]) =>
                    val field0 = info0 match { case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                    val field1 = info1 match { case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                    val field2 = info2 match { case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]; case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr] }
                    (field0.asType, field1.asType, field2.asType) match {
                      case ('[f0], '[f1], '[f2]) =>
                        val companionRef = Ref(targetSymbol.companionModule)
                        val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
                        val constructorTerm = Lambda(owner = Symbol.spliceOwner, tpe = MethodType(List("x","y","z"))(_ => List(field0, field1, field2), _ => targetType), rhsFn = (sym, params) => companionRef.select(applyMethod).appliedToArgs(params.asInstanceOf[List[Term]]))
                        val constructorExpr = constructorTerm.asExprOf[(f0, f1, f2) => t]
                        '{
                          val raw = (a0: p0, a1: p1, a2: p2) => {
                            val v0 = ${ generateValidationExpr[eu, f0](info0, '{ a0 }) }
                            val v1 = ${ generateValidationExpr[eu, f1](info1, '{ a1 }) }
                            val v2 = ${ generateValidationExpr[eu, f2](info2, '{ a2 }) }
                            ZValidation.validateWith(v0, v1, v2)(${ constructorExpr })
                          }
                          caseclass3(raw)
                        }
                      case _ => report.errorAndAbort("Could not match field types for arity=3")
                    }
                  case _ => report.errorAndAbort("Could not compute primitive types for fields in arity=3")
                }

              case n =>
                report.errorAndAbort(s"Unsupported arity $n when generating builder")
            }
          case _ => report.errorAndAbort("Could not match target type in generateBuilderWithErrorType")
        }
      case _ => report.errorAndAbort("Could not match eu type in generateBuilderWithErrorType")
    }
  }
}
