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
  type ValidatedBuilder[T] = ValidatedBuilderFor[
    Tuple.Head[Split[Tup[T], 1]],
    Tuple.Last[Split[Tup[T], 1]],
    T
  ]

  type ValidatedBuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
        NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => ZValidation[Nothing, String, T]]]
      case h *: t =>
        NamedTuple[
          NamedTuple.Names[H],
          Tuple1[
            Tuple.Head[NamedTuple.DropNames[H]] => ValidatedBuilderFor[
              Tuple.Head[Split[R, 1]],
              Tuple.Last[Split[R, 1]],
              T
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
   * Properly typed to support NamedTuple field access syntax.
   *
   * Note: this macro returns a concrete NamedTuple-typed expression (constructed
   * using the primitive input types), so we let the inferred type come from the
   * macro result rather than coercing to the generic alias.
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

    // Generate the builder expression based on arity
    val builderExpr = generateBuilderExpression[T](tpe, typeSymbol, validatorInfos, arity)

    // Build a NamedTuple type that uses the primitive input types (for smart constructors)
    // while preserving the original field NAMES from `Tup[T]`. Then cast the generated
    // Tuple1 chain to the precisely-typed `ValidatedBuilderFor[...]` so callers can
    // use the named-field syntax with primitive parameter types.
    TypeRepr.of[T].asType match {
      case '[t] =>
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
                    type Builder = ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t]
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
                    type Builder = ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t]
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
                    type Builder = ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t]
                    def apply(): Builder = ${ builderExpr }.asInstanceOf[Builder]
                  }
                }.asExprOf[ValidatedBuilderGenerator[T]]
              case _ =>
                report.errorAndAbort("Could not compute primitive types for fields 0,1,2 when deriving builder for " + Type.show[t])
            }

          case n =>
            report.errorAndAbort(s"Unsupported arity $n when building typed validated builder for ${Type.show[T]}")
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

    val builderExpr = generateBuilderExpression[T](tpe, typeSymbol, validatorInfos, arity)

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

    val builderExpr = generateBuilderExpression[T](tpe, typeSymbol, validatorInfos, arity)

    TypeRepr.of[T].asType match {
      case '[t] =>
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
                  ${ builderExpr }
                    // .asInstanceOf[
                    //   ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t]
                    // ]
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
                  ${ builderExpr }
                    // .asInstanceOf[
                    //   ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t]
                    // ]
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
                  ${ builderExpr }
                    // .asInstanceOf[
                    //   ValidatedBuilderFor[Tuple.Head[Split[PrimNT, 1]], Tuple.Last[Split[PrimNT, 1]], t]
                    // ]
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
  }

  /**
   * Generate the builder expression based on arity.
   */
  private def generateBuilderExpression[T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo],
    arity: Int
  ): Expr[Any] = {
    import quotes.reflect.*

    arity match {
      case 1 =>
        val info = validatorInfos.head
        generateArity1Builder[T](targetType, targetSymbol, info)

      case 2 =>
        val (info0, info1) = (validatorInfos(0), validatorInfos(1))
        generateArity2Builder[T](targetType, targetSymbol, info0, info1)

      case 3 =>
        val (info0, info1, info2) = (validatorInfos(0), validatorInfos(1), validatorInfos(2))
        generateArity3Builder[T](targetType, targetSymbol, info0, info1, info2)

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
  def caseclass1[T, T0](transform: T0 => ZValidation[Nothing, String, T]): Tuple1[T0 => ZValidation[Nothing, String, T]] = Tuple1(transform)
  def caseclass2[T, T0, T1](transform: (T0, T1) => ZValidation[Nothing, String, T]) = 
    Tuple1(transform.curried.andThen(a => Tuple1(a)))
  def caseclass3[T, T0, T1, T2](transform: (T0, T1, T2) => ZValidation[Nothing, String, T]) = 
    Tuple1(transform.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply))))
  def caseclass4[T, T0, T1, T2, T3](transform: (T0, T1, T2, T3) => ZValidation[Nothing, String, T]) = 
    Tuple1(transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply))))))
  def caseclass5[T, T0, T1, T2, T3, T4](transform: (T0, T1, T2, T3, T4) => ZValidation[Nothing, String, T]) = 
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
  private def generateArity1Builder[T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    info: ValidatorInfo
  ): Expr[Any] = {
    import quotes.reflect.*

    val primitiveType = info match {
      case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    // Get field name
    val fieldName = info.fieldName

    // Get the field type for the constructor
    val fieldType = info match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    primitiveType.asType match {
      case '[p0] =>
        fieldType.asType match {
          case '[f0] =>
            targetType.asType match {
              case '[t] =>
                // Generate the constructor as a lambda that wraps the case class apply method
                val companionRef = Ref(targetSymbol.companionModule)
                val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
                
                // Create a lambda: (f0) => T(...)
                val constructorTerm = Lambda(
                  owner = Symbol.spliceOwner,
                  tpe = MethodType(List("x"))(
                    _ => List(fieldType),
                    _ => targetType
                  ),
                  rhsFn = (sym, params) => {
                    companionRef.select(applyMethod).appliedToArgs(params.asInstanceOf[List[Term]])
                  }
                )
                
                // Convert to Expr
                val constructorExpr = constructorTerm.asExprOf[f0 => t]
                
                '{
                  val raw: p0 => ZValidation[Nothing, String, t] = (a0: p0) => {
                    val v0 = ${ generateValidationExpr(info, '{ a0 }) }.asInstanceOf[ZValidation[Nothing, String, f0]]
                    v0.map(${ constructorExpr })
                  }
                  val chain = caseclass1(raw)
                  chain
                }
              case _ =>
                quotes.reflect.report.errorAndAbort("Could not match target type")
            }
          case _ =>
            quotes.reflect.report.errorAndAbort("Could not match field type")
        }
      case _ =>
        quotes.reflect.report.errorAndAbort("Could not match primitive type")
    }
  }

  /** 
   * Generate builder for arity 2.
   */
  private def generateArity2Builder[T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    info0: ValidatorInfo,
    info1: ValidatorInfo
  ): Expr[Any] = {
    import quotes.reflect.*

    val primitiveType0 = info0 match {
      case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
    val primitiveType1 = info1 match {
      case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    // Get field names
    val fieldName0 = info0.fieldName
    val fieldName1 = info1.fieldName

    // Get field types for the constructor
    val fieldType0 = info0 match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
    val fieldType1 = info1 match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    (primitiveType0.asType, primitiveType1.asType) match {
      case ('[p0], '[p1]) =>
        (fieldType0.asType, fieldType1.asType) match {
          case ('[f0], '[f1]) =>
            targetType.asType match {
              case '[t] =>
                // Generate the constructor as a simple Term expression outside the quoted context
                val companionRef = Ref(targetSymbol.companionModule)
                val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
                
                // Create a lambda that wraps the apply method: (f0, f1) => T(..., ...)
                val constructorTerm = Lambda(
                  owner = Symbol.spliceOwner,
                  tpe = MethodType(List("x", "y"))(
                    _ => List(fieldType0, fieldType1),
                    _ => targetType
                  ),
                  rhsFn = (sym, params) => {
                    companionRef.select(applyMethod).appliedToArgs(params.asInstanceOf[List[Term]])
                  }
                )
                
                // Convert to Expr
                val constructorExpr = constructorTerm.asExprOf[(f0, f1) => t]
                
                '{
                  val raw: (p0, p1) => ZValidation[Nothing, String, t] = (a0: p0, a1: p1) => {
                    val v0 = ${ generateValidationExpr(info0, '{ a0 }) }.asInstanceOf[ZValidation[Nothing, String, f0]]
                    val v1 = ${ generateValidationExpr(info1, '{ a1 }) }.asInstanceOf[ZValidation[Nothing, String, f1]]
                    
                    v0.zipWith(v1)(${ constructorExpr })
                  }
                  val chain = caseclass2(raw)
                  chain
                }
              case _ =>
                quotes.reflect.report.errorAndAbort("Could not match target type")
            }
          case _ =>
            quotes.reflect.report.errorAndAbort("Could not match field types")
        }
      case _ =>
        quotes.reflect.report.errorAndAbort("Could not match primitive types")
    }
  }

  /**
   * Generate builder for arity 3.
   */
  private def generateArity3Builder[T: Type](using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    info0: ValidatorInfo,
    info1: ValidatorInfo,
    info2: ValidatorInfo
  ): Expr[Any] = {
    import quotes.reflect.*

    val primitiveType0 = info0 match {
      case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
    val primitiveType1 = info1 match {
      case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
    val primitiveType2 = info2 match {
      case nv: ValidatorInfo.NeedsValidation => nv.primitiveType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    // Get field names
    val fieldName0 = info0.fieldName
    val fieldName1 = info1.fieldName
    val fieldName2 = info2.fieldName

    // Get field types for the constructor
    val fieldType0 = info0 match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
    val fieldType1 = info1 match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }
    val fieldType2 = info2 match {
      case nv: ValidatorInfo.NeedsValidation => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation => nv.plainType.asInstanceOf[TypeRepr]
    }

    (primitiveType0.asType, primitiveType1.asType, primitiveType2.asType) match {
      case ('[p0], '[p1], '[p2]) =>
        (fieldType0.asType, fieldType1.asType, fieldType2.asType) match {
          case ('[f0], '[f1], '[f2]) =>
            targetType.asType match {
              case '[t] =>
                // Generate the constructor as a lambda that wraps the case class apply method
                val companionRef = Ref(targetSymbol.companionModule)
                val applyMethod = targetSymbol.companionModule.declaredMethod("apply").head
                
                // Create a lambda: (f0, f1, f2) => T(..., ..., ...)
                val constructorTerm = Lambda(
                  owner = Symbol.spliceOwner,
                  tpe = MethodType(List("x", "y", "z"))(
                    _ => List(fieldType0, fieldType1, fieldType2),
                    _ => targetType
                  ),
                  rhsFn = (sym, params) => {
                    companionRef.select(applyMethod).appliedToArgs(params.map(_.asInstanceOf[Term]))
                  }
                )
                
                // Convert to Expr
                val constructorExpr = constructorTerm.asExprOf[(f0, f1, f2) => t]
                
                '{
                  val raw: (p0, p1, p2) => ZValidation[Nothing, String, t] = (a0: p0, a1: p1, a2: p2) => {
                    val v0 = ${ generateValidationExpr(info0, '{ a0 }) }.asInstanceOf[ZValidation[Nothing, String, f0]]
                    val v1 = ${ generateValidationExpr(info1, '{ a1 }) }.asInstanceOf[ZValidation[Nothing, String, f1]]
                    val v2 = ${ generateValidationExpr(info2, '{ a2 }) }.asInstanceOf[ZValidation[Nothing, String, f2]]
                    
                    ZValidation.validateWith(v0, v1, v2)(${ constructorExpr })
                  }
                  val chain = caseclass3(raw)
                  chain
                }
              case _ =>
                quotes.reflect.report.errorAndAbort("Could not match target type")
            }
          case _ =>
            quotes.reflect.report.errorAndAbort("Could not match field types")
        }
      case _ =>
        quotes.reflect.report.errorAndAbort("Could not match primitive types")
    }
  }

  /**
   * Generate validation expression for a field.
   */
  private def generateValidationExpr(using Quotes)(
    info: ValidatorInfo,
    paramExpr: Expr[Any]
  ): Expr[ZValidation[Nothing, String, Any]] = {
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
                    val eitherExpr = Apply(
                      Select(companionRef, methodSymbol),
                      List(typedParam.asTerm)
                    ).asExprOf[Either[String, w]]
                    
                    '{ ZValidation.fromEither($eitherExpr) }
                  
                  case FromValidation =>
                    val validationCall = Apply(
                      Select(companionRef, methodSymbol),
                      List(typedParam.asTerm)
                    )
                    validationCall.asExprOf[ZValidation[Nothing, String, w]]
                }
              case _ =>
                report.errorAndAbort("Could not match wrapped type")
            }
          case _ =>
            report.errorAndAbort("Could not match primitive type")
        }
      
      case nv: ValidatorInfo.NoValidation =>
        '{ ZValidation.succeed($paramExpr) }
    }
  }

  /**
   * Create an instance of the case class with validated values.
   * This method is removed since we're generating the constructor calls inline.
   */

  /**
   * Generates the ZValidation.validateWith call from a list of parameter Terms.
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
   * Generates the ZValidation.validateWith call (legacy version with symbol-term pairs).
   * This is kept for compatibility but delegates to the new implementation.
   */
  private def generateValidateWithCall(using Quotes)(
    targetType: quotes.reflect.TypeRepr,
    targetSymbol: quotes.reflect.Symbol,
    validatorInfos: List[ValidatorInfo],
    params: List[(quotes.reflect.Symbol, quotes.reflect.Term)]
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    val paramTerms = params.map(_._2)
    generateValidateWithCallFromTerms(targetType, targetSymbol, validatorInfos, paramTerms)
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
