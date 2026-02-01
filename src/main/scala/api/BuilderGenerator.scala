package api

// Based on https://blog.daniel-beskin.com/2025-04-14-whiteboxish-named-tuples
trait BuilderGeneratorSimplest[T](using val tc: BuilderTypeClass[T]) {
  import BuilderGeneratorSimplest.*

  type Fields = Builder[T]

  /**
   * Entry point for the macro-generated builder.
   * IntAndBoolean.builder.i(42).b(false)
   */
  def builder: Builder[T] = tc.builder
}


object BuilderGeneratorSimplest {
  import scala.NamedTuple
  import scala.NamedTuple.*
  import scala.NamedTuple.Split

  type Tup[T] = NamedTuple.From[T]

  /**
   * Builder named-tuple type for a case class T.
   */
  type Builder[T] = BuilderFor[Tuple.Head[Split[Tup[T], 1]], Tuple.Last[Split[Tup[T], 1]], T]


  type BuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
        NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => T]]
      case h *: t =>
        NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] =>
          BuilderFor[Tuple.Head[Split[R, 1]], Tuple.Last[Split[R, 1]], T]]]
    }


  import scala.quoted.*

  inline given build[T]: BuilderTypeClass[T] = ${ buildImpl[T] }

  def buildImpl[T: Type](using Quotes): Expr[BuilderTypeClass[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val companionSymbol = tpe.typeSymbol.companionModule
    if !companionSymbol.exists then
      report.errorAndAbort(s"No companion object found for ${tpe.show}")

    val applyMethod =
      companionSymbol.declaredMethods.find(_.name == "apply").getOrElse {
        report.errorAndAbort(s"No apply method found on companion of ${tpe.show}")
      }

    val companion = Ref(companionSymbol)
    val applyTerm = companion.select(applyMethod)
    val methType = applyTerm.tpe.widen

    def etaExpand(prefix: Term, tpe: TypeRepr): Term =
      tpe match {
        case MethodType(paramNames, paramTypes, returnType) =>
          Lambda(
            owner = Symbol.spliceOwner,
            tpe = MethodType(paramNames)(_ => paramTypes, _ => returnType),
            rhsFn = (_, args) => {
              val termArgs = args.map(_.asInstanceOf[Term])
              val applied   = prefix.appliedToArgs(termArgs)
              returnType match {
                case mt: MethodType => etaExpand(applied, mt)
                case _              => applied
              }
            }
          )

        case PolyType(_, _, returnType) =>
          etaExpand(prefix, returnType)

        case _ =>
          prefix
      }

    val etaExpanded = etaExpand(applyTerm, methType)

    val arity: Int =
      methType match {
        case mt: MethodType => mt.paramTypes.length
        case _              => 0
      }

    type TupT   = Tup[T]
    type NamesT = NamedTuple.Names[TupT]

    arity match {
      case 1 =>
        '{
          val f = ${ etaExpanded.asExpr/* Of[Any => T] */ }.asInstanceOf[Function1[?, T]]
          val nt = Tuple1(f)
          BuilderTypeClass[T](nt.asInstanceOf[Builder[T]])
        }

      case 2 =>
        '{
          val f2 = ${ etaExpanded.asExpr/* Of[(Any, Any) => T] */ }.asInstanceOf[Function2[?, ?, T]]
          val chain = caseclass2(f2)
          val nt = Tuple1(chain)
          BuilderTypeClass[T](nt.asInstanceOf[Builder[T]])
        }

      case 3 =>
        '{
          val f3 = ${ etaExpanded.asExpr/* Of[(Any, Any, Any) => T] */ }.asInstanceOf[Function3[?, ?, ?, T]]
          val chain = caseclass3(f3)
          val nt = Tuple1(chain)
          BuilderTypeClass[T](nt.asInstanceOf[Builder[T]])
        }

      case n =>
        report.errorAndAbort(
          s"Unsupported arity $n for ${tpe.show}. Extend buildImpl / caseclassN handling."
        )
    }
  }



  def caseclass1[T, T0](transform: T0 => T): T0 => T = transform
//  def caseclass2[T, T0, T1](transform: (T0, T1) => T) = (a: T0) => Tuple1((b: T1) => transform.tupled)
  def caseclass2[T, T0, T1](transform: (T0, T1) => T): T0 => Tuple1[T1 => T] = transform.curried.andThen(a => Tuple1(a))
  def caseclass3[T, T0, T1, T2](transform: (T0, T1, T2) => T) = transform.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply)))
  def caseclass4[T, T0, T1, T2, T3](transform: (T0, T1, T2, T3) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))
  def caseclass5[T, T0, T1, T2, T3, T4](transform: (T0, T1, T2, T3, T4) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))
  def caseclass6[T, T0, T1, T2, T3, T4, T5](transform: (T0, T1, T2, T3, T4, T5) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))
  def caseclass7[T, T0, T1, T2, T3, T4, T5, T6](transform: (T0, T1, T2, T3, T4, T5, T6) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))))
  def caseclass8[T, T0, T1, T2, T3, T4, T5, T6, T7](transform: (T0, T1, T2, T3, T4, T5, T6, T7) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))))))
  def caseclass9[T, T0, T1, T2, T3, T4, T5, T6, T7, T8](transform: (T0, T1, T2, T3, T4, T5, T6, T7, T8) => T) = transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))))))))))))

  private def nonNull[I] = (v: I) => {require(v != null); v}
  def caseclass1NonNull[T, T0](transform: T0 => T): T0 => T = caseclass1(transform).compose(nonNull[T0])
  def caseclass2NonNull[T, T0, T1](transform: (T0, T1) => T) = transform.curried.compose(nonNull[T0]).andThen(a => Tuple1(a.compose(nonNull[T1])))
  def caseclass3NonNull[T, T0, T1, T2](transform: (T0, T1, T2) => T) = transform.curried.compose(nonNull[T0]).andThen(t => Tuple1(t.compose(nonNull[T1]).andThen(f => Tuple1(f.compose(nonNull[T2])))))
}
