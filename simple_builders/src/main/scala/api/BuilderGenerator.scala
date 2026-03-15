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
  type Builder[T] = BuilderFor[
    Tuple.Head[Split[Tup[T], 1]],
    Tuple.Last[Split[Tup[T], 1]],
    T
  ]

  type BuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
        NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => T]]
      case h *: t =>
        NamedTuple[
          NamedTuple.Names[H],
          Tuple1[
            Tuple.Head[NamedTuple.DropNames[H]] => BuilderFor[
              Tuple.Head[Split[R, 1]],
              Tuple.Last[Split[R, 1]],
              T
            ]
          ]
        ]
    }

  import scala.quoted.*
  import scala.deriving.Mirror

  // Helper to obtain a builder for any T (including generic T like Box[A])
  def builderOf[T](using tc: BuilderTypeClass[T]): Builder[T] = tc.builder

  inline given build[T]: BuilderTypeClass[T] = ${ buildImpl[T] }

  def buildImpl[T: Type](using Quotes): Expr[BuilderTypeClass[T]] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
  
    // Determine arity from the primary constructor term parameters
        val ctorSym         = typeSymbol.primaryConstructor
    // For generic case classes, the first param list may be type params (non-term).
    // Pick the first non-empty list of term params.
    val termParamLists  = ctorSym.paramSymss.map(_.filter(_.isTerm)).filter(_.nonEmpty)
    val arity: Int      = termParamLists.headOption.map(_.length).getOrElse {
      report.errorAndAbort(s"Could not determine constructor arity for ${tpe.show}")
    }


    // Summon product mirror for T (case classes have these)
    val mirrorProd: Expr[Mirror.ProductOf[T]] =
      Expr.summon[Mirror.ProductOf[T]].getOrElse {
        report.errorAndAbort(s"Could not summon Mirror.ProductOf for ${tpe.show}")
      }

    arity match {
      case 1 =>
        '{
          val m = $mirrorProd
          val f: Any => T = (a0: Any) => m.fromProduct(Tuple1(a0))
          val nt = Tuple1(f)
          BuilderTypeClass[T](nt.asInstanceOf[Builder[T]])
        }

      case 2 =>
        '{
          val m = $mirrorProd
          val raw: (Any, Any) => T = (a0: Any, a1: Any) => m.fromProduct((a0, a1))
          val chain = caseclass2(raw)
          val nt = Tuple1(chain)
          BuilderTypeClass[T](nt.asInstanceOf[Builder[T]])
        }

      case 3 =>
        '{
          val m = $mirrorProd
          val raw: (Any, Any, Any) => T =
            (a0: Any, a1: Any, a2: Any) => m.fromProduct((a0, a1, a2))
          val chain = caseclass3(raw)
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

  def caseclass2[T, T0, T1](transform: (T0, T1) => T): T0 => Tuple1[T1 => T] =
    transform.curried.andThen(a => Tuple1(a))

  def caseclass3[T, T0, T1, T2](transform: (T0, T1, T2) => T) =
    transform.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply)))

  def caseclass4[T, T0, T1, T2, T3](transform: (T0, T1, T2, T3) => T) =
    transform.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))

  def caseclass5[T, T0, T1, T2, T3, T4](transform: (T0, T1, T2, T3, T4) => T) =
    transform.curried.andThen(t =>
      Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))
    ))

  def caseclass6[T, T0, T1, T2, T3, T4, T5](transform: (T0, T1, T2, T3, T4, T5) => T) =
    transform.curried.andThen(t =>
      Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t =>
        Tuple1(t.andThen(Tuple1.apply))
      )))))
    ))

  def caseclass7[T, T0, T1, T2, T3, T4, T5, T6](transform: (T0, T1, T2, T3, T4, T5, T6) => T) =
    transform.curried.andThen(t =>
      Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t =>
        Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply))))
      )))))
    ))

  def caseclass8[T, T0, T1, T2, T3, T4, T5, T6, T7](transform: (T0, T1, T2, T3, T4, T5, T6, T7) => T) =
    transform.curried.andThen(t =>
      Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t =>
        Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1.apply)))))
      )))))
    )))

  def caseclass9[T, T0, T1, T2, T3, T4, T5, T6, T7, T8](transform: (T0, T1, T2, T3, T4, T5, T6, T7, T8) => T) =
    transform.curried.andThen(t =>
      Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t =>
        Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t =>
          Tuple1(t.andThen(Tuple1.apply))
        )))))
      )))))
    )))

  private def nonNull[I] = (v: I) => { require(v != null); v }

  def caseclass1NonNull[T, T0](transform: T0 => T): T0 => T =
    caseclass1(transform).compose(nonNull[T0])

  def caseclass2NonNull[T, T0, T1](transform: (T0, T1) => T) =
    transform.curried.compose(nonNull[T0]).andThen(a => Tuple1(a.compose(nonNull[T1])))

  def caseclass3NonNull[T, T0, T1, T2](transform: (T0, T1, T2) => T) =
    transform.curried.compose(nonNull[T0]).andThen(t =>
      Tuple1(t.compose(nonNull[T1]).andThen(f => Tuple1(f.compose(nonNull[T2]))))
    )
}