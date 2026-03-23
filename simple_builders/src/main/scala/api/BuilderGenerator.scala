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

    // Configurable maximum arity (default 22). Can be overridden with system property "builders.maxArity".
    val MaxArity: Int =
      sys.props.get("builders.maxArity").flatMap(s => scala.util.Try(s.toInt).toOption).getOrElse(22)

    if (arity > MaxArity) {
      report.errorAndAbort(s"Unsupported arity $arity for ${tpe.show}. Increase builders.maxArity to support larger arities (current: $MaxArity).")
    }

    // Build an uncurried raw function Array[Any] => T which the runtime helper will convert into
    // the nested curried/Tuple1 chain. This is much simpler to construct in the macro API.
    val mirrorTerm: Term = mirrorProd.asTerm

    // Create the raw lambda (arr: Array[Any]) => m.fromProduct((arr(0), arr(1), ...))
    val rawLambda = Lambda(
      Symbol.spliceOwner,
      MethodType(List("arr"))(_ => List(TypeRepr.of[Array[Any]]), _ => TypeRepr.of[T]),
      (_, ps) => {
        val arrRef = ps.head.asInstanceOf[Term]
        val elems: Seq[Expr[Any]] = (0 until arity).toList.map { i =>
          val access = Apply(Select.unique(arrRef, "apply"), List(Literal(IntConstant(i))))
          access.asExprOf[Any]
        }
        val tupleExpr = Expr.ofTupleFromSeq(elems)
        Apply(Select.unique(mirrorTerm, "fromProduct"), List(tupleExpr.asTerm))
      }
    )

    '{
      val raw: Array[Any] => T = ${ rawLambda.asExprOf[Array[Any] => T] }
      val chain = BuilderGeneratorSimplest.buildChainFromArray(raw, ${Expr(arity)})
      val nt = Tuple1(chain)
      BuilderTypeClass[T](nt.asInstanceOf[Builder[T]])
    }
  }

  // Keep the simplest 1-arity helper for compatibility; multi-arity builders are generated in the macro now.
  def caseclass1[T, T0](transform: T0 => T): T0 => T = transform

  def caseclass2[T, T0, T1](transform: (T0, T1) => T): T0 => Tuple1[T1 => T] =
    transform.curried.andThen(a => Tuple1(a))

  def caseclass3[T, T0, T1, T2](transform: (T0, T1, T2) => T) =
    transform.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply)))

  private def nonNull[I] = (v: I) => { require(v != null); v }

  def caseclass2NonNull[T, T0, T1](transform: (T0, T1) => T) =
    transform.curried.compose(nonNull[T0]).andThen(a => Tuple1(a.compose(nonNull[T1])))

  def caseclass3NonNull[T, T0, T1, T2](transform: (T0, T1, T2) => T) =
    transform.curried.compose(nonNull[T0]).andThen(t =>
      Tuple1(t.compose(nonNull[T1]).andThen(f => Tuple1(f.compose(nonNull[T2]))))
    )

  // Runtime helper: convert an uncurried Array[Any] => T into the nested curried chain expected by
  // the Builder type (each step returns a Tuple1 wrapping the next function).
  def buildChainFromArray[T](raw: Array[Any] => T, arity: Int): Any = {
    def make(index: Int, acc: Array[Any]): Any = {
      if (index >= arity) raw(acc)
      else {
        (a: Any) =>
          val acc2 = acc.clone()
          acc2(index) = a
          if (index == arity - 1) raw(acc2)
          else Tuple1(make(index + 1, acc2))
      }
    }
    make(0, new Array[Any](arity))
  }
}