package api

// Based on https://blog.daniel-beskin.com/2025-04-14-whiteboxish-named-tuples
trait BuilderGeneratorSimplest[T](using tc: BuilderTypeClass[T]) extends Selectable {
  import BuilderGeneratorSimplest.*

  import scala.NamedTuple.Split

  type Fields = BuilderFor[Tuple.Head[Split[Tup[T], 1]], Tuple.Last[Split[Tup[T], 1]], T]
  inline def selectDynamic(name: String): Any =
    tc.builder
}

import scala.NamedTuple.Map

object BuilderGeneratorSimplest {
  import NamedTuple.*
  type Tup[T] = NamedTuple.From[T]

  type BuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
      NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => T]]
      case h *: t =>
      NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] =>
        BuilderFor[Tuple.Head[Split[R, 1]], Tuple.Last[Split[R, 1]], T]]]

    }

  import scala.quoted.*

  inline given build[T]: BuilderTypeClass[T] = {
/*    BuilderTypeClass(*/${ buildImpl[T] }/* match {
      case func1: Function1[?, ?] => caseclass1(func1)
      case func2: Function2[?, ?, ?] => caseclass2(func2)
      case func3: Function3[?, ?, ?, ?] => caseclass3(func3)
      case func4: Function4[?, ?, ?, ?, ?] => caseclass4(func4)
    })*/
  }


// Claude 4
  def buildImpl[T: Type](using Quotes): Expr[BuilderTypeClass[T]] = {
    import quotes.reflect.*

    val companionSymbol = TypeRepr.of[T].typeSymbol.companionModule
    val meth = companionSymbol.declaredMethods.find(_.name == "apply").get

    // Get the companion object instance
    val companion = Ref(companionSymbol)

    val applySymbol = companion.select(meth)
    // Get the full method type
    val methType = applySymbol.tpe.widen

    // Handle methods with any number of parameter lists
    def etaExpand(prefix: Term, tpe: TypeRepr): Term = {
      tpe match {
        case MethodType(paramNames, paramTypes, returnType) =>
          // Create lambda for this parameter list
          Lambda(
            owner = Symbol.spliceOwner,
            tpe = MethodType(paramNames)(_ => paramTypes, _ => returnType),
            rhsFn = (sym, args) => {
              // Convert List[Tree] to List[Term]
              val termArgs = args/* .flatten */.map(_.asInstanceOf[Term])
              val applied = prefix.appliedToArgs(termArgs)
              returnType match {
                case mt: MethodType =>
                  // More parameter lists to go
                  etaExpand(applied, mt)
                case _ =>
                  // No more parameter lists
                  applied
              }
            }
          )

        case PolyType(paramNames, paramTypes, returnType) =>
          // Skip type parameters and continue with the return type
          etaExpand(prefix, returnType)

        case _ =>
          // No parameters left
          prefix
      }
    }

    // Start eta expansion
    val etaExpanded = etaExpand(applySymbol, methType)

//    etaExpanded.asExprOf[Any /* Function2[?, ?, ?] */]
    val result = etaExpanded.asExprOf[Any]

    methType match {
      case MethodType(paramNames, _, _) if paramNames.length > 0 =>
        // Use a runtime call to curried
        '{
          BuilderTypeClass[T]($result match {
            case f: Function1[?, ?] => caseclass1(f)
            case f: Function2[?, ?, ?] => caseclass2(f)
            case f: Function3[?, ?, ?, ?] => caseclass3(f)
            case f: Function4[?, ?, ?, ?, ?] => caseclass4(f)
            // Add more cases as needed
//            case other => other
          }
          )
        }
//      case _ =>
//        result
    }

    // https://eed3si9n.com/intro-to-scala-3-macros/#apply
    // Select.unique(etaExpanded, "curried").appliedToNone.asExprOf[Function1[?, ?]]
  }


  def caseclass1[T, T0](transform: T0 => T): T0 => T = transform
//  def caseclass2[T, T0, T1](transform: (T0, T1) => T) = (a: T0) => Tuple1((b: T1) => transform.tupled)
  def caseclass2[T, T0, T1](transform: (T0, T1) => T) = transform.curried.andThen(a => Tuple1(a))
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
