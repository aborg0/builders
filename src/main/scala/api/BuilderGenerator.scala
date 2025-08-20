package api

// Based on https://blog.daniel-beskin.com/2025-04-14-whiteboxish-named-tuples
trait BuilderGenerator[T](using tc: BuilderTypeClass[T]/* builderMap: BuilderMap[T] */) extends Selectable {
  import scala.Tuple.FlatMap
  import BuilderGenerator.*

  import scala.NamedTuple.Split

//  type BuilderFor[T] = NamedTuple.From[T] `FlatMap` ToBuilder[T]
//  type Fields = BuilderForSimplest[T]
//  type Fields = NamedTuple.From[T] `Map` FromSimple[T]
//  type Fields = BuilderFor[Tuple.Head[Split[Tup[T], 1]], Tuple.Last[Split[Tup[T], 1]], T]
  type Fields = BuilderForRev[Tuple.Last[Split[Tup[T], 1]], Tuple.Head[Split[Tup[T], 1]], T]
  inline def selectDynamic(name: String): Any =
    tc.builder
    //builderMap.mapping.getOrElse(name, sys.error(s"Invalid field name: `$name`"))

}

import scala.NamedTuple.Map

object BuilderGenerator {
  import NamedTuple.*
  type FromSimple[T] = [S] =>> (S => T)
  type Tup[T] = NamedTuple.From[T]
//  type Mapping[Tup, T] =
  type BuilderForSimplest[T] = Tup[T] `Map` FromSimple[T]
//  type BuilderForSimplest[T] = NamedTuple["i" *: EmptyTuple, (Int => T) *: EmptyTuple]
  type BuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T] = (H, R, T) match {
  case (H, NamedTuple.Empty, T) => H `Map` FromSimple[T]
  case (H, R, T) => /* NamedTuple[NamedTuple.Names[H], NamedTuple.DropNames[H] */ H =>
    BuilderFor[Tuple.Head[Split[R, 1]], Tuple.Last[Split[R, 1]], T]
  }
 /*match {
    case ((s, NamedTuple.Empty), T) => [s] =>> (s => T)
    case ((h, t), T) =>
  }*/


  type BuilderForRev[R <: AnyNamedTuple, H <: AnyNamedTuple, T] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
    case EmptyTuple =>
      H `Map` FromSimple[T]
      // NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => T]]
    case h *: t =>
      //NamedTuple[NamedTuple.Names[H], NamedTuple.DropNames[H] =>
      NamedTuple[NamedTuple.Names[H], Tuple1[Tuple.Head[NamedTuple.DropNames[H]] =>
        BuilderForRev[Tuple.Last[Split[R, 1]], Tuple.Head[Split[R, 1]], T]]]

    }

//  type BuilderForFold[H <: AnyNamedTuple, T] = NamedTuple.Fold[]


  import scala.quoted.*

//  inline given derived[S]: BuilderMap[S] = ${ derivedImpl[S] }

  inline given derived[S]: BuilderMap[S] = BuilderMap(Map[String, Any]("a" -> ((i: Int) => ???)))

  inline given der[T]: BuilderTypeClass[T] = ???

//  def derivedImpl[S: Type](using Quotes): Expr[BuilderMap[S]] = {
//    import quotes.reflect.*
//    val productMirror = summonProduct[S]
//
//    val creators = productMirror.elems.map: elem =>
//      import elem.asType
//
//      val creator = '{ (elem) => ${ new S(elem) } }
//
//      elem.label -> creator
//
//    val map = Expr.ofMap(creators)
//
//    '{
//      BuilderMap($map)
//    }
//  }
//  def derivedImpl[S: Type, A: Type](selectorExpr: Expr[S => A])(using Quotes): Expr[BuilderMap[S]] = {
//    import quotes.reflect.*
//
//    selectorExpr.asTerm.underlyingArgument match
//      case Lambda(_, select@Select(s, a)) =>
//        val productMirror = MacroMirror.summonProduct[S]
//
//        val elem = productMirror
//          .elemForSymbol(select.symbol)
//          .getOrElse(
//            report.errorAndAbort(
//              s"Invalid selector ${select.show}, must be a field of ${productMirror.monoType.show}"))
//          .asElemOf[A]
//
//        '{
//          (s: S) => {
//            new A(a)
//          }
//        }
//      case other =>
//        report.errorAndAbort(s"Expected a selector of the form `s => a`, but got: ${other}")
//  }

}
