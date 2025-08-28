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
