package api

import api.BuilderGeneratorSimplest.Builder

/**
 * Support for deriving builders for unary type constructors, e.g. Box[A].
 *
 * Usage:
 *   final case class Box[A](value: A)
 *   object Box extends BuilderGeneratorGeneric1[Box]
 *   // then: Box.builder.value(42) : Box[Int]
 */
trait BuilderGeneratorGeneric1[F[_]] {
  /** Polymorphic builder accessor for any A. */
  def builder[A](using tc: BuilderTypeClass[F[A]]): Builder[F[A]] = tc.builder
}
