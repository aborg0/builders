package models

import api.{BuilderGeneratorGeneric1, BuilderTypeClass}
import api.BuilderGeneratorSimplest.{Builder, given}

final case class Box[A](value: A)

// Keep companion simple
object Box

// Generic builder provider, separate from the companion
object BoxBuilders extends BuilderGeneratorGeneric1[Box]

// // Extension for the syntax Box.builder[A]
// extension (c: Box.type)
//   def builder[A](using tc: BuilderTypeClass[Box[A]]): Builder[Box[A]] = tc.builder
