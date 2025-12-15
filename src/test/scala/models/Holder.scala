package models

final case class Holder[T](value: T)

// object Holder {
//     def apply[T](t: T) = new Holder(t)
// }

import api.BuilderGeneratorSimplest
import api.BuilderGeneratorSimplest.given

// TODO Find a fix for apply does not take type parameters
// class HolderBuilder[T] extends BuilderGeneratorSimplest[Holder[T]]
