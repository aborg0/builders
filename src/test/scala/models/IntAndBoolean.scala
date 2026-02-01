package models

import api.BuilderGeneratorSimplest.{caseclass2, caseclass2NonNull}
import api.{BuilderGeneratorSimplest, BuilderTypeClass}
import api.BuilderGeneratorSimplest.Builder

case class IntAndBoolean(i: Int | Null, b: Boolean | Null)

import api.BuilderGeneratorSimplest.given

object IntAndBoolean extends BuilderGeneratorSimplest[IntAndBoolean]//(using BuilderTypeClass(caseclass2(IntAndBoolean.apply)))
object IntAndBooleanNonNull extends BuilderGeneratorSimplest[IntAndBoolean](using BuilderTypeClass(Tuple1(caseclass2NonNull(IntAndBoolean.apply)): BuilderGeneratorSimplest.Builder[IntAndBoolean] /* : (Int | Null) => Tuple1[(Boolean | Null) => IntAndBoolean] */))

