package models

import api.BuilderGeneratorSimplest.{caseclass2, caseclass2NonNull}
import api.{BuilderGeneratorSimplest, BuilderTypeClass}

case class IntAndBoolean(i: Int | Null, b: Boolean | Null)

import api.BuilderGeneratorSimplest.given

object IntAndBoolean extends BuilderGeneratorSimplest[IntAndBoolean](using BuilderTypeClass(caseclass2(IntAndBoolean.apply)))
object IntAndBooleanNonNull extends BuilderGeneratorSimplest[IntAndBoolean](using BuilderTypeClass(caseclass2NonNull(IntAndBoolean.apply)))

