package models

import api.BuilderGeneratorSimplest.caseclass2
import api.{BuilderGeneratorSimplest, BuilderTypeClass}

case class IntAndBoolean(i: Int, b: Boolean)

import api.BuilderGeneratorSimplest.given

object IntAndBoolean extends BuilderGeneratorSimplest[IntAndBoolean](using BuilderTypeClass(caseclass2(IntAndBoolean.apply)))

