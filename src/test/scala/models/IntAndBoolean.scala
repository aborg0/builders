package models

import api.{BuilderGeneratorSimplest, BuilderTypeClass}

case class IntAndBoolean(i: Int, b: Boolean)

import api.BuilderGeneratorSimplest.given

object IntAndBoolean extends BuilderGeneratorSimplest[IntAndBoolean]

