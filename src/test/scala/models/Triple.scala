package models

import api.BuilderGeneratorSimplest.caseclass3

final case class Triple(a: Int, s: String, c: Option[String])

import api.*
import api.BuilderGeneratorSimplest.given
object Triple extends BuilderGeneratorSimplest[Triple](using BuilderTypeClass[Triple](caseclass3(Triple.apply)))
