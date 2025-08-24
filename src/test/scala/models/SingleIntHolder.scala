package models

import api.BuilderGeneratorSimplest.caseclass1
import api.{BuilderGeneratorSimplest, BuilderTypeClass}

final case class SingleIntHolder(i: Int)

import api.BuilderGeneratorSimplest.given

object SingleIntHolder extends BuilderGeneratorSimplest[SingleIntHolder](using BuilderTypeClass[SingleIntHolder](caseclass1(SingleIntHolder.apply)))
