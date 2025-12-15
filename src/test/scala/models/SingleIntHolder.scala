package models

import api.{BuilderGeneratorSimplest, BuilderTypeClass}

final case class SingleIntHolder(i: Int)

import api.BuilderGeneratorSimplest.given

object SingleIntHolder extends BuilderGeneratorSimplest[SingleIntHolder]
