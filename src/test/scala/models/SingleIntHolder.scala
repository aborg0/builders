package models

import api.{BuilderGenerator, BuilderMap, BuilderTypeClass}

final case class SingleIntHolder(i: Int)

import api.BuilderGenerator.given

object SingleIntHolder extends BuilderGenerator[SingleIntHolder](using BuilderTypeClass(SingleIntHolder.apply))
//(summon[BuilderTypeClass[SingleIntHolder]])//(BuilderMap(Map("i" -> ((i: Int) => SingleIntHolder(i)))))