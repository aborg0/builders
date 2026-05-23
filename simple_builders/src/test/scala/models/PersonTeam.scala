// src/test/scala/models/PersonTeam.scala
package models

import api.{BuilderGeneratorSimplest, BuilderTypeClass}
import api.BuilderGeneratorSimplest.{caseclass2, caseclass3}
import api.BuilderGeneratorSimplest.given

final case class Person(name: String, age: Int)
final case class Team(name: String, lead: Person, members: List[Person])

object Person extends BuilderGeneratorSimplest[Person]
object Team   extends BuilderGeneratorSimplest[Team]