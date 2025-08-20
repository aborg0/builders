package models

final case class Triple(a: Int, s: String, c: Option[String])

def Tuple1Unapply[A, B, C, D](f: ((A, B, C)) => D) = (a: A) => Tuple1((b: B) => Tuple1((c: C) => f(a, b, c)))

val g= Triple.apply.curried.andThen(t => Tuple1(t.andThen(Tuple1.apply)))

import api.*
import api.BuilderGenerator.given
object Triple extends BuilderGenerator[Triple](using BuilderTypeClass(g/*Tuple1Unapply(Triple.apply.tupled)*/))

//(summon[BuilderTypeClass[Triple]])//(BuilderMap(Map("a" -> ((a: Int) => Tuple1((s: String) => Tuple1((c: Option[String]) => Triple(a, s, c)))))))
