package models

import api.{BuilderGenerator, BuilderMap, BuilderTypeClass}

case class IntAndBoolean(i: Int, b: Boolean)

import api.BuilderGenerator.given

def curriedTupleFunction[A, B, C](f: ((A, B)) => C): A => Tuple1[B => C] =
  (a: A) => Tuple1((b: B) => f(a -> b))

val intToTupleBoolean: Int => Tuple1[Boolean => IntAndBoolean] = curriedTupleFunction(IntAndBoolean.apply.tupled)
object IntAndBoolean extends BuilderGenerator[IntAndBoolean](using BuilderTypeClass(intToTupleBoolean /*(i: Int) => Tuple1((b: Boolean) => IntAndBoolean(i, b))*/ ))

//(summon[BuilderTypeClass[IntAndBoolean]])//(BuilderMap(Map("i" -> ((i: Int) => (b=(b: Boolean) => IntAndBoolean(i, b))))))

