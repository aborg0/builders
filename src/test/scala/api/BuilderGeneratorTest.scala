package api

import utest.TestSuite

object BuilderGeneratorTest extends TestSuite {

  import utest.{Tests, test}

  val tests = Tests{
    test("SingleIntHolder") {
      import models.SingleIntHolder
      assert(SingleIntHolder(7) == SingleIntHolder.i(7))
    }

    test("Types") {
      import models.IntAndBoolean

      import scala.NamedTuple.NamedTuple
      summon[Tuple.Last[NamedTuple.Split[NamedTuple[Tuple1["i"], Tuple1[Int]], 1]] =:= NamedTuple.Empty]
      type I = NamedTuple[Tuple1["i"], Tuple1[Int]]
      type B = NamedTuple[Tuple1["b"], Tuple1[Boolean]]

      // summon[NamedTuple[Tuple1["b"], Tuple1[Boolean => NamedTuple[Tuple1["i"], Int => IntAndBoolean]]] =:=
      //   BuilderGenerator.BuilderForRev[NamedTuple[Tuple1["i"], Tuple1[Int]], B, IntAndBoolean]]
    }
    test("IntAndBoolean") {
      import models.IntAndBoolean
      assert(IntAndBoolean(43, false) == IntAndBoolean.i(43).b(false))
    }

    test("triple") {
      import models.Triple
      assert(Triple(1, "s", None) == Triple.a(1).s("s").c(None))
    }
  }

}
