package api

import utest.TestSuite

object BuilderGeneratorTest extends TestSuite {

  import utest.{Tests, assertThrows, test}

  val tests = Tests{
    test("SingleIntHolder") {
      import models.SingleIntHolder
      assert(SingleIntHolder(7) == SingleIntHolder.builder.i(7))

    }

    test("Types") {
      import models.IntAndBoolean

      import scala.NamedTuple.NamedTuple
      summon[Tuple.Last[NamedTuple.Split[NamedTuple[Tuple1["i"], Tuple1[Int]], 1]] =:= NamedTuple.Empty]
      type I = NamedTuple[Tuple1["i"], Tuple1[Int]]
      type B = NamedTuple[Tuple1["b"], Tuple1[Boolean]]

       summon[NamedTuple[Tuple1["b"], Tuple1[Boolean => NamedTuple[Tuple1["i"], Tuple1[Int => IntAndBoolean]]]] =:=
         BuilderGeneratorSimplest.BuilderFor[B, NamedTuple[Tuple1["i"], Tuple1[Int]], IntAndBoolean]]
    }
    test("IntAndBoolean") {
      import models.IntAndBoolean
      assert(IntAndBoolean(43, false) == IntAndBoolean.builder.i(43).b(false))

    }
//    test("IntAndBooleanNonNull first") {
//      import models.IntAndBoolean
//      import models.IntAndBooleanNonNull
//      assertThrows[IllegalArgumentException] {
//        IntAndBooleanNonNull.i(43).b(null: Boolean | Null)
//      }
//    }
    test("IntAndBooleanNonNull second") {
      import models.IntAndBoolean
      import models.IntAndBooleanNonNull
      assertThrows[IllegalArgumentException] {
        IntAndBooleanNonNull.builder.i(null: Int | Null)
      }

    }

    test("triple") {
      import models.Triple
      val partial = Triple.builder.a(1).s("s")
      assert(Triple(1, "s", None) == partial.c(None))

    }

    test("generic Box[A] builder") {
      import models.Box
      import models.BoxBuilders
      import api.BuilderGeneratorSimplest.given

      val bi: Box[Int] = BoxBuilders.builder[Int].value(42)
      val bs: Box[String] = BoxBuilders.builder[String].value("foo")

      assert(bi == new Box(42))
      assert(bs == new Box("foo"))
    }
    test("nested builders (Team/Person) – missing ergonomic support") {
      import models.{Person, Team}

      // Desired (not yet supported ergonomically):
      //
      val team =
        Team.builder
          .name("Scala Team")
          .lead(
            Person.builder
              .name("Alice")
              .age(30)
          )
          .members(List(
            Person.builder.name("Bob").age(25),
            Person.builder.name("Carol").age(27)
          ))
      //
      // This would require additional builder combinators or
      // extension methods that understand how to accept nested
      // builders as arguments.

      // What *is* supported today is using the inner builders
      // to construct actual values, and then passing those values
      // to the outer builder:

      val lead: Person =
        Person.builder.name("Alice").age(30)

      val members: List[Person] = List(
        Person.builder.name("Bob").age(25),
        Person.builder.name("Carol").age(27)
      )

      // val team =
      //   Team.builder
      //     .name("Scala Team")
      //     .lead(lead)
      //     .members(members)

      assert(team == Team("Scala Team", lead, members))
    }
  }

}
