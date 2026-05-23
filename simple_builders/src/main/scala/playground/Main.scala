package playground

object Main {
  def main(args: Array[String]): Unit = {
    import scala.NamedTuple.NamedTuple
    import scala.Tuple.{:*, FlatMap}
    type Reference = (
      firstName: (String) => (lastName: (String) => (city: String => Person), city: (String) => (lastName: (String) => Person)),
      lastName: (String) => (firstName: (String) => (city: String => Person), city: (String) => (firstName: (String) => Person)),
      city: (String) => (firstName: (String) => (lastName: String => Person), lastName: (String) => (firstName: (String) => Person)),
    )
    type ToBuilder[Person] = [T] =>> T match {
      case EmptyTuple => Person
//      case H *: R => (H => R `FlatMap` ToBuilder[Person])
    }
//    println(SingleIntHolderBuilder.i(44))
//    type Result = NamedTuple.From[Person] `FlatMap` ToBuilder[Person]
//    println(summon[Reference =:= Result])
    val s = Seq("firstName" -> "String", "lastName" -> "String", "city" -> "String")
    def combinations(remaining: Seq[(String, String)]): Seq[((String, String), Seq[?])] = for {
      select <- remaining
      rest = remaining.filterNot(_ == select)
    } yield select -> combinations(rest)
    println(combinations(s))
    def combinations2(remaining: Seq[(String, String)]): Seq[((String, String), String => ?)] = for {
      select <- remaining
      rest = remaining.filterNot(_ == select)
      result = if (rest.isEmpty) then
        select -> ((s: String) => Person(???, ???, ???): Any)
      else
        select -> ((s: String) => combinations2(rest): Any)
    } yield result


/*
    val personBuilder = Person.build()
//    val person = personBuilder _city "Budapest" _firstName "Scala" _lastName "Doodle"
    val person = personBuilder
      ._city("Budapest")._firstName("Scala")._lastName("Doodle")
    println(person)
    import PersonNonHole.{given, *}
//    import Person.{given => *}
    val p = PersonNonHole().city("Bp").firstName("Sc").lastName("Dood")
    println(p)*/
    val m = PersonMega().firstName("first").city("city").lastName("last")
    println(m)

  }
}
