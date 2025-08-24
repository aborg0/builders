package playground

import scala.annotation.targetName

final case class Person(firstName: String, lastName: String, city: String)

//object Person3 extends BuilderGenerator[Person]

object PersonMega {
  inline def apply(): (
    firstName: (String) => (lastName: (String) => (city: String => Person), city: (String) => (lastName: (String) => Person)),
    lastName: (String) => (firstName: (String) => (city: String => Person), city: (String) => (firstName: (String) => Person)),
    city: (String) => (firstName: (String) => (lastName: String => Person), lastName: (String) => (firstName: (String) => Person)),
  ) =
    (firstName = (firstName: String) => (lastName = (lastName: String) => (city = (city: String) => Person(firstName, lastName, city)),
      city = (city: String) => (lastName = (lastName: String) => Person(firstName, lastName, city))),
    lastName = (lastName: String) => (firstName= (firstName: String) => (city= (city: String) => Person(firstName, lastName, city)),
      city= (city: String) => (firstName = (firstName: String) => Person(firstName, lastName, city))),
    city = (city: String) => (firstName= (firstName: String) => (lastName= (lastName: String) => Person(firstName, lastName, city)),
      lastName = (lastName: String) => (firstName= (firstName: String) => Person(firstName, lastName, city))))
}
/*object PersonNonHole {

  import scala.NamedTuple.NamedTuple

  inline def apply(): (discriminator: Discriminator[Person.type]) = {
    (discriminator = null: Discriminator[Person.type])
  }

  extension (n: (discriminator: Discriminator[Person.type])) {
    @targetName("firstName")
    inline def firstName(value: String): (firstName: String, discriminator: Discriminator[Person.type]) =
      (firstName = value, discriminator = null: Discriminator[Person.type])
    @targetName("lastName")
    inline def lastName(value: String): (lastName: String, discriminator: Discriminator[Person.type]) =
      (lastName = value, discriminator = null: Discriminator[Person.type])
    @targetName("city")
    inline def city(value: String): (city: String, discriminator: Discriminator[Person.type]) =
      (city = value, discriminator = null: Discriminator[Person.type])
  }

  extension (n: (firstName: String, discriminator: Discriminator[Person.type])){
    @targetName("firstName_lastName")
    inline def lastName(value: String): (firstName: String, lastName: String, discriminator: Discriminator[Person.type]) =
      (firstName = n.firstName, lastName = value, discriminator = null: Discriminator[Person.type])
    @targetName("firstName_city")
    inline def city(value: String): (firstName: String, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = n.firstName, city = value, discriminator = null: Discriminator[Person.type])
  }
  extension (n: (lastName: String, discriminator: Discriminator[Person.type])) {
    @targetName("lastName_firstName")
    inline def firstName(value: String): (firstName: String, lastName: String, discriminator: Discriminator[Person.type]) =
      (firstName = value, lastName = n.lastName, discriminator = null: Discriminator[Person.type])
    @targetName("lastName_city")
    inline def city(value: String): (lastName: String, city: String, discriminator: Discriminator[Person.type]) =
      (lastName = n.lastName, city = value, discriminator = null: Discriminator[Person.type])
  }
  extension (n: (city: String, discriminator: Discriminator[Person.type])) {
    @targetName("city_firstName")
    inline def firstName(value: String): (firstName: String, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = value, city = n.city, discriminator = null: Discriminator[Person.type])
    @targetName("city_lastName")
    inline def lastName(value: String): (lastName: String, city: String, discriminator: Discriminator[Person.type]) =
      (lastName = value, city = n.city, discriminator = null: Discriminator[Person.type])

  }
//  extension [N <: ("firstName" *:  "city" *: "discriminator" *: EmptyTuple), V <: (String, String, Discriminator[Person.type])] (n: NamedTuple[N, V]) def lastName(value: String): Person =
  extension (n: (firstName: String,  city: String, discriminator: Discriminator[Person.type])) {
    @targetName("firstNameCity_lastName")
    inline def lastName(value: String): Person =
      Person(n.firstName, value, n.city)
  }
  extension (n: (lastName: String,  city: String, discriminator: Discriminator[Person.type])) {
    @targetName("lastNameCity_firstName")
    inline def firstName(value: String): Person =
      Person(value, n.lastName, n.city)
  }
  extension (n: (firstName: String,  lastName: String, discriminator: Discriminator[Person.type])) {
    @targetName("firstNameLastName_city")
    inline def city(value: String): Person =
      Person(n.firstName, n.lastName, value)
  }
}

object Person {


  def build(): (firstName: Hole, lastName: Hole, city: Hole, discriminator: Discriminator[Person.type]) = {
    (firstName = null: Hole, lastName = null: Hole, city = null: Hole, discriminator = null: Discriminator[Person.type])
  }

  extension (n: (firstName: Hole, lastName: Hole, city: Hole, discriminator: Discriminator[Person.type])) {
    @targetName("firstName")
    def _firstName(value: String): (firstName: String, lastName: Hole, city: Hole, discriminator: Discriminator[Person.type]) =
      (firstName = value, lastName = null: Hole, city = null: Hole, discriminator = null: Discriminator[Person.type])
    @targetName("lastName")
    def _lastName(value: String): (firstName: Hole, lastName: String, city: Hole, discriminator: Discriminator[Person.type]) =
      (firstName = null: Hole, lastName = value, city = null: Hole, discriminator = null: Discriminator[Person.type])
    @targetName("city")
    def _city(value: String): (firstName: Hole, lastName: Hole, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = null: Hole, lastName = null: Hole, city = value, discriminator = null: Discriminator[Person.type])
  }

  extension (n: (firstName: String, lastName: Hole, city: Hole, discriminator: Discriminator[Person.type])){
    @targetName("firstName_lastName")
    def _lastName(value: String): (firstName: String, lastName: String, city: Hole, discriminator: Discriminator[Person.type]) =
      (firstName = n.firstName, lastName = value, city = null: Hole, discriminator = null: Discriminator[Person.type])
    @targetName("firstName_city")
    def _city(value: String): (firstName: String, lastName: Hole, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = n.firstName, lastName = null: Hole, city = value, discriminator = null: Discriminator[Person.type])
  }
  extension (n: (firstName: Hole, lastName: String, city: Hole, discriminator: Discriminator[Person.type])) {
    @targetName("lastName_firstName")
    def _firstName(value: String): (firstName: String, lastName: String, city: Hole, discriminator: Discriminator[Person.type]) =
      (firstName = value, lastName = n.lastName, city = null: Hole, discriminator = null: Discriminator[Person.type])
    @targetName("lastName_city")
    def _city(value: String): (firstName: Hole, lastName: String, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = null: Hole, lastName = n.lastName, city = value, discriminator = null: Discriminator[Person.type])
  }
  extension (n: (firstName: Hole, lastName: Hole, city: String, discriminator: Discriminator[Person.type])) {
    @targetName("city_firstName")
    def _firstName(value: String): (firstName: String, lastName: Hole, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = value, lastName = null: Hole, city = n.city, discriminator = null: Discriminator[Person.type])
    @targetName("city_lastName")
    def _lastName(value: String): (firstName: Hole, lastName: String, city: String, discriminator: Discriminator[Person.type]) =
      (firstName = null: Hole, lastName = value, city = n.city, discriminator = null: Discriminator[Person.type])

  }
//  extension [N <: ("firstName" *:  "city" *: "discriminator" *: EmptyTuple), V <: (String, String, Discriminator[Person.type])] (n: NamedTuple[N, V]) def lastName(value: String): Person =
  extension (n: (firstName: String, lastName: Hole, city: String, discriminator: Discriminator[Person.type]))
    @targetName("firstNameCity_lastName")
    def _lastName(value: String): Person =
    Person(n.firstName, value, n.city)
  extension (n: (firstName: Hole, lastName: String, city: String, discriminator: Discriminator[Person.type]))
    @targetName("lastNameCity_firstName")
    def _firstName(value: String): Person =
    Person(value, n.lastName, n.city)
  extension (n: (firstName: String, lastName: String, city: Hole, discriminator: Discriminator[Person.type]))
    @targetName("firstNameLastName_city")
    def _city(value: String): Person =
    Person(n.firstName, n.lastName, value)
}
*/