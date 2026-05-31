import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple)
case class SimpleUser(i: Int, name: String)
