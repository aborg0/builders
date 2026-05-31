import builders.configuration.*

@GenerateBuilder(
  style = BuilderStyle.Validating,
  pathMode = PathMode.FullCollectionAware,
  primitivePolicy = PrimitivePolicy.PrimitiveAndWrappedIfDerivable
)
case class ValidatingUser(id: Int, code: String)
