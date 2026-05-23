package api

object CompanionProbe {
  case class Plain(a: Int, b: String)

  // This invocation should compile if companion lookup and combine/apply building works
  val genPlain = ValidatedBuilderGenerator.builderNoAllow[Plain]

  // A case with Newtype/Subtype-like owner: TestNew defined in test resources
  object TestNew {
    object TestNewObj {
      def make(i: Int): zio.prelude.Validation[String, TestNewObj.type] = zio.prelude.Validation.fail("no")
    }
  }
}

