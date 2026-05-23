package bench

import zio.prelude.Validation
import playground._

// Provide thin adapters so benchmarks can call a uniform API shape.
object ManualBuilders {

  // Adapter for the manual TripleValO builder implemented in with_zio_prelude
  object ManualTripleValO {
    def builder() = TripleValO()
  }

  // Adapter for the manual TripleValVal builder implemented in with_zio_prelude
  object ManualTripleValVal {
    def builder() = TripleValVal()
  }

  // Adapter for SmallValidated manual builder defined in this subproject
  object ManualSmallValidated {
    def builder() = SmallValidatedManual()
  }

}
