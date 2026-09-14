package builders.scalafix

import builders.scalafix.GenerateBuilderCompanionRenderer.SmartCtorResultKind
import utest._

object GenerateBuilderSmartCtorDiscoveryTests extends TestSuite {
  val tests: Tests = Tests {
    test("discovers ZIO smart constructor environment and error types") {
      val source =
        """import zio.ZIO

trait DbService
sealed trait DbError

opaque type DbWrapped = String
object DbWrapped {
  def apply(raw: String): ZIO[DbService, DbError, DbWrapped] = zio.ZIO.succeed(raw: DbWrapped)
}
"""

      val discovered = GenerateBuilderSmartCtorDiscovery.discoverFromSource(
        fieldTypeExpr = "DbWrapped",
        source = source,
        mode = SmartConstructorMode.ZValidation,
        enableEffectConstructors = true
      )

      assert(discovered.nonEmpty)
      val ctor = discovered.get
      assert(ctor.methodName == "apply")
      assert(ctor.resultKind == SmartCtorResultKind.ZioResult)
      assert(ctor.inputTypeExpr.contains("String"))
      assert(ctor.zioEnvironmentTypeExpr.contains("DbService"))
      assert(ctor.zioErrorTypeExpr.contains("DbError"))
    }

    test("discovers RIO smart constructor with inferred Throwable error") {
      val source =
        """trait DbService

opaque type DbWrapped = String
object DbWrapped {
  def apply(raw: String): zio.RIO[DbService, DbWrapped] = zio.ZIO.succeed(raw: DbWrapped)
}
"""

      val discovered = GenerateBuilderSmartCtorDiscovery.discoverFromSource(
        fieldTypeExpr = "DbWrapped",
        source = source,
        mode = SmartConstructorMode.ZValidation,
        enableEffectConstructors = true
      )

      assert(discovered.nonEmpty)
      val ctor = discovered.get
      assert(ctor.resultKind == SmartCtorResultKind.ZioResult)
      assert(ctor.zioEnvironmentTypeExpr.contains("DbService"))
      assert(ctor.zioErrorTypeExpr.contains("Throwable"))
    }
  }
}
