package e2e

import builders.configuration.*
import zio.prelude.Validation
import zio.prelude.ZValidation

enum FixtureError {
	case Missing
	case Invalid
}

opaque type OpaqueCode = String
object OpaqueCode {
	def apply(raw: String): Either[FixtureError, OpaqueCode] =
		raw match {
			case "ok" => Right(raw: OpaqueCode)
			case "miss" => Left(FixtureError.Missing)
			case _ => Left(FixtureError.Invalid)
		}
}

opaque type OpaqueOp = String
object OpaqueOp {
	def apply(raw: String): ZValidation[Nothing, String, OpaqueOp] =
		if (raw == "Op") {
			Validation.succeed(raw: OpaqueOp)
		} else {
			Validation.fail(s"Invalid op: $raw")
		}
}

class PositiveInt(val value: Int)
object PositiveInt {
	def make(raw: Int): Either[String, PositiveInt] =
		if (raw > 0) {
			Right(new PositiveInt(raw))
		} else {
			Left(s"Expected positive integer, got $raw")
		}
}

@GenerateBuilder(style = BuilderStyle.Validating)
case class ValidatedUser(id: Int, code: String)

@GenerateBuilder(style = BuilderStyle.Validating, generatedCodeShape = GeneratedCodeShape.Performance)
case class PerfValidatedUser(id: Int, code: OpaqueCode)

case class OpaqueSmartUser(id: Int, code: OpaqueCode, op: OpaqueOp)

case class MakeSmartUser(amount: PositiveInt, label: String)