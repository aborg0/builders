package playground

object Opaque {
  opaque type Op = String
  object Op {
    def apply(s: String): Either[String, Op] =
      s match {
        case "Op" => Right(s: Op).withLeft[String]
        case _ => Left(s"'$s' is not Op.").withRight[Op]
      }
  }
  opaque type ValidOp = String
  object ValidOp {

    import zio.prelude.Validation

    def apply(s: String): Validation[String, ValidOp] =
      s match {
        case "Op" => Validation.succeed(s: ValidOp)
        case _ => Validation.fail(s"'$s' is not Op.")
      }
  }
}
