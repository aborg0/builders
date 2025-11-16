package playground

case class Triple(i: Int, op: Opaque.Op, v: Opaque.ValidOp)

object TripleO {

  import zio.prelude.Validation

  def apply(): (i: Int => (op: String => (v: String => Validation[String, Triple]))) =
    (i= (i: Int) =>
      (op= (op: String) =>
        (v= (v: String) =>
          Validation.validateWith(
            Validation.succeed(i),
            Validation.fromEither(Opaque.Op(op)),
            Opaque.ValidOp(v))
            ((vi, vop, vv) => Triple(vi, vop, vv)))))
}
