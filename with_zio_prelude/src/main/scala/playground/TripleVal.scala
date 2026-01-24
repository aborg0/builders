package playground

import playground.Opaque.SequenceNumber

case class TripleVal(i: Int, op: Opaque.Op, v: Opaque.ValidOp, s: SequenceNumber)

object TripleValO {

  import zio.prelude.Validation

  def apply(): (i: Int => (op: String => (v: String => (s: Int => Validation[String, TripleVal])))) =
    (i= (i: Int) =>
      (op= (op: String) =>
        (v= (v: String) =>
          (s= (s: Int) =>
          Validation.validateWith(
            Validation.succeed(i),
            Validation.fromEither(Opaque.Op(op)),
            Opaque.ValidOp(v),
            SequenceNumber.make(s)
            )
            ((vi, vop, vv, vs) => TripleVal(vi, vop, vv, vs))))))
}

object TripleValVal {

  import zio.prelude.Validation

  def apply(): (i: Int | Validation[Nothing, Int] => (op: String | Validation[String, Opaque.Op] => (v: String | Validation[String, Opaque.ValidOp] => (s: Int | Validation[String, SequenceNumber] => Validation[String, TripleVal])))) =
    (i= (i: Int | Validation[Nothing, Int]) =>
      (op= (op: String | Validation[String, Opaque.Op]) =>
        (v= (v: String | Validation[String, Opaque.ValidOp]) =>
          (s= (s: Int | SequenceNumber | Validation[String, SequenceNumber]) =>
          Validation.validateWith(
            i match {
              case ri: Int => Validation.succeed(ri)
              case vi: Validation[Nothing, Int] => vi
            },
            op match {
              case sop: String => Validation.fromEither(Opaque.Op(sop))
              case vop: Validation[String, Opaque.Op] => vop
            },
            v match {
              case sv: String => Opaque.ValidOp(sv)
              case vv: Validation[String, Opaque.ValidOp] => vv
            },
            s match {
              case is: Int => SequenceNumber.make(is)
              case sn: SequenceNumber => Validation.succeed(sn)
              case vs: Validation[String, SequenceNumber] => vs
            })
            ((vi, vop, vv, vs) => TripleVal(vi, vop, vv, vs))))))
}
