package bench

// A tiny simple (non-validated) builder that mirrors SmallValidated's API shape.
final case class SimpleSmall(i: Int, op: String, v: String)

object SimpleSmallBuilder {
  def apply(): (i: Int => (op: String => (v: String => SimpleSmall))) =
    (i = (i: Int) =>
      (op = (op: String) =>
        (v = (v: String) =>
          SimpleSmall(i, op, v))))
}

