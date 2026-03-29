package api

import scala.annotation.StaticAnnotation

/**
 * Marks exactly one constructor parameter of a case class as the "identity name" field.
 *
 * When a validated builder produces errors for elements inside a collection field, the
 * runtime value of this field is extracted and inserted as a `ValidationPathPart.Named`
 * segment immediately after the `Index` segment, helping callers identify which element
 * failed by name in addition to position.
 *
 * Constraints:
 *  - At most one field per case class may carry this annotation; the macro rejects two or more.
 *  - The annotated field must be a `String` (or a type whose `toString` is meaningful);
 *    the macro extracts it as `Option[String]` so the path remains informative even when
 *    the value is not yet available (pre-validated inputs that fail).
 *
 * Example:
 * {{{
 * final case class Inner(@Name id: String, ops: Seq[Op])
 * }}}
 */
final class Name extends StaticAnnotation
