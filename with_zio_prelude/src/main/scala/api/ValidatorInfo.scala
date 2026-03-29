package api

import scala.quoted.*

/**
 * Represents information about how to validate a field at compile time.
 * This is extracted during macro expansion.
 * 
 * We use Any to store quote-dependent types and cast them back in the macro context.
 * This is safe because we only use these within a single macro invocation.
 */
sealed trait ValidatorInfo {
  def fieldName: String
}

object ValidatorInfo {
  /**
   * A field that needs validation via a smart constructor.
   */
  case class NeedsValidation(
    fieldName: String,
    primitiveType: Any,      // Actually quotes.reflect.TypeRepr
    wrappedType: Any,        // Actually quotes.reflect.TypeRepr
    errorType: Any,          // Actually quotes.reflect.TypeRepr
    companionSymbol: Any,    // Actually quotes.reflect.Symbol
    methodName: String,
    validationKind: ValidationKind,
    isNameAnnotated: Boolean = false
  ) extends ValidatorInfo

  /**
   * A field that doesn't need validation (plain type like Int, String, etc.)
   */
  case class NoValidation(
    fieldName: String,
    plainType: Any,          // Actually quotes.reflect.TypeRepr
    isNameAnnotated: Boolean = false
  ) extends ValidatorInfo

  /**
   * A Seq/List/Set/Vector field.  Elements may be pre-built (no per-element validation) or
   * pre-validated (Seq[ZValidation[Nothing, E, B]]) — the runtime validator discriminates.
   * `elemInfo` describes how to validate element type B (NoValidation if plain).
   * `elemNameField` is the @Name-annotated constructor parameter name in B, if present.
   */
  case class SeqLikeValidation(
    fieldName: String,
    collectionType: Any,           // TypeRepr: full collection type, e.g. List[Inner]
    elemType: Any,                 // TypeRepr: element type B
    elemInfo: ValidatorInfo,       // validator for B
    elemNameField: Option[String] = None,
    isNameAnnotated: Boolean = false
  ) extends ValidatorInfo

  /**
   * A Map[K,V] field.  Values may be pre-built or pre-validated
   * (Map[K, ZValidation[Nothing, E, V]]).
   * `valNameField` is the @Name-annotated constructor parameter name in V, if present.
   */
  case class MapValidation(
    fieldName: String,
    mapType: Any,                  // TypeRepr: Map[K, V]
    keyType: Any,                  // TypeRepr: K
    valueType: Any,                // TypeRepr: V
    valueInfo: ValidatorInfo,      // validator for V
    valNameField: Option[String] = None,
    isNameAnnotated: Boolean = false
  ) extends ValidatorInfo

  enum ValidationKind {
    case FromValidation  // Returns ZValidation[_, E, T] directly
    case FromEither      // Returns Either[E, T], needs ZValidation.fromEither
  }
}
