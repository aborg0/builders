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
    primitiveType: Any,   // Actually quotes.reflect.TypeRepr
    wrappedType: Any,      // Actually quotes.reflect.TypeRepr
    errorType: Any,        // Actually quotes.reflect.TypeRepr
    companionSymbol: Any,  // Actually quotes.reflect.Symbol
    methodName: String,
    validationKind: ValidationKind
  ) extends ValidatorInfo

  /**
   * A field that doesn't need validation (plain type like Int, String, etc.)
   */
  case class NoValidation(
    fieldName: String,
    plainType: Any  // Actually quotes.reflect.TypeRepr
  ) extends ValidatorInfo
  
  enum ValidationKind {
    case FromValidation  // Returns ZValidation[_, E, T] directly
    case FromEither      // Returns Either[E, T], needs ZValidation.fromEither
  }
}
