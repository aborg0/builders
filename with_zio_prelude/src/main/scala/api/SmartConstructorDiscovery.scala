package api

import scala.quoted.*
import api.ValidatorInfo.*

/**
 * Discovers smart constructors for types at compile time.
 * 
 * Discovery priority:
 * 1. companion.apply returning Validation[_, T]
 * 2. companion.make returning Validation[_, T]
 * 3. companion.apply returning Either[_, T]
 * 4. No validation needed (identity)
 * 
 * Note: These are regular functions used during macro expansion, not inline defs.
 */
object SmartConstructorDiscovery {
  
  /**
   * Discovers how to validate a field type.
   * Returns ValidatorInfo describing the validation strategy.
   */
  def discoverValidator(using Quotes)(
    fieldName: String,
    fieldType: quotes.reflect.TypeRepr
  ): ValidatorInfo = {
    import quotes.reflect.*
    
    // First, check if the type has a companion object
    val typeSymbol = fieldType.typeSymbol
    
    if (!typeSymbol.companionModule.exists) {
      // No companion - this is a plain type, no validation needed
      return ValidatorInfo.NoValidation(fieldName, fieldType)
    }
    
    val companion = typeSymbol.companionModule
    
    // Try to find a validation method
    findValidationMethod(companion, fieldType, fieldName) match {
      case Some(info) => info
      case None => ValidatorInfo.NoValidation(fieldName, fieldType)
    }
  }
  
  /**
   * Tries to find a suitable validation method in the companion object.
   */
  private def findValidationMethod(using Quotes)(
    companion: quotes.reflect.Symbol,
    targetType: quotes.reflect.TypeRepr,
    fieldName: String
  ): Option[ValidatorInfo.NeedsValidation] = {
    import quotes.reflect.*
    
    // Try apply method first, then make
    findMethodValidation(companion, "apply", targetType, fieldName)
      .orElse(findMethodValidation(companion, "make", targetType, fieldName))
  }
  
  /**
   * Looks for a method (apply or make) returning Validation or Either.
   */
  private def findMethodValidation(using Quotes)(
    companion: quotes.reflect.Symbol,
    methodName: String,
    targetType: quotes.reflect.TypeRepr,
    fieldName: String
  ): Option[ValidatorInfo.NeedsValidation] = {
    import quotes.reflect.*
    
    val methods = companion.declaredMethods.filter(_.name == methodName)
    
    methods.collectFirst {
      case method =>
        val companionRef = Ref(companion)
        val methodType = companionRef.select(method).tpe.widen
        
        methodType match {
          case MethodType(paramNames, paramTypes, returnType) if paramNames.length == 1 =>
            // Check if return type is Validation[E, T] or Either[E, T]
            analyzeReturnType(returnType, targetType) match {
              case Some((errorType, validationKind)) =>
                val primitiveType = paramTypes.head
                Some(ValidatorInfo.NeedsValidation(
                  fieldName = fieldName,
                  primitiveType = primitiveType,
                  wrappedType = targetType,
                  errorType = errorType,
                  companionSymbol = companion,
                  methodName = methodName,
                  validationKind = validationKind
                ))
              case None => None
            }
          case _ => None
        }
    }.flatten
  }
  
  /**
   * Analyzes a return type to see if it's Validation[E, T] or Either[E, T].
   * Returns Some((errorType, validationKind)) if valid, None otherwise.
   * 
   * Note: Validation[E, T] is a type alias for ZValidation[Nothing, E, T]
   */
  private def analyzeReturnType(using Quotes)(
    returnType: quotes.reflect.TypeRepr,
    expectedWrappedType: quotes.reflect.TypeRepr
  ): Option[(quotes.reflect.TypeRepr, ValidationKind)] = {
    import quotes.reflect.*
    
    returnType match {
      // Match ZValidation[W, E, T] (which includes Validation[E, T] as ZValidation[Nothing, E, T])
      case AppliedType(tycon, List(logType, errorType, wrappedType)) 
        if tycon.typeSymbol.fullName == "zio.prelude.ZValidation" =>
        // Check if wrappedType matches our expected type (allowing for opaque type equality)
        if (wrappedType =:= expectedWrappedType || wrappedType.simplified =:= expectedWrappedType.simplified) {
          Some((errorType, ValidationKind.FromValidation))
        } else {
          None
        }
      
      // Match Either[E, T]
      case AppliedType(tycon, List(errorType, wrappedType))
        if tycon.typeSymbol.fullName == "scala.util.Either" =>
        if (wrappedType =:= expectedWrappedType || wrappedType.simplified =:= expectedWrappedType.simplified) {
          Some((errorType, ValidationKind.FromEither))
        } else {
          None
        }
      
      case _ => None
    }
  }
  
  /**
   * Creates a Term that calls the validator on a given argument Term.
   * This generates the actual validation call at macro expansion time.
   */
  def createValidationCall(using Quotes)(
    validatorInfo: ValidatorInfo.NeedsValidation,
    argumentTerm: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    
    val companionSymbol = validatorInfo.companionSymbol.asInstanceOf[Symbol]
    val companionRef = Ref(companionSymbol)
    val methodSymbol = companionSymbol.declaredMethod(validatorInfo.methodName).head
    val methodCall = companionRef.select(methodSymbol).appliedTo(argumentTerm)
    
    validatorInfo.validationKind match {
      case ValidationKind.FromValidation =>
        // Already returns ZValidation, use as-is
        methodCall
        
      case ValidationKind.FromEither =>
        // Need to wrap with ZValidation.fromEither
        val zvalidationModule = TypeRepr.of[zio.prelude.ZValidation.type].typeSymbol.companionModule
        val zvalidationRef = Ref(zvalidationModule)
        val fromEitherMethods = zvalidationModule.declaredMethods.filter(_.name == "fromEither")
        val fromEitherMethod = fromEitherMethods.head
        
        // fromEither likely takes type parameters [E, A] only
        val eitherType = methodCall.tpe.widen
        // Extract E and A from Either[E, A]
        val (errorType, resultType) = eitherType match {
          case AppliedType(_, List(e, a)) => (e, a)
          case _ => (TypeRepr.of[Any], TypeRepr.of[Any])
        }
        zvalidationRef
          .select(fromEitherMethod)
          .appliedToTypes(List(errorType, resultType))
          .appliedTo(methodCall)
    }
  }
  
  /**
   * Creates a Term that wraps a plain value in ZValidation.succeed.
   * This produces ZValidation[Nothing, String, T] by widening the error channel
   */
  def createSucceedCall(using Quotes)(
    argumentTerm: quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    
    // Access ZValidation companion and call succeed
    val zvalidationModule = TypeRepr.of[zio.prelude.ZValidation.type].typeSymbol.companionModule
    val zvalidationRef = Ref(zvalidationModule)
    
    // Get the succeed method
    val succeedMethods = zvalidationModule.declaredMethods.filter(_.name == "succeed")
    val succeedMethod = succeedMethods.head
    
    // Create ZValidation.succeed[A](value) which gives ZValidation[Nothing, Nothing, A]
    val argType = argumentTerm.tpe.widen
    val succeedCall = zvalidationRef
      .select(succeedMethod)
      .appliedToType(argType)
      .appliedTo(argumentTerm)
    
    // Widen the error type from Nothing to String
    // We can call .mapError on the validation to widen the error channel
    // Or use identity function that widens the type
    // Actually, we need to use mapError with an identity-like function
    // Let's use: validation.asInstanceOf[ZValidation[Nothing, String, A]]
    // But that's not safe. Better: use mapError(_ => throw new Exception())
    // which will never be called but widens the type
    
    // Best approach: just use Typed to ascribe the wider type
    val widenedType = TypeRepr.of[zio.prelude.ZValidation]
      .appliedTo(List(TypeRepr.of[Nothing], TypeRepr.of[String], argType))
    
    Typed(
      succeedCall,
      Inferred(widenedType)
    )
  }
}
