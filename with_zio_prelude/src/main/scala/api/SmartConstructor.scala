package api

import zio.prelude.ZValidation

/**
 * Type class that describes how to validate a primitive type into a wrapped type.
 * 
 * Note: We use ZValidation[Nothing, E, W] which is the full form.
 * Validation[E, W] is just a type alias for ZValidation[Nothing, E, W].
 * 
 * @tparam Error The error type (using String for simplicity in v1)
 * @tparam Primitive The input type (e.g., String, Int)
 * @tparam Wrapped The validated output type (e.g., NonEmptyString, PositiveInt)
 */
trait SmartConstructor[Error, Primitive, Wrapped] {
  def validate(primitive: Primitive): ZValidation[Nothing, Error, Wrapped]
}

object SmartConstructor {
  /**
   * Helper to create a SmartConstructor instance
   */
  def apply[E, P, W](f: P => ZValidation[Nothing, E, W]): SmartConstructor[E, P, W] =
    new SmartConstructor[E, P, W] {
      def validate(primitive: P): ZValidation[Nothing, E, W] = f(primitive)
    }
    
  /**
   * Identity smart constructor for types that don't need validation
   */
  def identity[T]: SmartConstructor[Nothing, T, T] =
    new SmartConstructor[Nothing, T, T] {
      def validate(value: T): ZValidation[Nothing, Nothing, T] = ZValidation.succeed(value)
    }
}
