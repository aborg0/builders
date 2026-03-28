package api

import zio.prelude.ZValidation

object ValidationPathSupport {
  def fieldSegments(pathConfig: ValidationPathConfig, fieldName: String): Seq[ValidationPathPart] = {
    pathConfig.customPrefix match {
      case Some(prefix) => Seq(ValidationPathPart.Custom(prefix), ValidationPathPart.Name(fieldName))
      case None => Seq(ValidationPathPart.Name(fieldName))
    }
  }

  def attachPath[E, A](
    segments: Seq[ValidationPathPart],
    validation: ZValidation[Nothing, E, A]
  ): ZValidation[Nothing, ValidationPathError[E], A] = {
    validation.mapError(error => ValidationPathError(segments, error))
  }

  def prependExistingPath[E, A](
    segments: Seq[ValidationPathPart],
    validation: ZValidation[Nothing, ValidationPathError[E], A]
  ): ZValidation[Nothing, ValidationPathError[E], A] = {
    validation.mapError { case ValidationPathError(existingSegments, error) =>
      ValidationPathError(segments ++ existingSegments, error)
    }
  }
}
