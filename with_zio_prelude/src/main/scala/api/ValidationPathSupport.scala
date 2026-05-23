package api

import zio.prelude.ZValidation

object ValidationPathSupport {
  def fieldSegments(pathConfig: ValidationPathConfig, fieldName: String): Seq[ValidationPathPart] = {
    pathConfig.customPrefix match {
      case Some(prefix) => Seq(ValidationPathPart.Custom(prefix), ValidationPathPart.Field(fieldName))
      case None => Seq(ValidationPathPart.Field(fieldName))
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

// ─── Runtime helpers for collection field validators ──────────────────────────

object ValidationCollectionSupport {
  import zio.prelude.ZValidation

  /**
   * Discriminates between pre-built `Seq[B]` and pre-validated `Seq[ZValidation[Nothing, E, B]]`
   * inputs at runtime.
   *
   *  - Pre-built: returns `ZValidation.succeed(input)` with no path enrichment.
   *  - Pre-validated: combines all element ZValidations in order, prepending
   *    `Field(fieldName), Index(i)`, and if the element type has a `@Name` field,
   *    attempts to extract its value from successfully-validated elements and prepend
   *    `Named(Some(extractedValue))` to each element's error path.
   *
   * Returns `ZValidation[Nothing, Any, Any]` (erased at runtime; typed by the macro call site).
   */
  def combineSeqField(
    input: Any,
    fieldName: String,
    elemNameField: Option[String],   // Some(fieldFieldName) if @Name present in element type
    withPath: Boolean,
    elemNameExtractor: Any = null    // (elem: Any) => Option[String], or null if no extraction
  ): ZValidation[Nothing, Any, Any] = {
    val seq = input.asInstanceOf[Seq[Any]]
    if (seq.isEmpty || !seq.head.isInstanceOf[ZValidation[?, ?, ?]]) {
      // Pre-built Seq[B]: no element validation, succeed as-is
      ZValidation.succeed(seq).asInstanceOf[ZValidation[Nothing, Any, Any]]
    } else {
      // Pre-validated Seq[ZValidation[Nothing, E, B]]: combine with optional path enrichment
      val zvSeq = seq.asInstanceOf[Seq[ZValidation[Nothing, Any, Any]]]
      combinePreValidatedSeq(zvSeq, fieldName, elemNameField, elemNameExtractor, withPath)
    }
  }

  private def combinePreValidatedSeq(
    zvSeq: Seq[ZValidation[Nothing, Any, Any]],
    fieldName: String,
    elemNameField: Option[String],
    elemNameExtractor: Any,
    withPath: Boolean
  ): ZValidation[Nothing, Any, Any] = {
    val enriched: Seq[ZValidation[Nothing, Any, Any]] =
      if (!withPath) zvSeq
      else zvSeq.zipWithIndex.map { (zv, idx) =>
        val idxPart  = ValidationPathPart.Index(ValidationPathIndex.wrap(idx))
        // Try to extract Named value from successfully-validated element
        val elemNameOpt: Option[String] = 
          if (elemNameField.isEmpty || elemNameExtractor == null) None
          else {
            try {
              zv.fold(
                _ => None,  // Validation failed; can't extract element
                elem => {
                  val extractor = elemNameExtractor.asInstanceOf[(Any) => Option[String]]
                  extractor(elem)
                }
              ).asInstanceOf[Option[String]]
            } catch { case _: Throwable => None }
          }
        val basePath = buildElemPath(fieldName, idxPart, elemNameField, elemNameOpt)
        enrichError(zv, basePath)
      }
    foldCombine(enriched)
  }

  /**
   * Discriminates between pre-built `Map[K, V]` and pre-validated `Map[K, ZValidation[..., V]]`.
   *
   *  - Pre-built: returns `ZValidation.succeed(input)`.
   *  - Pre-validated: combines values in iterator order, prepending
   *    `Field(fieldName), Index(i)` and `Named(Some(key.toString))` for String-like keys.
   */
  def combineMapField(
    input: Any,
    fieldName: String,
    valNameField: Option[String],
    withPath: Boolean,
    valNameExtractor: Any = null   // (elem: Any) => Option[String], or null if no extraction
  ): ZValidation[Nothing, Any, Any] = {
    val map = input.asInstanceOf[Map[Any, Any]]
    if (map.isEmpty) {
      ZValidation.succeed(map).asInstanceOf[ZValidation[Nothing, Any, Any]]
    } else {
      val firstVal = map.iterator.next()._2
      if (!firstVal.isInstanceOf[ZValidation[?, ?, ?]]) {
        // Pre-built Map[K, V]
        ZValidation.succeed(map).asInstanceOf[ZValidation[Nothing, Any, Any]]
      } else {
        // Pre-validated Map[K, ZValidation[..., V]]
        val preVal = map.asInstanceOf[Map[Any, ZValidation[Nothing, Any, Any]]]
        combinePreValidatedMap(preVal, fieldName, valNameField, valNameExtractor, withPath)
      }
    }
  }

  private def combinePreValidatedMap(
    map: Map[Any, ZValidation[Nothing, Any, Any]],
    fieldName: String,
    valNameField: Option[String],
    valNameExtractor: Any,
    withPath: Boolean
  ): ZValidation[Nothing, Any, Any] = {
    val entries = map.iterator.toSeq
    val enriched: Seq[ZValidation[Nothing, Any, (Any, Any)]] =
      entries.zipWithIndex.map { case ((key, zv), idx) =>
        val idxPart  = ValidationPathPart.Index(ValidationPathIndex.wrap(idx))
        val basePath: Seq[ValidationPathPart] =
          if (!withPath) Seq.empty
          else {
            // Try to extract Named value from successfully-validated element
            val elemNameOpt: Option[String] = 
              if (valNameField.isEmpty || valNameExtractor == null) None
              else {
                try {
                  zv.fold(
                    _ => None,  // Validation failed; can't extract element
                    elem => {
                      val extractor = valNameExtractor.asInstanceOf[(Any) => Option[String]]
                      extractor(elem)
                    }
                  ).asInstanceOf[Option[String]]
                } catch { case _: Throwable => None }
              }
            val keyNamed = ValidationPathPart.Named(key match {
              case s: String => Some(s)
              case other     => Some(other.toString)
            })
            buildElemPath(fieldName, idxPart, valNameField, elemNameOpt) :+ keyNamed
          }
        val enrichedVal: ZValidation[Nothing, Any, Any] =
          if (!withPath) zv else enrichError(zv, basePath)
        enrichedVal.map(v => (key, v))
      }
    foldCombine(enriched).map(pairs =>
      pairs.asInstanceOf[Seq[(Any, Any)]].toMap
    ).asInstanceOf[ZValidation[Nothing, Any, Any]]
  }

  // ── private utilities ──────────────────────────────────────────────────────

  private def buildElemPath(
    fieldName: String,
    idxPart: ValidationPathPart,
    namedFieldOpt: Option[String],
    namedValueOpt: Option[String] = None
  ): Seq[ValidationPathPart] = {
    val named = namedFieldOpt.map(_ => ValidationPathPart.Named(namedValueOpt))
    Seq(ValidationPathPart.Field(fieldName), idxPart) ++ named
  }

  private def enrichError(
    zv: ZValidation[Nothing, Any, Any],
    basePath: Seq[ValidationPathPart]
  ): ZValidation[Nothing, Any, Any] = {
    if (basePath.isEmpty) zv
    else zv.mapError {
      case vpe: ValidationPathError[?] =>
        val p = vpe.asInstanceOf[ValidationPathError[Any]]
        ValidationPathError(basePath ++ p.path, p.error).asInstanceOf[Any]
      case other =>
        ValidationPathError(basePath, other).asInstanceOf[Any]
    }
  }

  /** Folds a sequence of ZValidations with parallel error accumulation. */
  private def foldCombine[A](
    zvs: Seq[ZValidation[Nothing, Any, A]]
  ): ZValidation[Nothing, Any, Seq[A]] = {
    zvs.foldLeft(
      ZValidation.succeed(Vector.empty[A]).asInstanceOf[ZValidation[Nothing, Any, Vector[A]]]
    ) { (acc, zv) =>
      acc.zipWithPar(zv)((vec, a) => vec :+ a)
    }.map(_.toSeq)
  }
}

