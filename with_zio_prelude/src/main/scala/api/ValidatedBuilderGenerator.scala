package api

import api.ValidatorInfo.ValidationKind.{FromEither, FromValidation}
import scala.quoted.*
import scala.NamedTuple.AnyNamedTuple
import zio.prelude.ZValidation

// ─── Main generator ───────────────────────────────────────────────────────────

trait ValidatedBuilderGenerator[T] {
  type Builder
  def apply(): Builder
}

trait NoAllowBuilderInstance[T] extends ValidatedBuilderGenerator[T]
trait AllowBuilderInstance[T] extends ValidatedBuilderGenerator[T]
trait NoAllowErrorType[T] {
  type E
}
trait AllowErrorType[T] {
  type E
}
trait AllowBuilderType[T] {
  type Builder
}
trait AllowNoTransparentBuilder[T] {
  type Builder
  def builder: Builder
}

object NoAllowBuilderInstance {
  type Aux[T, B] = NoAllowBuilderInstance[T] { type Builder = B }

  inline given [T]: NoAllowBuilderInstance[T] =
    ${ ValidatedBuilderGenerator.noAllowBuilderInstanceImpl[T] }
}

object AllowBuilderInstance {
  type Aux[T, B] = AllowBuilderInstance[T] { type Builder = B }

  inline given [T]: AllowBuilderInstance[T] =
    ${ ValidatedBuilderGenerator.allowBuilderInstanceImpl[T] }
}

object NoAllowErrorType {
  type Aux[T, E0] = NoAllowErrorType[T] { type E = E0 }

  inline given [T]: NoAllowErrorType[T] =
    ${ ValidatedBuilderGenerator.noAllowErrorTypeImpl[T] }
}

object AllowErrorType {
  type Aux[T, E0] = AllowErrorType[T] { type E = E0 }

  inline given [T]: AllowErrorType[T] =
    ${ ValidatedBuilderGenerator.allowErrorTypeImpl[T] }
}

object AllowBuilderType {
  type Aux[T, B] = AllowBuilderType[T] { type Builder = B }

  inline given [T]: AllowBuilderType[T] =
    ${ ValidatedBuilderGenerator.allowBuilderTypeImpl[T] }
}

object AllowNoTransparentBuilder {
  type Aux[T, B] = AllowNoTransparentBuilder[T] { type Builder = B }

  inline given [T]: AllowNoTransparentBuilder[T] =
    ${ ValidatedBuilderGenerator.allowNoTransparentBuilderImpl[T] }
}

object ValidatedBuilderGenerator {
  import scala.NamedTuple
  import scala.NamedTuple.*
  import scala.NamedTuple.Split
  import scala.compiletime.ops.boolean.&&
  import zio.prelude.ZValidation
  import api.ValidatorInfo.ValidationKind

  type Tup[T] = NamedTuple.From[T]
  import scala.quoted.*

  // ─── Match types ──────────────────────────────────────────────────────────────

  type IsOptionalLike[T] <: Boolean = T match {
    case Option[?] => true
    case java.util.Optional[?] => true
    case java.util.OptionalInt => true
    case java.util.OptionalLong => true
    case java.util.OptionalDouble => true
    case None.type => true
    case Null => true
    case a | b => scala.compiletime.ops.boolean.||[IsOptionalLike[a], IsOptionalLike[b]]
    case _ => false
  }

  type AllOptional[Ts <: Tuple] <: Boolean = Ts match {
    case EmptyTuple => true
    case h *: t => IsOptionalLike[h] && AllOptional[t]
  }

  sealed trait IsTrue[B <: Boolean]
  object IsTrue {
    given IsTrue[true] with {}
  }

  sealed trait CanComplete[R <: AnyNamedTuple]
  object CanComplete {
    inline given [R <: AnyNamedTuple]: CanComplete[R] = ${ canCompleteGivenImpl[R] }
  }

  private def canCompleteGivenImpl[R <: AnyNamedTuple: Type](using Quotes): Expr[CanComplete[R]] = {
    import quotes.reflect.*

    val remaining = namedTupleEntries(TypeRepr.of[R])
    if (remaining.isEmpty) {
      report.errorAndAbort("Cannot complete builder: no remaining fields were detected for this step")
    }

    val required = remaining.filterNot { case (_, tpe) => isOptionalInputType(tpe) }
    if (required.nonEmpty) {
      val rendered = required.map { case (name, tpe) => s"$name: ${tpe.show}" }.mkString(", ")
      report.errorAndAbort(
        s"Cannot complete builder with .!: required fields remain -> $rendered"
      )
    }

    '{ new CanComplete[R] {} }
  }

  type BuilderFields[R <: AnyNamedTuple, T, E] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case Tuple1[h] =>
        NamedTuple[NamedTuple.Names[R], Tuple1[h => ZValidation[Nothing, E, T]]]
      case h *: t =>
        NamedTuple[
          Tuple1[Tuple.Head[NamedTuple.Names[R]]],
          Tuple1[h => BuilderFields[
            NamedTuple[Tuple.Tail[NamedTuple.Names[R]], t],
            T,
            E
          ]]
        ]
    }

  // Legacy alias
  type ValidatedBuilderFor[H <: AnyNamedTuple, R <: AnyNamedTuple, T, E] <: AnyNamedTuple =
    NamedTuple.DropNames[R] match {
      case EmptyTuple =>
        NamedTuple[NamedTuple.Names[H],
          Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => ZValidation[Nothing, E, T]]]
      case h *: t =>
        NamedTuple[NamedTuple.Names[H],
          Tuple1[Tuple.Head[NamedTuple.DropNames[H]] => ValidatedBuilderFor[
            Tuple.Head[Split[R,1]], Tuple.Last[Split[R,1]], T, E]]]
    }

  type ValidatedBuilder[T, E] = ValidatedBuilderFor[
    Tuple.Head[Split[Tup[T], 1]], Tuple.Last[Split[Tup[T], 1]], T, E]

  type BuilderStep[K <: String & Singleton, In, Out] =
    NamedTuple[Tuple1[K], Tuple1[In => Out]]

  // ─── Public entry points ──────────────────────────────────────────────────────

  inline def derived[T]: ValidatedBuilderGenerator[T]        = ${ derivedImplNoAllow[T] }
  inline def derived[T](pathConfig: ValidationPathConfig): ValidatedBuilderGenerator[T] =
    ${ derivedImplNoAllowWithPath[T]('pathConfig) }
  inline def derived[T](customPrefix: Option[String]): ValidatedBuilderGenerator[T] =
    ${ derivedImplNoAllowWithPath[T]('{ ValidationPathConfig(customPrefix = customPrefix) }) }
  inline def derivedNoAllow[T]: ValidatedBuilderGenerator[T] = ${ derivedImplNoAllow[T] }
  inline def derivedNoAllow[T](pathConfig: ValidationPathConfig): ValidatedBuilderGenerator[T] =
    ${ derivedImplNoAllowWithPath[T]('pathConfig) }
  inline def derivedNoAllow[T](customPrefix: Option[String]): ValidatedBuilderGenerator[T] =
    ${ derivedImplNoAllowWithPath[T]('{ ValidationPathConfig(customPrefix = customPrefix) }) }
  inline def derivedAllow[T]: ValidatedBuilderGenerator[T]   = ${ derivedImplAllow[T] }
  inline def derivedAllow[T](pathConfig: ValidationPathConfig): ValidatedBuilderGenerator[T] =
    ${ derivedImplAllowWithPath[T]('pathConfig) }
  inline def derivedAllow[T](customPrefix: Option[String]): ValidatedBuilderGenerator[T] =
    ${ derivedImplAllowWithPath[T]('{ ValidationPathConfig(customPrefix = customPrefix) }) }

  transparent inline def builder[T]        = ${ builderImplNoAllow[T] }
  transparent inline def builderTyped[T]   = ${ builderImplNoAllow[T] }
  def builderTypedNoTransparent[T](using errorType: NoAllowErrorType[T], instance: NoAllowBuilderInstance[T]): ValidatedBuilder[T, errorType.E] =
    instance.asInstanceOf[NoAllowBuilderInstance.Aux[T, ValidatedBuilder[T, errorType.E]]].apply()
  def builderTypedStrict[T, B](using instance: NoAllowBuilderInstance.Aux[T, B]): B =
    instance.apply()
  transparent inline def builder[T](pathConfig: ValidationPathConfig) =
    ${ builderImplNoAllowWithPath[T]('pathConfig) }
  transparent inline def builder[T](customPrefix: Option[String]) =
    ${ builderImplNoAllowWithPath[T]('{ ValidationPathConfig(customPrefix = customPrefix) }) }
  transparent inline def builderNoAllow[T] = ${ builderImplNoAllow[T] }
  transparent inline def builderNoAllowTyped[T] = ${ builderImplNoAllow[T] }
  def builderNoAllowTypedStrict[T, B](using instance: NoAllowBuilderInstance.Aux[T, B]): B =
    instance.apply()
  transparent inline def builderNoAllow[T](pathConfig: ValidationPathConfig) =
    ${ builderImplNoAllowWithPath[T]('pathConfig) }
  transparent inline def builderNoAllow[T](customPrefix: Option[String]) =
    ${ builderImplNoAllowWithPath[T]('{ ValidationPathConfig(customPrefix = customPrefix) }) }
  transparent inline def builderAllow[T]   = ${ builderImplAllow[T] }
  transparent inline def builderAllowTyped[T] = ${ builderImplAllow[T] }
  inline def builderAllowTypedNoTransparentUnion[T](using noTransparent: AllowNoTransparentBuilder[T]): noTransparent.Builder =
    noTransparent.builder
  def withBuilderAllowTypedNoTransparent[T, R](using noTransparent: AllowNoTransparentBuilder[T])(f: noTransparent.Builder => R): R =
    f(noTransparent.builder)
  def withBuilderAllowTypedNoTransparentDependent[T, R](using noTransparent: AllowNoTransparentBuilder[T])(f: (n: AllowNoTransparentBuilder[T]) ?=> n.Builder => R): R =
    f(using noTransparent)(noTransparent.builder)
  def builderAllowTypedNoTransparent[T](using errorType: AllowErrorType[T], instance: AllowBuilderInstance[T]): ValidatedBuilder[T, errorType.E] =
    instance.asInstanceOf[AllowBuilderInstance.Aux[T, ValidatedBuilder[T, errorType.E]]].apply()
  def builderAllowTypedNoTransparentUnion[T](builderType: AllowBuilderType[T])(using instance: AllowBuilderInstance[T]): builderType.Builder =
    instance.asInstanceOf[AllowBuilderInstance.Aux[T, builderType.Builder]].apply()
  def builderAllowTypedStrict[T, B](using instance: AllowBuilderInstance.Aux[T, B]): B =
    instance.apply()
  transparent inline def builderAllow[T](pathConfig: ValidationPathConfig) =
    ${ builderImplAllowWithPath[T]('pathConfig) }
  transparent inline def builderAllow[T](customPrefix: Option[String]) =
    ${ builderImplAllowWithPath[T]('{ ValidationPathConfig(customPrefix = customPrefix) }) }

  // ─── Macro implementations ────────────────────────────────────────────────────

  def derivedImplAllow[T: Type](using Quotes): Expr[ValidatedBuilderGenerator[T]]   =
    derivedImpl[T](allowUnion = true, withPath = false, '{ ValidationPathConfig() })
  def derivedImplNoAllow[T: Type](using Quotes): Expr[ValidatedBuilderGenerator[T]] =
    derivedImpl[T](allowUnion = false, withPath = false, '{ ValidationPathConfig() })
  def derivedImplAllowWithPath[T: Type](pathConfig: Expr[ValidationPathConfig])(using Quotes): Expr[ValidatedBuilderGenerator[T]] =
    derivedImpl[T](allowUnion = true, withPath = true, pathConfig)
  def derivedImplNoAllowWithPath[T: Type](pathConfig: Expr[ValidationPathConfig])(using Quotes): Expr[ValidatedBuilderGenerator[T]] =
    derivedImpl[T](allowUnion = false, withPath = true, pathConfig)

  def noAllowBuilderInstanceImpl[T: Type](using Quotes): Expr[NoAllowBuilderInstance[T]] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion = false)
    val sel = buildSel(euRepr, tpe, sym, infos, allowUnion = false, withPath = false, '{ ValidationPathConfig() })
    tpe.asType match {
      case '[t] =>
        sel.asTerm.tpe.asType match {
          case '[b] =>
            '{ new NoAllowBuilderInstance[t] {
                 type Builder = b
                 def apply(): Builder = $sel.asInstanceOf[Builder]
               }
            }.asExprOf[NoAllowBuilderInstance[T]]
          case _ => report.errorAndAbort("Cannot match Builder type")
        }
      case _ => report.errorAndAbort("Cannot match T")
    }
  }

  def noAllowErrorTypeImpl[T: Type](using Quotes): Expr[NoAllowErrorType[T]] = {
    import quotes.reflect.*
    val (_, _, _, euRepr) = analyse[T](allowUnion = false)
    euRepr.asType match {
      case '[e] =>
        '{
          new NoAllowErrorType[T] {
            type E = e
          }
        }
      case _ => report.errorAndAbort("Cannot match error type E")
    }
  }

  def allowErrorTypeImpl[T: Type](using Quotes): Expr[AllowErrorType[T]] = {
    import quotes.reflect.*
    val (_, _, _, euRepr) = analyse[T](allowUnion = true)
    euRepr.asType match {
      case '[e] =>
        '{
          new AllowErrorType[T] {
            type E = e
          }
        }
      case _ => report.errorAndAbort("Cannot match error type E")
    }
  }

  def allowBuilderTypeImpl[T: Type](using Quotes): Expr[AllowBuilderType[T]] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion = true)
    val sel = buildSel(euRepr, tpe, sym, infos, allowUnion = true, withPath = false, '{ ValidationPathConfig() })
    sel.asTerm.tpe.asType match {
      case '[b] =>
        '{
          new AllowBuilderType[T] {
            type Builder = b
          }
        }
      case _ => report.errorAndAbort("Cannot match allow builder type")
    }
  }

  def allowNoTransparentBuilderImpl[T: Type](using Quotes): Expr[AllowNoTransparentBuilder[T]] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion = true)
    val sel = buildSel(euRepr, tpe, sym, infos, allowUnion = true, withPath = false, '{ ValidationPathConfig() })
    sel.asTerm.tpe.asType match {
      case '[b] =>
        '{
          new AllowNoTransparentBuilder[T] {
            type Builder = b
            def builder: Builder = $sel.asInstanceOf[Builder]
          }
        }
      case _ => report.errorAndAbort("Cannot match allow no-transparent builder type")
    }
  }

  def allowBuilderInstanceImpl[T: Type](using Quotes): Expr[AllowBuilderInstance[T]] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion = true)
    val sel = buildSel(euRepr, tpe, sym, infos, allowUnion = true, withPath = false, '{ ValidationPathConfig() })
    tpe.asType match {
      case '[t] =>
        sel.asTerm.tpe.asType match {
          case '[b] =>
            '{ new AllowBuilderInstance[t] {
                 type Builder = b
                 def apply(): Builder = $sel.asInstanceOf[Builder]
               }
            }.asExprOf[AllowBuilderInstance[T]]
          case _ => report.errorAndAbort("Cannot match Builder type")
        }
      case _ => report.errorAndAbort("Cannot match T")
    }
  }

  def derivedImpl[T: Type](allowUnion: Boolean, withPath: Boolean, pathConfig: Expr[ValidationPathConfig])(using Quotes): Expr[ValidatedBuilderGenerator[T]] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion)
    val sel = buildSel(euRepr, tpe, sym, infos, allowUnion, withPath, pathConfig)
    tpe.asType match {
      case '[t] =>
        sel.asTerm.tpe.asType match {
          case '[b] =>
            '{ new ValidatedBuilderGenerator[t] {
                 type Builder = b
                 def apply(): Builder = $sel.asInstanceOf[Builder]
               }
            }.asExprOf[ValidatedBuilderGenerator[T]]
          case _ => report.errorAndAbort("Cannot match Builder type")
        }
      case _ => report.errorAndAbort("Cannot match T")
    }
  }

  def builderImplAllow[T: Type](using Quotes): Expr[Any]   =
    builderImpl[T](allowUnion = true, withPath = false, '{ ValidationPathConfig() })
  def builderImplNoAllow[T: Type](using Quotes): Expr[Any] =
    builderImpl[T](allowUnion = false, withPath = false, '{ ValidationPathConfig() })
  def builderImplAllowWithPath[T: Type](pathConfig: Expr[ValidationPathConfig])(using Quotes): Expr[Any] =
    builderImpl[T](allowUnion = true, withPath = true, pathConfig)
  def builderImplNoAllowWithPath[T: Type](pathConfig: Expr[ValidationPathConfig])(using Quotes): Expr[Any] =
    builderImpl[T](allowUnion = false, withPath = true, pathConfig)

  def builderImpl[T: Type](allowUnion: Boolean, withPath: Boolean, pathConfig: Expr[ValidationPathConfig])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val (tpe, sym, infos, euRepr) = analyse[T](allowUnion)
    buildSel(euRepr, tpe, sym, infos, allowUnion, withPath, pathConfig)
  }

  // ─── Analysis ─────────────────────────────────────────────────────────────────

  private def analyse[T: Type](allowUnion: Boolean)(using Quotes)
      : (quotes.reflect.TypeRepr, quotes.reflect.Symbol, List[ValidatorInfo], quotes.reflect.TypeRepr) = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol
    if (!sym.flags.is(Flags.Case)) report.errorAndAbort(s"${tpe.show} must be a case class")
    val nameAnnotationFqn = "api.Name"
    val params = sym.primaryConstructor.paramSymss.flatten
    val fields = params.map { p =>
      val isName = p.annotations.exists { ann =>
        try ann.tpe.typeSymbol.fullName == nameAnnotationFqn
        catch { case _: Throwable => false }
      }
      (p.name, tpe.memberType(p), isName)
    }
    if (fields.isEmpty) report.errorAndAbort(s"${tpe.show} must have at least one field")
    val nameFields = fields.collect { case (n, _, true) => n }
    if (nameFields.size > 1) {
      report.errorAndAbort(
        s"${tpe.show} has ${nameFields.size} @api.Name annotations (${nameFields.mkString(", ")}); at most one is allowed"
      )
    }
    val infos = fields.map { case (n, t, isName) =>
      val base = SmartConstructorDiscovery.discoverValidator(n, t)
      if (!isName) base else base match {
        case nv: ValidatorInfo.NeedsValidation   => nv.copy(isNameAnnotated = true)
        case nv: ValidatorInfo.NoValidation      => nv.copy(isNameAnnotated = true)
        case sv: ValidatorInfo.SeqLikeValidation => sv.copy(isNameAnnotated = true)
        case mv: ValidatorInfo.MapValidation     => mv.copy(isNameAnnotated = true)
      }
    }
    // Debug: log discovered validator infos
    try {
      infos.zip(fields).foreach { case (info, (n, _, _)) =>
        info match {
          case nv: ValidatorInfo.NeedsValidation =>
            try {
              MacroDebugger.log(s"Analyse: field=$n NeedsValidation prim=${nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr].show} wrapped=${nv.wrappedType.asInstanceOf[quotes.reflect.TypeRepr].show} companion=${nv.companionSymbol.asInstanceOf[quotes.reflect.Symbol].fullName} method=${nv.methodName} nameAnnotated=${nv.isNameAnnotated}")
            } catch { case _: Throwable => MacroDebugger.log(s"Analyse: field=$n NeedsValidation <unprintable>") }
          case nv: ValidatorInfo.NoValidation =>
            try { MacroDebugger.log(s"Analyse: field=$n NoValidation plain=${nv.plainType.asInstanceOf[quotes.reflect.TypeRepr].show} nameAnnotated=${nv.isNameAnnotated}") } catch { case _: Throwable => MacroDebugger.log(s"Analyse: field=$n NoValidation <unprintable>") }
          case sv: ValidatorInfo.SeqLikeValidation =>
            try { MacroDebugger.log(s"Analyse: field=$n SeqLikeValidation elemType=${sv.elemType.asInstanceOf[quotes.reflect.TypeRepr].show} nameAnnotated=${sv.isNameAnnotated}") } catch { case _: Throwable => MacroDebugger.log(s"Analyse: field=$n SeqLikeValidation <unprintable>") }
          case mv: ValidatorInfo.MapValidation =>
            try { MacroDebugger.log(s"Analyse: field=$n MapValidation valueType=${mv.valueType.asInstanceOf[quotes.reflect.TypeRepr].show} nameAnnotated=${mv.isNameAnnotated}") } catch { case _: Throwable => MacroDebugger.log(s"Analyse: field=$n MapValidation <unprintable>") }
        }
      }
    } catch { case _: Throwable => MacroDebugger.log("Analyse: failed to log infos") }
    (tpe, sym, infos, computeUnifiedErrorType(infos))
  }

  // ─── Core: build the outermost named-tuple chain ─────────────────────────────

  /**
   * Generates a nested tuple chain typed as a named tuple builder shape.
   */
  private def buildSel(using Quotes)(
    euRepr:     quotes.reflect.TypeRepr,
    targetTpe:  quotes.reflect.TypeRepr,
    targetSym:  quotes.reflect.Symbol,
    infos:      List[ValidatorInfo],
    allowUnion: Boolean,
    withPath:   Boolean,
    pathConfig: Expr[ValidationPathConfig]
  ): Expr[Any] = {
    import quotes.reflect.*

    val n = infos.length
    if (n > 22) report.errorAndAbort(s"Arity $n > 22")

    val hasValidatedFields = infos.exists {
      case _: ValidatorInfo.NeedsValidation   => true
      case _: ValidatorInfo.SeqLikeValidation => true  // collection fields always get path enrichment
      case _: ValidatorInfo.MapValidation     => true
      case _ => false
    }
    val names      = infos.map(_.fieldName)
    val primTypes  = infos.map(computePrimType(_, allowUnion))
    val fieldTypes = infos.map(fieldTypeOf)
    val normalizedErrorRepr =
      if (withPath && hasValidatedFields) computeUnifiedErrorType(infos, normalizePathAware = true)
      else euRepr
    val outputErrorRepr =
      if (withPath && hasValidatedFields) {
        AppliedType(TypeRepr.of[ValidationPathError[Any]].typeSymbol.typeRef, List(normalizedErrorRepr))
      }
      else euRepr
    // Post-process: expand collection prim types to also accept Seq[ZValidation[Nothing, E, B]]
    // (or Map[K, ZValidation[...]]) so the generated setter type accepts both pre-built and
    // pre-validated inputs.  The expansion uses outputErrorRepr which is only available here.
    val zvTC  = TypeRepr.of[ZValidation[Nothing, Any, Any]] match {
      case AppliedType(tc, _) => tc
      case o => o
    }
    val expandedPrimTypes: List[TypeRepr] = primTypes.zip(infos).map {
      case (base, sv: ValidatorInfo.SeqLikeValidation) =>
        val collTpe = sv.collectionType.asInstanceOf[TypeRepr]
        val elemTpe = sv.elemType.asInstanceOf[TypeRepr]
        collTpe match {
          case AppliedType(seqTC, _) =>
            try {
              val zvTpe    = AppliedType(zvTC,  List(TypeRepr.of[Nothing], outputErrorRepr, elemTpe))
              val zvSeqTpe = AppliedType(seqTC, List(zvTpe))
              OrType(base, zvSeqTpe)
            } catch { case _: Throwable => base }
          case _ => base
        }
      case (base, mv: ValidatorInfo.MapValidation) =>
        val mapTpe = mv.mapType.asInstanceOf[TypeRepr]
        val keyTpe = mv.keyType.asInstanceOf[TypeRepr]
        val valTpe = mv.valueType.asInstanceOf[TypeRepr]
        mapTpe match {
          case AppliedType(mapTC, _) =>
            try {
              val zvTpe    = AppliedType(zvTC,  List(TypeRepr.of[Nothing], outputErrorRepr, valTpe))
              val mapZvTpe = AppliedType(mapTC, List(keyTpe, zvTpe))
              OrType(base, mapZvTpe)
            } catch { case _: Throwable => base }
          case _ => base
        }
      case (base, _) => base
    }
    val builderTypeRepr = buildBuilderTypeRepr(names, expandedPrimTypes, targetTpe, outputErrorRepr)

    // Build per-field validator: Any => ZValidation[Nothing, Any, Any]
    val validatorExprs: List[Expr[Any => Any]] = infos.map { info =>
      val primRepr    = computePrimType(info, allowUnion)
      val wrappedRepr = fieldTypeOf(info)
      val rawErrorRepr = rawErrorTypeOf(info)
      primRepr.asType match {
        case '[p] => wrappedRepr.asType match {
          case '[w] => rawErrorRepr.asType match {
            case '[rawEu] =>
              val fvBase: Expr[p => ZValidation[Nothing, rawEu, w]] = fieldValidator[p, rawEu, w](info, allowUnion, withPath)
              if (withPath && hasValidatedFields) {
                outputErrorRepr.asType match {
                  case '[outEu] =>
                    val fieldNameExpr = Expr(info.fieldName)
                    val pathSegmentsExpr: Expr[Seq[ValidationPathPart]] =
                      '{ ValidationPathSupport.fieldSegments($pathConfig, $fieldNameExpr) }
                    info match {
                      case _: ValidatorInfo.NoValidation
                         | _: ValidatorInfo.SeqLikeValidation
                         | _: ValidatorInfo.MapValidation =>
                        '{ (x: Any) => ($fvBase)(x.asInstanceOf[p]).asInstanceOf[ZValidation[Nothing, outEu, w]] }
                      case _: ValidatorInfo.NeedsValidation =>
                        unwrapPathAwareError(rawErrorRepr) match {
                          case Some(innerErrorRepr) =>
                            innerErrorRepr.asType match {
                              case '[innerEu] =>
                                val fvWithPrependedPath: Expr[p => ZValidation[Nothing, ValidationPathError[innerEu], w]] =
                                  '{ (input: p) =>
                                    ValidationPathSupport.prependExistingPath[innerEu, w](
                                      $pathSegmentsExpr,
                                      $fvBase(input).asInstanceOf[ZValidation[Nothing, ValidationPathError[innerEu], w]]
                                    )
                                  }
                                '{ (x: Any) => ($fvWithPrependedPath)(x.asInstanceOf[p]).asInstanceOf[ZValidation[Nothing, outEu, w]] }
                              case _ => report.errorAndAbort("innerEu")
                            }
                          case None =>
                            val fvWithAttachedPath: Expr[p => ZValidation[Nothing, ValidationPathError[rawEu], w]] =
                              '{ (input: p) =>
                                ValidationPathSupport.attachPath[rawEu, w](
                                  $pathSegmentsExpr,
                                  $fvBase(input)
                                )
                              }
                            '{ (x: Any) => ($fvWithAttachedPath)(x.asInstanceOf[p]).asInstanceOf[ZValidation[Nothing, outEu, w]] }
                        }
                    }
                  case _ => report.errorAndAbort("outEu")
                }
              } else {
                '{ (x: Any) => ($fvBase)(x.asInstanceOf[p]) }
              }
            case _ => report.errorAndAbort("rawEu")
          }
          case _ => report.errorAndAbort("w")
        }
        case _ => report.errorAndAbort("p")
      }
    }

    // Build the combine function: Array[ZValidation[Nothing,Any,Any]] => ZValidation[Nothing,Any,T]
    val combineExpr: Expr[Array[Any] => Any] = buildCombine(outputErrorRepr, targetTpe, targetSym, fieldTypes)

    val validatorsExpr: Expr[Array[Any => Any]] = '{ ${ Expr.ofList(validatorExprs) }.toArray }

    val namedTupleBuilderExpr: Expr[AnyNamedTuple] = '{
      Tuple1(buildNamedTupleChain($validatorsExpr, $combineExpr, 0, new Array[Any](0))).asInstanceOf[AnyNamedTuple]
    }
    Typed(namedTupleBuilderExpr.asTerm, Inferred(builderTypeRepr)).asExpr
  }

  // ─── Per-field validator ─────────────────────────────────────────────────────

  private def fieldValidator[P: Type, EU: Type, W: Type](using Quotes)(
    info:       ValidatorInfo,
    allowUnion: Boolean,
    withPath:   Boolean
  ): Expr[P => ZValidation[Nothing, EU, W]] = {
    import quotes.reflect.*

    def isCheckable(tp: TypeRepr): Boolean = {
      val s = tp.widen.dealias.typeSymbol
      if (s == Symbol.noSymbol) return false
      if (s.flags.is(Flags.Opaque)) return false
      // Newtype/Subtype: the inner `Type` alias is erased to the underlying type at runtime,
      // so a `case x: W` test would match the primitive too — always run validation instead.
      val dealiased = tp.widen.dealias
      if (dealiased.typeSymbol.name == "Type") return false
      // Also check if W's dealiased show ends in ".Type" (covers SequenceNumber.Type etc.)
      try {
        val show = dealiased.show
        if (show.endsWith(".Type")) return false
      } catch { case _: Throwable => () }
      true
    }

    info match {
      case nv: ValidatorInfo.NeedsValidation =>
        val rawPrim = nv.primitiveType.asInstanceOf[TypeRepr]
        val wrapped = TypeRepr.of[W]
        if (allowUnion && !(rawPrim =:= wrapped) && isCheckable(wrapped)) {
          rawPrim.asType match {
            case '[rawP] =>
              '{ (p: P) =>
                (p: Any) match {
                  case x: W => ZValidation.succeed(x.asInstanceOf[W])
                  case _    => ${ smartCall[EU, W](nv, '{ p.asInstanceOf[rawP] }) }
                }
              }
            case _ => report.errorAndAbort("Cannot match rawPrim for union")
          }
        } else {
          // Delegate to smartCall which builds the companion invocation term safely
          rawPrim.asType match {
            case '[rawP] =>
              '{ (p: P) => ${ smartCall[EU, W](nv, '{ p.asInstanceOf[rawP] }) } }
            case _ => report.errorAndAbort(s"Cannot match rawPrim ${rawPrim.show}")
          }
        }
      case _: ValidatorInfo.NoValidation =>
        val wrapped = TypeRepr.of[W].widen.dealias
        optionInnerType(wrapped) match {
          case Some(innerTpe) =>
            innerTpe.asType match {
              case '[inner] =>
                if (allowUnion) {
                  '{ (p: P) =>
                    val raw = p.asInstanceOf[Any]
                    val out: Option[inner] = raw match {
                      case opt: Option[?] => opt.asInstanceOf[Option[inner]]
                      case _ => Some(raw.asInstanceOf[inner])
                    }
                    ZValidation.succeed(out.asInstanceOf[W])
                  }
                } else {
                  '{ (p: P) =>
                    val raw = p.asInstanceOf[Any]
                    val out: Option[inner] = if (raw == None) None else Some(raw.asInstanceOf[inner])
                    ZValidation.succeed(out.asInstanceOf[W])
                  }
                }
              case _ => report.errorAndAbort("inner")
            }
          case None =>
            javaOptionalInnerType(wrapped) match {
              case Some(innerTpe) =>
                innerTpe.asType match {
                  case '[inner] =>
                    '{ (p: P) =>
                      val raw = p.asInstanceOf[Any]
                      val out: java.util.Optional[inner] = raw match {
                        case opt: java.util.Optional[?] => opt.asInstanceOf[java.util.Optional[inner]]
                        case null => java.util.Optional.empty[inner]()
                        case _ => java.util.Optional.ofNullable(raw.asInstanceOf[inner])
                      }
                      ZValidation.succeed(out.asInstanceOf[W])
                    }
                  case _ => report.errorAndAbort("inner")
                }
              case None =>
                if (isJavaOptionalInt(wrapped)) {
                  '{ (p: P) =>
                    val raw = p.asInstanceOf[Any]
                    val out: java.util.OptionalInt = raw match {
                      case opt: java.util.OptionalInt => opt
                      case null => java.util.OptionalInt.empty()
                      case _ => java.util.OptionalInt.of(raw.asInstanceOf[Int])
                    }
                    ZValidation.succeed(out.asInstanceOf[W])
                  }
                } else if (isJavaOptionalLong(wrapped)) {
                  '{ (p: P) =>
                    val raw = p.asInstanceOf[Any]
                    val out: java.util.OptionalLong = raw match {
                      case opt: java.util.OptionalLong => opt
                      case null => java.util.OptionalLong.empty()
                      case _ => java.util.OptionalLong.of(raw.asInstanceOf[Long])
                    }
                    ZValidation.succeed(out.asInstanceOf[W])
                  }
                } else if (isJavaOptionalDouble(wrapped)) {
                  '{ (p: P) =>
                    val raw = p.asInstanceOf[Any]
                    val out: java.util.OptionalDouble = raw match {
                      case opt: java.util.OptionalDouble => opt
                      case null => java.util.OptionalDouble.empty()
                      case _ => java.util.OptionalDouble.of(raw.asInstanceOf[Double])
                    }
                    ZValidation.succeed(out.asInstanceOf[W])
                  }
                } else {
                  '{ (p: P) => ZValidation.succeed(p.asInstanceOf[W]) }
                }
            }
        }
      case sv: ValidatorInfo.SeqLikeValidation =>
        val fieldNameExpr   = Expr(sv.fieldName)
        val elemNameExpr    = Expr(sv.elemNameField)
        val withPathExpr    = Expr(withPath)
        val extractorExpr: Expr[Any] = sv.elemNameField match {
          case None => '{ null }
          case Some(nf) =>
            val elemTpe = sv.elemType.asInstanceOf[TypeRepr]
            elemTpe.asType match {
              case '[elemTy] =>
                '{ (elem: Any) =>
                  try {
                    val e = elem.asInstanceOf[elemTy]
                    val sel = e.asInstanceOf[scala.reflect.Selectable]
                    val value = sel.selectDynamic(${ Expr(nf) })
                    Option(value.toString)
                  } catch { case _: Throwable => None }
                }
              case _ => '{ null }
            }
        }
        '{ (p: P) =>
          ValidationCollectionSupport
            .combineSeqField(p.asInstanceOf[Any], $fieldNameExpr, $elemNameExpr, $withPathExpr, $extractorExpr)
            .asInstanceOf[ZValidation[Nothing, EU, W]]
        }
      case mv: ValidatorInfo.MapValidation =>
        val fieldNameExpr   = Expr(mv.fieldName)
        val valNameExpr     = Expr(mv.valNameField)
        val withPathExpr    = Expr(withPath)
        val extractorExpr: Expr[Any] = mv.valNameField match {
          case None => '{ null }
          case Some(nf) =>
            val valTpe = mv.valueType.asInstanceOf[TypeRepr]
            valTpe.asType match {
              case '[valTy] =>
                '{ (elem: Any) =>
                  try {
                    val e = elem.asInstanceOf[valTy]
                    val sel = e.asInstanceOf[scala.reflect.Selectable]
                    val value = sel.selectDynamic(${ Expr(nf) })
                    Option(value.toString)
                  } catch { case _: Throwable => None }
                }
              case _ => '{ null }
            }
        }
        '{ (p: P) =>
          ValidationCollectionSupport
            .combineMapField(p.asInstanceOf[Any], $fieldNameExpr, $valNameExpr, $withPathExpr, $extractorExpr)
            .asInstanceOf[ZValidation[Nothing, EU, W]]
        }
    }
  }

  // ─── Combine validators ───────────────────────────────────────────────────────

  /**
   * Builds `Array[ZValidation[Nothing,Any,Any]] => ZValidation[Nothing,eu,T]`.
   * Uses `map` (n=1), `zipWithPar` (n=2), or `foldLeft`+`map` (n≥3, error-accumulating).
   */
  private def buildCombine(using Quotes)(
    euRepr:    quotes.reflect.TypeRepr,
    targetTpe: quotes.reflect.TypeRepr,
    targetSym: quotes.reflect.Symbol,
    fieldTypes: List[quotes.reflect.TypeRepr]
  ): Expr[Array[Any] => Any] = {
    import quotes.reflect.*
    val n      = fieldTypes.length
    val companionModuleSym = targetSym.companionModule
    try MacroDebugger.log(s"buildCombine: targetSym=${targetSym.fullName} companionModule=${companionModuleSym.fullName}") catch { case _: Throwable => () }
    val applyMethodOwnerPairs = findAccessibleApplyMethods(companionModuleSym)
    selectApplyMethodForArity(companionModuleSym, applyMethodOwnerPairs, None).foreach {
      case (applyM, applyOwner) =>
        try MacroDebugger.log(s"buildCombine: selected apply owner=${applyOwner.fullName} applySym=${applyM.name}") catch { case _: Throwable => () }
    }
    def applyOnCompanion(args: List[quotes.reflect.Term]): quotes.reflect.Term = {
      selectApplyMethodForArity(companionModuleSym, applyMethodOwnerPairs, Some(args.length)) match {
        case Some((applyMethod, applyOwner)) =>
          Ref(applyOwner).select(applyMethod).appliedToArgs(args)
        case None =>
          buildPrimaryConstructorCall(targetSym, args)
      }
    }
    euRepr.asType match {
      case '[eu] => targetTpe.asType match {
        case '[t] =>
          n match {
            case 1 =>
              fieldTypes(0).asType match {
                case '[f0] =>
                  '{ (arr: Array[Any]) =>
                    arr(0).asInstanceOf[ZValidation[Nothing, eu, f0]].map(a0 =>
                      ${ applyOnCompanion(List('a0.asTerm)).asExprOf[t] }
                    )
                  }
                case _ => report.errorAndAbort("f0")
              }
            case 2 =>
              (fieldTypes(0).asType, fieldTypes(1).asType) match {
                case ('[f0], '[f1]) =>
                  '{ (arr: Array[Any]) =>
                    arr(0).asInstanceOf[ZValidation[Nothing, eu, f0]]
                      .zipWithPar(arr(1).asInstanceOf[ZValidation[Nothing, eu, f1]])((a0, a1) =>
                        ${ applyOnCompanion(List('a0.asTerm, 'a1.asTerm)).asExprOf[t] }
                      )
                  }
                case _ => report.errorAndAbort("f0/f1")
              }
            case _ =>
              // ≥3 fields: accumulate with foldLeft+zipWithPar then map
              val applyFn: Expr[Array[Any] => t] = buildApply[t](targetSym, n, fieldTypes)
              '{ (arr: Array[Any]) =>
                val zvList = arr.toList.map(_.asInstanceOf[ZValidation[Nothing, eu, Any]])
                val combined = zvList.tail.foldLeft(
                  zvList.head.map(List(_))
                )((acc, zv) => acc.zipWithPar(zv)((list, v) => list :+ v))
                combined.map(args => $applyFn(args.toArray))
              }
          }
        case _ => report.errorAndAbort("t")
      }
      case _ => report.errorAndAbort("eu")
    }
  }

  // ─── Apply function for combining results ─────────────────────────────────────

  private def buildApply[T: Type](using Quotes)(
    targetSym:  quotes.reflect.Symbol,
    n:          Int,
    fieldTypes: List[quotes.reflect.TypeRepr]
  ): Expr[Array[Any] => T] = {
    import quotes.reflect.*
    val companionModuleSym = targetSym.companionModule
    val applyMethodOwnerPairs = findAccessibleApplyMethods(companionModuleSym)
    selectApplyMethodForArity(companionModuleSym, applyMethodOwnerPairs, None).foreach {
      case (applyM, applyOwner) =>
        try MacroDebugger.log(s"buildApply: selected apply owner=${applyOwner.fullName} applySym=${applyM.name}") catch { case _: Throwable => () }
    }
    val applyMethodForArityN = selectApplyMethodForArity(companionModuleSym, applyMethodOwnerPairs, Some(n))
    val lam = Lambda(
      Symbol.spliceOwner,
      MethodType(List("args"))(_ => List(TypeRepr.of[Array[Any]]), _ => TypeRepr.of[T]),
      (_, ps) => {
        val argsRef = ps.head.asInstanceOf[Term]
        val argTerms = fieldTypes.zipWithIndex.map { case (ft, i) =>
          val elem = Apply(Select(argsRef, TypeRepr.of[Array[Any]].typeSymbol.declaredMethods
            .find(_.name == "apply").get), List(Literal(IntConstant(i))))
          Typed(elem, Inferred(ft))
        }
        applyMethodForArityN match {
          case Some((applyMethod, applyOwner)) =>
            Ref(applyOwner).select(applyMethod).appliedToArgs(argTerms)
          case None =>
            buildPrimaryConstructorCall(targetSym, argTerms)
        }
      }
    )
    lam.asExprOf[Array[Any] => T]
  }

  private def buildPrimaryConstructorCall(using Quotes)(
    targetSym: quotes.reflect.Symbol,
    args: List[quotes.reflect.Term]
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    val ctor = targetSym.primaryConstructor
    Apply(Select(New(TypeIdent(targetSym)), ctor), args)
  }

  private def companionCandidates(using Quotes)(companionModuleSym: quotes.reflect.Symbol): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*

    val modClassCompanion =
      try if (companionModuleSym.moduleClass.exists) companionModuleSym.moduleClass.companionModule else Symbol.noSymbol
      catch { case _: Throwable => Symbol.noSymbol }
    val req1 = try Symbol.requiredModule(companionModuleSym.fullName) catch { case _: Throwable => Symbol.noSymbol }
    val req2 = try Symbol.requiredModule(companionModuleSym.fullName + "$") catch { case _: Throwable => Symbol.noSymbol }
    val req3 = try Symbol.requiredModule(companionModuleSym.fullName.stripSuffix("$")) catch { case _: Throwable => Symbol.noSymbol }
    val ownerBased = try Symbol.requiredModule(companionModuleSym.owner.fullName + "." + companionModuleSym.name) catch { case _: Throwable => Symbol.noSymbol }

    List(companionModuleSym, modClassCompanion, req1, req2, req3, ownerBased)
      .filter(_ != Symbol.noSymbol)
      .distinct
  }

  private def findAccessibleApplyMethods(using Quotes)(companionModuleSym: quotes.reflect.Symbol): List[(quotes.reflect.Symbol, quotes.reflect.Symbol)] = {
    import quotes.reflect.*

    try {
      def methodsOf(sym: Symbol): List[Symbol] =
        try sym.declaredMethods.toList
        catch { case _: Throwable => List.empty }

      companionCandidates(companionModuleSym)
        .flatMap { cand =>
          methodsOf(cand).map(m => (m, cand)) ++
          (try {
            if (cand.moduleClass.exists) methodsOf(cand.moduleClass).map(m => (m, cand)) else List.empty
          } catch {
            case _: Throwable => List.empty
          })
        }
        .filter { case (m, _) =>
          m.name == "apply" && !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected)
        }
    } catch {
      case _: Throwable => Nil
    }
  }

  private def selectApplyMethodForArity(using Quotes)(
    companionModuleSym: quotes.reflect.Symbol,
    applyMethodOwnerPairs: List[(quotes.reflect.Symbol, quotes.reflect.Symbol)],
    arity: Option[Int]
  ): Option[(quotes.reflect.Symbol, quotes.reflect.Symbol)] = {
    import quotes.reflect.*

    if (applyMethodOwnerPairs.isEmpty) {
      val objNames =
        try companionModuleSym.declaredMethods.map(_.name).mkString(",")
        catch { case _: Throwable => "<error>" }
      val classNames =
        try if (companionModuleSym.moduleClass.exists) companionModuleSym.moduleClass.declaredMethods.map(_.name).mkString(",") else "<no moduleClass>"
        catch { case _: Throwable => "<error>" }
      MacroDebugger.log(s"apply lookup failed: companion=${companionModuleSym.fullName} objMethods=$objNames classMethods=$classNames")
      None
    } else {
      arity match {
        case Some(expectedArity) =>
          applyMethodOwnerPairs.find { case (methodSym, _) =>
            try methodSym.paramSymss.flatten.length == expectedArity
            catch { case _: Throwable => false }
          }.orElse(applyMethodOwnerPairs.headOption)
        case None =>
          applyMethodOwnerPairs.headOption
      }
    }
  }

  // ─── Helpers ─────────────────────────────────────────────────────────────────

  private def smartCall[EU: Type, W: Type](using Quotes)(
    nv: ValidatorInfo.NeedsValidation, primExpr: Expr[Any]
  ): Expr[ZValidation[Nothing, EU, W]] = {
    import quotes.reflect.*
    val csRaw = nv.companionSymbol.asInstanceOf[quotes.reflect.Symbol]
    try MacroDebugger.log(s"smartCall: companion=${csRaw.fullName} method=${nv.methodName} prim=${nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr].show} wrapped=${nv.wrappedType.asInstanceOf[quotes.reflect.TypeRepr].show}") catch { case _: Throwable => MacroDebugger.log(s"smartCall: <unprintable companion> method=${nv.methodName}") }

    def methodExistsOn(sym: Symbol, name: String): Boolean = {
      try {
        val methodsObj = sym.declaredMethods.map(_.name)
        val classMethods = try if (sym.moduleClass.exists) sym.moduleClass.declaredMethods.map(_.name) else List.empty catch { case _: Throwable => List.empty }
        (methodsObj ++ classMethods).contains(name)
      } catch { case _: Throwable => false }
    }

    def resolveTerm(sym: Symbol): Symbol = {
      try MacroDebugger.log(s"resolveTerm: trying for symbol=${sym.fullName} flags=${sym.flags}") catch { case _: Throwable => () }
      try if (sym.flags.is(Flags.Module)) return sym catch { case _: Throwable => () }
      try {
        val c = sym.companionModule
        if (c != Symbol.noSymbol && c.flags.is(Flags.Module)) return c
      } catch { case _: Throwable => () }
      try {
        val mc = if (sym.moduleClass.exists) sym.moduleClass else Symbol.noSymbol
        if (mc != Symbol.noSymbol) {
          val mcComp = try mc.companionModule catch { case _: Throwable => Symbol.noSymbol }
          if (mcComp != Symbol.noSymbol && mcComp.flags.is(Flags.Module)) return mcComp
        }
      } catch { case _: Throwable => () }
      try {
        val fn = sym.fullName
        val trials = List(fn, fn + "$", fn.stripSuffix("$"))
        trials.view.flatMap { name =>
          try {
            val m = Symbol.requiredModule(name)
            if (m != Symbol.noSymbol && m.flags.is(Flags.Module)) Some(m) else None
          } catch { case _: Throwable => None }
        }.headOption.getOrElse(sym)
      } catch { case _: Throwable => sym }
    }

    /** Build a Term for `moduleTerm.methodName(arg)`, looking up the method
     *  via the module's type (so inherited methods like `make` on Newtype subobjects
     *  are found correctly and the generated Select has the right owner). */
    def applyMethodOnModule(moduleTerm: Term, methodName: String, arg: Term): Term = {
      // Look up the method on the static type of the module term (includes inherited methods)
      val modTpe = moduleTerm.tpe
      val methodSymOpt = modTpe.typeSymbol.methodMember(methodName).headOption
        .orElse(modTpe.typeSymbol.methodMember(methodName).headOption)
      methodSymOpt match {
        case Some(methodSym) => Apply(Select(moduleTerm, methodSym), List(arg))
        case None =>
          // fallback: Select.unique (may still work for some cases)
          Apply(Select.unique(moduleTerm, methodName), List(arg))
      }
    }

    val callTerm: quotes.reflect.Term = try {
      val originalSym = csRaw
      val arg = Typed(primExpr.asTerm, Inferred(nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr]))
      val resolved = resolveTerm(originalSym)
      try MacroDebugger.log(s"smartCall: resolved term symbol=${resolved.fullName} flags=${resolved.flags}") catch { case _: Throwable => () }

      if (resolved != Symbol.noSymbol && resolved.flags.is(Flags.Module) && !resolved.name.endsWith("$")) {
        // Use the recorded method name; fall back to alternatives if not directly declared
        val chosen =
          if (methodExistsOn(resolved, nv.methodName)) nv.methodName
          else List("make", "apply").find(nm => methodExistsOn(resolved, nm)).getOrElse(nv.methodName)
        applyMethodOnModule(Ref(resolved), chosen, arg)
      } else {
        def buildTermFromFullName(full: String): Option[quotes.reflect.Term] = {
          try {
            val parts = full.split('.').toList
            if (parts.isEmpty) return None
            def clean(s: String): String = s.stripPrefix("_$").stripSuffix("$")
            (parts.length - 1 to 1 by -1).view.flatMap { i =>
              val ownerName = parts.take(i).mkString(".")
              val nested = parts.drop(i).map(clean)
              try {
                val ownerModule = Symbol.requiredModule(ownerName)
                if (ownerModule == Symbol.noSymbol) None
                else {
                  val term0: Term = Ref(ownerModule)
                  val finalTerm = nested.foldLeft[Term](term0) { (acc, seg) => Select.unique(acc, seg) }
                  Some(finalTerm)
                }
              } catch { case _: Throwable => None }
            }.headOption
          } catch { case _: Throwable => None }
        }

        buildTermFromFullName(originalSym.fullName) match {
          case Some(objTerm) =>
            val termSym = objTerm.symbol
            val chosenName =
              if (methodExistsOn(termSym.asInstanceOf[Symbol], nv.methodName)) nv.methodName
              else List("make", "apply").find(nm => methodExistsOn(termSym.asInstanceOf[Symbol], nm)).getOrElse(nv.methodName)
            try applyMethodOnModule(objTerm, chosenName, arg)
            catch { case _: Throwable => Apply(Select.unique(Ref(originalSym), nv.methodName), List(arg)) }
          case None => applyMethodOnModule(Ref(originalSym), nv.methodName, arg)
        }
      }
    } catch {
      case ex: Throwable =>
        import quotes.reflect.*
        MacroDebugger.log(s"smartCall: final fallback due to ${ex.getMessage}")
        Apply(Select.unique(Ref(csRaw), nv.methodName), List(Typed(primExpr.asTerm, Inferred(nv.primitiveType.asInstanceOf[quotes.reflect.TypeRepr]))))
    }

    nv.validationKind match {
      case FromEither =>
        val eitherExpr = callTerm.asExprOf[Either[EU, W]]
        '{ zio.prelude.ZValidation.fromEither($eitherExpr).asInstanceOf[ZValidation[Nothing, EU, W]] }
      case FromValidation =>
        callTerm.asExprOf[ZValidation[Nothing, EU, W]]
    }
  }

  private def computeUnifiedErrorType(using Quotes)(
    infos: List[ValidatorInfo],
    normalizePathAware: Boolean = false
  ): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    infos.flatMap { info =>
      val raw = info match {
        case nv: ValidatorInfo.NeedsValidation   => Some(nv.errorType.asInstanceOf[TypeRepr])
        case sv: ValidatorInfo.SeqLikeValidation =>
          val e = rawErrorTypeOf(sv.elemInfo)
          if (e =:= TypeRepr.of[Nothing]) None else Some(e)
        case mv: ValidatorInfo.MapValidation     =>
          val e = rawErrorTypeOf(mv.valueInfo)
          if (e =:= TypeRepr.of[Nothing]) None else Some(e)
        case _: ValidatorInfo.NoValidation       => None
      }
      if (!normalizePathAware) raw
      else raw.map(r => unwrapPathAwareError(r).getOrElse(r))
    }.distinct match {
      case Nil      => TypeRepr.of[Nothing]
      case h :: Nil => h
      case hs       => hs.reduce(OrType(_, _))
    }
  }

  private def rawErrorTypeOf(using Quotes)(info: ValidatorInfo): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    info match {
      case nv: ValidatorInfo.NeedsValidation   => nv.errorType.asInstanceOf[TypeRepr]
      case _: ValidatorInfo.NoValidation       => TypeRepr.of[Nothing]
      case sv: ValidatorInfo.SeqLikeValidation => rawErrorTypeOf(sv.elemInfo)
      case mv: ValidatorInfo.MapValidation     => rawErrorTypeOf(mv.valueInfo)
    }
  }

  private def unwrapPathAwareError(using Quotes)(errorType: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val vpeSymbol = TypeRepr.of[ValidationPathError[Any]].typeSymbol
    errorType.widen.dealias match {
      case AppliedType(tc, List(nestedErrorType)) if tc.typeSymbol == vpeSymbol =>
        Some(nestedErrorType)
      case _ => None
    }
  }

  private def computePrimType(using Quotes)(info: ValidatorInfo, allowUnion: Boolean): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    info match {
      case nv: ValidatorInfo.NeedsValidation =>
        val p = nv.primitiveType.asInstanceOf[TypeRepr]
        val w = nv.wrappedType.asInstanceOf[TypeRepr]
        if (allowUnion && !(p.widen.dealias =:= w.widen.dealias)) OrType(p, w) else p
      case nv: ValidatorInfo.NoValidation =>
        val plain = nv.plainType.asInstanceOf[TypeRepr]
        optionInnerType(plain) match {
          case Some(inner) => if (allowUnion) OrType(inner, plain) else OrType(inner, TypeRepr.of[None.type])
          case None =>
            javaOptionalInnerType(plain) match {
              case Some(inner) => OrType(inner, plain)
              case None =>
                if (isJavaOptionalInt(plain)) {
                  OrType(TypeRepr.of[Int], plain)
                } else if (isJavaOptionalLong(plain)) {
                  OrType(TypeRepr.of[Long], plain)
                } else if (isJavaOptionalDouble(plain)) {
                  OrType(TypeRepr.of[Double], plain)
                } else {
                  plain
                }
            }
        }
      case sv: ValidatorInfo.SeqLikeValidation =>
        // Base prim type is the raw collection type; union expansion (adding ZValidation form)
        // happens in buildSel after outputErrorRepr is known.
        sv.collectionType.asInstanceOf[TypeRepr]
      case mv: ValidatorInfo.MapValidation =>
        mv.mapType.asInstanceOf[TypeRepr]
    }
  }

  private def fieldTypeOf(using Quotes)(info: ValidatorInfo): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    info match {
      case nv: ValidatorInfo.NeedsValidation   => nv.wrappedType.asInstanceOf[TypeRepr]
      case nv: ValidatorInfo.NoValidation      => nv.plainType.asInstanceOf[TypeRepr]
      case sv: ValidatorInfo.SeqLikeValidation => sv.collectionType.asInstanceOf[TypeRepr]
      case mv: ValidatorInfo.MapValidation     => mv.mapType.asInstanceOf[TypeRepr]
    }
  }

  private def optionInnerType(using Quotes)(tp: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case AppliedType(tc, List(inner)) if tc.typeSymbol == TypeRepr.of[Option[Any]].typeSymbol => Some(inner)
      case _ => None
    }
  }

  private def javaOptionalInnerType(using Quotes)(tp: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case AppliedType(tc, List(inner)) if tc.typeSymbol == TypeRepr.of[java.util.Optional[Any]].typeSymbol => Some(inner)
      case _ => None
    }
  }

  private def isJavaOptionalInt(using Quotes)(tp: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tp.widen.dealias =:= TypeRepr.of[java.util.OptionalInt]
  }

  private def isJavaOptionalLong(using Quotes)(tp: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tp.widen.dealias =:= TypeRepr.of[java.util.OptionalLong]
  }

  private def isJavaOptionalDouble(using Quotes)(tp: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tp.widen.dealias =:= TypeRepr.of[java.util.OptionalDouble]
  }

  private def isNullableUnion(using Quotes)(tp: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case OrType(a, b) =>
        a.widen.dealias =:= TypeRepr.of[Null] ||
        b.widen.dealias =:= TypeRepr.of[Null] ||
        isNullableUnion(a) ||
        isNullableUnion(b)
      case _ => false
    }
  }

  private def isOptionalInputType(using Quotes)(tp: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case t if t =:= TypeRepr.of[Null] => true
      case t if t =:= TypeRepr.of[None.type] => true
      case AppliedType(tc, _) if tc.typeSymbol == TypeRepr.of[Option[Any]].typeSymbol => true
      case AppliedType(tc, _) if tc.typeSymbol == TypeRepr.of[java.util.Optional[Any]].typeSymbol => true
      case t if t =:= TypeRepr.of[java.util.OptionalInt] => true
      case t if t =:= TypeRepr.of[java.util.OptionalLong] => true
      case t if t =:= TypeRepr.of[java.util.OptionalDouble] => true
      case OrType(a, b) => isOptionalInputType(a) || isOptionalInputType(b)
      case _ => false
    }
  }

  private def tupleElements(using Quotes)(tp: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case t if t =:= TypeRepr.of[EmptyTuple] => Nil
      case AppliedType(cons, List(h, t)) if cons.typeSymbol == TypeRepr.of[Int *: EmptyTuple].typeSymbol =>
        h :: tupleElements(t)
      case _ => Nil
    }
  }

  private def tupleStringConstants(using Quotes)(tp: quotes.reflect.TypeRepr): List[String] = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case t if t =:= TypeRepr.of[EmptyTuple] => Nil
      case AppliedType(cons, List(ConstantType(StringConstant(name)), tail))
          if cons.typeSymbol == TypeRepr.of[Int *: EmptyTuple].typeSymbol =>
        name :: tupleStringConstants(tail)
      case AppliedType(cons, List(_, tail)) if cons.typeSymbol == TypeRepr.of[Int *: EmptyTuple].typeSymbol =>
        "<field>" :: tupleStringConstants(tail)
      case _ => Nil
    }
  }

  private def namedTupleEntries(using Quotes)(tp: quotes.reflect.TypeRepr): List[(String, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*
    tp.widen.dealias match {
      case AppliedType(nt, List(names, values)) if nt.typeSymbol == TypeRepr.of[scala.NamedTuple.NamedTuple[Tuple1["x"], Tuple1[Int]]].typeSymbol =>
        val ns = tupleStringConstants(names)
        val vs = tupleElements(values)
        ns.zip(vs)
      case _ => Nil
    }
  }

  private def defaultCompletionValue(using Quotes)(tp: quotes.reflect.TypeRepr): Option[Expr[Any]] = {
    import quotes.reflect.*
    optionInnerType(tp) match {
      case Some(_) => Some('{ None })
      case None =>
        javaOptionalInnerType(tp) match {
          case Some(inner) =>
            inner.asType match {
              case '[i] => Some('{ java.util.Optional.empty[i]().asInstanceOf[Any] })
              case _ => None
            }
          case None =>
            if (isJavaOptionalInt(tp)) {
              Some('{ java.util.OptionalInt.empty().asInstanceOf[Any] })
            } else if (isJavaOptionalLong(tp)) {
              Some('{ java.util.OptionalLong.empty().asInstanceOf[Any] })
            } else if (isJavaOptionalDouble(tp)) {
              Some('{ java.util.OptionalDouble.empty().asInstanceOf[Any] })
            } else if (isNullableUnion(tp)) {
              Some('{ null })
            } else {
              None
            }
        }
    }
  }

  private def ntRepr(using Quotes)(names: List[String], types: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    type SampleNT = scala.NamedTuple.NamedTuple[Tuple1["x"], Tuple1[Int]]
    val ntTC: TypeRepr = TypeRepr.of[SampleNT] match {
      case AppliedType(tc, _) => tc
      case o => report.errorAndAbort(s"Cannot extract NamedTuple TC from ${o.show}")
    }
    val consTC: TypeRepr = TypeRepr.of[Int *: EmptyTuple] match {
      case AppliedType(tc, _) => tc
      case o => report.errorAndAbort(s"Cannot extract *: TC from ${o.show}")
    }
    val ns = names.foldRight[TypeRepr](TypeRepr.of[EmptyTuple])((n, acc) =>
      AppliedType(consTC, List(ConstantType(StringConstant(n)), acc)))
    val vs = types.foldRight[TypeRepr](TypeRepr.of[EmptyTuple])((t, acc) =>
      AppliedType(consTC, List(t, acc)))
    AppliedType(ntTC, List(ns, vs))
  }

  private def buildBuilderTypeRepr(using Quotes)(
    names: List[String],
    inputTypes: List[quotes.reflect.TypeRepr],
    targetTpe: quotes.reflect.TypeRepr,
    errorTpe: quotes.reflect.TypeRepr
  ): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    if (names.isEmpty || inputTypes.isEmpty || names.length != inputTypes.length) {
      report.errorAndAbort("Cannot build builder type: invalid named field inputs")
    }

    val fn1TC: TypeRepr = TypeRepr.of[Any => Any] match {
      case AppliedType(tc, _) => tc
      case other => report.errorAndAbort(s"Cannot extract Function1 type constructor from ${other.show}")
    }
    val zvTC: TypeRepr = TypeRepr.of[ZValidation[Nothing, Any, Any]] match {
      case AppliedType(tc, _) => tc
      case other => report.errorAndAbort(s"Cannot extract ZValidation type constructor from ${other.show}")
    }

    def loop(remNames: List[String], remInputs: List[TypeRepr]): TypeRepr = {
      remNames match {
        case name :: Nil =>
          val input = remInputs.head
          val resultType = AppliedType(zvTC, List(TypeRepr.of[Nothing], errorTpe, targetTpe))
          val fnType = AppliedType(fn1TC, List(input, resultType))
          ntRepr(List(name), List(fnType))
        case name :: tailNames =>
          val input = remInputs.head
          val tailType = loop(tailNames, remInputs.tail)
          val fnType = AppliedType(fn1TC, List(input, tailType))
          ntRepr(List(name), List(fnType))
        case Nil =>
          report.errorAndAbort("Cannot build builder type from empty fields")
      }
    }

    loop(names, inputTypes)
  }

  private[api] def buildNamedTupleChain(
    validators: Array[Any => Any],
    combine: Array[Any] => Any,
    idx: Int,
    collected: Array[Any]
  ): Any => Any = {
    (rawParam: Any) => {
      val zv = validators(idx)(rawParam)
      val newCollected = collected :+ zv
      if (idx == validators.length - 1) {
        combine(newCollected)
      } else {
        Tuple1(buildNamedTupleChain(validators, combine, idx + 1, newCollected))
      }
    }
  }

  @deprecated("Use macro-generated builder", "now")
  def caseclass1[T, T0, E](f: T0 => ZValidation[Nothing, E, T]): Tuple1[T0 => ZValidation[Nothing, E, T]] = Tuple1(f)
  @deprecated("Use macro-generated builder", "now")
  def caseclass2[T, T0, T1, E](f: (T0, T1) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(Tuple1(_)))
  @deprecated("Use macro-generated builder", "now")
  def caseclass3[T, T0, T1, T2, E](f: (T0, T1, T2) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(t => Tuple1(t.andThen(Tuple1(_)))))
  @deprecated("Use macro-generated builder", "now")
  def caseclass4[T, T0, T1, T2, T3, E](f: (T0, T1, T2, T3) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1(_)))))))
  @deprecated("Use macro-generated builder", "now")
  def caseclass5[T, T0, T1, T2, T3, T4, E](f: (T0, T1, T2, T3, T4) => ZValidation[Nothing, E, T]) = Tuple1(f.curried.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(t => Tuple1(t.andThen(Tuple1(_)))))))))
}




