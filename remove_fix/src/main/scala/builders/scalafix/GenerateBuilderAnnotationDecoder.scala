package builders.scalafix

final case class DecodedGenerateBuilder(
  style: BuilderStyle,
  primitivePolicy: PrimitivePolicy,
  pathMode: PathMode,
  effectMode: EffectMode,
  conversionMode: ConversionMode,
  staleCheckMode: StaleCheckMode,
  mergeMode: MergeMode,
  builderMethodName: String,
  generateExtraVariants: Boolean,
  simpleOptionalValues: SimpleOptionalValues,
  smartConstructorMode: SmartConstructorMode,
  combineErrors: ErrorCombination,
  effectFailureMode: EffectFailureMode,
  effectExecutionMode: EffectExecutionMode,
  generatedCodeShape: GeneratedCodeShape
)

object DecodedGenerateBuilder {
  val default: DecodedGenerateBuilder = DecodedGenerateBuilder(
    style = BuilderStyle.Validating,
    primitivePolicy = PrimitivePolicy.PrimitiveAndWrappedIfDerivable,
    pathMode = PathMode.FullCollectionAware,
    effectMode = EffectMode.ValidationOnly,
    conversionMode = ConversionMode.SynthesizeIfMissing,
    staleCheckMode = StaleCheckMode.SignatureHash,
    mergeMode = MergeMode.GeneratedRegionOnly,
    builderMethodName = "builder",
    generateExtraVariants = true,
    simpleOptionalValues = SimpleOptionalValues.ExplicitOptionalValues,
    smartConstructorMode = SmartConstructorMode.ZValidation,
    combineErrors = ErrorCombination.Union,
    effectFailureMode = EffectFailureMode.Propagate,
    effectExecutionMode = EffectExecutionMode.Sequential,
    generatedCodeShape = GeneratedCodeShape.Readable
  )
}

object GenerateBuilderAnnotationDecoder {
  private val knownKeys: Set[String] = Set(
    "style",
    "primitivePolicy",
    "pathMode",
    "effectMode",
    "conversionMode",
    "staleCheckMode",
    "mergeMode",
    "builderMethodName",
    "generateExtraVariants",
    "simpleOptionalValues",
    "smartConstructorMode",
    "combineErrors",
    "effectFailureMode",
    "effectExecutionMode",
    "generatedCodeShape"
  )

  def decode(arguments: Map[String, String]): Either[List[String], DecodedGenerateBuilder] = {
    val unknownKeys = arguments.keySet.diff(knownKeys).toList.sorted
    val unknownErrors = unknownKeys.map { key =>
      s"Unknown annotation argument: $key"
    }

    val styleResult = parseEnum(
      raw = arguments.get("style"),
      allValues = BuilderStyle.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.style,
      key = "style"
    )
    val primitivePolicyResult = parseEnum(
      raw = arguments.get("primitivePolicy"),
      allValues = PrimitivePolicy.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.primitivePolicy,
      key = "primitivePolicy"
    )
    val pathModeResult = parseEnum(
      raw = arguments.get("pathMode"),
      allValues = PathMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.pathMode,
      key = "pathMode"
    )
    val effectModeResult = parseEnum(
      raw = arguments.get("effectMode"),
      allValues = EffectMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.effectMode,
      key = "effectMode"
    )
    val conversionModeResult = parseEnum(
      raw = arguments.get("conversionMode"),
      allValues = ConversionMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.conversionMode,
      key = "conversionMode"
    )
    val staleCheckModeResult = parseEnum(
      raw = arguments.get("staleCheckMode"),
      allValues = StaleCheckMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.staleCheckMode,
      key = "staleCheckMode"
    )
    val mergeModeResult = parseEnum(
      raw = arguments.get("mergeMode"),
      allValues = MergeMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.mergeMode,
      key = "mergeMode"
    )
    val simpleOptionalValuesResult = parseEnum(
      raw = arguments.get("simpleOptionalValues"),
      allValues = SimpleOptionalValues.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.simpleOptionalValues,
      key = "simpleOptionalValues"
    )
    val smartConstructorModeResult = parseEnum(
      raw = arguments.get("smartConstructorMode"),
      allValues = SmartConstructorMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.smartConstructorMode,
      key = "smartConstructorMode"
    )
    val combineErrorsResult = parseEnum(
      raw = arguments.get("combineErrors"),
      allValues = ErrorCombination.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.combineErrors,
      key = "combineErrors"
    )
    val effectFailureModeResult = parseEnum(
      raw = arguments.get("effectFailureMode"),
      allValues = EffectFailureMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.effectFailureMode,
      key = "effectFailureMode"
    )
    val effectExecutionModeResult = parseEnum(
      raw = arguments.get("effectExecutionMode"),
      allValues = EffectExecutionMode.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.effectExecutionMode,
      key = "effectExecutionMode"
    )
    val generatedCodeShapeResult = parseEnum(
      raw = arguments.get("generatedCodeShape"),
      allValues = GeneratedCodeShape.all.map(v => v.value -> v).toMap,
      default = DecodedGenerateBuilder.default.generatedCodeShape,
      key = "generatedCodeShape"
    )
    val builderMethodNameResult = parseBuilderMethodName(arguments.get("builderMethodName"))
    val generateExtraVariantsResult = parseBoolean(
      raw = arguments.get("generateExtraVariants"),
      default = DecodedGenerateBuilder.default.generateExtraVariants,
      key = "generateExtraVariants"
    )

    val allErrors = unknownErrors ++
      styleResult.left.toOption.toList.flatten ++
      primitivePolicyResult.left.toOption.toList.flatten ++
      pathModeResult.left.toOption.toList.flatten ++
      effectModeResult.left.toOption.toList.flatten ++
      conversionModeResult.left.toOption.toList.flatten ++
      staleCheckModeResult.left.toOption.toList.flatten ++
      mergeModeResult.left.toOption.toList.flatten ++
      simpleOptionalValuesResult.left.toOption.toList.flatten ++
      smartConstructorModeResult.left.toOption.toList.flatten ++
      combineErrorsResult.left.toOption.toList.flatten ++
      effectFailureModeResult.left.toOption.toList.flatten ++
      effectExecutionModeResult.left.toOption.toList.flatten ++
      generatedCodeShapeResult.left.toOption.toList.flatten ++
      builderMethodNameResult.left.toOption.toList.flatten ++
      generateExtraVariantsResult.left.toOption.toList.flatten

    if (allErrors.nonEmpty) {
      Left(allErrors)
    } else {
      Right(
        DecodedGenerateBuilder(
          style = styleResult.toOption.get,
          primitivePolicy = primitivePolicyResult.toOption.get,
          pathMode = pathModeResult.toOption.get,
          effectMode = effectModeResult.toOption.get,
          conversionMode = conversionModeResult.toOption.get,
          staleCheckMode = staleCheckModeResult.toOption.get,
          mergeMode = mergeModeResult.toOption.get,
          builderMethodName = builderMethodNameResult.toOption.get,
          generateExtraVariants = generateExtraVariantsResult.toOption.get,
          simpleOptionalValues = simpleOptionalValuesResult.toOption.get,
          smartConstructorMode = smartConstructorModeResult.toOption.get,
          combineErrors = combineErrorsResult.toOption.get,
          effectFailureMode = effectFailureModeResult.toOption.get,
          effectExecutionMode = effectExecutionModeResult.toOption.get,
          generatedCodeShape = generatedCodeShapeResult.toOption.get
        )
      )
    }
  }

  private def parseBuilderMethodName(raw: Option[String]): Either[List[String], String] = {
    raw match {
      case None =>
        Right(DecodedGenerateBuilder.default.builderMethodName)
      case Some(value) =>
        val trimmed = value.trim
        val unquoted =
          if (trimmed.startsWith("\"") && trimmed.endsWith("\"") && trimmed.length >= 2) {
            trimmed.substring(1, trimmed.length - 1)
          } else {
            trimmed
          }
        val namePattern = "[A-Za-z_][A-Za-z0-9_]*".r
        if (namePattern.pattern.matcher(unquoted).matches()) {
          Right(unquoted)
        } else {
          Left(List(s"Invalid value '$value' for builderMethodName. Expected a Scala identifier."))
        }
    }
  }

  private def parseBoolean(raw: Option[String], default: Boolean, key: String): Either[List[String], Boolean] = {
    raw match {
      case None =>
        Right(default)
      case Some(value) =>
        value.trim.toLowerCase match {
          case "true" => Right(true)
          case "false" => Right(false)
          case _ => Left(List(s"Invalid value '$value' for $key. Supported values: true, false"))
        }
    }
  }

  private def parseEnum[A](
    raw: Option[String],
    allValues: Map[String, A],
    default: A,
    key: String
  ): Either[List[String], A] = {
    raw match {
      case None =>
        Right(default)
      case Some(value) =>
        val normalized = stripQualifier(value)
        allValues.get(normalized) match {
          case Some(found) =>
            Right(found)
          case None =>
            val supported = allValues.keys.toList.sorted.mkString(", ")
            Left(List(s"Invalid value '$value' for $key. Supported values: $supported"))
        }
    }
  }

  private def stripQualifier(value: String): String = {
    val trimmed = value.trim
    if (trimmed.contains(".")) {
      trimmed.split("\\.").lastOption.getOrElse(trimmed)
    } else {
      trimmed
    }
  }
}
