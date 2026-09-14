package builders.scalafix

import java.nio.file.{Files, Path, Paths}

import builders.scalafix.GenerateBuilderCompanionRenderer.SmartCtorResultKind

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

object GenerateBuilderSmartCtorDiscovery {
  final case class DiscoveredSmartCtor(
    methodName: String,
    resultKind: SmartCtorResultKind,
    inputTypeExpr: Option[String],
    zioEnvironmentTypeExpr: Option[String] = None,
    zioErrorTypeExpr: Option[String] = None
  )

  def discoverFromPath(
    fieldTypeExpr: String,
    sourceDir: Option[Path],
    mode: SmartConstructorMode,
    enableEffectConstructors: Boolean = false
  ): Option[DiscoveredSmartCtor] = {
    val normalized = fieldTypeExpr.trim
    val bareTypePattern = """^[A-Za-z_][A-Za-z0-9_\.]*$""".r
    if (bareTypePattern.findFirstIn(normalized).isEmpty) {
      None
    } else {
      val simpleTypeName = normalized.split('.').lastOption.getOrElse(normalized)
      val localCandidates = sourceDir.toList.flatMap { dir =>
        if (Files.isDirectory(dir)) {
          Files.list(dir).iterator().asScala.toList
            .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".scala"))
        } else {
          Nil
        }
      }
      val workspaceCandidates =
        if (isPotentialSubtypeOrNewtype(normalized)) {
          discoverWorkspaceScalaFiles(None)
        } else {
          discoverWorkspaceScalaFiles(Some(simpleTypeName))
        }
      val contents = (localCandidates ++ workspaceCandidates).distinct.flatMap { path =>
        Try(Files.readString(path)).toOption
      }
      discoverFromContents(fieldTypeExpr, contents, mode, enableEffectConstructors)
    }
  }

  def discoverFromSource(
    fieldTypeExpr: String,
    source: String,
    mode: SmartConstructorMode,
    enableEffectConstructors: Boolean = false
  ): Option[DiscoveredSmartCtor] = {
    discoverFromContents(fieldTypeExpr, List(source), mode, enableEffectConstructors)
  }

  private def discoverFromContents(
    fieldTypeExpr: String,
    contents: List[String],
    mode: SmartConstructorMode,
    enableEffectConstructors: Boolean
  ): Option[DiscoveredSmartCtor] = {
    val normalized = fieldTypeExpr.trim
    val candidateNames = candidateObjectNames(normalized, contents)

    contents
      .flatMap { content =>
        candidateNames.flatMap { candidateName =>
          val applyRegex: Regex = ("(?s)object\\s+" + Regex.quote(candidateName) + "\\b.*?def\\s+apply\\s*\\(([^)]*)\\)\\s*:\\s*([^=\\n{]+)").r
          val makeRegex: Regex = ("(?s)object\\s+" + Regex.quote(candidateName) + "\\b.*?def\\s+make\\s*\\(([^)]*)\\)\\s*:\\s*([^=\\n{]+)").r
          val applyFound = applyRegex.findAllMatchIn(content).flatMap { methodMatch =>
            val inputTypeExpr = extractFirstParameterType(methodMatch.group(1).trim)
            smartCtorResultKind(methodMatch.group(2).trim, normalized, mode, enableEffectConstructors).map {
              case (kind, zioEnvTypeExpr, zioErrorTypeExpr) =>
                DiscoveredSmartCtor("apply", kind, inputTypeExpr, zioEnvTypeExpr, zioErrorTypeExpr)
            }
          }.toList
          val makeFound = makeRegex.findAllMatchIn(content).flatMap { methodMatch =>
            val inputTypeExpr = extractFirstParameterType(methodMatch.group(1).trim)
            smartCtorResultKind(methodMatch.group(2).trim, normalized, mode, enableEffectConstructors).map {
              case (kind, zioEnvTypeExpr, zioErrorTypeExpr) =>
                DiscoveredSmartCtor("make", kind, inputTypeExpr, zioEnvTypeExpr, zioErrorTypeExpr)
            }
          }.toList
          val inheritedMake = inferSubtypeOrNewtypeMake(candidateName, content, mode)
          applyFound ++ makeFound ++ inheritedMake.toList
        }
      }
      .sortBy(discovered => (methodPriority(discovered.methodName), resultKindPriority(discovered.resultKind)))
      .headOption
  }

  private def candidateObjectNames(normalizedFieldType: String, contents: List[String]): List[String] = {
    val directNames = List(normalizedFieldType.split('.').lastOption.getOrElse(normalizedFieldType))
    val typeOwnerNames =
      if (normalizedFieldType.endsWith(".Type")) {
        List(normalizedFieldType.stripSuffix(".Type").split('.').lastOption.getOrElse(normalizedFieldType.stripSuffix(".Type")))
      } else {
        Nil
      }
    val aliasOwnerNames =
      contents.flatMap(content => resolveTypeAliasOwner(normalizedFieldType, content))

    (directNames ++ typeOwnerNames ++ aliasOwnerNames).distinct.filter(_.nonEmpty)
  }

  private def resolveTypeAliasOwner(normalizedFieldType: String, content: String): Option[String] = {
    val aliasRegex = ("(?m)\\b(?:opaque\\s+)?type\\s+" + Regex.quote(normalizedFieldType) + "\\s*=\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\.Type\\b").r
    aliasRegex.findFirstMatchIn(content).map(_.group(1).split('.').last)
  }

  private def inferSubtypeOrNewtypeMake(candidateName: String, content: String, mode: SmartConstructorMode): Option[DiscoveredSmartCtor] = {
    val subtypeLikeRegex = ("(?s)object\\s+" + Regex.quote(candidateName) + "\\s+extends\\s+(?:[A-Za-z_][A-Za-z0-9_\\.]*\\.)?(?:Subtype|Newtype|SubtypeCustom|NewtypeCustom)\\s*\\[([^\\]]+)\\]").r
    subtypeLikeRegex.findFirstMatchIn(content).flatMap { m =>
      if (allowedByMode(SmartCtorResultKind.Validation, mode)) {
        Some(DiscoveredSmartCtor("make", SmartCtorResultKind.Validation, Some(m.group(1).trim)))
      } else {
        None
      }
    }
  }

  private def extractFirstParameterType(parametersExpr: String): Option[String] = {
    val firstParam = parametersExpr
      .split(',')
      .iterator
      .map(_.trim)
      .find(part => part.nonEmpty && !part.startsWith("using") && !part.startsWith("implicit"))

    firstParam.flatMap { paramExpr =>
      val colonIndex = paramExpr.indexOf(':')
      if (colonIndex < 0) {
        None
      } else {
        val maybeType = paramExpr.substring(colonIndex + 1).trim
        if (maybeType.nonEmpty) Some(maybeType) else None
      }
    }
  }

  private def isPotentialSubtypeOrNewtype(normalizedFieldType: String): Boolean = {
    normalizedFieldType.endsWith(".Type") || !normalizedFieldType.contains('.')
  }

  private def smartCtorResultKind(
    returnType: String,
    normalizedFieldType: String,
    mode: SmartConstructorMode,
    enableEffectConstructors: Boolean
  ): Option[(SmartCtorResultKind, Option[String], Option[String])] = {
    val trimmed = returnType.replaceAll("\\s+", " ").trim
    val validationKinds = Set(
      "ZValidation[",
      "zio.prelude.ZValidation[",
      "Validation[",
      "zio.prelude.Validation["
    )
    val eitherKinds = Set("Either[", "scala.util.Either[")

    val detected =
      if (validationKinds.exists(trimmed.startsWith)) {
        Some((SmartCtorResultKind.Validation, None, None))
      } else if (eitherKinds.exists(trimmed.startsWith)) {
        Some((SmartCtorResultKind.EitherResult, None, None))
      } else if (enableEffectConstructors) {
        parseAdditionalEffectCtorTypes(trimmed, normalizedFieldType)
      } else {
        None
      }

    val effectDetected =
      if (detected.isDefined) {
        detected
      } else {
        parseZioEffectTypes(trimmed).flatMap {
          case (environmentTypeExpr, errorTypeExpr, valueTypeExpr) =>
            if (valueTypeExpr == normalizedFieldType) {
              Some((SmartCtorResultKind.ZioResult, Some(environmentTypeExpr), Some(errorTypeExpr)))
            } else {
              None
            }
        }
      }

    val resolved = effectDetected.orElse {
      if (trimmed == normalizedFieldType) {
        Some((SmartCtorResultKind.Direct, None, None))
      } else {
        None
      }
    }
    resolved.filter(entry => allowedByMode(entry._1, mode))
  }

  private def allowedByMode(resultKind: SmartCtorResultKind, mode: SmartConstructorMode): Boolean = mode match {
    case SmartConstructorMode.ZValidation =>
      true
    case SmartConstructorMode.Either =>
      resultKind != SmartCtorResultKind.Validation &&
      resultKind != SmartCtorResultKind.ZioResult &&
      resultKind != SmartCtorResultKind.PromiseScalaResult &&
      resultKind != SmartCtorResultKind.FutureJavaResult &&
      resultKind != SmartCtorResultKind.TryResult &&
      resultKind != SmartCtorResultKind.OptionResult
    case SmartConstructorMode.Direct =>
      resultKind == SmartCtorResultKind.Direct
  }

  private def methodPriority(methodName: String): Int = methodName match {
    case "apply" => 0
    case "make" => 1
    case _ => 2
  }

  private def resultKindPriority(resultKind: SmartCtorResultKind): Int = resultKind match {
    case SmartCtorResultKind.Validation => 0
    case SmartCtorResultKind.EitherResult => 1
    case SmartCtorResultKind.ZioResult => 2
    case SmartCtorResultKind.PromiseScalaResult => 3
    case SmartCtorResultKind.FutureJavaResult => 4
    case SmartCtorResultKind.TryResult => 5
    case SmartCtorResultKind.OptionResult => 6
    case SmartCtorResultKind.Direct => 7
  }

  private def parseAdditionalEffectCtorTypes(
    returnType: String,
    normalizedFieldType: String
  ): Option[(SmartCtorResultKind, Option[String], Option[String])] = {
    if (!returnType.endsWith("]")) {
      None
    } else {
      parseSimpleSingleTypeArg(returnType, List("scala.concurrent.Promise[", "Promise[")).collect {
        case valueTypeExpr if valueTypeExpr == normalizedFieldType =>
          (SmartCtorResultKind.PromiseScalaResult, None, None)
      }.orElse {
        parseSimpleSingleTypeArg(
          returnType,
          List(
            "java.util.concurrent.Future[",
            "java.util.concurrent.CompletionStage[",
            "java.util.concurrent.CompletableFuture[",
            "Future[",
            "CompletionStage[",
            "CompletableFuture["
          )
        ).collect {
          case valueTypeExpr if valueTypeExpr == normalizedFieldType =>
            (SmartCtorResultKind.FutureJavaResult, None, None)
        }
      }.orElse {
        parseSimpleSingleTypeArg(returnType, List("scala.util.Try[", "Try[")).collect {
          case valueTypeExpr if valueTypeExpr == normalizedFieldType =>
            (SmartCtorResultKind.TryResult, None, None)
        }
      }.orElse {
        parseSimpleSingleTypeArg(returnType, List("scala.Option[", "Option[")).collect {
          case valueTypeExpr if valueTypeExpr == normalizedFieldType =>
            (SmartCtorResultKind.OptionResult, None, None)
        }
      }
    }
  }

  private def parseSimpleSingleTypeArg(returnType: String, prefixes: List[String]): Option[String] = {
    prefixes.collectFirst {
      case prefix if returnType.startsWith(prefix) =>
        returnType.substring(prefix.length, returnType.length - 1)
    }.map(_.trim).filter(_.nonEmpty)
  }

  private def parseZioEffectTypes(returnType: String): Option[(String, String, String)] = {
    val zioPrefixes = List("zio.ZIO[", "ZIO[")
    val ioPrefixes = List("zio.IO[", "IO[")
    val taskPrefixes = List("zio.Task[", "Task[")
    val uioPrefixes = List("zio.UIO[", "UIO[")
    val rioPrefixes = List("zio.RIO[", "RIO[")
    val urioPrefixes = List("zio.URIO[", "URIO[")

    def argsAfterPrefix(prefixes: List[String], source: String): Option[List[String]] = {
      prefixes.collectFirst {
        case prefix if source.startsWith(prefix) =>
          source.substring(prefix.length, source.length - 1)
      }.flatMap(splitTypeArguments)
    }

    if (!returnType.endsWith("]")) {
      None
    } else {
      argsAfterPrefix(zioPrefixes, returnType).collect {
        case List(environmentTypeExpr, errorTypeExpr, valueTypeExpr) =>
          (environmentTypeExpr, errorTypeExpr, valueTypeExpr)
      }.orElse {
        argsAfterPrefix(ioPrefixes, returnType).collect {
          case List(errorTypeExpr, valueTypeExpr) =>
            ("Any", errorTypeExpr, valueTypeExpr)
        }
      }.orElse {
        argsAfterPrefix(taskPrefixes, returnType).collect {
          case List(valueTypeExpr) =>
            ("Any", "Throwable", valueTypeExpr)
        }
      }.orElse {
        argsAfterPrefix(uioPrefixes, returnType).collect {
          case List(valueTypeExpr) =>
            ("Any", "Nothing", valueTypeExpr)
        }
      }.orElse {
        argsAfterPrefix(rioPrefixes, returnType).collect {
          case List(environmentTypeExpr, valueTypeExpr) =>
            (environmentTypeExpr, "Throwable", valueTypeExpr)
        }
      }.orElse {
        argsAfterPrefix(urioPrefixes, returnType).collect {
          case List(environmentTypeExpr, valueTypeExpr) =>
            (environmentTypeExpr, "Nothing", valueTypeExpr)
        }
      }
    }
  }

  private def splitTypeArguments(argumentsExpr: String): Option[List[String]] = {
    val parts = scala.collection.mutable.ListBuffer.empty[String]
    val current = new StringBuilder
    var depthSquare = 0
    var depthRound = 0
    var depthCurly = 0

    argumentsExpr.foreach { ch =>
      ch match {
        case '[' =>
          depthSquare += 1
          current.append(ch)
        case ']' =>
          depthSquare -= 1
          if (depthSquare < 0) {
            return None
          }
          current.append(ch)
        case '(' =>
          depthRound += 1
          current.append(ch)
        case ')' =>
          depthRound -= 1
          if (depthRound < 0) {
            return None
          }
          current.append(ch)
        case '{' =>
          depthCurly += 1
          current.append(ch)
        case '}' =>
          depthCurly -= 1
          if (depthCurly < 0) {
            return None
          }
          current.append(ch)
        case ',' if depthSquare == 0 && depthRound == 0 && depthCurly == 0 =>
          parts += current.toString.trim
          current.clear()
        case other =>
          current.append(other)
      }
    }

    if (depthSquare != 0 || depthRound != 0 || depthCurly != 0) {
      None
    } else {
      parts += current.toString.trim
      Some(parts.toList.filter(_.nonEmpty))
    }
  }

  private def discoverWorkspaceScalaFiles(simpleTypeName: Option[String]): List[Path] = {
    val cwd = Paths.get("")
    val ignoredDirNames = Set("target", ".git", ".bloop", ".idea", ".metals", ".scala-build")

    Try {
      Files.walk(cwd)
        .iterator()
        .asScala
        .filter { path =>
          val fileName = path.getFileName.toString
          val isIgnoredDir = path.iterator().asScala.exists(part => ignoredDirNames.contains(part.toString))
          val fileMatches = simpleTypeName.forall(name => fileName == s"$name.scala")
          Files.isRegularFile(path) && !isIgnoredDir && fileName.endsWith(".scala") && fileMatches
        }
        .toList
    }.getOrElse(Nil)
  }
}