package builders.scalafix

import java.nio.file.{Files, Path, Paths}

import builders.scalafix.GenerateBuilderCompanionRenderer.SmartCtorResultKind

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

object GenerateBuilderSmartCtorDiscovery {
  final case class DiscoveredSmartCtor(methodName: String, resultKind: SmartCtorResultKind, inputTypeExpr: Option[String])

  def discoverFromPath(fieldTypeExpr: String, sourceDir: Option[Path], mode: SmartConstructorMode): Option[DiscoveredSmartCtor] = {
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
      discoverFromContents(fieldTypeExpr, contents, mode)
    }
  }

  def discoverFromSource(fieldTypeExpr: String, source: String, mode: SmartConstructorMode): Option[DiscoveredSmartCtor] = {
    discoverFromContents(fieldTypeExpr, List(source), mode)
  }

  private def discoverFromContents(fieldTypeExpr: String, contents: List[String], mode: SmartConstructorMode): Option[DiscoveredSmartCtor] = {
    val normalized = fieldTypeExpr.trim
    val candidateNames = candidateObjectNames(normalized, contents)

    contents
      .flatMap { content =>
        candidateNames.flatMap { candidateName =>
          val applyRegex: Regex = ("(?s)object\\s+" + Regex.quote(candidateName) + "\\b.*?def\\s+apply\\s*\\(([^)]*)\\)\\s*:\\s*([^=\\n{]+)").r
          val makeRegex: Regex = ("(?s)object\\s+" + Regex.quote(candidateName) + "\\b.*?def\\s+make\\s*\\(([^)]*)\\)\\s*:\\s*([^=\\n{]+)").r
          val applyFound = applyRegex.findAllMatchIn(content).flatMap { methodMatch =>
            val inputTypeExpr = extractFirstParameterType(methodMatch.group(1).trim)
            smartCtorResultKind(methodMatch.group(2).trim, normalized, mode).map(kind => DiscoveredSmartCtor("apply", kind, inputTypeExpr))
          }.toList
          val makeFound = makeRegex.findAllMatchIn(content).flatMap { methodMatch =>
            val inputTypeExpr = extractFirstParameterType(methodMatch.group(1).trim)
            smartCtorResultKind(methodMatch.group(2).trim, normalized, mode).map(kind => DiscoveredSmartCtor("make", kind, inputTypeExpr))
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

  private def smartCtorResultKind(returnType: String, normalizedFieldType: String, mode: SmartConstructorMode): Option[SmartCtorResultKind] = {
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
        Some(SmartCtorResultKind.Validation)
      } else if (eitherKinds.exists(trimmed.startsWith)) {
        Some(SmartCtorResultKind.EitherResult)
      } else if (trimmed == normalizedFieldType) {
        Some(SmartCtorResultKind.Direct)
      } else {
        None
      }

    detected.filter(allowedByMode(_, mode))
  }

  private def allowedByMode(resultKind: SmartCtorResultKind, mode: SmartConstructorMode): Boolean = mode match {
    case SmartConstructorMode.ZValidation =>
      true
    case SmartConstructorMode.Either =>
      resultKind != SmartCtorResultKind.Validation
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
    case SmartCtorResultKind.Direct => 2
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