package builders.scalafix

object GenerateBuilderAnnotationMatcher {
  // SemanticDB symbol candidates for the annotation class and constructor.
  private val CandidateSymbols: Set[String] = Set(
    "builders/configuration/GenerateBuilder#",
    "builders/configuration/GenerateBuilder#`<init>`()."
  )

  private val CandidateNormalized: Set[String] = Set(
    "builders.configuration.GenerateBuilder"
  )

  def matches(symbolValue: String, annotationName: String): Boolean = {
    val trimmed = Option(symbolValue).getOrElse("").trim
    val normalized = normalizeSymbol(trimmed)
    CandidateSymbols.contains(trimmed) ||
    CandidateNormalized.contains(normalized) ||
    annotationName.endsWith("GenerateBuilder")
  }

  private def normalizeSymbol(symbol: String): String = {
    symbol
      .replace('/', '.')
      .replace("#", "")
      .replace("`<init>`()", "")
      .stripSuffix(".")
      .trim
  }
}
