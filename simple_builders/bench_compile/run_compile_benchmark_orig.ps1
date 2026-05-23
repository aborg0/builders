param(
  [string[]]$ScalaVersions = @("3.7.4"),
  [int]$MinArity = 1,
  [int]$MaxArity = 22,
  [int]$Iterations = 3,
  [switch]$SkipWarmup,
  [switch]$NoSummary,
  [switch]$UseCompilerServer
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$simpleBuildersDir = Resolve-Path (Join-Path $scriptDir "..")
$generatedDir = Join-Path $scriptDir "generated"
$rawCsvPath = Join-Path $scriptDir "compile_times_raw.csv"
$summaryCsvPath = Join-Path $scriptDir "compile_times_summary.csv"

function Convert-ToPosixPath {
  param([string]$Path)
  return $Path.Replace("\", "/")
}

function New-FieldList {
  param([int]$Arity)
  $parts = @()
  foreach ($i in 1..$Arity) {
    $parts += "f${i}: Int"
  }
  return ($parts -join ", ")
}

function New-BuilderChain {
  param([int]$Arity)
  $lines = @("    Arity$Arity.builder")
  foreach ($i in 1..$Arity) {
    $lines += "      .f$i($i)"
  }
  return ($lines -join "`r`n")
}

function New-ScalaSource {
  param(
    [string]$ScalaVersion,
    [string]$JarPath,
    [int]$Arity,
    [ValidateSet("definition_only", "usage")][string]$Scenario
  )

  $fieldList = New-FieldList -Arity $Arity
  $builderChain = New-BuilderChain -Arity $Arity

  if ($Scenario -eq "definition_only") {
    $scenarioBody = @"
  val b = Arity$Arity.builder
  val _ = b
  ()
"@
  } else {
    $scenarioBody = @"
  val built: Arity$Arity = Program$arity.useBuilder()
  require(built.f1 == 1)
  ()
"@
  }

  return @"
//> using scala $ScalaVersion
//> using jar "$JarPath"

import api.BuilderGeneratorSimplest
import api.BuilderGeneratorSimplest.given

final case class Arity$arity($fieldList)
object Arity$arity extends BuilderGeneratorSimplest[Arity$arity]

object Program$arity {
  def touchDefinition(): Unit = {
    val b = Arity$arity.builder
    val _ = b
    ()
  }

  def useBuilder(): Arity$arity = {
$builderChain
  }
}

@main def run$arity(): Unit = {
  Program$arity.touchDefinition()
$scenarioBody
}
"@
}

function Invoke-ScalaCliCompile {
  param(
    [string]$SourceFile,
    [bool]$UseServer
  )

  $arguments = @("compile", $SourceFile)
  if (-not $UseServer) {
    $arguments += "--server=false"
  }

  $oldEap = $ErrorActionPreference
  $ErrorActionPreference = "Continue"

  $sw = [System.Diagnostics.Stopwatch]::StartNew()
  $output = & scala-cli @arguments 2>&1
  $exitCode = $LASTEXITCODE
  $sw.Stop()

  $ErrorActionPreference = $oldEap

  return [PSCustomObject]@{
    ElapsedMs = [math]::Round($sw.Elapsed.TotalMilliseconds, 3)
    ExitCode = $exitCode
    Output = ($output | Out-String)
  }
}

function Get-StdDev {
  param([double[]]$Values)

  if ($Values.Count -lt 2) {
    return 0.0
  }

  $avg = ($Values | Measure-Object -Average).Average
  $sumSq = 0.0
  foreach ($v in $Values) {
    $diff = $v - $avg
    $sumSq += $diff * $diff
  }

  $variance = $sumSq / ($Values.Count - 1)
  return [math]::Sqrt($variance)
}

Write-Host "Generating benchmark sources in $generatedDir"
if (-not (Test-Path $generatedDir)) {
  New-Item -ItemType Directory -Path $generatedDir | Out-Null
}

$rows = New-Object System.Collections.Generic.List[object]

foreach ($scalaVersion in $ScalaVersions) {
  $versionDir = Join-Path $generatedDir $scalaVersion
  if (-not (Test-Path $versionDir)) {
    New-Item -ItemType Directory -Path $versionDir | Out-Null
  }

  $jarPath = Join-Path $simpleBuildersDir "target/scala-$scalaVersion/simplebuilders_3-0.1.0-SNAPSHOT.jar"
  if (-not (Test-Path $jarPath)) {
    throw "Jar not found for Scala $scalaVersion at $jarPath"
  }
  $jarPathPosix = Convert-ToPosixPath -Path (Resolve-Path $jarPath)

  foreach ($arity in $MinArity..$MaxArity) {
    foreach ($scenario in @("definition_only", "usage")) {
      $fileName = "arity_$arity-$scenario.scala"
      $filePath = Join-Path $versionDir $fileName
      $source = New-ScalaSource -ScalaVersion $scalaVersion -JarPath $jarPathPosix -Arity $arity -Scenario $scenario
      $utf8NoBom = New-Object System.Text.UTF8Encoding($false)
      [System.IO.File]::WriteAllText($filePath, $source, $utf8NoBom)
    }
  }

  if (-not $SkipWarmup) {
    Write-Host "Warmup for Scala $scalaVersion"
    foreach ($scenario in @("definition_only", "usage")) {
      $warmupFile = Join-Path $versionDir "arity_1-$scenario.scala"
      $result = Invoke-ScalaCliCompile -SourceFile $warmupFile -UseServer:$UseCompilerServer
      $note = if ($result.ExitCode -eq 0) { "warmup" } else { "warmup_failed" }

      $rows.Add([PSCustomObject]@{
        timestamp = (Get-Date -Format o)
        scala_version = $scalaVersion
        arity = 1
        scenario = $scenario
        iteration = 0
        source_file = $warmupFile
        elapsed_ms = $result.ElapsedMs
        exit_code = $result.ExitCode
        warmup = $true
        notes = $note
      })

      if ($result.ExitCode -ne 0) {
        $snippet = ($result.Output -split "`r?`n" | Select-Object -First 8) -join " | "
        throw "Warmup failed for Scala $scalaVersion ($scenario): $snippet"
      }
    }
  }

  foreach ($arity in $MinArity..$MaxArity) {
    foreach ($scenario in @("definition_only", "usage")) {
      $sourceFile = Join-Path $versionDir "arity_$arity-$scenario.scala"

      foreach ($iteration in 1..$Iterations) {
        Write-Host "Compiling Scala $scalaVersion arity=$arity scenario=$scenario iteration=$iteration"
        $result = Invoke-ScalaCliCompile -SourceFile $sourceFile -UseServer:$UseCompilerServer

        $note = ""
        if ($result.ExitCode -ne 0) {
          $note = ($result.Output -split "`r?`n" | Select-Object -First 8) -join " | "
        }

        $rows.Add([PSCustomObject]@{
          timestamp = (Get-Date -Format o)
          scala_version = $scalaVersion
          arity = $arity
          scenario = $scenario
          iteration = $iteration
          source_file = $sourceFile
          elapsed_ms = $result.ElapsedMs
          exit_code = $result.ExitCode
          warmup = $false
          notes = $note
        })
      }
    }
  }
}

$rows | Export-Csv -Path $rawCsvPath -NoTypeInformation -Encoding UTF8
Write-Host "Raw CSV written to $rawCsvPath"

if (-not $NoSummary) {
  $summaryRows = New-Object System.Collections.Generic.List[object]

  $grouped = $rows |
    Where-Object { -not $_.warmup } |
    Group-Object scala_version, arity, scenario

  foreach ($group in $grouped) {
    $sample = $group.Group | Select-Object -First 1
    $values = @($group.Group | ForEach-Object { [double]$_.elapsed_ms })
    $mean = ($values | Measure-Object -Average).Average
    $min = ($values | Measure-Object -Minimum).Minimum
    $max = ($values | Measure-Object -Maximum).Maximum
    $std = Get-StdDev -Values $values
    $failures = @($group.Group | Where-Object { $_.exit_code -ne 0 }).Count

    $summaryRows.Add([PSCustomObject]@{
      scala_version = $sample.scala_version
      arity = [int]$sample.arity
      scenario = $sample.scenario
      samples = $values.Count
      failed_samples = $failures
      min_ms = [math]::Round([double]$min, 3)
      max_ms = [math]::Round([double]$max, 3)
      mean_ms = [math]::Round([double]$mean, 3)
      stddev_ms = [math]::Round([double]$std, 3)
    })
  }

  $summaryRows |
    Sort-Object scala_version, arity, scenario |
    Export-Csv -Path $summaryCsvPath -NoTypeInformation -Encoding UTF8

  Write-Host "Summary CSV written to $summaryCsvPath"
}

$failed = @($rows | Where-Object { -not $_.warmup -and $_.exit_code -ne 0 }).Count
if ($failed -gt 0) {
  Write-Host "Benchmark completed with $failed failed compile samples." -ForegroundColor Yellow
} else {
  Write-Host "Benchmark completed successfully with no failed compile samples." -ForegroundColor Green
}
