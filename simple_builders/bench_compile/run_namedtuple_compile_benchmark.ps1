param(
  [string[]]$ScalaVersions = @("3.7.4", "3.8.3"),
  [int]$MinArity = 1,
  [int]$MaxArity = 22,
  [int]$Iterations = 3,
  [switch]$SkipWarmup,
  [switch]$NoSummary,
  [switch]$UseCompilerServer
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$generatedDir = Join-Path $scriptDir "generated_namedtuple"
$rawCsvPath = Join-Path $scriptDir "namedtuple_compile_times_raw.csv"
$summaryCsvPath = Join-Path $scriptDir "namedtuple_compile_times_summary.csv"
$comparisonCsvPath = Join-Path $scriptDir "namedtuple_compile_times_comparison.csv"

function New-NamedTupleBuilderType {
  param(
    [int]$Index,
    [int]$Arity
  )

  if ($Index -eq $Arity) {
    return "NamedTuple[Tuple1[`"field$Index`"], Tuple1[Int => Int]]"
  }

  $next = New-NamedTupleBuilderType -Index ($Index + 1) -Arity $Arity
  return "NamedTuple[Tuple1[`"field$Index`"], Tuple1[Int => $next]]"
}

function New-NestedLambdaExpr {
  param(
    [int]$Index,
    [int]$Arity
  )

  if ($Index -eq $Arity) {
    $sumExpr = ((1..$Arity) | ForEach-Object { "x$_" }) -join " + "
    return "(x${Index}: Int) => $sumExpr"
  }

  $next = New-NestedLambdaExpr -Index ($Index + 1) -Arity $Arity
  return "(x${Index}: Int) => Tuple1($next)"
}

function New-BuilderChain {
  param([int]$Arity)

  $parts = @("builder")
  foreach ($i in 1..$Arity) {
    $parts += ".field$i($i)"
  }

  return ($parts -join "")
}

function New-ScalaSource {
  param(
    [string]$ScalaVersion,
    [int]$Arity,
    [ValidateSet("definition_only", "usage")][string]$Scenario
  )

  $builderType = New-NamedTupleBuilderType -Index 1 -Arity $Arity
  $lambdaExpr = New-NestedLambdaExpr -Index 1 -Arity $Arity
  $builderChain = New-BuilderChain -Arity $Arity

  $scenarioBody = ""
  if ($Scenario -eq "definition_only") {
    $scenarioBody = @"
  val _ = builder
  ()
"@
  } else {
    $scenarioBody = @"
  val result: Int = $builderChain
  require(result > 0)
  ()
"@
  }

  return @"
//> using scala $ScalaVersion

import scala.NamedTuple.NamedTuple

type Builder$arity = $builderType

object Program$arity {
  val builder: Builder$arity = Tuple1($lambdaExpr)

  def run(): Unit = {
$scenarioBody
  }
}

@main def run$arity(): Unit = {
  Program$arity.run()
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

if ($MinArity -lt 1) {
  throw "MinArity must be at least 1"
}

if ($MaxArity -lt $MinArity) {
  throw "MaxArity must be greater than or equal to MinArity"
}

if ($Iterations -lt 1) {
  throw "Iterations must be at least 1"
}

Write-Host "Generating NamedTuple benchmark sources in $generatedDir"
if (-not (Test-Path $generatedDir)) {
  New-Item -ItemType Directory -Path $generatedDir | Out-Null
}

$rows = New-Object System.Collections.Generic.List[object]

foreach ($scalaVersion in $ScalaVersions) {
  $versionDir = Join-Path $generatedDir $scalaVersion
  if (-not (Test-Path $versionDir)) {
    New-Item -ItemType Directory -Path $versionDir | Out-Null
  }

  foreach ($arity in $MinArity..$MaxArity) {
    foreach ($scenario in @("definition_only", "usage")) {
      $fileName = "arity_$arity-$scenario.scala"
      $filePath = Join-Path $versionDir $fileName
      $source = New-ScalaSource -ScalaVersion $scalaVersion -Arity $arity -Scenario $scenario
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

  $comparisonRows = New-Object System.Collections.Generic.List[object]
  $summaryLookup = @{}
  foreach ($row in $summaryRows) {
    $key = "$($row.scala_version)|$($row.arity)|$($row.scenario)"
    $summaryLookup[$key] = $row
  }

  foreach ($scalaVersion in $ScalaVersions) {
    foreach ($arity in $MinArity..$MaxArity) {
      $defKey = "$scalaVersion|$arity|definition_only"
      $useKey = "$scalaVersion|$arity|usage"

      if ($summaryLookup.ContainsKey($defKey) -and $summaryLookup.ContainsKey($useKey)) {
        $defRow = $summaryLookup[$defKey]
        $useRow = $summaryLookup[$useKey]

        $delta = [double]$useRow.mean_ms - [double]$defRow.mean_ms
        $ratio = if ([double]$defRow.mean_ms -ne 0.0) {
          [double]$useRow.mean_ms / [double]$defRow.mean_ms
        } else {
          [double]::NaN
        }

        $comparisonRows.Add([PSCustomObject]@{
          scala_version = $scalaVersion
          arity = $arity
          definition_only_mean_ms = [math]::Round([double]$defRow.mean_ms, 3)
          usage_mean_ms = [math]::Round([double]$useRow.mean_ms, 3)
          delta_ms = [math]::Round($delta, 3)
          usage_over_definition_ratio = [math]::Round($ratio, 4)
          definition_failed_samples = [int]$defRow.failed_samples
          usage_failed_samples = [int]$useRow.failed_samples
        })
      }
    }
  }

  $comparisonRows |
    Sort-Object scala_version, arity |
    Export-Csv -Path $comparisonCsvPath -NoTypeInformation -Encoding UTF8

  Write-Host "Comparison CSV written to $comparisonCsvPath"
}

$failed = @($rows | Where-Object { -not $_.warmup -and $_.exit_code -ne 0 }).Count
if ($failed -gt 0) {
  Write-Host "NamedTuple benchmark completed with $failed failed compile samples." -ForegroundColor Yellow
} else {
  Write-Host "NamedTuple benchmark completed successfully with no failed compile samples." -ForegroundColor Green
}
