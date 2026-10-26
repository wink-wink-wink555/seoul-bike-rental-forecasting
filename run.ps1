param(
  [switch]$RefreshData
)

$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$scriptFile = Join-Path $projectRoot "scripts\run_all.R"
$rscriptCommand = Get-Command Rscript.exe, Rscript -ErrorAction SilentlyContinue |
  Select-Object -First 1
if (-not $rscriptCommand) {
  Write-Error "Rscript was not found in PATH. Install R and add its bin directory to PATH."
  exit 1
}
$rscript = $rscriptCommand.Source
if (-not (Test-Path -LiteralPath $scriptFile)) {
  Write-Error "Missing project entry script: $scriptFile"
  exit 1
}

$runArgs = @("--vanilla", $scriptFile)
if ($RefreshData) { $runArgs += "--refresh-data" }
Push-Location $projectRoot
try {
  & $rscript @runArgs
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
finally {
  Pop-Location
}
