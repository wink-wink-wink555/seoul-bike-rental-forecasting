param(
  [switch]$RefreshData
)

$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$scriptFile = Join-Path $projectRoot "scripts\run_all.R"
$rscript = (Get-Command Rscript.exe -ErrorAction SilentlyContinue).Source
if (-not $rscript) {
  $candidates = @(
    "D:\R\R-4.6.1\bin\Rscript.exe",
    (Join-Path $env:ProgramFiles "R\R-4.6.1\bin\Rscript.exe"),
    "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
  )
  $rscript = $candidates | Where-Object { Test-Path -LiteralPath $_ } | Select-Object -First 1
}
if (-not $rscript) {
  Write-Error "Rscript.exe was not found. Install R or set up Rscript in PATH."
  exit 1
}
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
