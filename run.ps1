param([string]$Steps = '')
$ErrorActionPreference = 'Stop'
$taskRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$rscript = (Get-Command Rscript.exe -ErrorAction Stop).Source
$runArgs = @('--vanilla', (Join-Path $taskRoot 'scripts\run_all.R'))
if ($Steps) { $runArgs += "--steps=$Steps" }
Push-Location $taskRoot
try { & $rscript @runArgs; if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE } }
finally { Pop-Location }
