# Collects all evidence for the report into the report_output folder.
# Usage (from this folder):  powershell -ExecutionPolicy Bypass -File .\run_report.ps1
$ErrorActionPreference = 'Continue'
$env:Path = "C:\ghcup\bin;$env:Path"
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$OutputEncoding = New-Object System.Text.UTF8Encoding $false  # UTF-8 without BOM
$out = Join-Path $PSScriptRoot 'report_output'
New-Item -ItemType Directory -Force $out | Out-Null
Set-Location $PSScriptRoot

# Task 0: compiler version
ghc --version | Tee-Object "$out\00_ghc_version.txt"

# Compilation with all warnings enabled
ghc -Wall -O0 -outputdir build -o build\lab1.exe Main.hs *>&1 | Tee-Object "$out\01_compile.txt"

# Full run: automatic checks + interactive part (name and sentence piped in)
"Sergej`nfunctional   programming is    really fun" | .\build\lab1.exe | Tee-Object "$out\02_program_output.txt"
"exit code: $LASTEXITCODE" | Tee-Object -Append "$out\02_program_output.txt"

# Task 4: asking the interpreter for types (:t)
ghc Main.hs -e ':t tail' -e ':t sqrt' -e ':t pi' -e ':t exp' -e ':t (^)' -e ':t (/=)' -e ':t numbOfSol' |
    Tee-Object "$out\03_task4_types.txt"
