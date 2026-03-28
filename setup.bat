@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

if /I "%~1"=="-h" goto :usage
if /I "%~1"=="--help" goto :usage
if not "%~1"=="" (
  echo warning: unknown option: %~1 1>&2
  goto :usage_error
)

call :link_dir_to_home "%SCRIPT_DIR%\bash"
call :link_dir_to_home "%SCRIPT_DIR%\zsh"
call :link_dir_to_home "%SCRIPT_DIR%\vim"
call :link_dir_to_home "%SCRIPT_DIR%\tmux"
call :link_dir_to_home "%SCRIPT_DIR%\lisp"
call :link_dir_to_home "%SCRIPT_DIR%\readline"

call :link_path "%SCRIPT_DIR%\vimfiles" "%USERPROFILE%\.vim"
call :link_path "%SCRIPT_DIR%\emacs" "%USERPROFILE%\.emacs.d"
call :link_path "%SCRIPT_DIR%\lem" "%USERPROFILE%\.lem"

exit /b 0

:usage
echo Usage: setup.bat
echo.
echo Installs the default Windows-compatible dotfiles.
echo The --x option from setup.sh is not supported here.
exit /b 0

:usage_error
echo Usage: setup.bat 1>&2
exit /b 1

:link_dir_to_home
set "SRC_DIR=%~1"
if not exist "%SRC_DIR%" exit /b 0

for /F "delims=" %%F in ('dir /b /a-d "%SRC_DIR%"') do (
  call :link_path "%SRC_DIR%\%%F" "%USERPROFILE%\%%F"
)
exit /b 0

:link_path
set "SRC=%~1"
set "DST=%~2"

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference='Stop';" ^
  "$src=[System.IO.Path]::GetFullPath($env:SRC);" ^
  "$dst=[System.IO.Path]::GetFullPath($env:DST);" ^
  "$isDir=Test-Path -LiteralPath $src -PathType Container;" ^
  "if (Test-Path -LiteralPath $dst) {" ^
  "  $item=Get-Item -LiteralPath $dst -Force;" ^
  "  if ($item.Attributes -band [System.IO.FileAttributes]::ReparsePoint) { Remove-Item -LiteralPath $dst -Force; }" ^
  "  else { Write-Host ('warning: skip existing path: ' + $dst); exit 10; }" ^
  "}" ^
  "$parent=Split-Path -Parent $dst;" ^
  "if ($parent) { New-Item -ItemType Directory -Path $parent -Force | Out-Null; }" ^
  "$itemType = if ($isDir) { 'Junction' } else { 'SymbolicLink' };" ^
  "New-Item -ItemType $itemType -Path $dst -Target $src | Out-Null"

if errorlevel 10 exit /b 0
if errorlevel 1 exit /b 1
exit /b 0
