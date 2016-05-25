@echo off

chdir %HOME%\.emacs.d

set arg1=%1

if not "%arg1%"=="" (
   if "%arg1"=="profile" (
      goto :profile
   ) else if "%arg1%"=="debug" (
      goto :debug
   ) else if "%arg1%"=="up" (
      goto :up
   ) else if "%arg1%"=="min" (
      goto :min
   )
)
runemacs %*
goto :eof

:profile
runemacs -- --ig-profile=profiler
goto :eof

:debug
runemacs --debug-init
goto :eof

:up
rem git pull
rem emacs -batch -Q -l init.el -f ig-update-all-autoloads -- --ig-profile-minimal
emacs -batch -Q -l init.el -- --ig-profile=update
goto :eof

:min
runemacs -- --ig-profile=minimal
goto :eof

:eof
