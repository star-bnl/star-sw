@echo off
if not "%1"=="-v" goto skip
shift
@echo on
:skip

echo 0 = %0 > %tmp%\make.tmp
echo 1 = %1 >> %tmp%\make.tmp

rem Copyright (c) 1995-1997 Valery Fine (fine@vxcern.cern.ch)
rem CERNLIB command for Windows NT operating system
rem                              03/10/97 JINR/Dubna


rem Check CERN environment variables setting

if NOT "%CERN_LIBDIR%".=="". goto envOk
rem Set default env variables first
rem call CernEnv.bat

:envOk

rem
rem Clear the previous installations
rem 

if exist cernlib. del cernlib.
set CERNLIB=

if %1. == ?. goto help
if %1. == . goto tail

:next
echo %1.lib >>%tmp%\cernlib
    set CERNLIB=%CERNLIB% %1.lib
rem a little smarter nowadays
if %1. == paw_. goto paw
if %1. == cd_lib. goto cdlib
if %1. == cmz. goto cmz
if %1. == dzdoc. goto dzdoc
if %1. == graflib. goto graf
if %1. == grafX11. goto grafg
if %1. == geant321. goto geant321
goto shift

:cdlib
:cmz
:dzdoc
:geant321
:paw
    echo pawlib.lib >>%tmp%\cernlib
    echo graflib.lib  >>%tmp%\cernlib
    set CERNLIB=%CERNLIB% pawlib.lib graflib.lib
:graf
    echo grafX11.lib >>%tmp%\cernlib
    echo ix_higz.lib >>%tmp%\cernlib
    set CERNLIB=%CERNLIB% grafX11.lib ix_higz.lib
:grafg
rem     echo packlib.lib  >>%tmp%\cernlib
:shift
shift
if %1. == . goto tail
if %1. == mathlib. goto next
rem echo.
rem echo  WARNING: You may point only one CERNLIB library name
rem echo  All related libraries will be attached by automatic.
rem echo.
goto next

:tail
rem standard tail for all kind if CERNLIB's
echo packlib.lib  >>%tmp%\cernlib
set CERNLIB=%CERNLIB% packlib.lib
rem echo %lib%\packlib.lib

rem MS Visual C/C++ run-time libraries
echo user32.lib >> %tmp%\cernlib
echo kernel32.lib >> %tmp%\cernlib
echo advapi32.lib >> %tmp%\cernlib
echo oldnames.lib >> %tmp%\cernlib
echo wsock32.lib >> %tmp%\cernlib
set CERNLIB=%CERNLIB% wsock32.lib user32.lib kernel32.lib oldnames.lib 
echo %CERNLIB%
rem echo.
goto end

:help
echo +-----------------------------------------------------------------------+
echo ! cernib.bat generates an 'indirect' file with 'cernlib' name           !
echo ! in the currect directory and environment variable CERNLIB.            !
echo !                                                                       !
echo ! Attention: The name of this file is just 'cernlib' with NO extension  !
echo ! ---------                                                             !
echo ! This file contains the list of the CERN and system libraries to       !
echo ! build the CERNLIB-based application for Windows NT and Windows 95     !
echo ! properly                                                              !
echo !                                                                       !
echo ! Been created 'cernlib' may be pointed on the command line to build    !
echo ! your code.                                                            !
echo !                                                                       !
echo ! So to make you own CERNLIB-based applicaiton go as follows:           !
echo ! 1. Set Microsoft environment variables. (See FPSVARS.BAT in your      !
echo !                                          MSDEV\BIN directory )        !
echo ! 2. Set the LIB environment variable:                                  !
echo !    2.1. Via SET DOS command:  SET LIB = "path_to CERNLIB directory";^%LIB^% 
echo !    2.2. or Edit FPSVARS.BAT file                                      !
echo !    2.3. or Via Control Panel  (For Windows NT)                        !
echo !    2.4. or Via Autoexec.bat file                                      !
echo ! 
pause                                                                        ! 
echo ! 3. Type                                                               !
echo !          cernlib {list of libraries}                                  !
echo !    {list of_libraires}  may contain:                                  !
echo !     3.1. Nothing to use just packlib only or list of the "name" via blank!
echo !     3.2. name of the CERNLIB library as seen in the directory         !
echo !          (extension MUST be omited)                                   !
echo !     3.3 'geant321' to joint all libraries   geant 3.21 based          !
echo !          simulation needs                                             !
echo !     3.4 'graf' to allow packlib+graflib+grafX11 "unix" equivalent     !
echo !                                                                       !
echo ! 4. fl32  your_program_name.f @cernlib        - to build exe file      !
echo ! 5. your_program_name                         - to run your task up    !
echo !                                                                       !
echo ! 6. Complain  heplib.support@cern.ch by E-mail                         !
echo !                                                                       !
echo +-----------------------------------------------------------------------+
:end

