# Microsoft Developer Studio Project File - Name="lib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=lib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "lib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "lib.mak" CFG="lib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "lib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "lib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "lib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "lib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I ".." /I "..\..\inc" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"dsl.lib"

!ENDIF 

# Begin Target

# Name "lib - Win32 Release"
# Name "lib - Win32 Debug"
# Begin Source File

SOURCE=..\..\src\dscheck.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsdata.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dserror.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsjoin.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dslib.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dslock.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsmem.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsprint.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsqsort.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dstable.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dstid.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dstodo.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dstype.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsutil.c
# End Source File
# Begin Source File

SOURCE=..\..\src\dsxdrlib.c
# End Source File
# Begin Source File

SOURCE=..\..\src\sample.c
# End Source File
# Begin Source File

SOURCE=..\..\src\tcplib.c
# End Source File
# Begin Source File

SOURCE=..\..\src\testjoin.c
# End Source File
# Begin Source File

SOURCE=..\..\src\testlib.c
# End Source File
# Begin Source File

SOURCE=..\..\src\testsort.c
# End Source File
# Begin Source File

SOURCE=..\..\src\testtas.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_array.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_float.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_mem.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_rec.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_refer.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_sizeof.c
# End Source File
# Begin Source File

SOURCE=..\rpc\xdr_stdio.c
# End Source File
# Begin Source File

SOURCE=..\..\src\xdrtape.c
# End Source File
# End Target
# End Project
