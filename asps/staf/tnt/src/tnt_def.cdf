**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tnt_def.cdf
**:DESCRIPTION: Tables to NTuples Command Definition File 
**:HISTORY:     18jul96-v000b-cet- Remove unused commands
**:HISTORY:     30may96-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME TNT_DEF

************************************************************************
************************************************************************
** TNT
>MENU TNT
>GUIDANCE
CWNtuples to NTuples commands.
.
VERSION v0.00a (30apr96).
.
Commands for the Tables to NTuples ASP.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** ** TNT/COUNT
** >COMMAND COUNT
** >PARAMETERS
** >GUIDANCE
** Show count of known TNT objects.
** .
** >ACTION KAM_TNT_COUNT
** 
** ---------------------------------------------------------------------
** TNT/LIST
>COMMAND LIST
>PARAMETERS
>GUIDANCE
List all known TNT objects.
.
>ACTION KAM_TNT_LIST
** 
** ---------------------------------------------------------------------
** ** TNT/PAW
** >COMMAND PAW
** >PARAMETERS
** >GUIDANCE
** Start PAW.
** .
** >ACTION KAM_TNT_PAW
** 
** ---------------------------------------------------------------------
** ** TNT/SHARE
** >COMMAND SHARE
** >PARAMETERS
** >GUIDANCE
** Share TNT objects.
** .
** >ACTION KAM_TNT_SHARE
** 
** ---------------------------------------------------------------------
** TNT/NEWCWNTUPLE HID TABLE
>COMMAND NEWCWNTUPLE
>PARAMETERS
HID 'tntCWNtuple HBOOK ID number.' I
TABLE 'tdmTable name.' C
>GUIDANCE
Create a new tntCWNtuple from a tdmTable.
.
>ACTION KAM_TNT_NEWCWNTUPLE

************************************************************************
************************************************************************
** ** TNT/CWNTUPLE
>MENU CWNTUPLE
>GUIDANCE
tntCWNtuple commands.
.
************************************************************************
************************************************************************
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/HID NTUPLE
>COMMAND HID
>PARAMETERS
NTUPLE 'tntCWNtuple name.' C
>GUIDANCE
Show the HBOOK ID number of a tntCWNtuple Object.
.
>ACTION KAM_TNTCWNTUPLE_HID
**
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/TITLE NTUPLE
>COMMAND TITLE
>PARAMETERS
NTUPLE 'tntCWNtuple name.' C
>GUIDANCE
Show the HBOOK title of a tntCWNtuple Object.
.
>ACTION KAM_TNTCWNTUPLE_TITLE
**
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/ENTRYCOUNT NTUPLE
>COMMAND ENTRYCOUNT
>PARAMETERS
NTUPLE 'tntCWNtuple name.' C
>GUIDANCE
Show the number of entries filled.
.
>ACTION KAM_TNTCWNTUPLE_ENTRYCOUNT
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/COLUMNCOUNT NTUPLE
>COMMAND COLUMNCOUNT
>PARAMETERS
NTUPLE 'tntCWNtuple name.' C
>GUIDANCE
Show the number of columns.
.
>ACTION KAM_TNTCWNTUPLE_COLCOUNT
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/ZEBRADIR NTUPLE
** >COMMAND ZEBRADIR
** >PARAMETERS
** NTUPLE 'tntCWNtuple name.' C
** >GUIDANCE
** Show the ZEBRA directory.
** .
** >ACTION KAM_TNTCWNTUPLE_ZEBRADIR
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/APPEND NTUPLE TABLE
>COMMAND APPEND
>PARAMETERS
NTUPLE 'tntCWNtuple name.' I
TABLE 'tdmTable name.' C
>GUIDANCE
Get data from TABLE and append to existing NTUPLE.
.
>ACTION KAM_TNTCWNTUPLE_APPEND
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/IMPORT NTUPLE TABLE
>COMMAND IMPORT
>PARAMETERS
NTUPLE 'tntCWNtuple name.' I
TABLE 'tdmTable name.' C
>GUIDANCE
Get data from TABLE and put it in existing NTUPLE.
.
2>ACTION KAM_TNTCWNTUPLE_IMPORT
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/CLEAR NTUPLE
>COMMAND CLEAR
>PARAMETERS
NTUPLE 'tntCWNtuple name.' I
>GUIDANCE
Reset NTUPLE.
.
>ACTION KAM_TNTCWNTUPLE_CLEAR
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/PUTTABLE NTUPLE TABLE
** >COMMAND PUTTABLE
** >PARAMETERS
** NTUPLE 'tntCWNtuple name.' C
** TABLE 'tdmTable name.' C
** >GUIDANCE
** Put data from NTUPLE into TABLE.
** .
** >ACTION KAM_TNTCWNTUPLE_PUTTABLE
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/SHOW TNTCWNTUPLE [ OPTION ]
** >COMMAND SHOW
** >PARAMETERS
** TNTCWNTUPLE 'tntCWNtuple name.' C
** +
** OPTION 'Options.' C D=' ' R=' '
** >GUIDANCE
** Show the definition of a tntCWNtuple.
** .
** Possible OPTION values are:
**    ' '    Show everything.
** .
** >ACTION KAM_TNTCWNTUPLE_SHOW
** 
** ---------------------------------------------------------------------
** ** TNT/CWNTUPLE/PRINT TNTCWNTUPLE [ NROWS IFIRST ]
** >COMMAND PRINT
** >PARAMETERS
** TNTCWNTUPLE 'tntCWNtuple name.' C
** +
** NROWS 'Number of rows.' I D=10
** IFIRST 'First row.' I D=0
** >GUIDANCE
** Print the contents of a tntCWNtuple.
** .
** >ACTION KAM_TNTCWNTUPLE_PRINT
** 
