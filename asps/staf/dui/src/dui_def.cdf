**:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        dui_def.cdf
**:DESCRIPTION: Dataset Unix-like Interface Command Definition File 
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     08dec95-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME DUI_DEF

************************************************************************
************************************************************************
** DUI
>MENU DUI
>GUIDANCE
Dataset Unix-like Interface commands.
.
VERSION v1.00a (30apr96).
.
Commands for the Dataset Unix-like Interface ASP.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** DUI/DU [ PATH ] [ MINSIZE ]
>COMMAND DU
>PARAMETERS
+
PATH 'Directory Path' C D='/dui'
MINSIZE 'Minimum size of reported tables' I D=0
>GUIDANCE
Shows size (bytes) in memory of all tables under the node
defined by the PATH parameter.   PATH can be either
a directory or a table.  The size of a table is 
row size times number of rows.
.
If you are only interested in large tables, use MINSIZE parameter.
For example, if you set MINSIZE to 1000000, only tables using
more than one megabyte of memory are reported.
.
See also DUI/DF, which reports both table and non-table memory.
.
>ACTION KAM_DUI_DU
** ---------------------------------------------------------------------
** DUI/DF
>COMMAND DF
>PARAMETERS
>GUIDANCE
See also DUI/DU, which gives a table-by-table report.
.
Shows total current memory allocated to the STAF process.
This includes (1) non-table memory [eg, from malloc()],
(2) table memory, (3) memory overhead.  
.
Because of item 3, the numbers reported by this command will 
be a little larger than you might expect.  In particular,
a given table's effect will be larger on this command than on DUI/DU.
.
>ACTION KAM_DUI_DF
** ---------------------------------------------------------------------
** DUI/CD [ PATH ]
>COMMAND CD
>PARAMETERS
+
PATH 'Directory Path' C D='/dui'
>GUIDANCE
Change current working dataset.
.
>ACTION KAM_DUI_CD

** ---------------------------------------------------------------------
** DUI/LN SOURCE TARGET
>COMMAND LN
>PARAMETERS
SOURCE 'Link from source ...' C
TARGET '... to target' C
>GUIDANCE
Links tables and/or datasets, similar to a UNIX soft link.
.
You can use either a directory or a table for the target.
You can use paths.  The paths can be absolute or relative.
.
>ACTION KAM_DUI_LN

** ---------------------------------------------------------------------
** DUI/CP SOURCE TARGET
>COMMAND CP
>PARAMETERS
SOURCE 'Copy from source ...' C
TARGET '... to target' C
>GUIDANCE
Copy tables.
.
The target can be either a directory or a table, just like UNIX.
Caution: not all of the STAF UNIX-like commands are like UNIX.
.
>ACTION KAM_DUI_CP

** ---------------------------------------------------------------------
** DUI/LS [ PATH ]
>COMMAND LS
>PARAMETERS
+
PATH 'Directory Path' C D='.'
>GUIDANCE
List tables and/or datasets.
.
>ACTION KAM_DUI_LS

** ---------------------------------------------------------------------
** DUI/MKDIR PATH
>COMMAND MKDIR
>PARAMETERS
PATH 'Directory Path' C
>GUIDANCE
Create a new, empty dataset.
.
>ACTION KAM_DUI_MKDIR

** ---------------------------------------------------------------------
** DUI/MV SOURCE TARGET
>COMMAND MV
>PARAMETERS
SOURCE 'Move from SOURCE ...' C
TARGET '... to TARGET' C
>GUIDANCE
Move tables and/or datasets.
.
The target MUST BE A DIRECTORY.
.
>ACTION KAM_DUI_MV

** ---------------------------------------------------------------------
** DUI/PWD
>COMMAND PWD
>PARAMETERS
>GUIDANCE
Show current working directory.
.
>ACTION KAM_DUI_PWD

** ---------------------------------------------------------------------
** DUI/RM PATH
>COMMAND RM
>PARAMETERS
PATH 'Table Path' C
>GUIDANCE
Remove a table.
.
You can use a path to the table, eg. rm a/b/c, where a and b are
'directories' (datasets).
.
>ACTION KAM_DUI_RM

** ---------------------------------------------------------------------
** DUI/RMDIR PATH
>COMMAND RMDIR
>PARAMETERS
PATH 'Directory Path' C
>GUIDANCE
Remove datasets.
.
Caution: this is like 'rm -rf' in UNIX,
it removes the directory and all directories and tables thereunder.
.
>ACTION KAM_DUI_RMDIR

