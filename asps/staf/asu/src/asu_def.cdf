**:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        asu_def.cdf
**:DESCRIPTION: Dataset Unix-like Interface Command Definition File 
**:HISTORY:	30apr96-v100a-cet- Beta Release Version
**:HISTORY:     12feb96-v000a-cet- Creation
**:<--------------------------------------------------------------------

>NAME ASU_DEF

************************************************************************
************************************************************************
** ASU
>MENU ASU
>GUIDANCE
Analysis Service Utilities commands.
.
VERSION v1.00a (30apr96).
.
Commands for the Analysis Service Utilities ASP.
.
************************************************************************
************************************************************************

** ---------------------------------------------------------------------
** ASU/HELLO MESSAGE
>COMMAND HELLO
>PARAMETERS
MESSAGE 'Hello world message.' C
>GUIDANCE
Prints a "Hello World"-like message.
.
>ACTION KAM_ASU_HELLO

** ---------------------------------------------------------------------
** ASU/TIME
>COMMAND TIME
>PARAMETERS
>GUIDANCE
Prints the time.
.
>ACTION KAM_ASU_TIME

************************************************************************
************************************************************************
** ASU/MALLOC
>MENU MALLOC
>GUIDANCE
Commands controlling "asuAlloc.h" functions.
.
Functions in "asuAlloc.h" are helper functions to detect and debug
memory leaks.
.

** ---------------------------------------------------------------------
** ASU/MALLOC/STATS
>COMMAND STATS
>PARAMETERS
>GUIDANCE
Prints memory allocation statistics.
.
>ACTION KAM_ASUALLOCSTATS

** ---------------------------------------------------------------------
** ASU/MALLOC/LEVEL
>COMMAND LEVEL
>PARAMETERS
+
LEVEL 'Memory allocation debug level' I D=0 R=0,1,2,3,4,5
>GUIDANCE
Changes allocation debug level.
.
        0            Print current level.
        1 = FAST     Only call normal malloc & free...
        2 = COUNT    ...and count calls to malloc & free...
        3 = TRACE    ...and keep trace of memory locations...
        4 = FILL     ...and fill allocated memory w/ pattern...
        5 = VERBOSE  ...and print a message every time.
.
>ACTION KAM_ASUALLOCLEVEL

