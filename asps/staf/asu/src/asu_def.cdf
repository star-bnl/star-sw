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

** ---------------------------------------------------------------------
** ASU/ALLOCSTATS
>COMMAND ALLOCSTATS
>PARAMETERS
>GUIDANCE
Prints allocation statistics.
.
>ACTION KAM_ASUALLOCSTATS

