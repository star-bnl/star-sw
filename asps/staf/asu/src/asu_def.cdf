**######################################################################
**######################################################################
**######################################################################
**:Copyright 1997, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        asu_def.cdf
**:DESCRIPTION: Command Definition File for ASU package.
**:<--------------------------------------------------------------------
**
>NAME ASU_DEF
**
************************************************************************
** ASU
>MENU ASU
>GUIDANCE
Analysis_Service_Utilities commands.
.
 #(@)$Id: asu_def.cdf,v 1.4 1998/01/24 19:03:21 ward Exp $
.
ASU is utility package which contains several useful functions and
user-interface commands.
.
ASU does not contain any component objects in the StAF sense. All user
interface commands under ASU can be considered to invoke conventional
C functions.
.
** ---------------------------------------------------------------------
** ASU/HELLO MESSAGE
>COMMAND HELLO
>PARAMETERS
MESSAGE 'Salutory message.' C D=' '
>GUIDANCE
Print a salutory message to stdout.
.
DESCRIPTION: 
.
HELLO is a useful command for testing that the KUIP interface is
working properly. It has no side effects and no interesting or
comlicated code.
.
ARGUMENTS: 
.
   MESSAGE - Salutory message.
   - Any printable character string message to be printed to stdout.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   HELLO
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Print a typical salutation.
.
   Kuip> ASU/HELLO Bill
   ASU:    Hello, Bill
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_ASU_HELLO
**
** ---------------------------------------------------------------------
** ASU/TIME
>COMMAND TIME
>PARAMETERS
>GUIDANCE
Show a time increment since t0.
.
DESCRIPTION: 
.
TIME prints to stdout the time in seconds since the first time ASU/TIME
was called in this process.
.
TIME provides a convenient tool for timing execution within StAF.
.
More guidance needed here.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   TIME
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Time a WAIT statement.
.
   Kuip> ASU/TIME; WAIT ... 2; ASU/TIME
   ASU:    Time = 0.000000
    ...
   ASU:    Time = 1.994348
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_ASU_TIME
**
** ---------------------------------------------------------------------
** ASU/DATE
>COMMAND DATE
>PARAMETERS
>GUIDANCE
Print the current date and time to stdout.
.
DESCRIPTION: 
.
DATE prints to stdout a character string showing the current date and
time.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   DATE
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Print the current date and time.
.
   Kuip> ASU/DATE
   ASU:    Date = Tue Dec 16 10:28:25 1997
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_ASU_DATE
**
** ---------------------------------------------------------------------
** ASU/FFLUSH
>COMMAND FFLUSH
>PARAMETERS
>GUIDANCE
Flush the print buffers of all open streams.
.
DESCRIPTION: 
.
FFLUSH calls fflush(0);
.
This allows the stderr and stdout buffers to be explicitly flushed.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   FFLUSH
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. More guidance needed here.
.
   StAF> ASU/FFLUSH
.
EXCEPTIONS: 
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_ASU_FFLUSH
**
************************************************************************
** ASU/MALLOC
>MENU MALLOC
>GUIDANCE
Commands controlling "asuAlloc.h" functions.
.
Functions in "asuAlloc.h" are helper functions to detect and debug
memory leaks.
.
**
** ---------------------------------------------------------------------
** ASU/MALLOC/LEVEL [ NEW_VALUE ]
>COMMAND LEVEL
>PARAMETERS
+
NEW_VALUE 'Memory allocation debug level' I D=0
>GUIDANCE
Get or set the "asuAlloc.h" debug level.
.
DESCRIPTION: 
.
LEVEL is a read-writable parameter which determines the behavior of
functions in the asuAlloc.h package.
.
To get the current value of LEVEL, leaving LEVEL unchanged, do not
specify a new value in the optional argument NEW_VALUE.
.
To set a new value of LEVEL, specify the new value as the optional
argument NEW_VALUE.
.
ARGUMENTS: 
.
   NEW_VALUE - Memory allocation debug level.
   - 0            Print current level.
   - 1 = FAST     Only call normal malloc & free...
   - 2 = COUNT    ...and count calls to malloc & free...
   - 3 = TRACE    ...and keep trace of memory locations...
   - 4 = FILL     ...and fill allocated memory w/ pattern...
   - 5 = VERBOSE  ...and print a message every time.
   - DEFAULT: Show the current value of LEVEL, do not change it.
.
RETURN:
.
   The current value of LEVEL is pushed onto the STAF_RESULT stack
   (see SOC).
.
EXAMPLES: 
.
EG1. Show the current MALLOC debug level.
   Kuip> ASU/MALLOC/LEVEL
   ASU/MALLOC/LEVEL = ASU_MALLOC_FAST
.
EG2. Set the MALLOC debug level to COUNT and show current level.
.
   Kuip> ASU/MALLOC/LEVEL 2
   Kuip> ASU/MALLOC/LEVEL
   ASU/MALLOC/LEVEL = ASU_MALLOC_COUNT
.
EXCEPTIONS: 
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the asuMalloc interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION KAM_ASUMALLOC_LEVEL
**
** ---------------------------------------------------------------------
** ASU/MALLOC/STATS
>COMMAND STATS
>PARAMETERS
>GUIDANCE
Print memory allocation statistics.
.
DESCRIPTION: 
.
ASU/MALLOC/STATS prints to stdout the current statistics accrued by the
"asuAlloc.h" function.
.
ARGUMENTS: 
.
   None.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   STATS
   function is pushed on the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
EG1. Print memory allocation statistics before any work.
.
   Kuip> ASU/MALLOC/STATS

   ASU_MALLOC: Memory allocation statistics:
           mallocCalls 0, freeCalls 0, diff 0
           mallocSize 0, freeSize 0, diff 0
           asuMallocSize 0
.
   OBJECT_NOT_FOUND - No object specified by SOREF can be found which
      implements the asuMalloc interface.
      (See SOC/BIND to dynamically bind the proper resources, or
      rebuild executable with the proper resources statically linked.)
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   ASU/MALLOC/LEVEL
.
>ACTION KAM_ASUMALLOC_STATS
**
