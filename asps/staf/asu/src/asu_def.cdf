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
 #(@)$Id: asu_def.cdf,v 1.7 1998/08/23 18:27:41 perev Exp $
.
ASU is utility package which contains several useful functions and
user-interface commands.
.
ASU does not contain any component objects in the StAF sense. All user
interface commands under ASU can be considered to invoke conventional
C functions.
.
** ---------------------------------------------------------------------
** ---------------------------------------------------------------------
** ASU/TIME
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
>ACTION kam_asu_hello_%C
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
>ACTION kam_asu_time_%C
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
>ACTION kam_asu_date_%C
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
>ACTION kam_asu_fflush_%C
**
>MENU EML
** ASU/EML/PRETTY_FORMATTING [TRUE/FALSE/SHOW]
>COMMAND PRETTY_FORMATTING
>PARAMETERS
+
PRETTY 'Either TRUE, FALSE, or SHOW.' C D='SHOW'
>GUIDANCE
SHOW reports the current value.
The other possible values are self-explanatory.
.
DESCRIPTION: 
.
You can type ON/OFF instead of TRUE/FALSE.
.
Prettification formats error messages for readability.
It does not remove any information.
.
Pretty error messages are ON when Staf starts.
If you suspect a bug in the prettification mechanism, you
can try turning it off.
.
ARGUMENTS: 
.
   PRETTY - Either ON, OFF, or SHOW.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   asu_pretty
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
   Kuip> ASU/EML/PRETTY_FORMATTING
   ASU:    Error messaging prettification is ON.
   Kuip> ASU/EML/PRETTY_FORMATTING OFF
   Kuip> ASU/EML/PRETTY_FORMATTING 
   ASU:    Error messaging prettification is OFF.
.
EXCEPTIONS: 
.
BUGS: 
.
   None.
.
SEE ALSO: 
.
>ACTION kam_asu_pretty_%C
**
** ASU/EML/BEEP_ON_ERROR [TRUE/FALSE/SHOW]
>COMMAND BEEP_ON_ERROR
>PARAMETERS
+
BEEP 'Either TRUE, FALSE, or SHOW.' C D='SHOW'
>GUIDANCE
You can type ON/OFF instead of TRUE/FALSE.
.
SHOW reports the current value.
The other possible values are self-explanatory.
.
DESCRIPTION: 
.
Self-explanatory.
.
Beeping is ON by default when Staf starts.
.
ARGUMENTS: 
.
   BEEP - Either ON, OFF, or SHOW.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   asu_beep
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
   Kuip> ASU/EML/BEEP_ON_ERROR ON
.
EXCEPTIONS: 
.
BUGS: 
.
   None.
.
SEE ALSO: 
.
>ACTION kam_asu_beep_%C
**
** ASU/EML/DEMAND_ERROR_ACKNOWLEDGEMENT [TRUE/FALSE/SHOW]
>COMMAND DEMAND_ERROR_ACKNOWLEDGEMENT
>PARAMETERS
+
DEMAND 'Either TRUE, FALSE, or SHOW.' C D='SHOW'
>GUIDANCE
You can type ON/OFF instead of TRUE/FALSE.
.
SHOW reports the current value.
The other possible values are self-explanatory.
.
DESCRIPTION: 
.
The idea is to keep error messages from scrolling off
the top of your screen unnoticed.
.
However, if you have many benign errors, you will probably
want to turn DEMAND_ERROR_ACKNOWLEDGEMENT off.
.
This feature is OFF by default when Staf starts, for
backward compatibility.
.
ARGUMENTS: 
.
   DEMAND - Either ON, OFF, or SHOW.
.
RETURN:
.
   Success (STAFCV_OK) or failure (STAFCV_BAD) of the 
   asu_demand_ack
   function is pushed onto the STAF_STATUS stack (see SOC).
.
EXAMPLES: 
.
   Kuip> ASU/EML/DEMAND_ERROR_ACKNOWLEDGEMENT OFF
.
EXCEPTIONS: 
.
BUGS: 
.
   None.
.
SEE ALSO: 
.
>ACTION kam_asu_demand_ack_%C
**
************************************************************************
** ASU/MALLOC
>MENU \MALLOC
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
NEW_VALUE 'Memory allocation debug level' I D=-1 
>GUIDANCE
Get or set the asuAlloc debug level.
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
   - 0      	Memory occupancy.
   - 1          Full print of all objects
   - DEFAULT: Show the current value of LEVEL, do not change it.
.
RETURN:
.
.
EXAMPLES: 
.
EG1. Show the current MALLOC debug level.
   Kuip> ASU/MALLOC/LEVEL
   ASU/MALLOC/LEVEL = 1
.
EG2. Set the MALLOC debug level to COUNT and show current level.
.
   Kuip> ASU/MALLOC/LEVEL 1
.
EXCEPTIONS: 
.
None
.
BUGS: 
.
   None known.
.
SEE ALSO: 
.
>ACTION kam_asumalloc_level_%C
**
** ---------------------------------------------------------------------
** ASU/MALLOC/STATS
>COMMAND STATS
>PARAMETERS
+
NEW_VALUE 'Memory allocation debug level' I D=-1 
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

ASU_MALLOC: Memory allocation for Event:0 
 Size      0.066M  Blocks      54  Allocs      58  Free          4  Errors 0
.
BUGS: 
.
   None known.
.
SEE ALSO: 
   ASU/MALLOC/LEVEL
.
>ACTION kam_asumalloc_stats_%C
**
