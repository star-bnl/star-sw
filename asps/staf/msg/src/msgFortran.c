/*	This is the STAR message/error handling package (prefix Msg_).

	msgPublic.c

	========================+++=========================
	==  msg Fortran Interface Routines in this file.  ==
	========================+++=========================

	Converted to C, with shared memory:
	started  9-Oct-1996   Robert Hackenburg
	"done"  22-Oct-1996   Robert Hackenburg
	Classes:
	added   26-Jan-1994   Robert Hackenburg
	Full on/off capability:
	added    8-May-1992   Robert Hackenburg
	Modified 4-Feb-1992   Robert Hackenburg
	Created  1-Nov-1991   Robert Hackenburg


                              ***
                             ** **
                            **   **
                    *********     *********
                      ****           ****
                        ***         ***
                        **     *     **
                       **   *******    **
                      **  ***     ***  **
                     **                 **

*/

static const char sccsid[] = "@(#)"__FILE__"\t\t1.55\tCreated 3/8/98 03:34:06, \tcompiled "__DATE__" "__TIME__;

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>

#include <sys/ipc.h>  /*  Interprocess Communications  -- needed for Shared Memory.  */
#include <sys/shm.h>  /*  Shared Memory.  */

#include <errno.h>
#include <msg.h>
#include <msgData.h>
#include <msgf.h>

#ifdef sgi
#ifndef IRIX
#define IRIX TRUE
/*#error (This is not an error!)   Compiling maplun version (sgi only). */
#endif
#endif

#define MAX_FILE_VERSIONS 999

extern msgData_t msg;
extern FILE *JournalFILE;    /* Journal-file descriptor                          */

extern control_t *control;
extern prefix_t  *prefix;

extern int CPUtime0;
extern int ELAtime0;

extern funcPoint MsgAlarmRoutine;


FILE* MsgMapLUN( int *LUN, const char *type );
#ifdef UNDEF
int maplun_( int *LUN );
#endif


static char f1000[1000];  /*  Some "scratch" Fortran-string conversion space. */


	void	message_( const char *msg, int *Lines, int *ID, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:

	SUBROUTINE			Message( msg, Lines, ID )

*   Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER       Lines  !Number of lines in MSG.

*   Input/Output:
	INTEGER ID     !Fast-reference message ID.  Set to zero by caller 
                       !before first call, set by Message on first call by
                       !looking up or entering the prefix contained in msg
                       !(prefix is everything before the first space) in
	               !the index of MSG message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.
*   Description:  Display and log a message.
	Conditionally display a message MSG, containing Lines lines
	on the terminal and the journal, if enabled by a
	call to MSG_Journal_On.  The message is displayed unless disabled.
	The message is counted unless counting is disabled.
*/
{
	int Active, Counting, Alarming;
	int LID;   /* Local copy of ID. */
	int L, N;
	int i, j;
	int NeedCopy;

	LID = *ID;

	if (LID<=0) {
	  NeedCopy = TRUE;
	} else if ( prefix[LID].Active) {
	  NeedCopy = TRUE;
	} else if (!prefix[LID].Counting) {
	  NeedCopy = FALSE;
	} else if ( prefix[LID].Counts <= 0 ) {  /* This happens for counting class prefixes, if MsgMark defined prefix LID.   */
	  NeedCopy = TRUE;
	} else {
	  NeedCopy = FALSE;
	}
	if ( NeedCopy ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	Check or set the ID & flags.
	Enter msg's prefix in the index if needed.  */
	MsgCheck( f1000, &LID, &Active, &Alarming, &Counting );

/*	Counting is true only if found:  */
	if ( Counting ) MsgIncr( LID );

	if ( Active ) {           /* Display it:  */
	  for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	    N = msglen;
	    if ( N > 999 ) N = 999;
	    strncpy( f1000, &msg[j], N );
	    L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	    MessageOut( f1000 );
	  }
	  MsgAbortCheck( LID );   /* Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {         /* Alarm it:  */
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	  MsgAlarm( f1000, prefix[LID].AlarmLevel );
	}

	if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */

	return;
}




	void	message_out_( const char *msg, int *Lines, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			Message_Out( msg, Lines )

*   Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER        Lines !Number of lines in MSG.

*   Description:  Display and log a message, bypassing msg accounting.
	Always display a message msg, containing Lines lines
	on the terminal and the journal, if
	enabled by a call to Msg_Journal_On.  Displaying is unnaffected
	by calls to the Msg_Disable routine, and the message is not
	"counted".  This is the "pure" I/O part of Message.
*/
{
	int L, N;
	int i, j;

	for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, &msg[j], N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

/*	  Send message on standard output: */
	  fprintf(stderr, "%s\n", f1000 );
	  fflush( stderr );

/*	  Display time & message on journal file:  */
	  MsgToFileOut( f1000, JournalFILE );

	}
	return;
}




	void	msgalarm_( const char *msg, int  *severity, int   msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MsgAlarm( msg, severity )

*   Inputs:
	CHARACTER*(*)*msg   ! Character-string message, with prefix, submitted for display.
	INTEGER  severity   ! Severity level of alarm -- from msg class.

*  Description:  Fortran callable msg interface to application-specified alarm routine.
*/
{
	char Prefix[PREFIX_MAXLEN+1];
	char *Message;

/*	Do this only if an alarm routine has been declared:  */
	if ( MsgAlarmRoutine != NULL ) {
	  strncpy( f1000, msg, msglen );
	  f1000[msglen] = 0;
	  MsgParse( f1000, Prefix, &Message );
	  MsgAlarmRoutine( Prefix, Message, severity );
	}
	return;
}




	void	msgalarmregister_( funcPoint AlarmRoutine )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MsgAlarmRegister( AlarmRoutine )

*   Input:
	EXTERNAL AlarmRoutine ! Application-specified Alarm routine to use by msg.
* Description:   Register an alarm-routine to be called by active-alarm messages.  For Fortran.
	         Note that if "alarmroutine" is defined as a c-procedure, its name as defined
	         must contain the suffix "_" and must be in lower-case, in order to be used
	         with this call.  If it does not contain the suffix "_" or is not all in lower
	         case, the call must be made from a c-procedure, in which case a call to
	         MsgAlarmRegister is better than msgalarmregister_ .
*/
{
	MsgAlarmRoutine = AlarmRoutine;
	return;
}




	void	msg_class_define_( const char* Class, const char* State, int* CountLimit, int* AbortLimit
	                         , int ClassLen     , int StateLen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Class_Define( Class, State, Count_Limit, Abort_Limit )

*  Inputs:
	CHARACTER*(*) Class       !A STAR-standard message class.
	CHARACTER*(*) State       !'Active', 'Count' or 'Inactive'.
	INTEGER       Count_Limit !Default count-limit for this class.
	INTEGER       Abort_Limit !Default count-limit for this class.

*  Description:  Define a new MSG class (of prefixes).
	Each msg prefix is assigned to a class according to this scheme:

	     * If the prefix contains a "-", its class is the first character
	       after the "-", if non-blank.

	     * If the prefix ends with a "-" or has no "-", it is assigned
	       to the null class.

	The msg class to which a prefix is assigned is used (soley) for the
	purpose of setting the prefix's state and limits (count and abort).
	Classes permit the assignment of a variety of default msg states
	and counts for diffent groups of messages.  The predefined msg
	classes are these:

	Class Description            State    Count Limit    Abort Limit

	 ""   No-class message       Active       50            None
	  A   Abort message          Active      None        Abort on 1st (msg abort)
	  B   Bug message            Active      None           None
	  C   Count-only message   Counting      None           None
	  D   Debug message          Active      None           None (Often the 1st incarnation of a T class)
	  E   Error message          Active       20            None
	  F   Fatal message          Active      None           None (Application abort)
	  I   Informative message    Active      None           None
	  O   Once-only message      Active        1            None
	  T   Trace message        Inactive      None           None
	  W   Warning message        Active       10            None

	These can be changed or new classes added with a call to this routine.
*/
{
	int L, N;
	char Fclass[CLASS_MAXLEN+1];
	char Fstate[9];

	N = ClassLen;
	if ( N > CLASS_MAXLEN ) N = CLASS_MAXLEN;
	strncpy( Fclass, Class, N );
	L = MsgLNB( Fclass, N );   Fclass[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

	N = StateLen;
	if ( N > 8 ) N = 8;
	strncpy( Fstate, State, N );
	L = MsgLNB( Fstate, N );   Fstate[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

	MsgClassDefine( Fclass, Fstate, *CountLimit, *AbortLimit );
	return;
}




	void	msg_count_( const char *Prefix, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Count( Prefix )

*  Input:
	CHARACTER*(*) PREFIX !A message prefix.

*  Description:  Enable counting for a prefix.
*/
{
	int Active, Alarming, Counting;
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgCount( f1000 );
	return;
}




	void	msg_disable_( const char *Prefix, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Disable( Prefix )

*  Input:
	CHARACTER*(*) Prefix !A message prefix.

*  Description:  Disable message-display for a specified prefix.
	         Continue counting prefix occurances.
*/
{
	int Active, Alarming, Counting;
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgDisable( f1000 );
	return;
}




	void	msg_disablealarm_( const char *Prefix, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_DisableAlarm( Prefix )

*  Input:
	CHARACTER*(*) Prefix !A message prefix.

*  Description:  Disable message-alarming for a specified prefix.
*/
{
	int Active, Alarming, Counting;
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgDisableAlarm( f1000 );
	return;
}




	void	msg_display_( const char *msg, int *Lines, int *ID, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Display( msg, Lines, ID )

*   Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER Lines       !Number of lines in msg.

*   Input/Output:
	INTEGER ID     !Message ID.  Set to zero by caller 
                       !before first call, set by Message on first call by
                       !looking up or entering the prefix contained in msg
                       !(prefix is everything before the first space) in
	               !the index of message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Description:  Display a message on the terminal (only).
	Conditionally display & count a message msg, containing Lines lines,
	on the terminal.
*/
{
	int Active, Counting, Alarming;
	int LID;   /* Local copy of ID. */
	int L, N;
	int i, j;

	LID = *ID;

	if ( (LID<=0) || (prefix[LID].Active) ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	Check or set the ID & flags.
	Enter msg's prefix in the index if needed.  */
	MsgCheck( f1000, &LID, &Active, &Alarming, &Counting );

/*	Counting is true only if found:  */
	if ( Counting ) MsgIncr( LID );

	if ( Active ) {           /* Display it:  */
	  for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	    N = msglen;
	    if ( N > 999 ) N = 999;
	    strncpy( f1000, &msg[j], N );
	    L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	    MsgDisplayOut( f1000 );
	  }
	  MsgAbortCheck( LID );   /* Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {         /* Alarm it:  */
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	  MsgAlarm( f1000, prefix[LID].AlarmLevel );
	}

	if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */

	return;
}




	void	msg_display_out_( const char *msg, int *Lines, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Display_Out(MSG,LINES)

*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Outputs: none

*   Description:  Display a message on the terminal (only);  bypass accounting.
	Display a message msg, containing Lines lines, on the terminal.
*/
{
	int L, N;
	int i, j;

	for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, &msg[j], N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

/*	  Send message on standard output: */
	  fprintf(stderr, "%s\n", f1000 );
	  fflush( stderr );
	}
	return;
}




	void	msg_display_and_echo_( const char *msg, int *Lines, int *LUN, int *ID, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Display_and_Echo( msg, Lines, LUN, ID )

*   Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER Lines  !Number of lines in MSG.
	INTEGER LUN     !Fortran logical unit number for MSG to be echoed to.

*   Input/Output:
	INTEGER ID     !Message ID.  Set to zero by caller 
                       !before first call, set by Message on first call by
                       !looking up or entering the prefix contained in msg
                       !(prefix is everything before the first space) in
	               !the index of message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Description:  Display a message on the terminal and echo on a Fortran LUN.
	Conditionally display and count a message msg, containing Lines lines
	on the terminal, and echo the display on Fortran logical unit LUN.
*/
{
	int Active, Counting, Alarming;
	int LID;   /* Local copy of ID. */
	int L, N;
	int i, j;
	FILE *stream;

	LID = *ID;

	if ( (LID<=0) || (prefix[LID].Active) ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	Check or set the ID & flags.
	Enter msg's prefix in the index if needed.  */
	MsgCheck( f1000, &LID, &Active, &Alarming, &Counting );

/*	Counting is true only if found:  */
	if ( Counting ) MsgIncr( LID );

	if ( Active ) {           /* Display it:  */
	  if ( ( *LUN != 0 ) && ( *LUN != 6 ) ) {
	    stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	  }
	  for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	    N = msglen;
	    if ( N > 999 ) N = 999;
	    strncpy( f1000, &msg[j], N );
	    L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	    if ( *LUN == 6 ) { /* Standard Out */
	      MsgDisplayAndFileOut( f1000, NULL );
	    } else if ( *LUN == 0 ) { /* Journal File */
	      MsgDisplayAndFileOut( f1000, JournalFILE );
	    } else {
	      MsgDisplayAndFileOut( f1000, stream );
	    }
	  }
	  MsgAbortCheck( LID );   /* Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {         /* Alarm it:  */
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	  MsgAlarm( f1000, prefix[LID].AlarmLevel );
	}

	if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */

	return;
}




	void	msg_display_and_echo_out_( const char *msg, int *Lines, int *LUN, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Display_and_Echo_Out( msg, Lines, LUN )

*   Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER Lines   !Number of lines in msg.
	INTEGER LUN     !Fortran logical unit number for msg to be echoed to.

*   Description:  Display a message, echo on a Fortran LUN, and bypass accounting.
	Display a message msg, containing Lines lines
	on the terminal, and echo the display on
	Fortran logical unit LUN.
*/
{
	int L, N;
	int i, j;
	FILE *stream;

	if ( *LUN == 0 ) { /* Journal file  */
	  stream = JournalFILE;
	} else if ( *LUN == 6 ) { /* Standard out */
	  stream = NULL;
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	}

	for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, &msg[j], N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

/*	  Send message to standard output: */
	  fprintf(stderr, "%s\n", f1000 );
	  fflush( stderr );

	  MsgToFileOut( f1000, stream );  /*  If LUN is 6, this will cause lines to be doubled. */

	}

	return;
}




	void	msg_enable_( const char *Prefix, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Enable( Prefix )

*  Input:
	CHARACTER*(*) PREFIX !A message prefix.

*  Description:  Enable message-display and counting for a specified prefix.
*/
{
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgEnable( f1000 );
	return;
}




	void	msg_enablealarm_( const char *Prefix, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_EnableAlarm( Prefix )

*  Input:
	CHARACTER*(*) PREFIX !A message prefix.

*  Description:  Enable message-alarming for a specified prefix.
*/
{
	int Active, Alarming, Counting;
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgEnableAlarm( f1000 );
	return;
}




	int	msg_enabled_( const char *Prefix, int *ID, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	LOGICAL FUNCTION		MSG_Enabled( Prefix, ID )

*  Input:
	CHARACTER*(*) Prefix !A message prefix.

*  Input/Output:
	INTEGER ID     !Message ID.  Set to zero by caller 
                       !before first call, set on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Description:  Return enabled-status for a prefix;  define new prefix "enabled".
	Returns the message-display-enabled-status for the message
	with stored prefix Prefix, message-index ID.  If the message is not
	defined, it is defined and enabled, with counting turned on.

*  Return conditions:
	.TRUE. if the message is enabled (active),
	.FALSE. if the message is disabled (inactive).
*/
{
	int Active, Alarming, Counting;
	int L, N;

	if ( (*ID<=0) || (prefix[*ID].Active) ) {
	  N = length;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, Prefix, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}
	return( MsgEnabled( f1000, ID ) );
}




	int	msg_enabled_trace_( const char *Prefix, int *ID, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	LOGICAL FUNCTION		MSG_Enabled_Trace( Prefix, ID )

*	This routine is obviated by the introduction of classes,
*	but is retained for back-compatibility.

*  Input:
	CHARACTER*(*) Prefix !A message prefix.

*  Input/Output:
	INTEGER ID     !Message ID.  Set to zero by caller 
                       !before first call, set on first call by
                       !looking up or entering the prefix contained in msg
                       !(prefix is everything before the first space) in
	               !the index of message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Description:  Return enabled-status for a prefix;  define new prefix "disabled".
*	Returns the message-enabled-status for the message with
*	stored prefix Prefix, message-index ID.  If the message is not
*	defined, it is defined, but not enabled nor does it count, until
*	explicitly requested.
*	Identical to Msg_Enabled, except that if a message is defined here,
*	it is defined to be inactive and non-counting, regardless of whether
*	"enable *" (enable all) has been set.  "Trace" messages must be
*	specifically enabled.
*	This is intended to be used with trace messages, which normally
*	reside in code, but are intended to be inactive until explicitly
*	activated to trace the flow of the code, as in debugging.

*   Return conditions:
*	.TRUE. if the message is enabled (active),
*	.FALSE. if the message is disabled (inactive).
*/
{
	int LID;
	int Active, Alarming, Counting;
	int L, N;
	int Found;
	int ret;

	LID = *ID;

	if ( (LID<=0) || (prefix[LID].Active) ) {
	  N = length;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, Prefix, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	This prevents a test with Msg_Enabled_Trace from automatically
	defining (& enabling!) a message:  */
	if ( LID <= 0 ) {  /* Look up the message in the index:  */
	  Found = MsgFind( f1000, &LID, &Active, &Alarming, &Counting );   /* Set the LID & flags. */
	  if ( !Found ) { /*  Enter the prefix in the index:  */
	    MsgEnter( f1000, &LID );  /*  Enter it, permitting faster ID-reference.  */
	    if ( LID > 0 ) {                /*  Check that it was entered OK.  */
	      prefix[LID].Active   = FALSE; /*  But don't enable a message defined here */
	      prefix[LID].Counting = FALSE; /*  And don't even let such a one count */
	    }
	    if ( *ID == 0 ) *ID = LID; /* Ensure it's changed only if zero.  */
	    return( FALSE );
	  }
	} else {  /* Fast lookup:  */
	  Active = prefix[LID].Active;
	  Alarming = prefix[LID].Alarming;
	  Counting = prefix[LID].Counting;
	}

	if ( Active ) {  /* Message is enabled;  return .TRUE.  */
	  return( TRUE );
	} else {         /* Message is disabled; return .FALSE.  */
	  if ( Counting ) {  /* Count here, if disabled.  */
	    MsgIncr( LID );
	  }
	  return( FALSE );
	}
}




	FILE*	msg_file_open_( const char* FileName, const char* Type, int FileLen, int TypeLen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	INTEGER FUNCTION	MSG_File_Open( FileName, Type )

*  Inputs:
	CHARACTER*(*) FileName   ! The file-name.
	CHARACTER*(*) Type       ! The file's type, for an fopen.

*  Description:  Open a file with fopen, pushing old versions down a version stack.

*  Returns:
	NULL if failed,
	File descriptor from fopen if succeeded.                           */
{
	int L, N;
	char fType[10];

	N = FileLen;
	if ( N > 999 ) N = 999;
	strncpy( f1000, FileName, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

	N = TypeLen;
	if ( N > 9 ) N = 9;
	strncpy( fType, Type, N );
	L = MsgLNB( fType, N );   fType[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */

	return( MsgFileOpen( f1000, fType ) );
}




	void 	msg_finish_( int *Nevents )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Finish( Nevents )

*  Input:
	INTEGER Nevents !Caller-specified number of events, to normalize message frequencies.

*  Description:  MSG package finish-up.
*/
{
	MsgFinish( "", *Nevents );
	return;
}




	void 	msg_finish_shared_( int *Nevents )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Finish_Shared( Nevents )

*  Input:
	INTEGER Nevents !Caller-specified number of events, to normalize message frequencies.

*  Description:  MSG package finish-up, with removal of this process' shared memory segment.
*/
{
	MsgFinish( "s", *Nevents );
	return;
}




	void	msg_get_lun_( int *Terminal_LUN, int *Journal_LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Get_LUN( Terminal_LUN, Journal_LUN )

*  Outputs:
	INTEGER Terminal_LUN !FORTRAN logical unit number of terminal
	                     !for output, always comes back 6.
	INTEGER Journal_LUN  !FORTRAN logical unit number of journal file.
	                     !Always comes back 0.

*  Description:  For back compatibility.  Not supported in future releases.
*	Get the message handler terminal and journal LUNs and pass them
*	to the caller.  This back-compatibility version simply returns 6 and 0.
*/
{
	*Terminal_LUN = 6;
	*Journal_LUN  = 0;
	return;
}




	void	msg_incr_( int *ID )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			Msg_Incr( ID )

*  Input:
	INTEGER ID  ! Message-ID -- must be valid (ie, no prefix search is possible).

*  Description:  Increment a prefix's counter, referenced by ID.
	Increment the counter for the message indexed
	by ID.  Also, if a CPU time for ID has been marked, take a difference
	from that CPU and the current, and add it to the CPU total for ID.
*/
{
	MsgIncr( *ID );
	return;
}




	void 	msg_ini_( int *Journal_LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Ini( Journal_LUN )

*  Input:
	INTEGER Journal_LUN !FORTRAN logical unit number of journal file (ignored).


*  Description:  MSG package initializion and journal-LUN specification.
*/
{
	MsgInit("");
	return;
}




	void 	msg_ini_shared_( int *Journal_LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Ini_Shared( Journal_LUN )

*  Input:
	INTEGER Journal_LUN !FORTRAN logical unit number of journal file (ignored).


*  Description:  MSG package initializion and journal-LUN specification, shared memory.
*/
{
	MsgInit("s");
	return;
}




	int	msg_journal_close_( void )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	LOGICAL FUNCTION		MSG_Journal_Close()

*  Description:  Close the MSG journal file.
	Call this subroutine to close the message journal file.
	Disables message-logging in the journal file.

*  Returns:
	TRUE for successful close of journal file.
	FALSE for close failure of journal file.
*/
{
	return( MsgJournalClose() );
}




	void	msg_journal_off_(void)
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Journal_Off

*  Description:  Disable journal logging of messages.
*/
{
	MsgJournalOff();
	return;
}




	void	msg_journal_on_(void)
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Journal_On

*  Description:  Enable journal logging of messages.
*/
{
	MsgJournalOn();
	return;
}




	int	msg_journal_open_( const char *FileName, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	LOGICAL FUNCTION		MSG_Journal_Open( FILE_NAME )

*  Input:
	CHARACTER*(*) FILE_NAME !The journal file-name.

*  Description:  Open a message-logging journal file.
	Call this subroutine to specify FILE_NAME as the
	message journal file and to open that file.

*  Return conditions:
	.TRUE. for successful open of journal file.
	.FALSE. for open failure of journal file.
*/
{
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, FileName, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	return( MsgJournalOpen( f1000 ) );
}




	void	msg_journal_page_( void )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Journal_Page

*  Description:  Send a form-feed to the journal file.
*/
{
	MsgJournalPage();
	return;
}




	void	msg_lun_page_( int *LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_LUN_Page( LUN )

*  Input:
	INTEGER LUN  !FORTRAN logical unit number.
	
*  Description:  Send a form-feed to a FORTRAN LUN.
*/
{
	FILE *stream;

	if ( *LUN == 0 ) { /* Journal file  */
	  stream = JournalFILE;
	} else if ( *LUN == 6 ) { /* Standard out */
	  stream = NULL;
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	}
	MsgToFileOut( "\f", stream );

	return;
}




	void	msg_mark_( const char *Prefix, int *ID, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Mark( Prefix, ID )

*  Inputs:
	CHARACTER*(*) Prefix !MSG prefix.

*  Input/Output:
	INTEGER ID     !Fast-reference MSG ID.  Uniquely assigned on first call.

	INCLUDE 'msg_inc'

*  Description:  Set the given Prefix's "Marked CPU time" to current.
	When this prefix occurs in a call to an MSG routine which does Msg accounting
	(eg, Message), and when that Prefix has had its "Marked CPU time" set (ie,
	to non-zero), that prefix's "total CPU time" is incremented by Current CPU
	time, less the "Marked CPU time".  The Marked CPU time is then zeroed.
*/
{
	int i;
	int LID;  /* Local copy of ID. */
	int Active, Alarming, Counting;
	int L, N;
	int CPUtime;

	LID = *ID;

	if ( (LID<=0) || (prefix[LID].Active) ) {
	  N = length;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, Prefix, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	Check that the message-prefix is in the index, enter it
	in the index if necessary, and return its ID:               */
	MsgCheck( f1000, &LID, &Active, &Alarming, &Counting );

	if ( LID > 0 ) prefix[LID].Marked = TRUE;

	if ( Counting ) {  /* Counting is true only if found.  */
/*	  Prefix was found or defined, counting is enabled for it, and LID is now valid.  */
	  CPUtime = MsgCPU() - CPUtime0; /* Get "absolute" CPU time, since CPUtime0 was set. */
	  prefix[LID].CPUmark = CPUtime;
	}

	if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */

	return;
}




	void	msg_name_node_( const char *NodeName, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Name_Node( NodeName )

*  Input:
	CHARACTER*(*) NodeName

*  Description:  Specify the local (ASCII) node name for node-time stamping.
	Enters the ASCII node-name in the msg package, for use
	in node-stamping (along with the time-stamp) journal
	entries of message-occurrances.
*/
{
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, NodeName, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgNameNode( f1000 );
	return;
}




	void	msg_nocount_( const char *Prefix, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_NoCount( Prefix )

*  Input:
	CHARACTER*(*) PREFIX !A message prefix.

*  Description:  Disable Msg counting of a previously disabled prefix.
	         This routine affects only for prefixes which are
	         disabled, but still counting.
*/
{
	int Active, Alarming, Counting;
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgNoCount( f1000 );
	return;
}




	void	msg_set_abort_limit_( const char *Prefix, int *Limit, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Abort_Limit( Prefix, Limit )

*  Inputs:
	CHARACTER*(*) Prefix !A message prefix.
	INTEGER Limit !Maximum no. of times to display a message before aborting.

*  Description:  Set abort limit for a prefix.
	Set the abort limit for the message recognized by Prefix;
	Program termination results when the prefix's
	count has exceeded Limit.  Wildcards are permitted.
	The existing count-limit (after which message-display is disabled
	for Prefix), is set to the same value -- aborting msg calls
	should not be quiet without deliberate intervention (ie, re-specify
	the count-limit after this call to obtain silent aborts -- don't
	do it!).
*/
{
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgSetAbortLimit( f1000, *Limit );
	return;
}




	int	msg_set_by_command_( const char *com, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	LOGICAL FUNCTION		MSG_Set_By_Command( com )

*  Input:
	CHARACTER*(*) com !Command-string describing features to be set.

*  Description:  Issue an ASCII command to msg.
	Set the MSG package parameters on individual messages, referenced
	by prefix, according to arguments contained in a single character-
	string command contained in COM.  Commands take these forms:

	To list a summary of msg prefixes' CPU usages:
	CPU

	To examine the summary page length:
	GETLINES

	To examine the shared memory ID (shmid):
	GETSHMID

	To examine the node name:
	GETNODE

	To list the available commands:
	HELP

	To list a summary of msg prefixes:
	LIST

	To have time-stamps occur on a changed CPU time (in addition to
	getting them on changed real-time or node-name):
	TIMESTAMP CPU

	To have time-stamps not occur on a changed CPU time (ie, only
	getting them on changed real-time or node-name):
	TIMESTAMP NOCPU

	To set the number of lines per page in the summary-output:
	LINES line-count

	To set the node name:
	SETNODE <nodename>

	To load the entire Msg state from an Msg state file:
	LOAD <filename>

	To store the entire Msg state to an Msg state file:
	STORE <filename>

	To set message-abort-limits on specific messages:
	ABORT prefix-1=limit-1 prefix-2=limit-2 ... prefix-n=limit-n

	To enable specific alarms:
	ALARM prefix-1 prefix-2 ... prefix-n

	To disable specific alarms, but to continue counting their occurances and displaying messages:
	NOALARM prefix-1 prefix-2 ... prefix-n

	To resume counting of message occurances, whether or not disabled:
	COUNT prefix-1 prefix-2 ... prefix-n

	To stop counting of message occurances, whether or not disabled:
	NOCOUNT prefix-1 prefix-2 ... prefix-n

	To delete prefixes:
	DELETE prefix-1 prefix-2 ... prefix-n

	To disable specific messages, but to continue counting their occurances:
	DISABLE prefix-1 prefix-2 ... prefix-n

	To enable specific messages:
	ENABLE prefix-1 prefix-2 ... prefix-n

	To get the complete state of specific messages:
	GET prefix-1 prefix-2 ... prefix-n

	To set the level on specific alarms:
	LEVEL prefix-1=level-1 prefix-2=level-2 ... prefix-n=level-1

	To set message-limits on specific messages:
	LIMIT prefix-1=limit-1 prefix-2=limit-2 ... prefix-n=limit-n

	To set the state of specific messages:
	SET prefix-1=countLimit-1,level-1,abortLimit-1,active(T/F),counting(T/F),alarming(T/F)> 



	Spacing is not critical in the command line, provided at least one
	or more spaces, tabs or commas separate the arguments.
	The "=" symbols in the LIMIT command are essential, but spaces around
	it are optional and not at all critical.

	Wildcards are permitted in the prefix specifications.

  Returns:
	TRUE for successful completion of command,
	FALSE for failure.                                                    */
{

	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, com, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	return(MsgSetByCommand( f1000 ));
}





	int	msg_set_from_file_( int *LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	LOGICAL FUNCTION		MSG_Set_From_File( LUN )

*  Input:
	INTEGER LUN !Fortran Logical Unit Number, on which a command (ASCII)
	            !file should be (already) open.

*  Description:  ASCII-file command interface to control msg.
	Reads lines from LUN and interprets them as Msg_Set_By_Command commands
	until either an <EOF> or a line containing MSG_EXIT is encountered.

  Returns:
	TRUE for successful completion of command,
	FALSE for failure.                                                    */
{
	FILE *stream;

	if ( *LUN == 6 ) { /* Standard Out -- can't read this! */
	  static int id=0;
	  Message( "Msg_Set_From_File-E1  Attempting to read from standard out (LUN=6)", &id );
	  return(FALSE);
	} else if ( *LUN == 0 ) { /* Journal File  -- can't read this! */
	  static int id=0;
	  Message( "Msg_Set_From_File-E2  Attempting to read from the journal file (LUN=0)", &id );
	  return(FALSE);
	} else {
	  stream = MsgMapLUN( LUN, "r" );    /*  Get stream descriptor for Fortran LUN. */
	  return(MsgSetFromFile( stream ));
	}
}




	void	msg_set_level_( const char *Prefix, int *Limit, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Level( Prefix, Limit )

*  Inputs:
	CHARACTER*(*) Prefix !A message prefix.
	INTEGER       Level  !Alarm level for specified prefix.

*  Description:  Set alarm level for a prefix.
	Set the alarm level for the message recognized by Prefix;
	Wildcards are permitted.
*/
{
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgSetLevel( f1000, *Limit );
	return;
}




	void	msg_set_limit_( const char *Prefix, int *Limit, int length )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Limit( Prefix, Limit )

*  Inputs:
	CHARACTER*(*) Prefix !A message prefix.
	INTEGER       Limit  !Maximum no. of (additional) times to display a message.


*  Description:  Set auto-disable count limit for a prefix.
	Set the count limit for the message recognized by Prefix;
	displays of the specified message are disabled once its
	count has exceeded Limit.  Wildcards are permitted.
*/
{
	int L, N;

	N = length;
	if ( N > 999 ) N = 999;
	strncpy( f1000, Prefix, N );
	L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	MsgSetLimit( f1000, *Limit );
	return;
}




	void	msg_set_lun_( int *Terminal_LUN, int *Journal_LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_LUN( Terminal_LUN, Journal_LUN )

*  Inputs:  (All ignored!)
	INTEGER Terminal_LUN !Fortran logical unit number of terminal
	                     !for output, typically 6.
	INTEGER Journal_LUN  !Fortran logical unit number of journal file.

*  Description:  Back compatibility routine, to satisfy existing references.
	         This routine does nothing.
*/
{
	return;
}




	void	msg_set_summary_mode_aborted_( int *Mode )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Summary_Mode_Aborted( Mode )

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Description:  Set the msg summary mode for "Aborted" messages.
*/
{
	MsgSetSummaryModeAborted( *Mode );
	return;
}




	void	msg_set_summary_mode_active_( int *Mode )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Summary_Mode_Active( Mode )

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Description:  Set the msg summary mode for "Active" messages.
*/
{
	MsgSetSummaryModeActive( *Mode );
	return;
}




	void	msg_set_summary_mode_counting_( int *Mode )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Summary_Mode_Counting( Mode )

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Description:  Set the msg summary mode for "Counting" messages.
*/
{
	MsgSetSummaryModeCounting( *Mode );
	return;
}




	void	msg_set_summary_mode_inactive_( int *Mode )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Summary_Mode_Inactive( Mode )

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Description:  Set the msg summary mode for "Inactive" messages.
*/
{
	MsgSetSummaryModeInactive( *Mode );
	return;
}




	void	msg_set_summary_page_length_( int *PageLength )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_Summary_Page_Length( PageLength )

*  Input:
	INTEGER PageLength  !Length of summary page (lines).

*  Description:  Set the msg summary table page length.
*/
{
	MsgSetSummaryPageLength( *PageLength );
	return;
}




	void	msg_set_timestamp_cpu_( int *Mode )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Set_TimeStamp_CPU( Mode )

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Description:  Set the msg time-stamp mode for stamping CPU changes.
*/
{
	MsgSetTimeStampCPU( *Mode );
	return;
}




	void	msg_summary_( int *LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Summary( LUN )

*  Input:
	INTEGER LUN !Device on which the summary is output.

*  Description:  Generate an msg summary table on a Fortran LUN.
	         LUN = 0 refers to the journal file, LUN = 6 is standard out.
*/
{
	int zero=0;
	msg_summary_event_( LUN, &zero );
	return;
}




	void	msg_summary_cpu_( int *LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Summary_CPU( LUN )

*  Input:
	INTEGER LUN !Device on which the summary is output.

*  Description:  Generate a CPU-usage msg-summary table.
	Output a CPU-usage summary of the MSG-handled & marked messages
	on the specified LUN.  "Marked" messages are messages whose occurances
	were preceded, one-for-one, by calls to MSG_Mark, with the same
	msg prefix.
	LUN = 0 refers to the journal file, LUN = 6 is standard out.
*/
{
	FILE *stream;

	if ( *LUN == 6 ) {
	  MsgSummaryCPUFile( NULL );  /*  This will put it on standard out.  */
	} else if ( *LUN == 0 ) {
	  MsgSummaryCPUFile( JournalFILE );
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	  MsgSummaryCPUFile( stream );
	}
	return;
}




	void	msg_summary_event_( int *LUN, int *Nevents )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Summary_Event( LUN, Nevents )

*  Input:
	INTEGER LUN     !Device on which the summary is output.
	INTEGER Nevents !Number of events by which to normalize.

*  Description:  Generate an Msg summary table with a frequency column on a Fortran LUN.
	Output a summary of the Msg-handled messages on the specified LUN.
	Include a column of normalized occurances of each message,
	which consists of a column of the number of occurances of each
	message, divided by Nevents.  If Nevents is not positive, then the
	column is ommitted.
	LUN = 0 refers to the journal file, LUN = 6 is standard out.
*/
{
	FILE *stream;

	if ( *LUN == 6 ) {
	  MsgSummaryEventFile( NULL, *Nevents );  /*  This will put it on standard out.  */
	} else if ( *LUN == 0 ) {
	  MsgSummaryEventFile( JournalFILE, *Nevents );
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	  MsgSummaryEventFile( stream, *Nevents );
	}
	return;
}




	void	msg_time_stamp_( int *LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Time_Stamp( LUN )

*  Inputs:
	INTEGER LUN  !Fortran logical unit number for time-stamp to be written on.

*  Description:  Write a node and time (real and CPU) stamp to a Fortran LUN, if new.
	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
	"Stamp" the node-name on the same line, if node-name has been specified
	with a "call msg_name_node(<node_name>)".
	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
	as 17-character ASCII strings.

	Produces output only if different than the previous output from
	this subroutine.  eg, a call to this routine in a tight loop will
	only produce an actual time-stamp once each second.
*/
{
	FILE *stream;

	if ( *LUN == 6 ) {
	  MsgTimeStampFile( NULL );  /*  This will put it on standard out.  */
	} else if ( *LUN == 0 ) {
	  MsgTimeStampFile( JournalFILE );
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	  MsgTimeStampFile( stream );
	}
	return;
}




	void	msg_time_stamp_out_( int *LUN )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_Time_Stamp_Out( LUN )

*  Inputs:
	INTEGER LUN  !Fortran logical unit number for time-stamp to be written on.

*  Description:  Write a node and time (real and CPU) stamp to a Fortran LUN, always.
	"Stamp" the date and time on a line on Fortran logical unit LUN.
	"Stamp" the node-name on the same line, if node-name has been specified
	with a "call msg_name_node(<node_name>)".
	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
	as 17-character ASCII strings.

	Always produces output.
*/
{
	FILE *stream;

	if ( *LUN == 6 ) {
	  MsgTimeStampFileOut( NULL );  /*  This will put it on standard out.  */
	} else if ( *LUN == 0 ) {
	  MsgTimeStampFileOut( JournalFILE );
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	  MsgTimeStampFileOut( stream );
	}
	return;
}




	void	msg_to_journal_( const char *msg, int *Lines, int *ID, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_to_Journal( MSG, LINES, ID )

*  Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER Lines  !Number of lines in MSG.

*  Input/Output:
	INTEGER ID     !A message ID.  Set to zero by caller 
                       !before first call, set by Message on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Description:  Write a message to the journal (only), if enabled.
	Conditionally display & count a message msg, containing Lines lines
	on the journal file, if open & enabled.
*/
{
	int Active, Counting, Alarming;
	int LID;   /* Local copy of ID. */
	int L, N;
	int i, j;

	LID = *ID;

	if ( (LID<=0) || (prefix[LID].Active) ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	Check or set the ID & flags.
	Enter msg's prefix in the index if needed.  */
	MsgCheck( f1000, &LID, &Active, &Alarming, &Counting );

/*	Counting is true only if found:  */
	if ( Counting ) MsgIncr( LID );

	if ( Active ) {           /* Display it:  */
	  for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	    N = msglen;
	    if ( N > 999 ) N = 999;
	    strncpy( f1000, &msg[j], N );
	    L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	    MsgToJournalOut( f1000 );
	  }
	  MsgAbortCheck( LID );   /* Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {         /* Alarm it:  */
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	  MsgAlarm( f1000, prefix[LID].AlarmLevel );
	}

	if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */

	return;
}




	void	msg_to_journal_out_( const char *msg, int *Lines, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_to_Journal_Out( msg, Lines )

*  Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER Lines  !Number of lines in msg.

*  Description:  Write a message to the journal (only);  bypass accounting.
	Unconditionally display a message msg, containing Lines lines
	on the journal file, if open & enabled.
*/
{
	int L, N;
	int i, j;

	for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, &msg[j], N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
/*	  Display time & message on journal file:  */
	  MsgToFileOut( f1000, JournalFILE );
	}
	return;
}




	void	msg_to_lun_( const char *msg, int *Lines, int *LUN, int *ID, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_to_LUN( msg, Lines, LUN, ID )

*  Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER Lines  !Number of lines in msg.
	INTEGER LUN    !Fortran logical unit number for msg to be sent to.

*  Input/Output:
	INTEGER ID     !A message ID.  Set to zero by caller 
                       !before first call, set by Message on first call by
                       !looking up or entering the prefix contained in msg
                       !(prefix is everything before the first space) in
	               !the index of message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Description:  Write a message to a Fortran LUN, if enabled.
	Conditionally display & count a message MSG, containing LINES lines on LUN.
*/
{
	int Active, Counting, Alarming;
	int LID;   /* Local copy of ID. */
	int L, N;
	int i, j;
	FILE *stream;

	if ( *LUN == 0 ) { /* Journal file  */
	  stream = JournalFILE;
	} else if ( *LUN == 6 ) { /* Standard out */
	  stream = NULL;
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	}

	LID = *ID;

	if ( (LID<=0) || (prefix[LID].Active) ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	}

/*	Check or set the ID & flags.
	Enter msg's prefix in the index if needed.  */
	MsgCheck( f1000, &LID, &Active, &Alarming, &Counting );

/*	Counting is true only if found:  */
	if ( Counting ) MsgIncr( LID );

	if ( Active ) {           /* Display it:  */
	  for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	    N = msglen;
	    if ( N > 999 ) N = 999;
	    strncpy( f1000, &msg[j], N );
	    L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	    MsgToFileOut( f1000, stream );
	  }
	  MsgAbortCheck( LID );   /* Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {         /* Alarm it:  */
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, msg, N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	  MsgAlarm( f1000, prefix[LID].AlarmLevel );
	}

	if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */

	return;
}




	void	msg_to_lun_out_( const char *msg, int *Lines, int *LUN, int msglen )
/*  Fortran callable interface to msg;  Fortran equivalent to:
	SUBROUTINE			MSG_to_LUN_Out( msg, Lines, LUN )

*  Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message submitted for display.
	INTEGER       Lines  !Number of lines in msg.
	INTEGER       LUN    !Fortran logical unit number for msg to be written to.

*  Description:  Write a message to a Fortran LUN;  bypass accounting.
	Display a message, containing one or more lines of no particular
	length (or length-limit), on Fortran logical unit LUN.

	This routine ends by flushing the output buffer.
	Without this, a crash leaves the last several lines of the output to
	LUN not in the listing file -- a terrible thing indeed for debugging.
*/
{
	int L, N;
	int i, j;
	FILE *stream;



	if ( *LUN == 0 ) { /* Journal file  */
	  stream = JournalFILE;
	} else if ( *LUN == 6 ) { /* Standard out */
	  stream = NULL;
	} else {
	  stream = MsgMapLUN( LUN, "w" );    /*  Get stream descriptor for Fortran LUN. */
	}

	for ( i=0,j=0;  i<*Lines;  i++,j+=msglen ) {
	  N = msglen;
	  if ( N > 999 ) N = 999;
	  strncpy( f1000, &msg[j], N );
	  L = MsgLNB( f1000, N );   f1000[L+1] = 0;  /*  Find Last Non-Blank &append NULL (L is index: L<N). */
	  MsgToFileOut( f1000, stream );
	}
	return;
}




	FILE*	MsgMapLUN( int *LUN, const char *type ) 
{
/*  Description:  Get the stream descriptor corresponding to a Fortran LUN.

	Not Fortran callable !  This is an interface to the maplun_ routine,
	which may or may not exist on a given platform.  If it does not
	exist, this routine forces a "sensible" behaviour.

	This works absolutely correctly only on sgi.  On other platforms,
	this routine returns NULL (for standard out) if LUN is 6.  Otherwise,
	it returns the stream descriptor for the journal file if the journal
	file is enabled and opened.
	"type" is that of the fopen (stream i/o) call.                      */

	FILE *stream;
	int   fid;

/*  from "man fdopen":   FILE *fdopen (int fildes, const char *type);       */

#ifdef UNDEF
	if ( *LUN == 0 ) {
/*	  Special value of *LUN == 0 refers to the journal file:  */
	  if ( JournalFILE ) {
	    stream = JournalFILE;
	  } else {
	    stream = NULL;
	  }
	} else {
	  fid = maplun_(LUN);
	  stream = fdopen ( fid, type );
	}
#else
/*	Kludge in some probable values in the absence of maplun:   */
	if ( *LUN == 6 ) {
	  stream = NULL;
	} else if ( JournalFILE ) {
	  stream = JournalFILE;
	} else {
	  stream = NULL;
	}
#endif
	return(stream);
}



	void	msgalarmroutinesample_( char* Prefix, char* sansPrefix, int *Level )
{
/*  Description:  Fortran-callable interface to MsgAlarmRoutineSample.
*/

	MsgAlarmRoutineSample( Prefix, sansPrefix, Level );

	return;
}
