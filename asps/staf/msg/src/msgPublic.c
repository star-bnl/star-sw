/*	This is the STAR message/error handling package (prefix Msg_).

	msgPublic.c

	==================+++====================
	==  msg Public Routines in this file.  ==
	==================+++====================

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

static const char sccsid[] = "@(#)"__FILE__"\t\t1.55\tCreated 3/8/98 03:34:14, \tcompiled "__DATE__" "__TIME__;

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#if !defined(_AIX) && !defined(Linux) && !defined(HPUX)
#include <sys/systeminfo.h>
#endif

#include <errno.h>
#include <msg.h>
#include <msgData.h>

#define MAX_FILE_VERSIONS 99

extern msgData_t *Msg;
extern msgData_t msg;

extern control_t *control;   /* See msgPrivate.c for address assignments of these.  */
extern prefix_t  *prefix;
extern class_t   *msgClass;

extern FILE *JournalFILE;    /* Journal-file descriptor                          */
extern int   JournalEnabled; /* Journal-file enabled-flag                        */
extern int CPUtime0;
extern int ELAtime0;

extern funcPoint MsgAlarmRoutine;

static char   m1000[1000];  /*  Some "scratch" message space.  */
static char   s1000[1000];  /*  Some "scratch" string space.  */

int MsgInitialized = FALSE; /* This is set to TRUE when initialized.  */
static int AppendReturn   = FALSE; /* Default is to not append carriage return at end of summary lines.  Set to TRUE for gui. */

	void	Message(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */

/*   Input/Output:                                                                         */
	, int       *ID ) /* Fast-reference message ID.  Set to zero by caller 
	                     before first call, set by Message on first call by
	                     looking up or entering the prefix contained in MSG
	                     (prefix is everything before the first space) in
	                     the index of MSG message prefixes.
	                     If ID is negative or NULL, ID remains unchanged, and lookup
	                     is then always by prefix.                                     */
{
/* Description:  Display and log a message.
	Conditionally display a message msg on standard out and the journal,
	if enabled by a call to MsgJournalOn.
	The message is displayed unless disabled.
	The message is counted unless counting is disabled.
	If the prefix in msg is undefined, it is defined at this call and set to
	the default state for its class (see MsgClassDefine).

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.  */

	int Active, Counting, Alarming;
	int LID;   /* Local copy of ID. */

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}

/*	Check or set the ID & flags.
	Enter MSG's prefix in the index if needed.  */

	MsgCheck( msg, &LID, &Active, &Alarming, &Counting );

/*	Counting is true only if found:  */
	if ( Counting ) MsgIncr( LID );

	if ( Active ) {           /* Display it:  */
	  MessageOut( msg );
	  MsgAbortCheck( LID );   /* Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {         /* Alarm it:  */
	  MsgAlarm( msg, prefix[LID].AlarmLevel );
	}

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return;
}




	void	MessageOut(

/*   Inputs:                                                                                       */
	  const char *msg ) /* Character-string message, prefix irrelevant, submitted for display. */
{
/* Description:  Display and log a message, bypassing msg accounting.
	Display (always) a message msg on standard out and the journal, if enabled by a
	call to MsgJournalOn.
	The message may not be disabled.
	The message is not counted.
	This is the "pure" I/O part of Message.
	A prefix contained in msg is regarded as simply a part of msg and is ignored as a prefix.
	No definitions are made here for an undefined prefix.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.  */

/*	Send message on standard output: */
	fprintf(stderr, "%s\n", msg );
	fflush( stderr );

/*	Display time & message on journal file:  */
	MsgToFileOut( msg, JournalFILE );
	return;
}




	void	MsgAlarm( 

/*   Inputs:                                                                                */
	  const char *msg  /* Character-string message, with prefix, submitted for display. */
	, int  severity )  /* Severity level of alarm -- from msg class.                    */

{
/* Description:	msg interface to application-specified alarm routine.
*/

	char *sansPrefix=NULL;
	char Prefix[PREFIX_MAXLEN+1];

/*	Do this only if nothing's been declared:  */
	if ( MsgAlarmRoutine ) {
	  MsgParse(   msg, Prefix, &sansPrefix );  /*  Get the prefix and the message-that-follows-prefix pointer.  */
	  MsgAlarmRoutine( Prefix,  sansPrefix, &severity );  /*  severity needs to be a pointer, for historical reasons.  */
	}
	return;
}




	void	MsgAlarmRegister(

/*    Input:                                                                             */
	 funcPoint AlarmRoutine ) /* Application-specified Alarm routine to use by msg.  */
{
/* Description:  Register an alarm-routine to be called by active-alarm messages.
*/
	MsgAlarmRoutine = AlarmRoutine;
	return;
}




	void	MsgAlarmRoutineSample( char* Prefix, char* sansPrefix, int *Level )
{
/*  Description:  Msg sample alarm routine.  Specify in MsgAlarmRegister call.
	Applications may create their own msg alarm routine, with the arguments being
	the same as specified in this sample.

	This routine will be called on the occurance of any (message) prefix which
	is alarm-enabled (designated by "!Active!" on the summary listing,
	enabled with the MsgEnableAlarm call).

	All arguments are filled by Msg before this routine is called, and are
	derived from the originating message.

	Prefix     is the stripped off prefix from the alarm-originating message.
	sansPrefix is the rest of the message (ie, without the prefix or intervening space).
	Level      is the alarm level, set by a MsgSetLevel call, and has application-defined
	           meaning (Level has no meaning in this sample routine).
*/

	MessageOut( "\nMsgAlarmRoutineSample-I1   Alarm!!!!!!!!!!!!!" );
	MessageOut( Prefix );
	MessageOut( sansPrefix );
	MessageOut( "MsgAlarmRoutineSample-I1   Alarm!!!!!!!!!!!!!\n" );
	return;
}




	void	MsgAppendReturn(){   AppendReturn = -1; return; }
	void	MsgNoAppendReturn(){ AppendReturn =  0; return; }

	void	MsgClassDefine(

/*   Inputs:                                                                                */
	  const char *Class  /* The message class -- generally, one character               */
	, const char *State  /* Default state for messages in this class:                   */
	                     /* "Active", "Count" or "Inactive".                            */
	, int   CountLimit   /* Default count-limit for this class.                         */
	, int   AbortLimit ) /* Default count-limit for this class.                         */

{
/* Description:  Define a new msg class (of prefixes).
	Each msg prefix is assigned to a class according to this scheme:

	     * If the prefix contains a "-", its class is the first character
	       after the "-", if non-blank.

	     * If the prefix ends with a "-" or has no "-", it is assigned
	       to the null class.

	The MSG class to which a prefix is assigned is used (soley) for the
	purpose of setting the prefix's state and limits (count and abort).
	Classes permit the assignment of a variety of default MSG states
	and counts for diffent groups of messages.  The predefined MSG
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

	Conditionally display a message msg on standard out and the journal,
	if enabled by a call to MsgJournalOn.  The message is displayed
	unless disabled.  The message is counted unless counting is disabled.  */
	int Found;
	int ID;

	Found = MsgFindClass( Class, &ID );      /* Look up the class in the index.  */
	if ( !Found ) {                          /* Didn't find it.                  */
	  if ( !MsgEnterClass( Class, &ID ) ) {  /* Failed & already complained.     */
	    fprintf(stderr, "MsgDefineClass-D1  Didn't find class [%s]\n", Class );
	    fflush( stderr );
	    return;
	  }
	}

	if        ( !strcasecmp( State, "ACTIVE" ) ) {
	  msgClass[ID].Active   = TRUE;
	  msgClass[ID].Counting = TRUE;
	} else if ( !strcasecmp( State, "INACTIVE" ) ) {
	  msgClass[ID].Active   = FALSE;
	  msgClass[ID].Counting = FALSE;
	} else if ( !strcasecmp( State, "COUNTING" ) ) {
	  msgClass[ID].Active   = FALSE;
	  msgClass[ID].Counting = TRUE;
	}

	msgClass[ID].Alarming = FALSE;
	msgClass[ID].CountLimit = CountLimit;
	msgClass[ID].AbortLimit = AbortLimit;
	msgClass[ID].AlarmLevel = 3;

/*	fprintf(stderr, "MsgDefineClass-D2  Defined class [%s] ID:%d Active:%d Counting:%d Alarming:%d\n", Class, ID
	      , msgClass[ID].Active, msgClass[ID].Counting, msgClass[ID].Alarming );  */

	return;
}




	void	MsgCount(

/*   Inputs:                                                                                */
	  const char *Prefix )  /* A message prefix */
{
/* Description:  Enable counting for a prefix.
	Enable counting, and continue displaying the message
	recognized by Prefix.
	Has no effect if counting has not been disabled.
	If Prefix is undefined, it is defined at this call and set to the "Counting" state
	(ie, no displays).  */
	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) prefix[ID].Counting = TRUE; /* It will count. */

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Counting = TRUE;
	  }
	  control->Counting = TRUE;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) prefix[ID].Counting = TRUE;
	  }
	}

	return;
}




	void	MsgDelete(
/*  Input:                                          */
	  const char *Prefix ) /* A message prefix. */
{
/*  Description:  Delete the specified prefix.
*/

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  if ( MsgFind( Prefix, &ID, &Active, &Alarming, &Counting ) ) {  /*  It exists -- delete it:  */
	    MsgDeleteID( ID );
	  }

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  control->Nprefixes = 0;  /* It's easy to delete them all.   */

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      MsgDeleteID( ID );
	    }
	  }
	}

	return;
}




	void	MsgDisable(

/*   Inputs:                                                                                */
	  const char *Prefix )  /* A message prefix */
{
/* Description:  Disable display of messages with the given prefix (continue counting).
	Disable displays and logging for, but continue counting, the message
	recognized by Prefix.  Has no effect if already disabled.                           */

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) prefix[ID].Active = FALSE; /* It will not display or log. */

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Active = FALSE;
	  }
	  control->Active = FALSE;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) prefix[ID].Active = FALSE;
	  }
	}

	return;
}




	void	MsgDisableAlarm(

/*   Inputs:                                                                                */
	  const char *Prefix )  /* A message prefix */
{
/* Description:  Disable alarms of messages with the given prefix (continue counting).
	Disable alarms for, but continue counting the message
	recognized by Prefix.  Has no effect if already disabled.  */
	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) prefix[ID].Alarming = FALSE; /* It will not alarm. */

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Alarming = FALSE;
	  }
	  control->Alarming = FALSE;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) prefix[ID].Alarming = FALSE;
	  }
	}

	return;
}




	void	MsgDisplay(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */

/*   Input/Output:                                                                         */
	, int       *ID ) /* Fast-reference message ID.  Set to zero by caller 
	                     before first call, set by Message on first call by
	                     looking up or entering the prefix contained in MSG
	                     (prefix is everything before the first space) in
	                     the index of MSG message prefixes.
	                     If ID is negative, ID remains unchanged, and lookup
	                     is then always by prefix.                                     */
{
/* Description:  Display and log a message.
	Conditionally display a message msg on standard out and the journal,
	if enabled by a call to MsgJournalOn.  The message is displayed
	unless disabled.  The message is counted unless counting is disabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.  */

	int Active, Alarming, Counting;

	int LID;  /* Local copy of ID.  */

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}

/*	Check or set the ID & flags.
	Enter MSG's prefix in the index if needed.  */
	MsgCheck( msg, &LID, &Active, &Alarming, &Counting );

	if ( Counting ) {  /*  Counting is true only if found.  */
	  MsgIncr(LID);
	}

	if ( Active ) {  /* Display it:  */
	  MsgDisplayOut( msg );
	  MsgAbortCheck( LID );  /*  Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {
	  MsgAlarm( msg, prefix[LID].AlarmLevel );
	}

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return;
}




	void	MsgDisplayOut(

/*   Inputs:                                                                                       */
	  const char *msg ) /* Character-string message, prefix irrelevant, submitted for display. */
{
/* Description:  Display and log a message, bypassing msg accounting.
	Display (always) a message msg on standard out and the journal, if enabled by a
	call to MsgJournalOn.
	The message may not be disabled.
	The message is not counted.
	This is the "pure" I/O part of MsgDisplay.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.                           */

/*	Display message on standard output:  */
	fprintf(stderr, "%s\n", msg );
	fflush( stderr );

	return;
}




	void	MsgDisplayAndFileOut(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */
	, FILE  *fid )    /* File Descriptor for msg to be echoed to;                      */
{
/* Description:  Display a message on standard out and echo on fid.
	Display (always) the message msg on standard output and echo it to fid.
	The message may not be disabled.
	The message is not counted.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.                 */


/*	Display message on standard out: */
	MsgDisplayOut( msg );

/*	Echo message to fid:  */
	MsgToFileOut( msg, fid );

	return;
}




	void	MsgDisplayAndFile(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */
	, FILE  *stream   /* Stream Descriptor for msg to be echoed to;                      */

/*   Input/Output:                                                                         */
	, int       *ID ) /* Fast-reference message ID.  Set to zero by caller 
	                     before first call, set by Message on first call by
	                     looking up or entering the prefix contained in MSG
	                     (prefix is everything before the first space) in
	                     the index of MSG message prefixes.
	                     If ID is negative, ID remains unchanged, and lookup
	                     is then always by prefix.                                     */
{
/* Description:  Display a message on standard out and echo on stream.
	Conditionally display the message msg on standard output and echo it to stream.
	The message is displayed unless disabled.
	The message is counted unless counting is disabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.                 */


	int Active, Alarming, Counting;
	int LID;  /* Local copy of ID.  */

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}

/*	Check or set the ID & flags.
	Enter MSG's prefix in the index if needed. */
	MsgCheck( msg, &LID, &Active, &Alarming, &Counting );

	if ( Counting ) {  /* Counting is true only if found. */
	  MsgIncr(LID);
	}

	if ( Active ) {  /* Display it:  */
	  MsgDisplayAndFileOut( msg, stream );
	  MsgAbortCheck( LID ); /*  Check if abort limit is exceeded, and maybe abort. */
	}

	if ( Alarming ) {
	  MsgAlarm( msg, prefix[LID].AlarmLevel );
	}

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return;
}




	void	MsgEnable(

/*   Inputs:                                         */
	  const char *Prefix )  /* A message prefix */
{
/* Description:  Enable display and counting of messages with the given prefix.
*/

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) {
	    prefix[ID].Active     = TRUE; /* It will display and log. */
	    prefix[ID].Counting   = TRUE; /* Active, No-Counting is an illegal state. */
	    prefix[ID].CountLimit = 0;    /* Always reset counting limit here, too.  */
	  }

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Active = TRUE;
	    prefix[ID].Counting = TRUE;  /* Active, No-Counting is an illegal state. */
	    prefix[ID].CountLimit = 0;   /* Always reset counting limit here, too.  */
	  }
	  control->Active     = TRUE;
	  control->Counting   = TRUE;
	  control->CountLimit = 0;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      prefix[ID].Active   = TRUE;
	      prefix[ID].Counting = TRUE;  /* Active, No-Counting is an illegal state. */
	      prefix[ID].CountLimit = 0;   /* Always reset counting limit here, too.  */
	    }
	  }
	}

	return;
}




	void	MsgEnableAlarm(

/*   Inputs:                                                                                */
	  const char *Prefix )  /* A message prefix */
{
/* Description:  Enable alarms of messages with the given prefix.
*/

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) {
	    prefix[ID].Alarming = TRUE; /* Alarming, No-Counting is an illegal state.  */
	    prefix[ID].Counting = TRUE; /* Alarming, No-Counting is an illegal state.  */
	  }

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Alarming = TRUE;
	    prefix[ID].Counting = TRUE;
	  }
	  control->Alarming = TRUE;
	  control->Counting = TRUE;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      prefix[ID].Alarming = TRUE;
	      prefix[ID].Counting = TRUE;
	    }
	  }
	}

	return;
}




	int	MsgEnabled(

/*   Inputs:                                     */
	  const char *Prefix /* A message prefix */

/*   Input/Output:                                                                         */
	, int       *ID ) /* Fast-reference message ID.  Set to zero by caller 
	                     before first call, set by Message on first call by
	                     looking up or entering the prefix contained in MSG
	                     (prefix is everything before the first space) in
	                     the index of MSG message prefixes.
	                     If ID is negative, ID remains unchanged, and lookup
	                     is then always by prefix.                                     */
{
/* Description:  Return enabled-status for a prefix;  define new prefix "enabled".
	Returns the message-display-enabled-status for the message
	with stored prefix <Prefix>, message-index ID.  If the message is not
	defined, it is defined and enabled, with counting turned on.

  Return conditions:
	TRUE  if the message is enabled (active),
	FALSE if the message is disabled (inactive).                                 */

	int Active, Alarming, Counting;
	int LID;   /* Local copy of ID.  */
	int i;

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}
/*	Check that the message-prefix is in the index, enter it
	in the index if necessary, and return its ID:               */
	MsgCheck( Prefix, &LID, &Active, &Alarming, &Counting );

	if ( !Active && Counting) MsgIncr(LID); /*  Increment if not active, but counting. */

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return( Active );
}




	void	MsgFilePage( FILE *fid )
{
/* Description:  Send a form-feed to a file.
*/
	MsgToFileOut( "\f", fid );
	return;
}




	FILE*	MsgFileOpen(
/*  Inputs:                                                                */
	  const char* FileName  /* The file-name                           */
	, const char* Type  )   /* The file's type, for an fopen.          */
{
/* Description:  Open a file with fopen, pushing old versions down a version stack.

   Returns:
	NULL if failed,
	File descriptor from fopen if succeeded.                           */

	struct stat buf;

	if ( !stat( FileName, &buf ) ) {  /*  If file exists, some work is needed.  */
	  int version = 1;
	  sprintf( s1000, "rm %s.p%3.3d", FileName, version ); /*  Make file name with ready-to-go rm prepended. */
	  while ( !stat( &s1000[3], &buf ) ) {    /*  While versions exist, work continues.  */
	    if ( version > MAX_FILE_VERSIONS ) {  /*  Discard versions higher than this. */
	      if ( system( s1000 ) ) {            /*  rm the file.  */
	        fprintf(stderr, "MsgFileOpen-E1  Failed to %s\n", s1000 );
	        fflush( stderr );
	        return( NULL );
	      }
	    } else {
	      version++;
	      sprintf( s1000, "rm %s.p%3.3d", FileName, version ); /*  Make file name with ready-to-go rm prepended. */
	    }
	  }
	  while ( version > 1 ) {
	    version--;
	    sprintf( s1000, "mv %s.p%3.3d %s.p%3.3d", FileName, version, FileName, version+1 );
	    if ( system( s1000 ) ) {            /*  mv the file.  */
	      fprintf(stderr, "MsgFileOpen-E2  Failed to %s\n", s1000 );
	      fflush( stderr );
	      return( NULL );
	    }
	  }
	  sprintf( s1000, "mv %s %s.p%3.3d", FileName, FileName, 1 );
	  if ( system( s1000 ) ) {            /*  mv the file.  */
	    fprintf(stderr, "MsgFileOpen-E3  Failed to %s\n", s1000 );
	    fflush( stderr );
	    return( NULL );
	  }
	}

	return( fopen( FileName, Type ) );
}




	void 	MsgFinish( const char *switches  /*  One-letter switch string:                                       */
	                                         /*  "s"  shared msg (remove shmid) -- default: not shared           */
	                 , int   Nevents )       /*  User-suplied number-of-events, to normalize message frequencies */
{
/* Description:  msg package finish: output message summary, close journal and remove shmid if "s" is specified.
*/

	int ID;
	int ret;
	FILE *fid;
	

	MsgSummaryEventFile( NULL, Nevents ); /* List them all, to the terminal.  */
	MsgSummaryCPUFile(   NULL );          /* CPU usage measurements.          */

	fid = MsgJournalGet();
	MsgSummaryEventFile( fid, Nevents ); /* List them all, in the journal.   */
	MsgSummaryCPUFile(   fid );          /* CPU usage measurements.          */

	if ( switches ) {
	  if ( strchr( switches, 's' ) ) {
	    ret = MsgRemoveSharedMemory( control->ProcessID );
	  }
	}
	ret = MsgJournalClose();
	return;
}




	void 	MsgInit( const char *switches /*  One-letter switch string:                    */
	                                      /*  "s"  shared msg -- default: not shared       */
	                                      /*  "h"  hard initialize -- default: hard        */
	                                      /*       initialize on first call, soft          */
	                                      /*       initialize afterwards                   */
	               , ... )
{
/* Description:  MSG package initializion.

	Shared msg:
	  Permits other processes to read and modify msg's prefix accounting.

	Hard Initialize:
	  Wipes out all msg prefix accounting;  starts with no prefixes defined.

	Soft Initialize:
	  Zeroes all msg prefix accounting, but retains all prefix definitions
	  and states.                                                                          */

	int ID;
	
	control->shmid = -1;                            /*  Indicate msg memory not (yet) shared.  */
	control->ProcessID = 0;
	if ( switches ) {
	  if ( strchr( switches, 'h' ) ) MsgInitialized = FALSE;  /*  Force hard initialization.   */
	}

	if ( MsgInitialized ) {   /* Already initialized -- don't hose the existing prefixes:  */
	  control->Nlookups = 0;   /* Count of all prefix (character-search) lookups.          */
	  for ( ID = 1;  ID <= control->Nprefixes;  ID++ ) {
	    MsgResetID( ID );     /* Reset counters, flags and limits to the defaults.         */
	  }
 	} else {                  /* Cold-start initialization -- get everything:              */
	  MsgInitialized = TRUE;
	  if ( gethostname( s1000, 1000) < 0 ) s1000[0] = 0;
	  MsgNodeNameSet( s1000 );
	  if ( switches ) {
	    if ( strchr( switches, 's' ) ) {
	      control->shmid = MsgShare();  /*  Share the msg memory.  */
	    }
	  }
	  JournalFILE = fopen( "/dev/null", "w" );  /* Ensure that this isn't NULL, or it'll start outputing to standard out.  */
	  control->Nprefixes    = 0;
	  control->Nclasses     = 0;
	  control->Nlookups     = 0;   /* Count of all prefix (character-search) lookups.      */
	  control->CountLimit   = 50;  /* Default for no-class messages.                       */
	  control->AlarmLevel   = 3;
	  control->TimeStampCPU = FALSE; /* If true, time-stamp on changed CPU time.           */
	  control->Active       = FALSE;
	  control->Counting     = FALSE;
	  control->Alarming     = FALSE;
	  control->Sorted       = FALSE;
	  MsgJournalOff();

/*	  Msg Summary defaults:                                                                */
	  MsgSetSummaryPageLength (   60 );   /* 60 lines per page.                            */
	  MsgSetSummaryModeActive(   TRUE  ); /* List active messages.                         */
	  MsgSetSummaryModeCounting( TRUE  ); /* List counting (but not displaying) messages.  */
	  MsgSetSummaryModeInactive( FALSE ); /* Do not list inactive messages.                */
	  MsgSetSummaryModeAborted(  TRUE );  /* List the aborted message.                     */

/*	  Define the predefined classes:                                                       */
	  MsgClassDefine( " ",   "Active", 50, 0 ); /* Null or blank class.                    */
	  MsgClassDefine( "A",   "Active",  0, 1 ); /* Abort message class (MSG abort).        */
	  MsgClassDefine( "B",   "Active",  0, 0 ); /* Bug message class.                      */
	  MsgClassDefine( "C", "Counting",  0, 0 ); /* Counting (CPU-measuring) class.         */
	  MsgClassDefine( "D",   "Active",  0, 0 ); /* Debug message class.                    */
	  MsgClassDefine( "E",   "Active", 20, 0 ); /* Error message class.                    */
	  MsgClassDefine( "F",   "Active",  0, 0 ); /* Fatal message class (application abort).*/
	  MsgClassDefine( "I",   "Active",  0, 0 ); /* Informative message class.              */
	  MsgClassDefine( "O",   "Active",  1, 0 ); /* Once-only message class.                */
	  MsgClassDefine( "T", "Inactive",  0, 0 ); /* Trace (silient debug) message class.    */
	  MsgClassDefine( "W",   "Active", 10, 0 ); /* Warning message class.                  */

	}

	CPUtime0 = MsgCPU();
	ELAtime0 = MsgTime();

	return;

}




	int	MsgJournalClose( void )
{
/* Description:  Close the MSG journal file.
	Call this subroutine to close the message journal file.
	Disables message-logging in the journal file.

    Returns:
	TRUE  for successful close of journal file.
	FALSE for close failure of journal file.                                    */

	static int id1 = 0;

	MsgJournalOff();                   /*  Make sure it's disabled.             */
	if ( JournalFILE )              {  /*  The journal file is open.            */
	  if ( fclose( JournalFILE ) ) {
	   Message( "MsgJournalClose-E1 Unable to close journal file.", &id1);
	    return FALSE;
	  }
	}
	return TRUE;
}




	int	MsgJournalEnabled() {
/* Description:  Return enabled-status for a the journal file.
    Returns:
	TRUE  for journal file enabled.
	FALSE for journal file disabled.                                    */
	return(JournalEnabled);
}


	FILE*	MsgJournalGet( void )
{
/* Description:  Returns the journal file descriptor.
*/
	return(JournalFILE);
}





	void	MsgJournalOff() {
/* Description:  Disable journal logging of messages.
	Call this subroutine to disable the message journal file.  */
	JournalEnabled = FALSE;
	return;
}


	void	MsgJournalOn() {
/* Description:  (Re)enable journal logging of messages.
	Call this subroutine to (Re)enable the message journal file.  */
	JournalEnabled = TRUE;
	return;
}


	int	MsgJournalOpen(
/*  Input:                                                                 */
	const char* FileName  ) /* The journal file-name                   */
{
/* Description:  Open a message-logging journal file
	and enable message logging to the journal file.

   Returns:
	TRUE if the file was successfully opened,
	FALSE if the file could not be opened.  */

	struct stat buf;

	if ( !MsgJournalClose() ) return( FALSE );  /*  Ensure no file is now open.  */

	JournalFILE = MsgFileOpen( FileName, "w" );
	if ( !JournalFILE ) return( FALSE );

	MsgJournalOn();
	return( TRUE );
}




	void	MsgJournalPage( void )
{
/* Description:  Send a form-feed to the journal file.
*/

	MsgToFileOut( "\f", JournalFILE );
	return;
}




	void	MsgMark(

/*  Inputs:                             */
	  const char *Prefix /* MSG prefix    */

/*  Input/Output:                       */
	, int *ID  )   /* Fast-reference MSG ID.  Uniquely assigned on first call */
{
/*   Description: Set the given Prefix's "Marked CPU time" to current.
	When this prefix occurs in a call to an MSG routine which does Msg accounting
	(eg, Message), and when that Prefix has had its "Marked CPU time" set (ie,
	to non-zero), that prefix's "total CPU time" is incremented by Current CPU
	time, less the "Marked CPU time".  The Marked CPU time is then zeroed.  */

	int i;
	int Active, Alarming, Counting;
	int LID;  /* Local copy of ID. */
	int CPUtime;

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}

/*	Check that the message-prefix is in the index, enter it
	in the index if necessary, and return its ID:               */
	MsgCheck( Prefix, &LID, &Active, &Alarming, &Counting );

	prefix[LID].Marked = TRUE;

	if ( Counting ) {  /* Counting is true only if found.  */
/*	  Prefix was found or defined, counting is enabled for it, and LID is now valid.  */
	  CPUtime = MsgCPU() - CPUtime0; /* Get "absolute" CPU time, since CPUtime0 was set. */
	  prefix[LID].CPUmark = CPUtime;
	}

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return;
}




	void	MsgNoCount(

/*  Input:                                         */
	const char *Prefix ) /* A message prefix.  */
{
/*  Description:  Disable Msg counting of a previously disabled prefix.
	          This routine affects only for prefixes which are
	          disabled, but still counting.                          */

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) {
/*	    Can't "no-count" an active or alarming message:  */
	    prefix[ID].Counting = prefix[ID].Active || prefix[ID].Alarming;
	  }
	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Counting = prefix[ID].Active || prefix[ID].Alarming;
	  }
	  control->Counting = control->Active || control->Alarming;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      prefix[ID].Counting = prefix[ID].Active || prefix[ID].Alarming;
	    }
	  }
	}

	return;
}




	void	MsgNodeNameGet(

/*  Output:                                                                        */
	char *NodeName )  /* Name of node, as will appear on the node-time stamp.  */
{
/*  Description:  Get the (prefiously specified) local (ASCII) node name for node-time
	          stamping of journal entries of message-occurrances.              */

	strncpy( NodeName, control->NodeName, NODE_NAME_MAXLEN+1 );

	return;
}




	void	MsgNodeNameSet(

/*  Input:                  */
	const char *NodeName )  /* Name of node, as will appear on the node-time stamp.  */
{
/*  Description:  Specify the local (ASCII) node name for node-time stamping
	          of journal entries of message-occurrances.  */

	strncpy( control->NodeName, NodeName, NODE_NAME_MAXLEN+1 );

	return;
}




	void	MsgPrefixGet(
/*   Inputs:                                                         */
	  const char *Prefix /* A message prefix                     */
/*  Outputs:                                                         */
	, int *Counts     /*  Count of prefix occurances.             */
	, int *CountLimit /*  Shut-off limit for count.               */
	, int *Level      /*  Alarm level of prefix.                  */
	, int *AbortLimit /*  Abort limit for count.                  */
	, int *Active     /*  TRUE/FALSE:  whether prefix is active   */
	, int *Counting   /*  TRUE/FALSE:  whether prefix is counting */
	, int *Alarming   /*  TRUE/FALSE:  whether prefix is alarming */
	, char State[9] )/*  8-character (plus null) state string    */
	/* (Active, Inactive, Counting, Alarming, !active!, Aborted) */
{
/*  Description:  Get the state of a prefix.
*/

	int ID;

	ID = 0; /*  Clear this to do a lookup-by-prefix.             */
/*	Check that the message-prefix is in the index, enter it
	in the index if necessary, and return its ID:               */
	MsgCheck( Prefix, &ID, Active, Alarming, Counting );
	if ( ID > 0 ) {
	  *Counts     = prefix[ID].Counts;
	  *CountLimit = prefix[ID].CountLimit;
	  *Level      = prefix[ID].AlarmLevel;
	  *AbortLimit = prefix[ID].AbortLimit;
/*	  Fill in "State":  */
	  MsgState(  *Counts, *CountLimit, *Level, *AbortLimit, *Active, *Counting, *Alarming, State );
	}

	return;
}




	void	MsgPrefixSet(
/*  Inputs:                                                          */
	  const char *Prefix /* A message prefix.                    */
	, int CountLimit /*  Shut-off limit for count.               */
	, int Level      /*  Alarm level of prefix.                  */
	, int AbortLimit /*  Abort limit for count.                  */
	, int Active     /*  TRUE/FALSE:  whether prefix is active   */
	, int Counting   /*  TRUE/FALSE:  whether prefix is counting */
	, int Alarming ) /*  TRUE/FALSE:  whether prefix is alarming */
{

/*  Description:  Set the state of a prefix.
*/

	int ID;
	int DUMactive;
	int DUMcounting;
	int DUMalarming;

	ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	Check that the message-prefix is in the index, enter it
	in the index if necessary, and return its ID:               */
	MsgCheck( Prefix, &ID, &DUMactive, &DUMalarming, &DUMcounting );
	if ( ID > 0 ) {
	  prefix[ID].CountLimit = CountLimit;
	  prefix[ID].AlarmLevel = Level;
	  prefix[ID].AbortLimit = AbortLimit;
	  prefix[ID].Active     = Active;
	  prefix[ID].Counting   = Counting;
	  prefix[ID].Alarming   = Alarming;
	  if ( Active ) prefix[ID].Counting = TRUE; /*  Must count if active.  */
	}
	return;
}




	void	MsgSetAbortLimit(

/*  Inputs:  */
	  const char *Prefix /* A message prefix. */
	, int Limit ) /* Maximum no. of times to display a message before aborting. */
{
/*  Description:  Set the abort limit for the message recognized by Prefix.
	Program termination results when the prefix's
	count has exceeded Limit.  Wildcards are permitted.
	The existing count-limit (after which message-display is disabled
	for Prefix), is set to the same value -- aborting msg calls
	should not be quiet without deliberate intervention (ie, re-specify
	the count-limit after this call to obtain silent aborts -- don't
	do it!).  */

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) {
	    prefix[ID].Active     = TRUE;
	    prefix[ID].Counting   = TRUE;
	    prefix[ID].CountLimit = Limit;
	    prefix[ID].AbortLimit = Limit;
	  }

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].Active     = TRUE;
	    prefix[ID].Counting   = TRUE;
	    prefix[ID].CountLimit = Limit;
	    prefix[ID].AbortLimit = Limit;
	  }
	  control->Active   = TRUE;
	  control->Counting = TRUE;
	  control->CountLimit = Limit;
	  control->AbortLimit = Limit;

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      prefix[ID].Active     = TRUE;
	      prefix[ID].Counting   = TRUE;
	      prefix[ID].CountLimit = Limit;
	      prefix[ID].AbortLimit = Limit;
	    }
	  }
	}

	return;
}




	int	MsgSetByCommand(

/*  Input:  */
	const char *Command ) /* Command-string describing features to be set. */
{
/*  Description:  Issue an ASCII command to msg.
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

	int done, error, justHelping;
	char *com;
	char *arg[7];
	char state[9];
	char c1000[1000];  /*  Some "scratch" string space.  */
	int counts, countLimit, level, abortLimit;
	int active, counting, alarming;
	int i;


/*	char *strtok (char *s1, const char *s2);  */

	strncpy( c1000, Command, 1000 );  /* make a modifiable copy, for strtok.  */

	error = FALSE;
	justHelping = FALSE;
	com = strtok( c1000, " \t\n\f");  /* separators: Space, tab, newline, formfeed */
	if ( !com ) {
	  error = TRUE;
	} else if ( !strcasecmp( com, "HELP" ) ) {
	  error = TRUE;
	  justHelping = TRUE;
	} else if ( !strcasecmp( com, "LIST" ) ) {
	  MsgSummaryFile( NULL );     /*  To standard out.  */
	  MsgSummaryFile( JournalFILE );
	  return(TRUE);
	} else if ( !strcasecmp( com, "CPU" ) ) {
	  MsgSummaryCPUFile( NULL );  /*  To standard out.  */
	  MsgSummaryCPUFile( JournalFILE );
	  return(TRUE);
	} else if ( !strcasecmp( com, "GETLINES" ) ) {
	  sprintf( m1000, "%d", control->SummaryPageLength );
	  MessageOut( m1000 );
	  return(TRUE);
	} else if ( !strcasecmp( com, "GETNODE" ) ) {
	  sprintf( m1000, "%s", control->NodeName );
	  MessageOut( m1000 );
	  return(TRUE);
	} else if ( !strcasecmp( com, "GETSHMID" ) ) {
	  sprintf( m1000, "%d", control->shmid );
	  MessageOut( m1000 );
	  return(TRUE);
	}

/*	Remaining commands require at least one additional argument:  */
	arg[0] = strtok( NULL, " =\t\n\f");  /* separators: Space, "=", tab, newline, formfeed */
	if ( !arg[0] ) {
	  error = TRUE;
	} else if ( !strcasecmp( com, "TIMESTAMP" ) ) {
	  if        ( !strcasecmp( arg[0], "CPU" ) ) {
	    MsgSetTimeStampCPU( TRUE );
	    return(TRUE);
	  } else if ( !strcasecmp( arg[0], "NOCPU" ) ) {
	    MsgSetTimeStampCPU( FALSE );
	    return(TRUE);
	  } else {
	    error = TRUE;
	  }
	} else if ( !strcasecmp( com, "LINES" ) ) {
	  MsgSetSummaryPageLength( atoi(arg[0]) );
	  return(TRUE);
	} else if ( !strcasecmp( com, "LOAD" ) ) {
	  return(MsgStateLoad( arg[0] ));
	} else if ( !strcasecmp( com, "STORE" ) ) {
	  return(MsgStateStore( arg[0] ));
	} else if ( !strcasecmp( com, "SETNODE" ) ) {
	  MsgNodeNameSet( arg[0] );
	  return(TRUE);
	}


/*	Multiple argument commands:  */
	done = FALSE;
	while ( !done && !error ) {

/*	  One-argument-at-a-time commands:  */
	  if        ( !strcasecmp( com, "NOALARM" ) ) {
	    MsgDisableAlarm( arg[0] );
	  } else if ( !strcasecmp( com, "ALARM" ) ) {
	    MsgEnableAlarm( arg[0] );
	  } else if ( !strcasecmp( com, "DISABLE" ) ) {
	    MsgDisable( arg[0] );
	  } else if ( !strcasecmp( com, "ENABLE" ) ) {
	    MsgEnable( arg[0] );
	  } else if ( !strcasecmp( com, "COUNT" ) ) {
	    MsgCount( arg[0] );
	  } else if ( !strcasecmp( com, "NOCOUNT" ) ) {
	    MsgNoCount( arg[0] );
	  } else if ( !strcasecmp( com, "DELETE" ) ) {
	    MsgDelete( arg[0] );
	  } else if ( !strcasecmp( com, "GET" ) ) {
	    MsgPrefixGet( arg[0], &counts, &countLimit, &level, &abortLimit
	                  , &active, &counting, &alarming, state );
	    sprintf( m1000, "%s %d %d %d %d %d %d %d %s"
	           , arg[0], counts, countLimit, level, abortLimit
	           , active, counting, alarming, state );
	    MessageOut( m1000 );
	  } else {

/*	    Two-arguments-at-a-time commands:  */
	    arg[1] = strtok( NULL, " =\t\n\f");  /* separators: Space, "=", tab, newline, formfeed */
	    if ( !arg[1] ) {
	      error = TRUE;
	    } else if ( !strcasecmp( com, "LEVEL" ) ) {
	      MsgSetLevel( arg[0], atoi( arg[1] ) );
	    } else if ( !strcasecmp( com, "LIMIT" ) ) {
	      MsgSetLimit( arg[0], atoi( arg[1] ) );
	    } else if ( !strcasecmp( com, "ABORT" ) ) {
	      MsgSetAbortLimit( arg[0], atoi( arg[1] ) );

	    } else {

/*	      Eight-arguments-at-a-time commands:  */
	      for (i=2; i<7; i++) {
	        arg[i] = strtok( NULL, " =\t\n\f");  /* separators: Space, "=", tab, newline, formfeed */
	        if ( !arg[i] ) error = TRUE;
	      }

	      if ( error ) {
	      } else if ( !strcasecmp( com, "SET" ) ) {
	        countLimit = atoi(arg[1]);
	        level      = atoi(arg[2]);
	        abortLimit = atoi(arg[3]);
	        active     = FALSE;
	        counting   = FALSE;
	        alarming   = FALSE;
	        if ( !strcasecmp( arg[4], "T" ) ) active   = TRUE;
	        if ( !strcasecmp( arg[5], "T" ) ) counting = TRUE;
	        if ( !strcasecmp( arg[6], "T" ) ) alarming = TRUE;
/*	        Or maybe they like "1" instead:  */
	        if ( !strcasecmp( arg[4], "1" ) ) active   = TRUE;
	        if ( !strcasecmp( arg[5], "1" ) ) counting = TRUE;
	        if ( !strcasecmp( arg[6], "1" ) ) alarming = TRUE;
	        MsgPrefixSet( arg[0], countLimit, level, abortLimit, active, counting, alarming );
	      } else {
	        error = TRUE;
	      }  /*  Eight-arguments-at-a-time commands */
	    }    /*  Two-arguments-at-a-time commands   */
	  }      /*  One-argument-at-a-time commands    */

	  if ( !error ) { /* Set up for next loop or be done: */
	    arg[0] = strtok( NULL, " =\t\n\f");  /* separators: Space, "=", tab, newline, formfeed */
	    if ( !arg[0] ) {
	      done = TRUE;
	    }
	  }

	}        /*  while ( !done && !error )          */

	if (error) {

	  sprintf( c1000, "MsgSetByCommand-E1  Unrecognized command [%s] from:\n%s\n", com, Command );
	  MessageOut( c1000 );
	  MessageOut( "\
  Legal commands are:\n\
     CPU,  GETLINES,  GETNODE,  GETSHMID,  HELP,  LIST    (no arguments)\n\
     LINES <value>,      TIMESTAMP <CPU or NOCPU>,    SETNODE <nodename>\n\
     LOAD  <filename>,   STORE  <filename>\n\
     ALARM, NOALARM, COUNT, NOCOUNT, DISABLE, ENABLE, DELETE, GET  (all with prefix list)\n\
     ABORT, LEVEL, LIMIT                                (with a <prefix>=<value> list)\n\
     SET <prefix>=<countLim>,<level>,<abortLim>,<active T/F>,<counting T/F>,<alarming T/F>");

	  if ( justHelping ) return(TRUE);
	  return(FALSE);

	}

	return(TRUE);
}




	int	MsgSetFromFile(
/*  Input:  */
	FILE  *stream )  /* Stream Descriptor for which a command (ASCII)
	                    file should be (already) open.  */
{
/*  Description:  ASCII-file command interface to control MSG.
	Reads lines from stream and interprets them as MsgSetByCommand commands
	until either an <EOF> or a line containing MSG_EXIT is encountered.

  Returns:
	TRUE for successful completion of command,
	FALSE for failure.                                                    */

	while ( fgets( m1000, 1000, stream ) ) {
	  if ( !strcasecmp( m1000, "MSG_EXIT" ) ) return(TRUE);
	  if (!MsgSetByCommand( m1000 )) return(FALSE);   /*  Set the feature.  */
	}
	return(TRUE);
}




	void	MsgSetLevel(

/*  Inputs:  */
	  const char *Prefix  /* A message prefix. */
	, int         Level ) /* Level of alarm.  */
{
/*  Description:  Set alarm level for a prefix.
*/

	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) {
	    prefix[ID].AlarmLevel = Level;
	  }

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    prefix[ID].AlarmLevel = Level;
	  }
	  control->AlarmLevel = Level;

	} else {
/*	  Index to last character before the wildcard:  */
	   
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      prefix[ID].AlarmLevel = Level;
	    }
	  }
	}

	return;
}




	void	MsgSetLimit(

/*  Inputs:  */
	  const char *Prefix  /* A message prefix. */
	, int         Limit ) /* Maximum no. of (additional) times to display a message. */
{
/*  Description:  Set auto-disable count limit for a prefix.
	Displays of the specified message are disabled once its
	count has exceeded LIMIT.  Wildcards are permitted.  */


	int ID;
	int i;
	int Active, Alarming, Counting;
	void *wildCardPt;

/*	First check to see if there is a wildcard in the specified prefix: */

	wildCardPt = strstr( Prefix, "*" );

	if        ( !wildCardPt )          {  /*  No wildcard found.  */
	  ID = 0;  /* Clear this to do a lookup-by-prefix.            */
/*	  Check that the message-prefix is in the index, enter it
	  in the index if necessary, and return its ID:               */
	  MsgCheck( Prefix, &ID, &Active, &Alarming, &Counting );
	  if ( ID > 0 ) {
	    if ( Limit == 0 ) {
	      prefix[ID].CountLimit = 0;
	    } else {  /*  Set limit to be the current number of counts plus Limit:  */
	      prefix[ID].CountLimit = prefix[ID].Counts + Limit;
	    }
	  }

	} else if ( wildCardPt == Prefix ) {  /*  Same pointer?  */

	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( Limit == 0 ) {
	      prefix[ID].CountLimit = 0;
	    } else {  /*  Set limit to be the current number of counts plus Limit:  */
	      prefix[ID].CountLimit = prefix[ID].Counts + Limit;
	    }
	  }
	  control->CountLimit = Limit;  /*  This affects new messages only, (ie, counts=0).  */

	} else {
/*	  Index to last character before the wildcard:  */
	  i = ( (int)wildCardPt - 1 ) - (int)Prefix;
	  for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	    if ( !strcmp( ( ((char*)(int)wildCardPt-1) ), &prefix[ID].Prefix[i] ) ) {
	      if ( Limit == 0 ) {
	        prefix[ID].CountLimit = 0;
	      } else {  /*  Set limit to be the current number of counts plus Limit:  */
	        prefix[ID].CountLimit = prefix[ID].Counts + Limit;
	      }
	    }
	  }
	}

	return;
}




	void	MsgSetSummaryModeAborted(

/*  Input:  */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE.  */

{
/*  Description:  Set the MSG summary mode for "Aborted" messages.
*/

	control->SummaryModeAborted = Mode;

	return;
}




	void	MsgSetSummaryModeActive(

/*  Input:                                                         */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE. */
{
/*  Description:  Set the MSG summary mode for "Active" messages.
*/

	control->SummaryModeActive = Mode;

	return;
}




	void	MsgSetSummaryModeCounting(

/*  Input:  */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE.  */
{
/*  Description:  Set the MSG summary mode for "Counting" messages.
*/

	control->SummaryModeCounting = Mode;

	return;
}




	void	MsgSetSummaryModeInactive(

/*  Input:  */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE. */
{
/*  Description:  Set the MSG summary mode for "Inactive" messages.
*/

	control->SummaryModeInactive = Mode;

	return;
}




	void	MsgSetSummaryPageLength(

/*  Input:  */
	int PageLength ) /* Length of summary page (lines).  */
{
/*  Description:  Set the MSG summary table page length.
*/

	control->SummaryPageLength = PageLength;

	return;
}




	void	MsgSetTimeStampCPU(

/*  Input:   */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE. */
{
/*  Description:  Set the MSG time-stamp mode for stamping CPU changes.
*/

	control->TimeStampCPU = Mode;

	return;
}




	void	MsgState(
/*  Inputs:                                                          */
	  int Counts     /*  Count of prefix occurances.             */
	, int CountLimit /*  Shut-off limit for count.               */
	, int Level      /*  Alarm level of prefix.                  */
	, int AbortLimit /*  Abort limit for count.                  */
	, int Active     /*  TRUE/FALSE:  whether prefix is active   */
	, int Counting   /*  TRUE/FALSE:  whether prefix is counting */
	, int Alarming   /*  TRUE/FALSE:  whether prefix is alarming */
/*  Output:                                                          */
	, char State[9] )/*  8-character (plus null) state string    */
{
/*  Description:  Set the ASCII state from the msg flags.
*/

/*	Determine the state:                                         */
	if ( AbortLimit <= 0 ) {
	  State[0] = 0;
	} else if ( Counts >= AbortLimit ) {
/*	  This message caused a program termination -- should only be one!  */
	  strcpy( State, " Aborted" );
	} else {
	  State[0] = 0;
	}

	if (State[0]) {  /*  (if not NULL, that is.)  */
	} else if ( Active ) {
	  if ( Alarming ) {
	    strcpy( State, "!Active!" );
	  } else {
	    strcpy( State, " Active" );
	  }
	} else if ( Alarming ) {
	  strcpy( State, "Alarming" );
	} else if ( Counting ) {
	  strcpy( State, "Counting" );
	} else {
	  strcpy( State, "Inactive" );
	}

	return;
}




	int	MsgStateLoad(
/*  Inputs:                                               */
	const char* fileName ) /* The msg-state file-name */
{
/* Description:  Open a previously written msg state file and set msg's state.

   Returns:
	TRUE for successful completion of command,
	FALSE for failure.                                                    */

	int ID, Iclass;
	float Version;
	FILE *fid;
	char file[500];

	int Trace = FALSE;
	static idT1 = 0;

	Trace = MsgEnabled( "MsgStateLoad-T1", &idT1 );

	if ( strlen(fileName) <= 0 ) {
	  strcpy( file, "default.msg" );
	} else {
	  strcpy( file, fileName );
	}

	fid = fopen( file, "r" );
	if ( !fid ) {
	  static int id = 0;
	  sprintf( m1000, "MsgStateLoad-E1  Failed to open file [%s] for loading of msg's state.", file );
	  Message( m1000, &id );
	  return(FALSE);
	}

	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%f", &Version );
	if ( Trace ) {
	  sprintf( m1000, "MsgStateLoad-T1 Version:%f  VERSION:%f  line:\n[%s]", Version, VERSION, s1000 );
	  Message( m1000, &idT1 );
	}

	if ( Version > VERSION ) {
	  sprintf( m1000,
"MsgStateLoad-E1  Msg state file [%s] version:%f is later than compiled version:%f.", file, Version, VERSION );
	  MessageOut( m1000 );
	  MessageOut(
"                 Msg state not loaded." );
	  return(FALSE);
	}


	fgets( control->NodeName, NODE_NAME_MAXLEN+1, fid ); /* Application-specified node-name.  */
	MsgTruncate( control->NodeName, NODE_NAME_MAXLEN );
	if ( Trace ) {
	  sprintf( m1000, "                NodeName[%s]", control->NodeName );
	  MessageOut( m1000 );
	}

	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d", &JournalEnabled );     /*  Journal-enabled control-flag.     */
	if ( Trace ) {
	  sprintf( m1000, "          JournalEnabled[%d] line:\n[%s]", JournalEnabled, s1000 );
	  MessageOut( m1000 );
	}

	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d", &control->TimeStampCPU );       /*  Time-stamp mode-select.           */
	if ( Trace ) {
	  sprintf( m1000, "            TimeStampCPU[%d] line:\n[%s]", control->TimeStampCPU, s1000 );
	  MessageOut( m1000 );
	}

	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d", &control->Sorted );             /*  Whether ID list is sorted.        */
	if ( Trace ) {
	  sprintf( m1000, "                  Sorted[%d] line:\n[%s]", control->Sorted, s1000 );
	  MessageOut( m1000 );
	}

/*	msg Summary output features:      */
	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d", &control->SummaryPageLength );
	if ( Trace ) {
	  sprintf( m1000, "       SummaryPageLength[%d] line:\n[%s]", control->SummaryPageLength, s1000 );
	  MessageOut( m1000 );
	}

	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d%d%d%d", &control->SummaryModeActive  , &control->SummaryModeCounting
	                       , &control->SummaryModeInactive, &control->SummaryModeAborted );
	if ( Trace ) {
	  sprintf( m1000, "  SummaryModeActive[%d] Counting[%d] Inactive[%d] Aborted[%d]  line:\n[%s]"
	         , control->SummaryModeActive
	         , control->SummaryModeCounting
	         , control->SummaryModeInactive
	         , control->SummaryModeAborted
	         , s1000 );
	  MessageOut( m1000 );
	}

/*	msg new-definition defaults:      */
	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d%d%d%d%d", &control->Active, &control->Counting, &control->Alarming
	                         , &control->CountLimit, &control->AlarmLevel );
	if ( Trace ) {
	  sprintf( m1000, "  Active[%d] Counting[%d] Alarming[%d] CountLimit[%d] AlarmLevel[%d] line:\n[%s]"
	         , control->Active
	         , control->Counting
	         , control->Alarming
	         , control->CountLimit
	         , control->AlarmLevel
	         , s1000 );
	  MessageOut( m1000 );
	}

/*	General counters -- how many so far:  */
	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d", &control->Nlookups ); /* msg count of total slow prefix-lookups (ie, character-searches) */
	if ( Trace ) {
	  sprintf( m1000, "                Nlookups[%d] line:\n[%s]", control->Nlookups, s1000 );
	  MessageOut( m1000 );
	}

	fgets( s1000, 999, fid );
	MsgTruncate( s1000, 1000 );
	sscanf( s1000, "%d%d", &control->Nprefixes, &control->Nclasses );
	if ( Trace ) {
	  sprintf( m1000, "   Nprefixes[%d]  MAXPREFIXES[%d] Nclasses[%d] MAXCLASSES[%d] line:\n[%s]"
	         , control->Nprefixes, MAXPREFIXES, control->Nclasses, MAXCLASSES, s1000 );
	  MessageOut( m1000 );
	}

	if ( control->Nprefixes > MAXPREFIXES ) {
	  sprintf( m1000,
"MsgStateLoad-E2  Msg state file [%s] has more prefixes [%d] than permitted by me [%d]."
	         , file, control->Nprefixes, MAXPREFIXES );
	  MessageOut( m1000 );
	  MessageOut(
"                 Msg cleared." );
	  control->Nprefixes = 0;
	  return(FALSE);
	} else if ( control->Nclasses > MAXCLASSES ) {
	  sprintf( m1000,
"MsgStateLoad-E2  Msg state file [%s] has more classes [%d] than permitted by me [%d]."
	         , file, control->Nclasses, MAXCLASSES );
	  MessageOut( m1000 );
	  MessageOut(
"                 Msg cleared." );
	  control->Nprefixes = 0;
	  return(FALSE);
	}

/*	The Prefixes' data structure:  */
	for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {

	  fgets( prefix[ID].Prefix, PREFIX_MAXLEN+1, fid );
	  MsgTruncate( prefix[ID].Prefix, PREFIX_MAXLEN );
	  if ( Trace ) {
	    sprintf( m1000, "   Prefix[%s] ID[%d]", prefix[ID].Prefix, ID );
	    MessageOut( m1000 );
	  }

	  fgets( prefix[ID].Sample, SAMPLE_MAXLEN+1, fid );
	  MsgTruncate( prefix[ID].Sample, SAMPLE_MAXLEN );
	  if ( Trace ) {
	    sprintf( m1000, "   Sample[%s]", prefix[ID].Sample );
	    MessageOut( m1000 );
	  }

	  fgets( s1000, 999, fid );
	  MsgTruncate( s1000, 1000 );
	  sscanf( s1000, "%d%d%d%d"
          , &prefix[ID].SID, &prefix[ID].Iclass, &prefix[ID].Counts, &prefix[ID].Lookups );
	  if ( Trace ) {
	    sprintf( m1000, "   SID[%d] Iclass[%d] Counts[%d] Lookups[%d] line:\n[%s]"
	           , prefix[ID].SID, prefix[ID].Iclass, prefix[ID].Counts, prefix[ID].Lookups, s1000 );
	    MessageOut( m1000 );
	  }

	  fgets( s1000, 999, fid );
	  MsgTruncate( s1000, 1000 );
	  sscanf( s1000, "%d%d%d"
	  , &prefix[ID].CPUmark   , &prefix[ID].CPUdelta  , &prefix[ID].CPUtotal );
	  if ( Trace ) {
	    sprintf( m1000, "   CPUmark[%d] CPUdelta[%d] CPUtotal[%d] line:\n[%s]"
	           , prefix[ID].CPUmark, prefix[ID].CPUdelta, prefix[ID].CPUtotal, s1000 );
	    MessageOut( m1000 );
	  }

	  fgets( s1000, 999, fid );
	  MsgTruncate( s1000, 1000 );
	  sscanf( s1000, "%d%d%d"
	  , &prefix[ID].CountLimit, &prefix[ID].AbortLimit, &prefix[ID].AlarmLevel );
	  if ( Trace ) {
	    sprintf( m1000, "   CountLimit[%d] AbortLimit[%d] AlarmLevel[%d] line:\n[%s]"
	           , prefix[ID].CountLimit, prefix[ID].AbortLimit, prefix[ID].AlarmLevel, s1000 );
	    MessageOut( m1000 );
	  }

	  fgets( s1000, 999, fid );
	  MsgTruncate( s1000, 1000 );
	  sscanf( s1000, "%d%d%d", &prefix[ID].Active, &prefix[ID].Counting, &prefix[ID].Alarming );
	  if ( Trace ) {
	    sprintf( m1000, "   Active[%d] Counting[%d] Alarming[%d] line:\n[%s]"
	           , prefix[ID].Active, prefix[ID].Counting, prefix[ID].Alarming, s1000 );
	    MessageOut( m1000 );
	  }
	}

	for ( Iclass = 1;  Iclass <= control->Nclasses; Iclass++ ) {
	  fgets( msgClass[Iclass].Class, CLASS_MAXLEN+1, fid );
	  MsgTruncate( msgClass[Iclass].Class, CLASS_MAXLEN );

	  if ( Trace ) {
	    sprintf( m1000, "   Class[%s] Iclass[%d]", msgClass[Iclass].Class, Iclass );
	    MessageOut( m1000 );
	  }

	  fgets( s1000, 999, fid );
	  MsgTruncate( s1000, 1000 );
	  sscanf( s1000, "%d%d%d"
          , &msgClass[Iclass].Active    , &msgClass[Iclass].Counting  , &msgClass[Iclass].Alarming );
	  if ( Trace ) {
	    sprintf( m1000, "   Active[%d] Counting[%d] Alarming[%d] line:\n[%s]"
	           , msgClass[Iclass].Active, msgClass[Iclass].Counting, msgClass[Iclass].Alarming, s1000 );
	    MessageOut( m1000 );
	  }

	  fgets( s1000, 999, fid );
	  MsgTruncate( s1000, 1000 );
	  sscanf( s1000, "%d%d%d"
          , &msgClass[Iclass].CountLimit, &msgClass[Iclass].AbortLimit, &msgClass[Iclass].AlarmLevel );
	  if ( Trace ) {
	    sprintf( m1000, "   CountLimit[%d] AbortLimit[%d] AlarmLevel[%d] line:\n[%s]"
                   , msgClass[Iclass].CountLimit, msgClass[Iclass].AbortLimit, msgClass[Iclass].AlarmLevel, s1000 );
	    MessageOut( m1000 );
	  }
	}

	fclose( fid );


	return(TRUE);
}




	int	MsgStateStore(
/*  Inputs:                                         */
	const char* FileName ) /* The msg-state file-name */
{
/* Description:  Write an msg state file describing msg's state.

   Returns:
	TRUE for successful completion of command,
	FALSE for failure.                                                    */

	int ID, Iclass;
	FILE *fid;
	char file[500];

	if ( strlen(FileName) <= 0 ) {
	  strcpy( file, "default.msg" );
	} else {
	  strcpy( file, FileName );
	}

	fid = fopen( file, "w" );
	if ( !fid ) {
	  static int id = 0;
	  sprintf( m1000, "MsgStateStore-E1  Failed to open file [%s] for storing of msg's state.", file );
	  Message( m1000, &id );
	  return(FALSE);
	}

	fprintf( fid, "%f  VERSION\n", VERSION );

	fprintf( fid, "%s\n"                , control->NodeName );          /*  Application-specified node-name.  */
	fprintf( fid, "%d  JournalEnabled\n", JournalEnabled );             /*  Journal-enabled flag.             */
	fprintf( fid, "%d  TimeStampCPU\n"  , control->TimeStampCPU );      /*  Time-stamp mode-select.           */
	fprintf( fid, "%d  Sorted\n"        , control->Sorted );            /*  Whether ID list is sorted.        */

/*	msg Summary output features:      */
	fprintf( fid, "%d  SummaryPageLength\n", control->SummaryPageLength );
	fprintf( fid, "%d %d %d %d  ModeActive, ModeCounting, ModeInactive, ModeAborted\n"
	            , control->SummaryModeActive  , control->SummaryModeCounting
	            , control->SummaryModeInactive, control->SummaryModeAborted );

/*	msg new-definition defaults:      */
	fprintf( fid, "%d %d %d %d %d  Active, Counting, Alarming, CountLimit, AlarmLevel\n"
	            , control->Active, control->Counting, control->Alarming
	            , control->CountLimit, control->AlarmLevel );

/*	General counters -- how many so far:  */
	fprintf( fid, "%d  Nlookups\n", control->Nlookups ); /* msg count of total slow prefix-lookups (ie, character-searches) */
	fprintf( fid, "%d %d  Nprefixes, Nclasses\n", control->Nprefixes, control->Nclasses );

/*	The Prefixes' data structure:  */
	for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	  fprintf( fid, "%s\n", prefix[ID].Prefix );
	  fprintf( fid, "%s\n", prefix[ID].Sample );
	  fprintf( fid, "  %d %d %d %d  SID, Iclass, Counts, Lookups\n"
                 , prefix[ID].SID, prefix[ID].Iclass, prefix[ID].Counts, prefix[ID].Lookups );
	  fprintf( fid, "  %d %d %d  CPUmark, CPUdelta, CPUtotal\n"
	         , prefix[ID].CPUmark   , prefix[ID].CPUdelta  , prefix[ID].CPUtotal );
	  fprintf( fid, "  %d %d %d  CountLimit, AbortLimit, AlarmLevel\n"
	         , prefix[ID].CountLimit, prefix[ID].AbortLimit, prefix[ID].AlarmLevel );
	  fprintf( fid, "  %d %d %d  Active, Counting, Alarming\n"
	         , prefix[ID].Active, prefix[ID].Counting, prefix[ID].Alarming );
	}

	for ( Iclass = 1;  Iclass <= control->Nclasses; Iclass++ ) {
	  fprintf( fid, "%s\n", msgClass[Iclass].Class );
	  fprintf( fid, "  %d %d %d  Active, Counting, Alarming\n"
                 , msgClass[Iclass].Active    , msgClass[Iclass].Counting  , msgClass[Iclass].Alarming );
	  fprintf( fid, "  %d %d %d  CountLimit, AbortLimit, AlarmLevel\n"
                 , msgClass[Iclass].CountLimit, msgClass[Iclass].AbortLimit, msgClass[Iclass].AlarmLevel );
	}

	fclose( fid );


	return(TRUE);
}




	void	MsgStateZero( void )
{
/* Description:  "Zero" msg's state -- zero all message counts.
*/

	int ID;

	for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	  prefix[ID].Counts   = 0; /*  Count of total occurances.                                  */
	  prefix[ID].Lookups  = 0; /*  Count of slow ASCII lookups.                                */
	  prefix[ID].CPUmark  = 0; /*  CPU usage "Mark", set at call to MsgMark.                   */
	  prefix[ID].CPUdelta = 0; /*  Most recent CPU-usage between calls to MsgMark and MsgIncr. */
	  prefix[ID].CPUtotal = 0; /*  Sum of all CPU-usages between calls to MsgMark and MsgIncr. */
	}
	return;
}




	void	MsgSummaryFile(

/*  Input: */
	FILE *fid )  /*  File descriptor of opened file, to write summary to.  */
{
/*  Description:  Generate an MSG summary table in a file.
*/

	MsgSummaryEventFile( fid, 0 );  /* "0" ==> no column of normalized entries. */

	return;
}




	void	MsgSummaryCPUFile(

/*  Input:  */
	FILE *fid )  /*  File descriptor of opened file, to write summary to.
	                 fid = 0  directs results to standard out.             */
{
/*  Description:  Generate a CPU-usage MSG-summary table.
	Output a CPU-usage summary of the MSG-handled & marked messages
	on the specified file (fid).  "Marked" messages are messages whose occurances
	were preceded, one-for-one, by calls to MsgMark, with the same
	MSG prefix.  */

	int ID;

	char *truncate, *truncate1, *truncate2, *truncate3;

	char msg[   SUMMARY_WIDTH+1];
	char Ghost[ SUMMARY_WIDTH+1];
	char Sample[SAMPLE_MAXLEN+1];

	char Cavg[16];
	char Ctot[16];
	int i;
	int N;
	int SID;
	int PageLine, EPT;
	float CPUavg, CPUtot, TicksPerSecond;
	int ListThisMessage;


	TicksPerSecond = (float) MsgTPS() ;   /*  Get platform-dependent ticks-per-second. */

	MsgSort();

	if (control->Nprefixes <= 0) {  /*  No messages.  */
	  sprintf( s1000, "\f\
             --- CPU Usage Summary --- (%-23.23s)\n\
         ---------  No Measurements  ----------\n", MsgCtime() );
	  MsgToFileOut( s1000, fid );
	}
	  
	PageLine = 0;
	for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	  SID = prefix[ID].SID;  /*  Sorted index map */
	  if ( PageLine == 0 ) {     /*  Do the header:  */

	    sprintf( s1000, "\f\n\
             --- CPU Usage Summary --- (%-23.23s)\n\
Message-Prefix & Truncated Sample of last occurance              Total CPU Usage     Avg CPU Usage       Counts   Lookups\n"
	                , MsgCtime() );
	    MsgToFileOut( s1000, fid );

	    PageLine = 2;
	  }

	  strncpy( Sample, prefix[SID].Sample, SAMPLE_MAXLEN+1 );

/*	  Sometimes a \n (newline) character is embedded in a message, or even a \f, \b or \0:      */
	  truncate1 = strstr( Sample, "\n" );
	  truncate2 = strstr( Sample, "\f" );
	  truncate3 = strstr( Sample, "\b" );
/*	  If not found, set each pointer all the way "right":  */
	  if ( !truncate1 ) truncate1 = &Sample[SAMPLE_MAXLEN];
	  if ( !truncate2 ) truncate2 = &Sample[SAMPLE_MAXLEN];
	  if ( !truncate3 ) truncate3 = &Sample[SAMPLE_MAXLEN];
	  N = (int)truncate1;
	  if ( N > (int)truncate2 ) N = (int)truncate2;
	  if ( N > (int)truncate3 ) N = (int)truncate3;
	  truncate = (char*)N;  /*  leftmost is the one.  */
	  *truncate = 0;     /*  Guarrantee NULL-term, maybe truncate if \n, \f or \b is in the sample string.  */

	  MsgClean( SAMPLE_MAXLEN, Sample );      /*  Clean out any non-printables.  */
	  EPT = MsgLNB( Sample, SAMPLE_MAXLEN );  /*  Find index to last non-blank.  */
	  for (  i = EPT+1;  i < SAMPLE_MAXLEN;  Sample[i++]=' ' );
	  Sample[SAMPLE_MAXLEN] = 0;  /* NULL termination -- note that Sample is defined [...MAXLEN+1]  */

	  if ( EPT < (SAMPLE_MAXLEN-7) ) {               /* More than 6 blanks.      */
	    for ( i=SAMPLE_MAXLEN-3; i>=EPT+2; i-=2 ) {  /* Append alternating dots. */
	      Sample[i] = '.';
	    }
	  }

	  if        ( prefix[SID].Counts   <= 0 ) {  /* This one never happened.    */
	    ListThisMessage = FALSE;
	  } else if ( !prefix[SID].Marked ) {        /* This one never got marked.  */
	    ListThisMessage = FALSE;
	  } else {                                              /* List this one.              */
	    ListThisMessage = TRUE;
	  }


	  if ( ListThisMessage ) {  /* List only those selected by the mode flags:  */

	    CPUtot = ( (float) prefix[SID].CPUtotal ) / TicksPerSecond;
	    CPUavg = CPUtot / ( (float) prefix[SID].Counts );

	    if ( CPUtot > 1000000. ) {
	      sprintf( Ctot, "%15.7e", CPUtot );
	    } else if ( CPUtot < 0.001 ) {
	      sprintf( Ctot, "%15.7e", CPUtot );
	    } else {
	      sprintf( Ctot, "%15.6f", CPUtot );
	    }

	    if ( CPUavg > 1000000. ) {
	      sprintf( Cavg, "%15.7e", CPUavg );
	    } else if ( CPUavg < 0.001 ) {
	      sprintf( Cavg, "%15.7e", CPUavg );
	    } else {
	      sprintf( Cavg, "%15.6f", CPUavg );
	    }

	    sprintf( s1000, "%-64.64s|%-15.15s  |%-15.15s  %10d %10d"
	           , Sample, Ctot, Cavg, prefix[SID].Counts, prefix[SID].Lookups );
	    EPT = MsgLNB( s1000, SUMMARY_WIDTH );  /*Find last non-blank.  */
	    for (  i = EPT+1;  i < SUMMARY_WIDTH;  s1000[i]=' ', Ghost[i++]=' ' );
	    s1000[SUMMARY_WIDTH] = 0;  /* NULL termination */
	    Ghost[SUMMARY_WIDTH] = 0;  /* NULL termination */

	    for ( i=SAMPLE_MAXLEN-1; i<=EPT-2; i+=2 ) {  /* Fill in with alternating dots. */
	      if ( !strncmp( "   ", &s1000[i-1], 3 ) ) {  /* Three blanks in a row:  */
	        s1000[i] = '.';
	        Ghost[i] = '.';
	      }
	    }
	    for ( i=SAMPLE_MAXLEN+1; i<=EPT-4; i+=2 ) {  /* Check for lone (inserted) dots & remove them.  */
	      if ( !strncmp( "  .  ", &Ghost[i-2], 5 ) ) {
	        s1000[i] = ' ';
	      }
	    }
	    MsgToFileOut( s1000, fid );

	    PageLine += 1;
	    if ( control->SummaryPageLength <= 0 ) {  /*  Do no more headers.  */
	    } else if ( PageLine >= control->SummaryPageLength) {
	      PageLine = 0;   /*  Start a new page.  */
	    }

	  }  /*  if ( ListThisMessage )                           */

	}    /*  for ( ID = 1;  ID <= control->Nprefixes; ID++ )   */

	if ( ( JournalEnabled ) && ( fid ) ) fflush( fid );
	return;
}




	void	MsgSummaryEventFile(

/*  Inputs:                                                                       */
	  FILE *fid     /*  File descriptor of opened file, to write summary to.
	                    fid = 0  directs results to standard out.             */
	, int Nevents ) /* Number of events by which to normalize.                */
{
/*  Description:  Generate an MSG summary table with a frequency column in a file.
	Output a summary of the MSG-handled messages to the specified file descriptor (fid).
	Include a column of normalized occurances of each message,
	which consists of a column of the number of occurances of each
	message, divided by Nevents.  If Nevents is not positive, then the
	column is omitted.  */

	int ID;

	char *truncate, *truncate1, *truncate2, *truncate3;

	char msg[   SUMMARY_WIDTH+1];
	char Ghost[ SUMMARY_WIDTH+1];
	char Sample[SAMPLE_MAXLEN+1];

	char Cavg[16];
	char Ctot[16];
	char State[9];
	int i;
	int N;
	int SID;
	int PageLine, EPT;
	float CPUavg, CPUtot, TicksPerSecond;
	float Fraction;
	int ListThisMessage;
	int Active, Alarming, Counting;
	int Counts, CountLimit, Level, AbortLimit;


	TicksPerSecond = (float) MsgTPS();   /*  Get platform-dependent ticks-per-second. */

	MsgSort();

	if (control->Nprefixes <= 0) {  /*  No messages.  */
	  sprintf( s1000, "\f\n\
             --- Message Accounting Summary --- (%-23.23s)\n\
         ---------  No Messages  ----------\n", MsgCtime() );
	  MsgToFileOut( s1000, fid );
	}

	PageLine = 0;
	for ( ID = 1;  ID <= control->Nprefixes; ID++ ) {
	  SID = prefix[ID].SID;  /*  Sorted index map */
	  if ( PageLine != 0 ) {     /*  Don't do the header:  */
	  } else if ( Nevents <= 0 ) {  /*  No fractional occurances:  */

	    if ( AppendReturn ) {
	      sprintf( s1000, "\f\n\
             --- Message Accounting Summary --- (%-23.23s)\n\
Message-Prefix & Truncated Sample of last occurance                  Counts      Limit      Lookups  AbortLimit    State\r\n"
	                , MsgCtime() );
	    } else {
	      sprintf( s1000, "\f\n\
             --- Message Accounting Summary --- (%-23.23s)\n\
Message-Prefix & Truncated Sample of last occurance                  Counts      Limit      Lookups  AbortLimit    State\n"
	                , MsgCtime() );
	    }
	    MsgToFileOut( s1000, fid );

	    PageLine = 2;
	  } else {

	    if ( AppendReturn ) {
	      sprintf( s1000, "\f\n\
             --- Message Accounting Summary --- (%-23.23s)\n\
Message-Prefix & Truncated Sample of last occurance                  Counts   Counts/evt      Limit    Lookups  AbortLimit    State\
\r\n"                , MsgCtime() );
	    } else {
	      sprintf( s1000, "\f\n\
             --- Message Accounting Summary --- (%-23.23s)\n\
Message-Prefix & Truncated Sample of last occurance                  Counts   Counts/evt      Limit    Lookups  AbortLimit    State\
\n"                , MsgCtime() );
	    }
	    MsgToFileOut( s1000, fid );

	    PageLine = 2;
	  }

	  strncpy( Sample, prefix[SID].Sample, SAMPLE_MAXLEN+1 );

/*	  Sometimes a \n (newline) character is embedded in a message, or even a \f, \b or \0:   */
	  truncate1 = strstr( Sample, "\n" );
	  truncate2 = strstr( Sample, "\f" );
	  truncate3 = strstr( Sample, "\b" );
/*	  If not found, set each pointer all the way "right":  */
	  if ( !truncate1 ) truncate1 = &Sample[SAMPLE_MAXLEN];
	  if ( !truncate2 ) truncate2 = &Sample[SAMPLE_MAXLEN];
	  if ( !truncate3 ) truncate3 = &Sample[SAMPLE_MAXLEN];
	  N = (int)truncate1;
	  if ( N > (int)truncate2 ) N = (int)truncate2;
	  if ( N > (int)truncate3 ) N = (int)truncate3;
	  truncate = (char*)N;  /*  leftmost is the one.  */
	  *truncate = 0;   /*  Guarrantee NULL-term, maybe truncate if \n, \f or \b is in the sample string.  */

	  MsgClean( SAMPLE_MAXLEN, Sample );      /*  Clean out any non-printables.  */
	  EPT = MsgLNB( Sample, SAMPLE_MAXLEN );  /*  Find index to last non-blank.  */
	  for (  i = EPT+1;  i < SAMPLE_MAXLEN;  Sample[i++]=' ' );
	  Sample[SAMPLE_MAXLEN] = 0;  /* NULL termination -- note that Sample is defined [...MAXLEN+1]  */

	  if ( EPT < (SAMPLE_MAXLEN-7) ) {               /* More than 6 blanks.      */
	    for (  i = SAMPLE_MAXLEN-3;  i >= EPT+2;  i -= 2  ) {  /* Append alternating dots. */
	      Sample[i] = '.';
	    }

	  }

	  Active     = prefix[SID].Active;
	  Counting   = prefix[SID].Counting;
	  Alarming   = prefix[SID].Alarming;
	  Counts     = prefix[SID].Counts;
	  CountLimit = prefix[SID].CountLimit;
	  Level      = prefix[SID].AlarmLevel;
	  AbortLimit = prefix[SID].AbortLimit;
/*	  Fill in "State":  */
	  MsgState(  Counts, CountLimit, Level, AbortLimit, Active, Counting, Alarming, State );

	  ListThisMessage = FALSE;
	  if ( prefix[SID].AbortLimit <= 0 ) {
	  } else if ( prefix[SID].Counts >= prefix[SID].AbortLimit ) {
	    ListThisMessage |= control->SummaryModeAborted;
	  }
	  if ( prefix[SID].Active ) {
	    ListThisMessage |= control->SummaryModeActive;
	  }
	  if ( prefix[SID].Alarming ) {
	    ListThisMessage |= control->SummaryModeActive;
	  }
	  if ( prefix[SID].Counting ) {
	    ListThisMessage |= control->SummaryModeCounting;
	  } else {
	    ListThisMessage |= control->SummaryModeInactive;
	  }

	  if ( ListThisMessage ) {  /* List only those selected by the mode flags:  */

	    for ( i=0;  i<8;  i++ ) {  /*  State is output one char at a time, always 8, so eliminate a term. NULL.  */
	      if ( State[i] == 0 ) State[i] = ' ';
	    }

	    if ( Nevents <= 0 ) {
	      if ( AppendReturn ) {
	        sprintf( s1000, "%-64.64s|%10d %10d   %10d %11d %c%c%c%c%c%c%c%c\r"
	             , Sample, prefix[SID].Counts
	             , prefix[SID].CountLimit, prefix[SID].Lookups, prefix[SID].AbortLimit
	             , State[0], State[1], State[2], State[3], State[4], State[5], State[6], State[7] );
	      } else {
	        sprintf( s1000, "%-64.64s|%10d %10d   %10d %11d %c%c%c%c%c%c%c%c"
	             , Sample, prefix[SID].Counts
	             , prefix[SID].CountLimit, prefix[SID].Lookups, prefix[SID].AbortLimit
	             , State[0], State[1], State[2], State[3], State[4], State[5], State[6], State[7] );
	      }
	    } else {
	      Fraction = ( (float) prefix[SID].Counts ) / ( (float) Nevents );
	      if ( AppendReturn ) {
	        sprintf( s1000, "%-64.64s|%10d %12.4f %10d  %9d %11d %c%c%c%c%c%c%c%c\r"
	             , Sample, prefix[SID].Counts, Fraction
	             , prefix[SID].CountLimit, prefix[SID].Lookups, prefix[SID].AbortLimit
	             , State[0], State[1], State[2], State[3], State[4], State[5], State[6], State[7] );
	      } else {
	        sprintf( s1000, "%-64.64s|%10d %12.4f %10d  %9d %11d %c%c%c%c%c%c%c%c"
	             , Sample, prefix[SID].Counts, Fraction
	             , prefix[SID].CountLimit, prefix[SID].Lookups, prefix[SID].AbortLimit
	             , State[0], State[1], State[2], State[3], State[4], State[5], State[6], State[7] );
	      }
	    }

	    EPT = MsgLNB( s1000, SUMMARY_WIDTH );  /*Find last non-blank.  */
	    for (  i = EPT+1;  i < SUMMARY_WIDTH;  s1000[i]=' ', Ghost[i++]=' ' );
	    s1000[SUMMARY_WIDTH] = 0;  /* NULL termination */
	    Ghost[SUMMARY_WIDTH] = 0;  /* NULL termination */

	    for ( i=SAMPLE_MAXLEN-1; i<=EPT-2; i+=2 ) {  /* Fill in with alternating dots. */
	      if ( !strncmp( "   ", &s1000[i-1], 3 ) ) {  /* Three blanks in a row:  */
	        s1000[i] = '.';
	        Ghost[i] = '.';
	      }
	    }
	    for ( i=SAMPLE_MAXLEN+1; i<=EPT-4; i+=2 ) {  /* Check for lone (inserted) dots & remove them.  */
	      if ( !strncmp( "  .  ", &Ghost[i-2], 5 ) ) {
	        s1000[i] = ' ';
	      }
	    }
	    s1000[SUMMARY_WIDTH] = 0;  /* NULL termination -- last chance. */
	    MsgToFileOut( s1000, fid );

	    PageLine += 1;
	    if ( control->SummaryPageLength <= 0 ) {  /*  Do no more headers.  */
	    } else if ( PageLine >= control->SummaryPageLength) {
	      PageLine = 0;   /*  Start a new page.  */
	    }

	  }  /*  if ( ListThisMessage )                           */

	}    /*  for ( ID = 1;  ID <= control->Nprefixes; ID++ )   */

	return;
}




	void	MsgTimeStampFile(

/*  Inputs:                                                                       */
	  FILE *fid )   /*  File descriptor of opened file, to write summary to.
	                    fid = 0  directs results to standard out.             */
{
/*  Description:  Write a node and time (real and CPU) stamp to a file, if the stamp is new.
	"Stamp" the date and time on a line the file specified by fid.
	"Stamp" the node-name on the same line, if node-name has been specified
	with a "MsgNodeNameSet(<node_name>)".
	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
	as 17-character ASCII strings.

	Produces output only if different than the previous output from
	this subroutine.  eg, a call to this routine in a tight loop will
	only produce an actual time-stamp once each second.                       */

	int DoTimeStamp  = FALSE;
	int HaveNodeName = FALSE;

	int CPU;
	int ELA;

	static char LastNodeName[NODE_NAME_MAXLEN+1]="";
	static char LastDateTime[24]="";
	static char LastCPUtime[18]="";
	char DateTime[24];
	char CPUtime[18];
	char Elapsed[15];


	strcpy( DateTime, MsgCtime() );
	CPU = MsgCPU() - CPUtime0; /* Get "absolute" CPU time, since CPUtime0 was set. */
	strcpy( CPUtime,  MsgCCPU(CPU) );
	ELA = MsgTime() - ELAtime0;
	strcpy( Elapsed,  MsgCela(ELA) );

	if ( control->NodeName[0] == 0 ) {   /*  No node name:  */
	  HaveNodeName = FALSE;
/*	  Don't bother with a time-stamp if it hasn't changed:   */
	  if ( !strcmp( DateTime, LastDateTime ) ) {  /* No change: */
	    DoTimeStamp = FALSE;
	  } else {  /* It has changed:  */
	    DoTimeStamp = TRUE;
	  }
	} else {                        /*  Append node name:  */
	  HaveNodeName = TRUE;
/*	  Don't bother with a time-stamp if node or time hasn't changed.  */
	  if ( !strcmp( DateTime, LastDateTime ) && !strcmp( control->NodeName, LastNodeName ) ) {  /* No change.  */
	    DoTimeStamp = FALSE;
	  } else {         /* Time or node has changed:  */
	    DoTimeStamp = TRUE;
	  }
	}

	if ( DoTimeStamp ) {                           /*  Already determined to stamp. */
	} else if ( !control->TimeStampCPU ) {           /*  Don't check the CPU time.    */
	} else if ( !strcmp( CPUtime, LastCPUtime ) ) {  /*  No change.                   */
	} else {                                         /*  Change -- stamp.             */
	  DoTimeStamp = TRUE;
	}

	if ( DoTimeStamp ) {                          /*  Do the time stamp.  */
	  if ( !HaveNodeName ) {
	    sprintf( s1000, "%-23s  Elapsed:%-14s  CPU:%-18s", DateTime, Elapsed, CPUtime );
	    strcpy( LastDateTime, DateTime );
	    strcpy( LastCPUtime, CPUtime );
	  } else {
	    sprintf( s1000, "%-23s [%s]  Elapsed:%-14s  CPU:%-18s"
                 , DateTime, control->NodeName, Elapsed, CPUtime );
	    strcpy( LastDateTime, DateTime );
	    strcpy( LastCPUtime, CPUtime );
	    strcpy( LastNodeName, control->NodeName );
	  }
	  if ( !fid ) { /*  Put it on standard out:  */
	    fprintf(stderr, "%s\n", s1000 );
	    fflush( stderr );
	  } else if ( fid == JournalFILE ) {
	    if ( JournalEnabled ) {
	      fprintf( fid, "%s\n", s1000 );
	      fflush( fid );
	    }
	  } else if ( fid ) {
	    fprintf( fid, "%s\n", s1000 );
	    fflush( fid );
	  }
	}
	return;
}




	void	MsgTimeStampFileOut(

/*  Inputs:                                                                       */
	  FILE *fid )   /*  File descriptor of opened file, to write summary to.
	                    fid = 0  directs results to standard out.             */
{
/*  Description:  Write a node and time (real and CPU) stamp to a file, always.
	"Stamp" the date and time on a line the file specified by fid.
	"Stamp" the node-name on the same line, if node-name has been specified
	with a "MsgNodeNameSet(<node_name>)".
	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
	as 17-character ASCII strings.

	Always produces output.                                                   */

	int CPU;
	int ELA;

	char DateTime[24];
	char CPUtime[18];
	char Elapsed[15];

	strcpy( DateTime, MsgCtime() );
	CPU = MsgCPU() - CPUtime0; /* Get "absolute" CPU time, since CPUtime0 was set. */
	strcpy( CPUtime,  MsgCCPU(CPU) );
	ELA = MsgTime() - ELAtime0;
	strcpy( Elapsed,  MsgCela(ELA) );

	if ( control->NodeName[0] == 0 ) {   /*  No node name:  */
	  sprintf( s1000, "%-23s  Elapsed:%-14s  CPU:%-18s", DateTime, Elapsed, CPUtime );
	  MsgToFileOut( s1000, fid );
	} else {
	  sprintf( s1000, "%-23s [%s]  Elapsed:%-14s  CPU:%-18s"
                 , DateTime, control->NodeName, Elapsed, CPUtime );
	  MsgToFileOut( s1000, fid );
	}
	return;
}




	void	MsgToJournal(
	
/*  Inputs:                                                                                    */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
/*  Input/Output:                                                                              */
	, int *ID ) /* Message ID.  Set to zero by caller 
                       before first call, set by Message on first call by
                       looking up or entering the prefix contained in msg
                       (prefix is everything before the first space) in
	               the index of message prefixes.
	               If ID is negative, ID remains unchanged, and lookup
	               is then always by prefix.                                         */
{
/*  Description:  Write a message to the journal (only), if enabled.
	Conditionally display & count a message msg on the journal file, if open & enabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.  */

	int Active, Alarming, Counting;

	int LID;   /*  Local copy of ID.  */

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}

/*	Check or set the ID & flags.
	Enter MSG's prefix in the index if needed.  */

	MsgCheck( msg, &LID, &Active, &Alarming, &Counting );

	if ( Counting ) {  /* Counting is true only if found.  */
	  MsgIncr(LID);
	}

	if ( Active ) {  /* Display it on the journal file:  */
	  MsgToJournalOut( msg );
	  MsgAbortCheck( LID ); /*  Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {
	  MsgAlarm( msg, prefix[LID].AlarmLevel );
	}

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return;
}




	void	MsgToJournalOut(
	
/*  Inputs:                                                                                */
	  const char *msg ) /* 1 or more line character-string message
	                       submitted for display;  lines separated by "newline" characters.  */
{
/*  Description:  Write a message to the journal (only);  bypass accounting.
	Unconditionally display a message msg on the journal file, if open & enabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.  */
	

/*	Display time & message on journal file:  */
	MsgToFileOut( msg, JournalFILE );

	return;
}




	void	MsgToFile(
	
/*  Inputs:                                                                                    */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
	, FILE *fid     /*  File descriptor of opened file, to write summary to.
	                    fid = 0  directs results to standard out.             */

/*  Input/Output:                                                                        */
	, int *ID ) /* Message ID.  Set to zero by caller 
                       before first call, set by Message on first call by
                       looking up or entering the prefix contained in msg
                       (prefix is everything before the first space) in
	               the index of message prefixes.
	               If ID is negative, ID remains unchanged, and lookup
	               is then always by prefix.                                         */
{
/*  Description:  Write a message to a file, specified by fid.
	Conditionally display & count a message msg.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, the caller should NOT include a "\n" at the very end, since msg puts
	a "\n" at the very end.  msg may have as many newlines as desired.  */

	int Active, Alarming, Counting;

	int LID;  /* Local copy of ID.  */

	if ( ID ) {
	  LID = *ID;
	} else {
	  LID = 0;
	}

/*	Check or set the ID & flags.
	Enter MSG's prefix in the index if needed.  */

	MsgCheck( msg, &LID, &Active, &Alarming, &Counting );

	if ( Counting ) {  /* Counting is true only if found.  */
	  MsgIncr(LID);
	}

	if ( Active ) {  /* Display it on the journal file:  */
	  MsgToFileOut( msg, fid );
	  MsgAbortCheck( LID ); /*  Check if abort limit is exceeded, and maybe abort.  */
	}

	if ( Alarming ) {
	  MsgAlarm( msg, prefix[LID].AlarmLevel );
	}

	if ( ID ) {
	  if ( *ID == 0 ) *ID = LID;   /* Ensure it's changed only if zero. */
	}

	return;
}




	void	MsgToFileOut(
	
/*  Inputs:                                                                                    */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
	, FILE *fid )     /* File descriptor of opened file, to write summary to.
	                     fid = 0  directs results to standard out.                         */

/*  Description:  Write a message to a file, specified by fid, bypassing msg accounting.
*/
{

	if ( !fid ) { /*  Put it on standard error:  */
	  fprintf(stderr, "%s\n", msg );
	  fflush( stderr );
	} else if ( JournalFILE && (fid == JournalFILE) ) {
	  if ( JournalEnabled ) {
	    MsgTimeStampFile( fid );  /* Time.  */
	    fprintf( fid, "%s\n", msg );
	    fflush( fid );
	  }
	} else if ( fid ) {
	  fprintf( fid, "%s\n", msg );
	  fflush( fid );
	}
	return;
}




