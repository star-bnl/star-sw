
/*
	This is the C interface to the msg message handling package.

	===================================
	==  User Routines in this file.  ==
	===================================

	Created  20-Sep-1994   Robert Hackenburg
*/

#define TRUE -1
#define FALSE 0

#include <stdio.h>
#include <string.h>

typedef	void	(*funcPoint)(char*,...);

static int  One=1;
static int* one=&One;
static funcPoint MsgAlarmRoutine = NULL;

	void message_( const char *msg, int *one, int *ID, int len );
	void message_out_( const char *msg, int *one, int len );
	void msg_class_define_( const char *Class, const char *State, int *CountLimit, int *AbortLimit, int Clen, int Slen );
	void msg_count_( const char *Prefix, int len );
	void msg_disable_( const char *Prefix, int len );
	void msg_display_( const char *msg, int *one, int *ID, int len );
	void msg_display_out_( const char *msg, int *one, int len );
	void msg_display_and_echo_( const char *msg, int *one, int *LUN, int *ID, int len );
	void msg_display_and_echo_out_( const char *msg, int *one, int *LUN, int len );
	void msg_enable_( const char *Prefix, int len );
	int  msg_enabled_( const char *Prefix, int *ID, int len );
	void msg_get_lun_( int *Terminal_LUN, int *Journal_LUN );
	void msg_ini_( int *Journal_LUN );
	int  msg_journal_close_( void );
	void msg_journal_off_( void );
	void msg_journal_on_( void );
	int  msg_journal_open_( const char *FileName, int len );
	void msg_journal_page_( void );
	void msg_lun_page_( int *LUN );
	void msg_parse_( const char *msg, int *isep, int *nprefix, int *nmessage, int len );
	void msg_mark_( const char *Prefix, int *ID, int len );
	void msg_name_node_( const char *NodeName, int len );
	void msg_nocount_( const char *Prefix, int len );
	void msg_set_abort_limit_( const char *Prefix, int *Limit, int len );
	void msg_set_by_command_( const char *Command, int len );
	void msg_set_from_file_( int *LUN );
	void msg_set_limit_( const char *Prefix, int *Limit, int len );
	void msg_set_lun_( int *TERMINAL_LUN, int *JOURNAL_LUN );
	void msg_set_summary_mode_aborted_( int *Mode );
	void msg_set_summary_mode_active_( int *Mode );
	void msg_set_summary_mode_counting_( int *Mode );
	void msg_set_summary_mode_inactive_( int *Mode );
	void msg_set_summary_page_length_( int *Page_Length );
	void msg_set_timestamp_cpu_( int *Mode );
	void msg_sort_( void );
	void msg_summary_( int *LUN );
	void msg_summary_cpu_( int *LUN );
	void msg_summary_event_( int *LUN, int *EVENTS );
	void msg_time_stamp_( int *LUN );
	void msg_time_stamp_out_( int *LUN );
	void msg_to_journal_( const char *msg, int *one, int *ID, int len );
	void msg_to_journal_out_( const char *msg, int *one, int len );
	void msg_to_lun_( const char *msg, int *one, int *LUN, int *ID, int len );
	void msg_to_lun_out_( const char *msg, int *one, int *LUN, int len );
	void strfc( char *msg, int msglen, char *cstring, int cmax, int *clen );


	void	msgalarm_( 

/*   Inputs:                                                                              */
	  char *msg      /* Character-string message, with prefix, submitted for display. */
	, int  *severity /* Severity level of alarm -- from msg class.                    */
	, int   msglen ) /* (FORTRAN-hidden) Length of msg.                               */

{

/*   Fortran callable, as:

	msgalarm( character*(*) message
	         ,integer       severity )
*/

/* Description:	FORTRAN callable msg interface to application-specified alarm routine. */

	char mstring[1000];
	char pstring[1000];
	int  mlen, plen;
	int  isep, nprefix, nmessage;

/*	Do this only if nothing's been declared:  */
	if ( MsgAlarmRoutine != NULL )
	{
	  msg_parse_( msg, &isep, &nprefix, &nmessage, msglen );

	  if ( nprefix > 0 )  /*  Make c-string containing the prefix only:  */
	  {
	    strfc( msg, nprefix, pstring, 1000, &plen );  /* Make c-string from FORTRAN-string (append null). */
	  }
	  else  /*  Make c-string containing the whole thing:  */
	  {
	    strfc( msg, msglen, pstring, 1000, &plen );  /* Make c-string from FORTRAN-string (append null). */
	  }

	  if ( isep <= 0 )
	  {                         /*  No message here -- prefix-only.
	    strfc( msg, msglen, mstring, 1000, &mlen );  /* Make c-string from FORTRAN-string (append null). */
	  }
	  else if ( nmessage > 0 )  /*  Make c-string containing the message-without-prefix:  */
	  {                         /*  isep is FORTRAN index;  points to first char in message in C.  */
	    strfc( &msg[isep], nmessage, mstring, 1000, &mlen );  /* Make c-string from FORTRAN-string (append null). */
	  }
	  else                      /*  Make c-string containing the whole thing:  */
	  {
	    strfc( msg, msglen, mstring, 1000, &mlen );  /* Make c-string from FORTRAN-string (append null). */
	  }

	  MsgAlarmRoutine( pstring, mstring, *severity );
	}
	return;
}


	void	MsgAlarmRegister(

/*    Input:                                                                                                */
	 funcPoint AlarmRoutine ) /* Application-specified Alarm routine to use by msg.  */
{
/* Description:  Register an alarm-routine to be called by active-alarm messages.
*/

	MsgAlarmRoutine = AlarmRoutine;
	return;
}

	void	msgalarmregister_(

/*    Input:                                                                         */
	 funcPoint AlarmRoutine ) /* Application-specified Alarm routine to use by msg.  */
{

/*   Fortran callable, as:

	msgalarmregister( external alarmroutine )
*/

/* Description:  Register an alarm-routine to be called by active-alarm messages.  For FORTRAN.
	         Note that if "alarmroutine" is defined as a c-procedure, its name as defined
	         must contain the suffix "_" and must be in lower-case, in order to be used
	         with this call.  If it does not contain the suffix "_" or is not all in lower
	         case, the call must be made from a c-procedure, in which case a call to
	         MsgAlarmRegister is better than msgalarmregister_ .
*/

	MsgAlarmRoutine = AlarmRoutine;
	return;
}

	void	Message(

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
	if enabled by a call to MsgJournalOn.
	The message is displayed unless disabled.
	The message is counted unless counting is disabled.
	If the prefix in msg is undefined, it is defined at this call and set to
	the default state for its class (see MsgDefineClass).

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	message_( msg, one, ID, strlen(msg) );
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
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	message_out_( msg, one, strlen(msg) );
	return;
}





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
	unless disabled.  The message is counted unless counting is disabled.
*/
	msg_class_define_( Class, State, &CountLimit, &AbortLimit, strlen(Class), strlen(State) );
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
	(ie, no displays).
*/

	msg_count_( Prefix, strlen(Prefix) );
	return;
}





	void	MsgDisable(

/*   Inputs:                                                                                */
	  const char *Prefix )  /* A message prefix */
{
/* Description:  Disable display of messages with the given prefix (continue counting).
	Disable displays for, but continue counting the message
	recognized by Prefix.  Has no effect if already disabled.
*/

	msg_disable_( Prefix, strlen(Prefix) );
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
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_display_( msg, one, ID, strlen(msg) );
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
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_display_out_( msg, one, strlen(msg) );
	return;
}





	void	MsgDisplayAndEcho(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */
	, int         LUN /* FORTRAN logical unit number for msg to be echoed to;          */
	                  /* The file refered to by LUN must be opened FORTRAN style.      */

/*   Input/Output:                                                                         */
	, int       *ID ) /* Fast-reference message ID.  Set to zero by caller 
	                     before first call, set by Message on first call by
	                     looking up or entering the prefix contained in MSG
	                     (prefix is everything before the first space) in
	                     the index of MSG message prefixes.
	                     If ID is negative, ID remains unchanged, and lookup
	                     is then always by prefix.                                     */
{
/* Description:  Display a message on the terminal and echo on a FORTRAN LUN.
	Conditionally display a message msg on standard out and echo it to LUN.
	The message is displayed unless disabled.
	The message is counted unless counting is disabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_display_and_echo_( msg, one, &LUN, ID, strlen(msg) );
	return;
}





	void	MsgDisplayAndEchoOut(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */
	, int       LUN ) /* FORTRAN logical unit number for msg to be echoed to;          */
	                  /* The file refered to by LUN must be opened FORTRAN style.      */
{
/* Description:  Display a message on the terminal and echo on a FORTRAN LUN.
	Display (always) a message msg on standard out and echo it to LUN.
	The message may not be disabled.
	The message is not counted.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_display_and_echo_out_( msg, one, &LUN, strlen(msg) );
	return;
}





	void	MsgEnable(

/*   Inputs:                                         */
	   const char *Prefix )  /* A message prefix */
{
/* Description:  Enable display and counting of messages with the given prefix.
	Enable counting, and continue displaying the message
	recognized by Prefix.  Has no effect if counting
	has not been disabled.
*/
	msg_enable_( Prefix, strlen(Prefix) );
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
	Returns the message-display-enabled-status for the STAR-standard message
	with stored prefix <Prefix>, message-index ID.  If the message is not
	defined, it is defined and enabled, with counting turned on.

  Return conditions:
	.TRUE. (-1) if the message is enabled (active),
	.FALSE. (0) if the message is disabled (inactive).
*/
	return( msg_enabled_( Prefix, ID, strlen(Prefix) ) );
}




	void	MsgGetLUN(

/*  Outputs:                                                               */
	int *Terminal_LUN  /* FORTRAN logical unit number of terminal
	                      for output, typically 6.                     */
      ,	int *Journal_LUN ) /* FORTRAN logical unit number of journal file. */

{
/* Description:  Get in-use terminal and journal FORTRAN LUNs.
	Get the message handler terminal and journal LUNs and pass them
	to the caller.
*/

	msg_get_lun_( Terminal_LUN, Journal_LUN );
	return;
}





	void 	MsgIni(

/*  Input:                                                                 */
	int Journal_LUN ) /* FORTRAN logical unit number of journal file. */
{
/* Description:  MSG package initializion and journal-LUN specification.
	Initialize the STAR-standard message-handling package and set the
	journal LUN.
*/
	msg_ini_( &Journal_LUN );
	return;
}





	int	MsgJournalClose()

{
/* Description:  Close the MSG journal file.
	Call this subroutine to close the message journal file.
	Disables message-logging in the journal file.
	Returns .TRUE. (-1) if the file was successfully closed,
	returns .FALSE. (0) if the file could not be closed (probably
	because it was not opened)

    Return conditions:
	.TRUE. (-1) for successful close of journal file.
	.FALSE. (0) for close failure of journal file.
*/

	return( msg_journal_close_() );
}





	void	MsgJournalOff()
{
/* Description:  Disable journal logging of messages.
	Call this subroutine to disable the message journal file.
*/
	msg_journal_off_();
	return;
}
 




	void	MsgJournalOn()
{
/* Description:  (Re)enable journal logging of messages.
	Call this subroutine to (Re)enable the message journal file.
*/
	msg_journal_on_();
	return;
}





	int	MsgJournalOpen(
/*  Input:                                         */
	const char* FileName  ) /* The journal file-name */
{
/* Description:  Open a message-logging journal file.
	Call this subroutine to specify FILE_NAME as the
	message journal file and to open that file.
	Returns .TRUE. (-1) if the file was successfully opened
	and enables message logging to the journal file;
	returns .FALSE. (0) if the file could not be opened, and
	disables message-logging in the journal file.
*/
	return( msg_journal_open_( FileName, strlen(FileName) ) );
}





	void	MsgJournalPage()
{
/* Description:  Send a form-feed to the journal file.
	Write a form-feed to the journal file, if open & enabled.
*/
	msg_journal_page_();
	return;
}




	void	MsgLUNPage(

/*  Input:                                        */
	int LUN ) /* FORTRAN logical unit number. */
{
/* Description:  Send a form-feed to a FORTRAN LUN.
	Write a form-feed to the device openned on LUN.
*/
	msg_lun_page_( &LUN );
	return;
}





	void	MsgMark(

/*  Inputs:                             */
	  const char *Prefix /* MSG prefix    */

/*  Input/Output:                       */
	, int *ID  )   /* Fast-reference MSG ID.  Uniquely assigned on first call */
{
/*   Description:
	Set the given Prefix's "Marked CPU time" to current.  When this prefix
	occurs in a call to an MSG routine which does MSG accounting (eg, Message),
	and when that Prefix has had its "Marked CPU time" set (ie, to non-zero),
	that prefix's "total CPU time" is incremented by Current CPU time, less
	the "Marked CPU time".  The Marked CPU time is then zeroed.
*/
	msg_mark_( Prefix, ID, strlen(Prefix) );
	return;
}




	void	MsgNameNode(

/*  Input:                  */
	const char *NodeName )  /* Name of node, as will appear on the node-time stamp.  */
{
/*  Description:  Specify the local (ASCII) node name for node-time stamping
	          of journal entries of message-occurrances.
*/
	msg_name_node_( NodeName, strlen(NodeName) );
	return;
}





	void	MsgNoCount(

/*  Input:                                         */
	const char *Prefix ) /* A message prefix.  */
{
/*  Description:  Disables MSG counting of a previously disabled prefix.
*/
	msg_nocount_( Prefix, strlen(Prefix) );
	return;
}




	void	MsgSetAbortLimit(

/*  Inputs:  */
	  char *Prefix /* A message prefix. */
	, int Limit ) /* Maximum no. of times to display a message before aborting. */
{
/*  Description:  Set the abort limit for the message recognized by Prefix.
	Program termination results when the prefix's
	count has exceeded Limit.  Wildcards are permitted.
	The existing count-limit (after which message-display is disabled
	for Prefix), is set to the same value -- aborting msg calls
	should not be quiet without deliberate intervention (ie, re-specify
	the count-limit after this call to obtain silent aborts -- don't
	do it!).
*/
	msg_set_abort_limit_( Prefix, &Limit, strlen(Prefix) );
	return;
}





	void	MsgSetByCommand(

/*  Input:  */
	const char *Command ) /* Command-string describing features to be set. */
{
/*  Description:  Issue an ASCII command to msg.
	Set the MSG package parameters on individual messages, referenced
	by prefix, according to arguments contained in a single character-
	string command contained in COM.  Commands take these forms:

	To disable specific messages, but to continue counting their occurances:
	DISABLE prefix-1 prefix-2 ... prefix-n

	To enable specific messages:
	ENABLE prefix-1 prefix-2 ... prefix-n

	To resume counting of message occurances, whether or not disabled:
	COUNT prefix-1 prefix-2 ... prefix-n

	To stop counting of message occurances, whether or not disabled:
	NOCOUNT prefix-1 prefix-2 ... prefix-n

	To set message-limits on specific messages:
	LIMIT prefix-1=limit-1 prefix-2=limit-2 ... prefix-n=limit-n

	To set message-abort-limits on specific messages:
	ABORT prefix-1=limit-1 prefix-2=limit-2 ... prefix-n=limit-n

	To set the number of lines per page in the summary-output:
	LINES line-count

	To have time-stamps occur on a changed CPU time (in addition to
	getting them on changed real-time or node-name):
	TIMESTAMP CPU

	To have time-stamps not occur on a changed CPU time (ie, only
	getting them on changed real-time or node-name):
	TIMESTAMP NOCPU

	Spacing is not critical in the command line, provided at least one
	or more spaces, tabs or commas separate the arguments.
	The "=" symbols in the LIMIT command are essential, but spaces around
	it are optional and not at all critical.

	Wildcards are permitted in the prefix specifications.
*/

	msg_set_by_command_( Command, strlen(Command) );
	return;
}






	void	MsgSetFromFile(

/*  Input:  */
	int LUN ) /* FORTRAN Logical Unit Number, on which a command (ASCII)
	             file should be (already) open.  */
{
/*  Description:  ASCII-file command interface to control MSG.
	Reads lines from LUN and interprets them as MSG_SET_BY_COMMAND commands
	until either an <EOF> or a line containing MSG_EXIT is encountered.
*/

	msg_set_from_file_( &LUN );
	return;
}





	void	MsgSetLimit(

/*  Inputs:  */
	  const char *Prefix  /* A message prefix. */
	, int         Limit ) /* Maximum no. of (additional) times to display a message. */
{
/*  Description:  Set auto-disable count limit for a prefix.
	Displays of the specified message are disabled once its
	count has exceeded LIMIT.  Wildcards are permitted.
*/
	msg_set_limit_( Prefix, &Limit, strlen(Prefix) );
	return;
}





	void	MsgSetLUN(

/*  Inputs: */
	  int TERMINAL_LUN  /* FORTRAN logical unit number of terminal
	                       for output, typically 6.  */
	, int JOURNAL_LUN ) /* FORTRAN logical unit number of journal file.  */
{
/*  Description:  Set the terminal and journal LUNs for MSG.
	(ie, the message handler LUNs.)  The caller specifies
	the FORTRAN logical unit number of the terminal in
	TERMINAL_LUN and the journal file in JOURNAL_LUN.  */

	msg_set_lun_( &TERMINAL_LUN, &JOURNAL_LUN );
	return;
}





	void	MsgSetSummaryModeAborted(

/*  Input:  */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE.  */

{
/*  Description:  Set the MSG summary mode for "Aborted" messages.
*/
	msg_set_summary_mode_aborted_( &Mode );
	return;
}





	void	MsgSetSummaryModeActive(

/*  Input:                                                         */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE. */
{
/*  Description:  Set the MSG summary mode for "Active" messages.
*/
	msg_set_summary_mode_active_( &Mode );
	return;
}





	void	MsgSetSummaryModeCounting(

/*  Input:  */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE.  */
{
/*  Description:  Set the MSG summary mode for "Counting" messages.
*/
	msg_set_summary_mode_counting_( &Mode );
	return;
}





	void	MsgSetSummaryModeInactive(

/*  Input:  */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE. */
{
/*  Description:  Set the MSG summary mode for "Inactive" messages.
*/
	msg_set_summary_mode_inactive_( &Mode );
	return;
}





	void	MsgSetSummaryPageLength(

/*  Input:  */
	int Page_Length ) /* Length of summary page (lines).  */
{
/*  Description:  Set the MSG summary table page length.
*/
	msg_set_summary_page_length_( &Page_Length );
	return;
}






	void	MsgSetTimeStampCPU(

/*  Input:   */
	int Mode ) /* Whether the mode is to be set TRUE or FALSE. */
{
/*  Description:  Set the MSG time-stamp mode for stamping CPU changes.
*/
	msg_set_timestamp_cpu_( &Mode );
	return;
}





	void	MsgSort()
{
/*  Description:  Sort the MSG prefixes, in alphabetical order.
*/
	msg_sort_();
	return;
}





	void	MsgSummary(

/*  Input: */
	int LUN ) /* Device on which the summary is output. */
{
/*  Description:  Generate an MSG summary table on a FORTRAN LUN.
*/
	msg_summary_( &LUN );
	return;
}





	void	MsgSummaryCPU(

/*  Input:  */
	int LUN ) /* Device on which the summary is output.  */
{
/*  Description:  Generate a CPU-usage MSG-summary table.
	Output a CPU-usage summary of the MSG-handled & marked messages
	on the specified LUN.  "Marked" messages are messages whose occurances
	were preceded, one-for-one, by calls to MSG_Mark, with the same
	MSG prefix.
*/
	msg_summary_cpu_( &LUN );
	return;
}





	void	MsgSummaryEvent(

/*  Inputs:   */
	  int LUN      /* Device on which the summary is output.  */
	, int EVENTS ) /* Number of events by which to normalize. */
{
/*  Description:  Generate an MSG summary table with a frequency column on a FORTRAN LUN.
	Output a summary of the MSG-handled messages on the specified LUN.
	Include a column of normalized occurances of each message,
	which consists of a column of the number of occurances of each
	message, divided by EVENTS.  If EVENTS is not positive, then the
	column is ommitted.
*/
	msg_summary_event_( &LUN, &EVENTS );
	return;
}





	void	MsgTimeStamp(

/*  Inputs:  */
	int LUN ) /* FORTRAN logical unit number for time-stamp to be written on.  */
{
/*  Description:  Write a node and time (real and CPU) stamp to a FORTRAN LUN, if new.
	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
	"Stamp" the node-name on the same line, if node-name has been specified
	with a "call msg_name_node(<node_name>)".
	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
	as 17-character ASCII strings.

	Produces output only if different than the previous output from
	this subroutine.  eg, a call to this routine in a tight loop will
	only produce an actual time-stamp once each second.
*/
	msg_time_stamp_( &LUN );
	return;
}





	void	MsgTimeStampOut(

/*  Inputs:   */
	int LUN ) /* FORTRAN logical unit number for time-stamp to be written on.  */
{
/*  Description:  Write a node and time (real and CPU) stamp to a FORTRAN LUN, always.
	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
	"Stamp" the node-name on the same line, if node-name has been specified
	with a "call msg_name_node(<node_name>)".
	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
	as 17-character ASCII strings.

	Always produces output.
*/
	msg_time_stamp_out_( &LUN );
	return;
}





	void	MsgToJournal(
	
/*  Inputs:                                                                                    */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
/*  Input/Output:                                                                              */
	, int *ID ) /* STAR-standard message ID.  Set to zero by caller 
                       before first call, set by MESSAGE on first call by
                       looking up or entering the prefix contained in MSG
                       (prefix is everything before the first space) in
	               the index of STAR-standard message prefixes.
	               If ID is negative, ID remains unchanged, and lookup
	               is then always by prefix.                                         */
{
/*  Description:  Write a message to the journal (only), if enabled.
	Conditionally display & count a message MSG, containing LINES lines
	of up to 132 characters each, on the journal file, if open & enabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_to_journal_( msg, one, ID, strlen(msg) );
	return;
}





	void	MsgToJournalOut(
	
/*  Inputs:                                                                                */
	  const char *msg ) /* 1 or more line character-string message
	                       submitted for display;  lines separated by "newline" characters.  */
{
/*  Description:  Write a message to the journal (only);  bypass accounting.
	Unconditionally display a message MSG, containing LINES lines
	of up to 132 characters each, on the journal file, if open & enabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_to_journal_out_( msg, one, strlen(msg) );
	return;
}





	void	MsgToLUN(
	
/*  Inputs:                                                                                    */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
	, int       LUN   /* FORTRAN logical unit number for time-stamp to be written on.      */

/*  Input/Output:                                                                        */
	, int *ID ) /* STAR-standard message ID.  Set to zero by caller 
                       before first call, set by MESSAGE on first call by
                       looking up or entering the prefix contained in MSG
                       (prefix is everything before the first space) in
	               the index of STAR-standard message prefixes.
	               If ID is negative, ID remains unchanged, and lookup
	               is then always by prefix.                                         */
{
/*  Description:  Write a message to a FORTRAN LUN, if enabled.
	Conditionally display & count a message MSG, containing LINES lines
	of up to 132 characters each, on LUN.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	msg_to_lun_( msg, one, &LUN, ID, strlen(msg) );
	return;
}




	void	MsgToLUNOut(
	
/*  Inputs:                                                                              */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
	, int       LUN ) /* FORTRAN logical unit number for time-stamp to be written on.      */
{
/*  Description:  Write a message to a FORTRAN LUN;  bypass accounting.
	Display a message, containing one or more lines of no particular
	length (or length-limit), on FORTRAN logical unit LUN.

	This routine ends with a call to STRFlush( LUN ), flushing the output
	output buffer.  This seems to be necessary to get some machines to
	keep the LUN file (or terminal) up-to-date;  without this, a crash
	leaves the last several lines of the output to LUN not in the listing
	file -- a terrible thing indeed for debugging:

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/

	msg_to_lun_out_( msg, one, &LUN, strlen(msg) );
	return;
}




