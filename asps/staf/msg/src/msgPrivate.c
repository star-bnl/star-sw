/*	This is the STAR message/error handling package (prefix Msg_).

	===================+++====================
	==  msg Private Routines in this file.  ==
	===================+++====================

	Modified Shared memory to use process ID,
	and <stderr> replaces <stdout>:
	        19-Nov-1997   Robert Hackenburg
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

static const char sccsid[] = "@(#)"__FILE__"\t\t1.55\tCreated 9-Oct-1996, \tcompiled "__DATE__" "__TIME__;

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <sys/ipc.h>  /*  Interprocess Communications  -- needed for Shared Memory.  */
#include <sys/shm.h>  /*  Shared Memory.  */

#include <sys/systeminfo.h>

#include <errno.h>
#include <msg.h>
#include <msgData.h>

msgData_t msg;
msgData_t *Msg;
FILE *JournalFILE;    /* Journal-file descriptor                          */

control_t *control = &msg.control;
prefix_t  *prefix = &msg.prefix[0];
class_t   *class = &msg.class[0];

extern int MsgInitialized; /* This starts out FALSE, and is set to TRUE when initialized.  */

int CPUtime0 = 0;
int ELAtime0 = 0;

funcPoint MsgAlarmRoutine = NULL;

char   m1000[1000];  /*  Some "scratch" message space.  */
char   s1000[1000];  /*  Some "scratch" string space.  */


	void	MsgClean(
/*  Input                                                                         */
	    int  length      /*  (maximum) number of characters to clean.         */
/*  Input/Output                                                                  */
	  , char string[] )  /*  String to be cleaned.                            */
{
/*  Description:  Replace all non-prinable characters with blanks up to NULL.     */

	int  i=0;
	while ( (i<length) && string[i] ) {
	  if ( string[i] < *" " ) string[i] = *" ";
	  if ( string[i] > *"~" ) string[i] = *" ";
	  i++;
	}
	return;
}




	char*	MsgCCPU( 
/*  Input:                                                                        */
	int CPU )  /*  CPU time, from MsgCPU                                      */
{
/*  Description: Return 17 character ASCII CPU time (18 including NULL).

    Returns:
	17 character CPU time: "ddddd hh:mm:ss.tt"                                */


	static char str[50];

	int TPS; /*  Ticks per second.  */
	int CPUdays, CPUhours, CPUmins, CPUsecs, CPUticks;
	int CPUbal;

	TPS = MsgTPS();  /* Get native cpu clock ticks-per-second.  */

	CPUdays  = CPU/(TPS*3600*24);  /* TPS(ticks/sec), 3600 sec/hr, etc.  */
	CPUbal   = CPU - (CPUdays*(TPS*3600*24));
	CPUhours = CPUbal/(TPS*3600);
	CPUbal   = CPUbal - (CPUhours*(TPS*3600));
	CPUmins  = CPUbal/(TPS*60);
	CPUbal   = CPUbal - (CPUmins*(TPS*60));
	CPUsecs  = CPUbal/TPS;
	CPUbal   = CPUbal - (CPUsecs*(TPS));
/*	Map 0-59 to 0-5900 or 0-99 to 0-9900:  */
	CPUticks = CPUbal*100;
	CPUticks = CPUticks/TPS;  /*  Map 0-5900 to 0-98 or 0-9900 to 0-99.  */

	sprintf( str, "%5d %2.2d:%2.2d:%2.2d.%2.2d", CPUdays, CPUhours, CPUmins, CPUsecs, CPUticks );

	return( str );
}




	char*	MsgCela( 
/*  Input:                                                                        */
	int Time )  /*  Elapsed time, from MsgTime                                */
{
/*  Description: Return 14 character ASCII elapsed time (15 including NULL).

    Returns:
	14 character elapsed time: "ddddd hh:mm:ss"                           */


	static char str[50];

	int days, hours, mins, secs;
	int bal;

	days  = Time/(3600*24);  /* 3600 sec/hr, etc.  */
	bal   = Time - (days*3600*24);
	hours = bal/3600;
	bal   = bal - (hours*3600);
	mins  = bal/60;
	bal   = bal - (mins*60);
	secs  = bal;

	sprintf( str, "%5d %2.2d:%2.2d:%2.2d", days, hours, mins, secs );

	return( str );
}




	char*	MsgCtime( void )
{
/*  Description: Return standard 23 character ASCII date and time.
	ie: years, months, days, hours, minutes and seconds,
	with an appended NULL (24th character).

    Returns:
	23 character time & date: "dd-mmm-yy hh:mm:ss zone"
	Last four characters are time-zone.  NULL appended.                       */

/*	From man pages:                                                           */
/*	time_t time(time_t *tloc);       tloc is an optional copy of time()       */
/*	struct tm *localtime (const time_t *clock);                               */
/*	size_t strftime (char *s, size_t maxsize, const char *format,             */
/*	                 const struct tm *timeptr);                               */

	static char str[50];
	time_t t;
	size_t length;

        t      = time(NULL);
	length = strftime ( str, sizeof(str), "%d-%b-%y %T %Z", localtime(&t) );
	if ( length <= 0 ) return(NULL);
	str[50] = NULL;  /*  Make it bullet proof.                                */
	return( str );
}




	long	MsgCPU(void)
/*  Description:  Returns the integer number of user CPU clock ticks.  */
{
	struct tms CPU;
	times( &CPU );   /*  Fill the "cpu" structure (tms).  */
	return( CPU.tms_utime );   /* user CPU time, in clock-ticks. */
}

	int	MsgLNB(
/*  Inputs */
	    const char *string
	  , int   n )  /* Max. chars in string to examine.  */
{
/*  Description:   Find string's Last-Non-Blank.
	Set MsgLNB to point at the last non-blank in string,
	or -1 if all blanks.  Tabs are regarded as blanks.
	0 is the index of the first character in string.

    Returns:  The position in the string of the last non-blank.

*/
	int  L, i, LNB;

	L = strlen( string ) - 1;  /* Index of last char in string, or -1 if zero length. */
	if ( L >= n ) L = n - 1;   /* Protection.  */
/*	Search for the end of the line:  */
	LNB = L; /* No trailing blanks confirmed yet (reset value).  */
	for ( i = 0; i <= L; i++ ) {
	  if        ( string[i] == *" "  ) {  /* blank */
/*	    If the first blank/tab since last reset, might be trailing space.  */
	    if ( LNB == L ) LNB  = i - 1;   /* If a trailing blank,  point to previous character.  */
	  } else if ( string[i] == *"\t" ) {  /* tab   */
/*	    If the first blank/tab since last reset, might be trailing space.  */
	    if ( LNB == L ) LNB  = i - 1;   /* If a trailing blank,  point to previous character.  */
	  } else if ( string[i] == *"\n" ) {  /* newline   */
/*	    If the first blank/tab since last reset, might be trailing space.  */
	    if ( LNB == L ) LNB  = i - 1;   /* If a trailing blank,  point to previous character.  */
	  } else if ( string[i] == *"\f" ) {  /* form feed   */
/*	    If the first blank/tab since last reset, might be trailing space.  */
	    if ( LNB == L ) LNB  = i - 1;   /* If a trailing blank,  point to previous character.  */
	  } else {                           /* Non-blank/tab */
	    LNB = L;  /*  Reset to last character in S. */
	  }
	}

	return(LNB);
}




	void	MsgTruncate(
/*  Input/Output */
	    char *string
/*  Input */
	  , int   Size )  /* Size of string.  */
{
/*  Description:   Truncate string to Last Non Blank
	           Especially used following an fgets, to remove the newline.  */

	int L;
	L = MsgLNB( string, Size );
	if ( L < 1 ) {
	  string[0] = NULL;
	} else if ( L >= Size ) {
	  string[Size] = NULL;
	} else {
	  string[L+1] = NULL;
	}
	return;
}




	long	MsgTime(void)
/*  Description:  Returns the integer number of seconds since 01jan70.  */
{
	time_t t;
	t = time( NULL );
	return( t );
}

	long	MsgTPS(void)
/*  Description: Inquires from the system and returns the number of CPU ticks-per-second. */
{
	return(sysconf( _SC_CLK_TCK ));
}





	void	MsgGetPrefix(
/*  Input:  */
	  const char *msg  /*  A message, with prefix. */
/*  Output:  */
	, char *Prefix )   /* The message-prefix from msg.  */
{
/*   Description:  Strip the prefix off a message and return it.
	Extract the prefix from the character string MSG
	and return it in Prefix.
*/

	int firstNonBlank,In,L;
	int C;

	char *sepPoint;
	const char seplist[] = " \t\n\f\0";  /* Space, tab, newline, form-feed, null */


/*	Scan for first non-separator:  */
	L  = strlen( msg ) - 1;
	firstNonBlank = 0;
	C  = msg[firstNonBlank]; /*  Integer copy of a character for strchr.  */
	while (  ( strchr( seplist, C ) != NULL )  && ( firstNonBlank < L )  ) {
	  C   = msg[++firstNonBlank]; /*  Integer copy of a character for strchr.  */
	}
	if ( firstNonBlank >= L ) {   /*  No non-separators or only one at the very end.  */
	  C   = msg[L]; /*  Integer copy of a character for strchr.  */
	  if ( strchr( seplist, C ) != NULL ) {  /*  Last is a blank etc. */
	    strcpy( Prefix, " " );
	  } else {                               /*  Last is a non-blank. */
	    Prefix[0] = msg[L];
	    Prefix[1] = NULL;
	  }
	  return;
	}

/*	Scan for the next separator:  */
	sepPoint = strpbrk( &msg[firstNonBlank], seplist );  /*  Find, in &msg[firstNonBlank], any char. in seplist.  */

	if ( sepPoint == NULL ) {   /* Prefix with no following separator (it's all prefix):  */
	  strncpy( Prefix, &msg[firstNonBlank], PREFIX_MAXLEN+1 );
	  Prefix[PREFIX_MAXLEN] = NULL;
	} else {
	  L = (int)sepPoint-(int)&msg[firstNonBlank];      /*  Number of characters before last separator or max. */
	  if ( L > PREFIX_MAXLEN ) L = PREFIX_MAXLEN;
	  strncpy( Prefix, &msg[firstNonBlank], L+1 );
	  Prefix[L] = NULL;
	}

	return;
}




	int	MsgFindClass(
/*  Input:  */
	  const char *Class /* A prefix-class. */
/*  Output: */
	, int *ID )         /* ID of Class.    */
{
/*   Description:  Lookup a class in the MSG class index, return ID.

	Find the character string Class in the index of
	Prefix Classes.  If found in the index, return
	the index *ID and return MsgFindClass TRUE.
	If Class is null or blank, then *ID = 0 is returned,
	but MsgFindClass still returns true.
	*ID is never used to find Class.
	If Class is not found, returns FALSE .

   Return conditions:
	TRUE if Class is found in the index or is the null class.
	FALSE if Class is not found in the index.
*/


	int Found;
	int LID = 1;
	int L;

	Found = FALSE;
	L = MsgLNB( Class, CLASS_MAXLEN );  /* Always returns L<=CLASS_MAXLEN-1  */
	if ( L < 0 ) {   /* The Null class. */
	  LID = 0;   /*  This is a class unto itself. */
	  Found = TRUE;
	}

/*	L is an index, for CLASS_MAXLEN = 1, L = 0 always, if not Found (LID=0;  L+1 is the count (ie, 1),  */

	while (  !Found && ( LID <= control->Nclasses ) ) {
	  if ( !strncmp( class[LID].Class, Class, L+1 ) ) {   /* It's the right one:  */
	    Found = TRUE;
	  } else {
	    LID += 1;
	  }
	}

	if (!Found) {
/*	  Not in index -- set this like this:  */
	  LID = 0;
	}

	*ID = LID;

	return( Found );
}




	void	MsgGetClass(
/*  Input:  */
	  const char *Prefix  /*  A message-prefix. */

/*  Output: */
	, char *Class ) /* The prefix-class from Prefix. */
{
/* Description:  Strip the class off of a prefix and return it.
	Extract the class from the character string Prefix
	and return it in Class.
*/


	char *sepPoint;
	const char seplist[] = "-";  /* Dash */

/*	Scan for the separator:  */
	sepPoint = strpbrk( Prefix, "-" );  /*  Find a dash in Prefix.  */

	if ( sepPoint == NULL ) {   /* Prefix with no "-"  -- blank class:  */
	  Class[0] = NULL;
	} else if ( sepPoint[1] == NULL ) {   /* Prefix ending in "-"  -- blank class:  */
	  Class[0] = NULL;
	} else {
/*	  Class is everything after separator to end-of-prefix, up to CLASS_MAXLEN:  */
	  strncpy( Class, &sepPoint[1], CLASS_MAXLEN ); /*  Class is dimensioned CLASS_MAXLEN+1. */
	  Class[CLASS_MAXLEN] = NULL;        /*  Append NULL.  */
	}

	return;
}




	void	MsgIncr(
/*  Input:  */
	const int ID )  /* Message-ID -- must be valid.  */
{
/*  Description:  Increment a prefix's counter, referenced by ID.
	Increment the counter for the message indexed
	by ID.  Also, if a CPU time for ID has been marked, take a difference
	from that CPU and the current, and add it to the CPU total for ID.
*/
	int CPUtime;
	int Delta;

	prefix[ID].Counts += 1;

	if ( prefix[ID].Marked ) {  /* CPU time is marked:  */
	  CPUtime = MsgCPU() - CPUtime0;
	  Delta   = CPUtime - prefix[ID].CPUmark;
	  prefix[ID].CPUmark = 0;   /* Remove the mark -- needs to be marked each time. */
	  if ( Delta > 0 ) {
	    prefix[ID].CPUtotal += Delta;
	    prefix[ID].CPUdelta  = Delta;
	  } else {
	    prefix[ID].CPUdelta = 0;
	  }
	}

	return;
}




	void	MsgResetID(
/*  Input:  */
	const int ID )  /* Fast-reference ID of an already-entered message-prefix.  */
{
/*   Description:  Reset the message prefix in the index.
	Do nothing if ID is .LE. 0.
*/

	char Class[CLASS_MAXLEN+1];
	int CID;

	if ( ID <= 0 ) return;

	prefix[ID].Counts  = 0;
	prefix[ID].Lookups = 0;
	prefix[ID].Marked   = FALSE;
	prefix[ID].CPUmark  = 0; /*  CPU usage "Mark", set at call to MsgMark.  */
	prefix[ID].CPUdelta = 0; /*  Most recent CPU-usage between calls to MsgMark and MsgIncr.  */
	prefix[ID].CPUtotal = 0; /*  Sum of all CPU-usages between calls to MsgMark and MsgIncr.  */
	strncpy( prefix[ID].Sample, prefix[ID].Prefix, SAMPLE_MAXLEN+1 );   /*  Initialize sample with prefix.  */
	CID = prefix[ID].Iclass;
	if ( CID <= 0 ) {
	  MsgGetClass( prefix[ID].Prefix, Class ); /*  Get the class, from the prefix.        */
	  MsgFindClass( Class, &CID );             /*  Find the class in the index.           */
	}
	if ( CID > 0 ) {                           /*  Have the class -- set class defaults:  */
	  prefix[ID].Active     = class[CID].Active;
	  prefix[ID].Alarming   = class[CID].Alarming;
	  prefix[ID].Counting   = class[CID].Counting;
	  prefix[ID].CountLimit = class[CID].CountLimit;
	  prefix[ID].AbortLimit = class[CID].AbortLimit;
	  prefix[ID].AlarmLevel = class[CID].AlarmLevel;
	  prefix[ID].Iclass     = CID;
	} else {                                   /* Don't have the class:                   */
	  prefix[ID].Active     = control->Active;
	  prefix[ID].Alarming   = control->Alarming;
	  prefix[ID].Counting   = control->Counting;
	  prefix[ID].CountLimit = control->CountLimit;
	  prefix[ID].AbortLimit = 0;               /*  No default abort limit.                */
	  prefix[ID].AlarmLevel = control->AlarmLevel;
	  prefix[ID].Iclass     = 0;               /*  No class.                              */
	}

	return;
}




	int	MsgFind(
/*  Input:  */
	  const char *Prefix   /* A message prefix. */

/*  Outputs: */
	, int *ID         /* Message ID. */
	, int *Active     /* Flag set to .TRUE. if the message selected
                             (by Prefix and/or ID) is active. */
	, int *Alarming   /* Flag set to .TRUE. if the message selected
                             (by Prefix and/or ID) is enabled for alarming. */
	, int *Counting ) /* Flag set to .TRUE. if the message selected
                             (by Prefix and/or ID) is enabled for counting. */
{
/*   Description:  Lookup a prefix in the MSG prefix index, return fast-ref ID.

	Find the character string Prefix in the index of
	message prefixes (sic).  If found
	in the index, return the index ID and return
	MSG_Find true.  The ID is never used to find Prefix.
	If Prefix is not found, returns FALSE.  The flags Active, Alarming
	and Counting are set according to the whether the message
	is active (output enabled) and enabled for counting.

   Return conditions:
	TRUE  if Prefix is found in the index.
	FALSE if Prefix is not found in the index.
*/

	int Found;
	int LID;

	if ( strlen( Prefix ) < 0 ) {
	  return( FALSE );
	}

	control->Nlookups += 1;

	Found = FALSE;
	LID   = 1;
	while (  !Found && ( LID <= control->Nprefixes ) ) {
	  if ( !strncmp( prefix[LID].Prefix, Prefix, PREFIX_MAXLEN+1 ) ) {   /* It's the right one:  */
	    Found = TRUE;
	    prefix[LID].Lookups += 1;  /* Record this lookup. */
	  } else {
	    LID += 1;
	  }
	}

	if (!Found) {
/*	  Not in index -- set these like this:  */
	  *Active   = FALSE;
	  *Alarming = FALSE;
	  *Counting = FALSE;
	  LID = 0;
	} else {  /* In index -- look up the flags:  */
	  if ( !prefix[LID].Active) {
	  } else if (prefix[LID].CountLimit <= 0 ) {  /* No counting limit.  */
	  } else if (prefix[LID].Counts >= prefix[LID].CountLimit) {
/*	    Turn off messages -- continue counting, of course:  */
	    prefix[LID].Active = FALSE;
/*	    And display a message-shut-off warning:  */
/*	    MessageOut( "%s Disabled -- count limit reached", prefix[LID].Prefix ); */
	    sprintf( m1000, "%s Disabled -- count limit reached", prefix[LID].Prefix );
	    MessageOut( m1000 );
	  } /*  !Active  */
	  *Active   = prefix[LID].Active;
	  *Alarming = prefix[LID].Alarming;
	  *Counting = prefix[LID].Counting;
	}

	*ID = LID;

	return( Found );
}


 

	void	MsgEnter(
/*  Input:  */
	  const char *Prefix   /* A message prefix.  */

/*  Output:  */
	, int *ID )  /* Fast-reference ID of the newly-entered
	                message-prefix, or zero if no more room.  */
{
/*   Description:  Define new prefixes and assign fast-ref IDs.

	Enter the message prefix into the index.
	If there is no more room in the index, display a message
	and return ID=0.
*/

	int EID;
	int Active, Alarming, Counting, Found;

	if ( control->Nprefixes < MAXPREFIXES ) {  /* There's room:  */
	  control->Sorted = FALSE;   /* Indicate that a new sort will be needed. */
	  control->Nprefixes += 1;
	  *ID = control->Nprefixes;
	  strncpy( prefix[*ID].Prefix, Prefix, PREFIX_MAXLEN+1 );
	  prefix[*ID].Iclass = 0;
	  MsgResetID( *ID );        /*  Reset counters, flags and default limits (class determined).  */
	} else {   /* There's no room -- message:  */
	  *ID = 0;

	  sprintf( m1000, "\
MsgEnter-E1 No room left for new prefixes;  prefix not entered:\n\
            [%s]\n\
            Change parameter MAXPREFIXES in msgdata.h", Prefix );
	  MessageOut( m1000 );

	  Found = MsgFind( "MsgEnter-E1", &EID, &Active, &Alarming, &Counting );
	  if ( Found && Active   ) MessageOut( m1000 );
	  if ( Found && Alarming ) MsgAlarm( m1000, prefix[EID].AlarmLevel );  /* Level 3 alarm.  */
	  if ( Found && Counting ) MsgIncr( EID );
	}

	return;
}





	void	MsgAbortCheck(
/*  Input:  */
	const int ID ) /* Fast-reference msg ID of a message to be checked  */
{
/*  Description:  Abort program if abort limit reached.
	Check the specified message-prefix's (ID) abort limit.  If it
	has been reached, display a message, then output the summary
	to the journal file, then abort.
*/

	int Void;

	if        ( prefix[ID].AbortLimit <= 0 ) {
	} else if ( prefix[ID].Counts     >= prefix[ID].AbortLimit ) {
/*	  MessageOut( "MsgAbortCheck  Aborting on message [%s]", prefix[ID].Prefix );  */
	  MessageOut( "MsgAbortCheck  Aborting on message [%s]" );
	  if ( JournalFILE != NULL ) {
	    MsgSummaryFile(    JournalFILE ); /* Put an msg summary on the journal file. */
	    MsgSummaryCPUFile( JournalFILE ); /* Put an msg CPU-usage summary on the journal file. */
	    Void = MsgJournalClose();              /* Close the file, ignore return status.  */
	  }
	  exit(1);

	}

	return;
}




	void	MsgCheck(
/*  Input:  */
	  const char *msg /* A message, with prefix.  msg can be just a prefix.  */

/*  Input/output:  */
	, int *ID    /* Message ID.  Set to zero by caller 
                        before first call, set by Message on first call by
                        looking up or entering the prefix contained in MSG
                        (prefix is everything before the first space) in
	                the index of message prefixes.  */

/*  Outputs:  */
	, int *Active     /* Flag set to .TRUE. if the message selected
                             (by prefix and/or ID) is active.                */
	, int *Alarming   /* Flag set to .TRUE. if the message selected
                             (by prefix and/or ID) is enabled for alarming.  */
	, int *Counting ) /* Flag set to .TRUE. if the message selected
                             (by prefix and/or ID) is enabled for counting.  */
{
/*   Description:  Lookup a prefix, define new ones, return fast-ref ID.
	Find the character string consisting of MSG's prefix in the
	index of message prefixes (sic).  If found
	in the index, return the index ID and the flags Active
	and Counting.  If ID is specified as positive,
	a lookup in the index is not preformed, but a
	check that ID is in the index and a check that
	MSG's prefix agrees with the stored message
	prefix selected by ID are made.  If the stored prefix
	does not agree with msg's prefix, MsgCheck issues an error
	message (which indicates a program bug!), then zeros ID and starts
	again, with ID set to the correct index ID if MSG's prefix is in the
	index, and entering MSG's prefix if it is not in the index.
	If not found, the prefix is entered in the index.
	The flags Active and Counting are set according to the whether the
	message is active (output enabled) and enabled for counting.
*/


	int Found;
	int E_Found, E_Active, E_Alarming, E_Counting, CallerBug;
	char Prefix[PREFIX_MAXLEN], *PrefixGiven, *PrefixStored;

	int firstNonBlank, L, LID;
	int C;
	int LastNonBlank;

	static int EID=0;

	const char seplist[] = " \t\n\f\0";  /* Space, tab, newline, form-feed, null */



	LID = *ID;   /* Make a local copy.  */
	CallerBug = FALSE;

	if        ( LID <= 0 ) { /* Look up the message in the index: */
	  if ( !MsgInitialized ) MsgInit( "" );
	  MsgGetPrefix( msg, Prefix );
	  Found = MsgFind( Prefix, &LID, Active, Alarming, Counting ); /*  Get its ID & flags.  */
	} else if ( LID <= control->Nprefixes ) {   /* It's in the index:  */
	  Found    = TRUE;
	  *Active   = prefix[LID].Active;
	  *Alarming = prefix[LID].Alarming;
	  *Counting = prefix[LID].Counting;
	  if ( !*Active ) {  /* Waste no more time unless active.  */
	    return;
	  } else { /* It's active -- it's OK to spend some time:  */
	    MsgGetPrefix( msg, Prefix );
	    if ( !strncmp( Prefix, prefix[LID].Prefix, PREFIX_MAXLEN+1 ) ) {   /* It's the right one:  */
	    } else {  /* It's wrong -- this is a caller-bug:  */
	      CallerBug = TRUE;
/*	      First check if this message is disabled:  */
	      E_Found = MsgFind( "MsgCheck-B1", &EID, &E_Active, &E_Alarming, &E_Counting );
	      if ( !E_Found ) {   /* Enter the prefix in the index.  */
	        MsgEnter( "MsgCheck-B1", &EID );
	        E_Active   = TRUE;
	        E_Alarming = FALSE;
	        E_Counting = TRUE;
	        prefix[EID].Active   = E_Active;  /* Overwrite usual defaults.  */
	        prefix[EID].Alarming = E_Alarming;
	        prefix[EID].Counting = E_Counting;
	      }
	      if ( E_Active || E_Alarming ) {
	        PrefixGiven  = Prefix;
	        PrefixStored = prefix[LID].Prefix;

/*	        This is a nice long message -- it should only occur if someone misuses
	        the message library (msglib) routines in code, and as such should only
	        occur "once", after which the programmer should fix the problem (he
	        should feel glad the thing didn't crash!  So don't complain.)            */

	        sprintf( m1000, "\
MsgCheck-B1 Program bug in caller.  The given ID: %d\n\
doesn't select the given prefix: [%s]\n\
The prefix stored as that ID is: [%s]\n\
Note that each message needs to have its own ID variable."
	               , LID, PrefixGiven, PrefixStored );

	        if ( E_Active ) {
	          MessageOut( m1000 );
	        }
	        if ( E_Alarming ) {
	          MsgAlarm( m1000, 3 ); /* Alarm level 3. */
	        }
	      }
	      if ( E_Counting ) {
	        MsgIncr( EID );
	      }
	      Found = MsgFind( Prefix, &LID, Active, Alarming, Counting ); /* Set the ID & flags. */
	    }
	  }       /*  if ( !*Active )  */
	} else {  /*  if ( LID <= 0 )  */
	    Found = FALSE;
	}         /*  if ( LID <= 0 )  */

	if ( !Found ) {  /* Enter the prefix in the index. */
	  MsgEnter( Prefix, &LID );
	}

/*	Check whether it's exceeded its counting limit:  */
	if ( LID > 0 ) {
	  *Active   = prefix[LID].Active;
	  *Alarming = prefix[LID].Alarming;
	  *Counting = prefix[LID].Counting;
	  if       ( !prefix[LID].Active ) {
	  } else if ( prefix[LID].CountLimit <= 0 )  { /* No counting limit.  */
	  } else if ( prefix[LID].Counts >= prefix[LID].CountLimit ) {
/*	    Turn off messages -- continue counting, of course:  */
	    prefix[LID].Active = FALSE;
	    *Active            = FALSE;
/*	    Display a message-shut-off warning:  */
/*	    MessageOut( "%s Disabled -- count limit reached", prefix[LID].Prefix );  */
	    sprintf( m1000, "%s Disabled -- count limit reached", prefix[LID].Prefix );
	    MessageOut( m1000 );
	  } /* !prefix[LID].Active */
	} else { /* Not found, not entered -- total failure.  Just set these false:  */
	  *Active   = FALSE;
	  *Alarming = FALSE;
	  *Counting = FALSE;
	}

	if ( Found && *Active ) { /*  Make the latest message this one's sample:  */
/*	  Left-justify the message-sample -- don't want indentations in the table.
	  Scan for first non-separator:  */
	  L  = strlen( msg ) - 1;
	  firstNonBlank = 0;
	  C  = msg[firstNonBlank]; /*  Integer copy of a character for strchr.  */
	  while (  ( strchr( seplist, C ) != NULL )  && ( firstNonBlank < L )  ) {
	    C   = msg[++firstNonBlank]; /*  Integer copy of a character for strchr.  */
	  }

	  strncpy( prefix[LID].Sample, &msg[firstNonBlank], SAMPLE_MAXLEN+1 );

	}

	if ( CallerBug ) { /*  Don't set the given ID.  */
	} else {            /*  Set it. */
	  *ID = LID;
	}


	return;
}




	void	MsgDeleteID(
/*  Input:  */
	const int ID ) /* Fast-reference ID of a prefix.  */
{
/*  Description:  Delete the specified prefix.
*/

	int i;

	if ( ID <= 0                ) return;
	if ( ID > control->Nprefixes ) return;

	control->Nprefixes -= 1;

	for ( i = ID; i <= control->Nprefixes; i++ ) {
	  memcpy( &prefix[i], &prefix[i+1], sizeof(prefix[i]) );
/*	  prefix[i].Iclass     = prefix[i+1].Iclass;
	  prefix[i].Counts     = prefix[i+1].Counts;
	  prefix[i].Lookups    = prefix[i+1].Lookups;
	  prefix[i].CPUmark    = prefix[i+1].CPUmark;
	  prefix[i].CPUdelta   = prefix[i+1].CPUdelta;
	  prefix[i].CPUtotal   = prefix[i+1].CPUtotal;
	  prefix[i].CountLimit = prefix[i+1].CountLimit;
	  prefix[i].AbortLimit = prefix[i+1].AbortLimit;
	  prefix[i].AlarmLevel = prefix[i+1].AlarmLevel;
	  prefix[i].Active     = prefix[i+1].Active;
	  prefix[i].Alarming   = prefix[i+1].Alarming;
	  prefix[i].Counting   = prefix[i+1].Counting;
	  prefix[i].Prefix     = prefix[i+1].Prefix;
	  prefix[i].Sample     = prefix[i+1].Sample;  */
	}

	control->Sorted  = FALSE;  /* Indicate that a new sort will be needed. */

	return;
}




	int	MsgEnterClass(
/*  Input:  */
	  const char *Class /* A prefix class.  */
/*  Output:  */
	, int *ID ) /* Class ID of the newly-entered
	               prefix-class Class;  zero for the
	               null class.  */
{
/*   Description:  Define new class and assign an ID.

	Enter the prefix-class into the index.
	If there is no more room in the index, display a message
	and return false.

  Returns:
	TRUE  If Class is entered in the index.
	FALSE If there is no more room in the index.
*/

	int EID;
	int L;
	int Found, Active, Alarming, Counting;

	if ( control->Nclasses < MAXCLASSES ) {   /* There's room: */
	  L = MsgLNB( Class, CLASS_MAXLEN );  /* Always returns L<=CLASS_MAXLEN-1  */
	  if ( L < 0 ) { /*  Null class -- don't actually enter it.  */
	    *ID = 0;
	  } else {
	    control->Nclasses += 1;
	    *ID = control->Nclasses;
	    strncpy( class[*ID].Class, Class, L+2 );  /*  See note in MsgParsePrefix, about firstDigit.  */
	  }
	  return(TRUE);
	} else {   /* There's no room -- message: */

	  sprintf( m1000, "\
MsgEnterClass-E1 No room left for new classes;  class not entered:\n\
                 [%s]\n\
                 Change parameter MAXCLASSES in msgdata.h", Class );

	  Found = MsgFind( "MsgEnterClass-E1", &EID, &Active, &Alarming, &Counting );
	  if ( Found && Active   ) MessageOut( m1000 );
	  if ( Found && Alarming ) MsgAlarm( m1000, prefix[EID].AlarmLevel );  /* Level 3 alarm.  */
	  if ( Found && Counting ) MsgIncr( EID );
	  return(FALSE);
	}
}




	void	MsgParse(
/*  Input:                                                                               */
	  const char   *msg  /* A message, with prefix.                                  */
/*  Outputs:                                                                             */
	, char   *prefix     /* Separate prefix. (must have length > PREFIX_MAXLEN       */
	, char  **sansPrefix )  /* Start of message-without-prefix (and without separator). */
{
/*  Description:  Locate the separator between the prefix and the rest of the message.
	Set Nprefix = 0 if no prefix is found, Nmessage = 0 if no message is found.
	Set Isep = -1 if no separator is found.                                          */

	int firstNonBlank, In, L, C, N;
	int prefixEnd;
	int Isep;
	char *sepPoint;

	const char seplist[] = " \t\n\f\0";  /* Space, tab, newline, form-feed, null */


/*	Scan for first non-separator:  */
	L  = strlen( msg ) - 1;  /*  Index to last character in msg, before null.  */
	firstNonBlank = 0;
	C  = msg[firstNonBlank];            /*  Integer copy of a character for strchr.  */
	while (  ( strchr( seplist, C ) != NULL )  && ( firstNonBlank < L )  ) {
	  C = msg[++firstNonBlank];       /*  Integer copy of a character for strchr.  */
	}

/*	firstNonBlank is index in msg to start of prefix or else points past msg.  */
	if ( firstNonBlank >= L ) {   /*  No non-separators or only one at the very end.  */
	  C   = msg[L];    /*  Integer copy of a character for strchr.  */
	  if ( strchr( seplist, C ) != NULL ) {  /*  Last is a blank etc. */
	    prefix[0]  = NULL;                   /*  It has no prefix.  */
	    *sansPrefix    = NULL;                   /*  It has no message.  */
	  } else {                               /*  Last is a non-blank. */
	    prefix[0] = msg[L];                  /*  It has only a one-char prefix.  */
	    prefix[1] = NULL;                    /*  It has only a one-char prefix.  */
	    *sansPrefix   = NULL;                    /*  It has no message.  */
	  }
	  return;
	}


/*	Scan for the next separator:  */
	sepPoint = strpbrk( &msg[firstNonBlank], seplist );  /*  Find, in &msg[firstNonBlank], any char. in seplist.  */
	Isep = (int)sepPoint - (int)msg;          /* Index to separator.     */

	if ( sepPoint == NULL ) {   /* Prefix with no following separator (it's all prefix):  */
	  strncpy( prefix, &msg[L], PREFIX_MAXLEN+1 ); /*  Copy as much as is there or fits as a prefix. */
	  prefix[PREFIX_MAXLEN] = NULL;
	  *sansPrefix = NULL;                           /*  No message.  */
	} else {
	  prefixEnd = Isep - 1;   /*  Prefix last-char is one before the separator.  */
	  N = prefixEnd + 1;      /*  Character count is one more than index (index 0 is first). */
	  if ( N > PREFIX_MAXLEN ) N = PREFIX_MAXLEN;
	  strncpy( prefix, &msg[firstNonBlank], N+1 );            /*  Copy as much as is there or fits as a prefix. */
	  prefix[N] = NULL;

/*	  Scan for next non-separator:  */
	  L  = strlen( sepPoint ) - 1;
	  firstNonBlank = 0;
	  C  = sepPoint[firstNonBlank]; /*  Integer copy of a character for strchr.  */
	  while (  ( strchr( seplist, C ) != NULL )  && ( firstNonBlank < L )  ) {
	    C   = sepPoint[++firstNonBlank]; /*  Integer copy of a character for strchr.  */
	  }
	  if ( firstNonBlank >= L ) {   /*  No non-separators or only one at the very end.  */
	    firstNonBlank = 1;
	  }
	  *sansPrefix = sepPoint + firstNonBlank;   /*  Point to everything after the separator.  */
	}


	return;
}




	void	MsgParsePrefix(
/*  Input:  */
	  const char *Prefix      /* Messsage prefix.  */
/*  Outputs: */
	, char *PrefixStripped  /*  Prefix stripped of trailing digits. */
	, int  *PrefixNumber )  /*  Prefix trailing digits, decoded as integer.
	                            If there are no trailing digits, set to zero. */
{
/*  Description:  Parse a prefix into a string and a number.
	Parse the specified message prefix "Prefix" into two
	components:
	1)	The prefix without trailing digits,
	2)	The trailing digits, converted to an integer.
	The principal use of this is for sorting, to preserve the sequence
	1,2,3...8,9,10,11, etc., instead of the straight ASCII sequence,
	which would go as 1,10,11,...,2,20,21,... etc.

	Note that classes are irrelevant to this purpose, and are ignored.
*/
	int done;
	int firstDigit, lastDigit, Ndigits;
	int i, LNB, C;
	char Cnumber[12];


	lastDigit  = MsgLNB( Prefix, PREFIX_MAXLEN );  /* Always returns lastDigit<=PREFIX_MAXLEN-1  */
	firstDigit = -1;

	i = lastDigit;
	Ndigits = 0;
	done = FALSE;
	while ( !done ) {
	  C   = Prefix[i];    /*  Integer copy of a character for strchr.  */
	  if ( strchr( "0123456789", C ) != NULL ) {  /*  "i" points to a digit. */
	    firstDigit = i;
	    Ndigits += 1;
	  } else {                                    /*  Ran past the digits, if any:  */
	    done = TRUE;
	  }
	  i -= 1;
	  if ( i < 0 ) {                              /*  No more characters to test.  */
	    done = TRUE;
	  }
	}


/*	Index "lastDigit" should be no more than PREFIX_MAXLEN-1,  whereas                                                  */
/*	these strings are dimensioned PREFIX_MAXLEN+1, hence "lastDigit+2".                                                 */

	if        ( Ndigits <= 0 ) {                       /*   Found no trailing digits:                                   */
	  strncpy( PrefixStripped, Prefix, lastDigit+2 );  /*     Copy over the whole prefix.                               */
	  PrefixStripped[lastDigit+1] = NULL;              /*     Append NULL.                                              */
	  *PrefixNumber = 0;                               /*     Take zero for the prefix number.                          */
	} else if ( Ndigits > 11 ) {                       /*   Found too many trailing digits:                             */
	  strncpy( PrefixStripped, Prefix, lastDigit+2 );  /*     Copy over the whole prefix.                               */
	  PrefixStripped[lastDigit+1] = NULL;              /*     Append NULL.                                              */
	  *PrefixNumber = 0;                               /*     Take zero for the prefix number.                          */
	} else                     {                       /*   Just right.. .                                              */
	  strncpy( Cnumber, &Prefix[firstDigit], Ndigits ); /*    Put it here so a null can be placed after the digits.     */
	  Cnumber[Ndigits] = NULL;                         /*     Append NULL.                                              */
	  *PrefixNumber = strtoul( Cnumber, NULL, 10 );    /*     Set the prefix number from the digits.                    */
	  if ( firstDigit == 0 ) {                         /*   Wierd case -- all digits (NULL prefix):                     */
	  } else {                                         /*   Common case -- prefix and digits:                           */
	    strncpy( PrefixStripped, Prefix, firstDigit ); /*     Copy over the prefix without the digits.                  */
	  }                                                /*       (firstdigit is an index, one less than the count.)      */
	  PrefixStripped[firstDigit] = NULL;               /*   Append NULL.                                                */
	}

	return;
}




	int	MsgShare(
/*  Input:                                                                          */
	const pid_t  ProcessID )   /* The ID of the process to share msg memory with. */
{
/* Description:  Set Msg up to share its memory.
	         When this routine returns TRUE, the memory is being shared.
	         This should be the very first Msg call.
	         If a journal file was open at the time of this call, it is closed.
	         The journal will need to be reopened if it had been open.

   Returns:
	shmid if successful,
	-1 for failure.                                                          */

	void *SharedAddress;
	void *addr;
	int size;
	int i;

	int shmid;
	key_t key;
	int shmflg;
	unsigned long L;

	const int ID = 1;
	FILE *shmidLogFILE; /* shmid log-file descriptor                         */

	JournalFILE = fopen( "/dev/null", "w" );  /* Ensure that this isn't NULL, or it'll start outputing to standard out.  */

	size = sizeof( msg );
	addr = &msg;
	addr = 0;

/*	fprintf(stderr,"MsgShare-d1  ID:%d ProcessID:%d  Size:%d  addr:%d 0x%x\n", ID, ProcessID, size, addr, addr);  */

	if ( ProcessID <= 0 ) {
	  control->ProcessID = getpid();
	} else {
	  control->ProcessID = ProcessID;
	}
	key = (key_t)( control->ProcessID );

	shmflg = 0660 | IPC_CREAT;  /*  Read/Write Owner/Group  */
/*	fprintf(stderr,"MsgShare-d2 key: %d shmflg: 0%o \n", key, shmflg );  */

	shmid = shmget( key, size, shmflg );  /*  unique key, size, read/write user/group.  */


/*	fprintf(stderr,"MsgShare-d3  shmid: %d  errno: %d\n", shmid, errno);  */
	if ( shmid < 0 ) {
	  perror( "MsgShare-e1 system error:\n" );
	  return( -1 );
	}

	SharedAddress = shmat( shmid, addr, shmflg );
/*	fprintf(stderr, "MsgShare-d4  SharedAddress: %x  Address: %x\n", SharedAddress, addr );  */

	Msg = SharedAddress;

/*	fprintf(stderr, "MsgShare-d5  Old msg.control address: %x\n", (int)(control) );  */

	control = &Msg->control;
	prefix  = &Msg->prefix[0];
	class   = &Msg->class[0];

/*	fprintf(stderr, "MsgShare-d6  New msg.control address: %x\n", (int)(control) ); */

	if ( !SharedAddress ) {
	  perror( "MsgShare-e2 system error:\n" );
	  return( -1 );
	}

/*	Need to re-establish this:  */
	if ( sysinfo( SI_HOSTNAME, s1000, 1000) < 0 ) s1000[0] = NULL;
	MsgNodeNameSet( s1000 );

	shmidLogFILE = fopen( "msg.shmid", "a" );  /* Append to this log file -- keep track of shmids!  */
	MsgTimeStampFileOut( shmidLogFILE );       /* Put a time stamp on it.                           */
	fprintf( shmidLogFILE, "msg Shared Memory segment attached/created, shmid:%d  pid:%d\n", shmid, key );
	fclose( shmidLogFILE );

	return( shmid );
}




	int	MsgShareNoCreate(
/*  Input:                                                                          */
	const pid_t  ProcessID )   /* The ID of the process to share msg memory with. */
{
/* Description:  Set Msg up to share its memory.
	         When this routine returns TRUE, the memory is being shared.
	         This should be the very first Msg call.
	         If a journal file was open at the time of this call, it is closed.
	         The journal will need to be reopened if it had been open.
	         If the segment doesn't exist, a new one will not be created.

   Returns:
	shmid if successful,
	-1 for failure.                                                          */

	void *SharedAddress;
	void *addr;
	int size;
	int i;

	int shmid;
	key_t key;
	int shmflg;
	unsigned long L;

	const int ID = 1;

	JournalFILE = fopen( "/dev/null", "w" );  /* Ensure that this isn't NULL, or it'll start outputing to standard out.  */

	size = sizeof( msg );
	addr = &msg;
	addr = 0;

/*	fprintf(stderr,"MsgShare-d1  ID:%d ProcessID:%d  Size:%d  addr:%d 0x%x\n", ID, ProcessID, size, addr, addr);  */

	if ( ProcessID <= 0 ) {
	  control->ProcessID = getpid();
	} else {
	  control->ProcessID = ProcessID;
	}
	key = (key_t)( control->ProcessID );

	shmflg = 0660;  /*  Read/Write Owner/Group  */
/*	fprintf(stderr,"MsgShare-d2 key: %d shmflg: 0%o \n", key, shmflg );  */

	shmid = shmget( key, size, shmflg );  /*  unique key, size, read/write user/group.  */


/*	fprintf(stderr,"MsgShare-d3  shmid: %d  errno: %d\n", shmid, errno);  */
	if ( shmid < 0 ) {
	  perror( "MsgShare-e1 system error:\n" );
	  return( -1 );
	}

	SharedAddress = shmat( shmid, addr, shmflg );
/*	fprintf(stderr, "MsgShare-d4  SharedAddress: %x  Address: %x\n", SharedAddress, addr );  */

	Msg = SharedAddress;

/*	fprintf(stderr, "MsgShare-d5  Old msg.control address: %x\n", (int)(control) );  */

	control = &Msg->control;
	prefix  = &Msg->prefix[0];
	class   = &Msg->class[0];

/*	fprintf(stderr, "MsgShare-d6  New msg.control address: %x\n", (int)(control) ); */

	if ( !SharedAddress ) {
	  perror( "MsgShare-e2 system error:\n" );
	  return( -1 );
	}

	return( shmid );
}




	void	MsgSort(void)
{
/*  Description:  Sort the MSG prefixes, in alphabetical order.
*/

	int PrefixNumber1, PrefixNumber2;
	char PrefixStripped1[PREFIX_MAXLEN];
	char PrefixStripped2[PREFIX_MAXLEN];
	int i;
	int compare;
	int ID;


	if ( control->Sorted ) return;   /*  Already sorted.  */

	for ( ID = 1;  ID <= control->Nprefixes; prefix[ID].SID = ID++ );

	while ( !control->Sorted ) {
	  control->Sorted = TRUE;
	  for ( ID = 1;  ID < control->Nprefixes; ID++ ) {
	    MsgParsePrefix( prefix[ prefix[ID  ].SID ].Prefix, PrefixStripped1, &PrefixNumber1 );
	    MsgParsePrefix( prefix[ prefix[ID+1].SID ].Prefix, PrefixStripped2, &PrefixNumber2 );
	    compare = strncmp( PrefixStripped1, PrefixStripped2, PREFIX_MAXLEN );
	    if        ( compare > 0 )                   {  /*  Swap.        */
	      control->Sorted = FALSE;
	      i                = prefix[ID  ].SID;
	      prefix[ID  ].SID = prefix[ID+1].SID;
	      prefix[ID+1].SID = i;
	    } else if ( compare < 0 )                   {  /*  Do nothing.  */
	    } else if ( PrefixNumber1 > PrefixNumber2 ) {  /*  Swap.        */
	      control->Sorted = FALSE;
	      i                = prefix[ID  ].SID;
	      prefix[ID  ].SID = prefix[ID+1].SID;
	      prefix[ID+1].SID = i;
	    }
	  }
	}
	return;
}

