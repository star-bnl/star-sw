/*	This is the STAR message/error handling package (prefix Msg_).

	msgBackCom.c


	Compatibility Routines -- C-callable (non-Fortran callable):


	These routines are for compatibility with earlier C-routines which had to
	deal with the Fortran-based aspects of the original msg incarnation.
	These are not Fortran-callable, but nevertheless are "of Fortran" in a sense.
	They should be regarded as destined for the dust bin, and ought not to be
	used in new code.

	==============================+++==============================
	==  msg Back Compatibility Interface Routines in this file.  ==
	==============================+++==============================

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

static const char sccsid[] = "@(#)"__FILE__"\t\t1.55\tCreated 3/8/98 03:34:03, \tcompiled "__DATE__" "__TIME__;

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

#define MAX_FILE_VERSIONS 999

extern msgData_t msg;
extern FILE *JournalFILE;    /* Journal-file descriptor                          */

extern control_t *control;
extern prefix_t  *prefix;

extern int CPUtime0;
extern int ELAtime0;

extern funcPoint MsgAlarmRoutine;


	void	MsgGetLUN(

/*  Outputs:                                                                          */
	int *Terminal_LUN  /* Fortran logical unit number of terminal, always 6.      */
      ,	int *Journal_LUN ) /* (Substitute for) Fortran LUN of journal file, always 0. */

{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgGetFile instead.  <----
	Get the message handler terminal and journal LUNs and pass them
	to the caller.
*/

	*Terminal_LUN = 6;
	*Journal_LUN  = 0;
	return;
}




	void	MsgDisplayAndEcho(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */
	, int         LUN /* FORTRAN logical unit number for msg to be echoed to;          */
	                  /* The file refered to by LUN must be already opened.            */

/*   Input/Output:                                                                         */
	, int       *ID ) /* Fast-reference message ID.  Set to zero by caller 
	                     before first call, set by Message on first call by
	                     looking up or entering the prefix contained in MSG
	                     (prefix is everything before the first space) in
	                     the index of MSG message prefixes.
	                     If ID is negative, ID remains unchanged, and lookup
	                     is then always by prefix.                                     */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgDisplayAndFile instead.  <----
	Conditionally display a message msg on standard out and echo it to LUN.
	The message is displayed unless disabled.
	The message is counted unless counting is disabled.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	int one=1;
	msg_display_and_echo_( msg, &one, &LUN, ID, strlen(msg) );
	return;
}




	void	MsgDisplayAndEchoOut(

/*   Inputs:                                                                               */
	  const char *msg /* Character-string message, with prefix, submitted for display. */
	, int       LUN ) /* Fortran logical unit number for msg to be echoed to;          */
	                  /* The file refered to by LUN must be already opened.      */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgDisplayAndFileOut instead.  <----
	Display (always) a message msg on standard out and echo it to LUN.
	The message may not be disabled.
	The message is not counted.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	int one=1;
	msg_display_and_echo_out_( msg, &one, &LUN, strlen(msg) );
	return;
}




	void	MsgIni(

/*  Input:                                        */
	int LUN ) /* FORTRAN logical unit number. (ignored) */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgInit instead.  <----
*/
	MsgInit("");
	return;
}



	void	MsgLUNPage(

/*  Input:                                        */
	int LUN ) /* FORTRAN logical unit number. */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgFilePage instead.  <----
	Write a form-feed to the device opened on LUN.
*/
	msg_lun_page_( &LUN );
	return;
}




	void	MsgNameNode(

/*  Input:                  */
	const char *NodeName )  /* Name of node, as will appear on the node-time stamp.  */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgNodeNameSet instead.  <----  */
	MsgNodeNameSet( NodeName );

	return;
}




	void	MsgSetLUN(

/*  Inputs: */
	  int TERMINAL_LUN  /* FORTRAN logical unit number of terminal
	                       for output, typically 6.  */
	, int JOURNAL_LUN ) /* FORTRAN logical unit number of journal file.  */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use nothing instead.  <----
	Formerly, it set the terminal and journal LUNs for msg.
	(ie, the message handler LUNs.)  The caller specifies
	the Fortran logical unit number of the terminal in
	TERMINAL_LUN and the journal file in JOURNAL_LUN.  */

	msg_set_lun_( &TERMINAL_LUN, &JOURNAL_LUN );
	return;
}




	void	MsgSummary(

/*  Input: */
	int LUN ) /* Device on which the summary is output. */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgSummaryFile instead.  <----
	         Generate an msg summary table.  */
	if ( LUN == 6 ) {
	  MsgSummaryFile( NULL );  /*  This will put it on standard out.  */
	} else if ( LUN == 0 ) {
	  MsgSummaryFile( JournalFILE );
	} else {
	  MsgSummaryFile( JournalFILE );
	}
	return;
}




	void	MsgSummaryCPU(

/*  Input:  */
	int LUN ) /* Device on which the summary is output.  */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgSummaryCPUFile instead.  <----
	Generate a CPU-usage MSG-summary table.
	Output a CPU-usage summary of the MSG-handled & marked messages
	on the specified LUN.  "Marked" messages are messages whose occurances
	were preceded, one-for-one, by calls to MSG_Mark, with the same
	MSG prefix.
*/
	if ( LUN == 6 ) {
	  MsgSummaryCPUFile( NULL );  /*  This will put it on standard out.  */
	} else if ( LUN == 0 ) {
	  MsgSummaryCPUFile( JournalFILE );
	} else {
	  MsgSummaryCPUFile( JournalFILE );
	}
	return;
}




	void	MsgSummaryEvent(

/*  Inputs:   */
	  int LUN       /* Device on which the summary is output.  */
	, int Nevents ) /* Number of events by which to normalize. */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgSummaryEventFile instead.  <----
	Generate an MSG summary table with a frequency column.
	Include a column of normalized occurances of each message,
	which consists of a column of the number of occurances of each
	message, divided by Nevents.  If Nevents is not positive, then the
	column is omitted.  */

	if ( LUN == 6 ) {
	  MsgSummaryEventFile( NULL, Nevents );  /*  This will put it on standard out.  */
	} else if ( LUN == 0 ) {
	  MsgSummaryEventFile( JournalFILE, Nevents );
	} else {
	  MsgSummaryEventFile( JournalFILE, Nevents );
	}
	return;
}



	void	MsgTimeStamp(

/*  Inputs:  */
	int LUN ) /* Fortran logical unit number for time-stamp to be written on.  */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgTimeStampFile instead.  <----
	Write a node and time (real and CPU) stamp to a FORTRAN LUN, if new.
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
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgTimeStampFileOut instead.  <----
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
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgToFile instead.  <----
	Conditionally display & count a message MSG, containing LINES lines
	of up to 132 characters each, on LUN.

	Note that unlike the FORTRAN message call, there is no "Lines" argument;
	lines are defined with "\n" (newline) characters.
	However, unlike standard C practice, the caller should NOT include a "\n" at
	the very end, since this interfaces to FORTRAN, which effectively puts a
	"\n" at the very end.  msg may have as many newlines as desired.
*/
	int one=1;
	msg_to_lun_( msg, &one, &LUN, ID, strlen(msg) );
	return;
}




	void	MsgToLUNOut(
	
/*  Inputs:                                                                              */
	  const char *msg /* 1 or more line character-string message
	                     submitted for display;  lines separated by "newline" characters.  */
	, int       LUN ) /* FORTRAN logical unit number for time-stamp to be written on.      */
{
/* Description:  Compatibility routine -- don't use in new code.
	             ---->  Use MsgToFileOut instead.  <----
	Display a message, containing one or more lines of no particular
	length (or length-limit), on FORTRAN logical unit LUN.  Bypass msg accounting.

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
	int one=1;
	msg_to_lun_out_( msg, &one, &LUN, strlen(msg) );
	return;
}




