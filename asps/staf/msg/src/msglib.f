*	This is the STAR message/error handling package (prefix MSG_).

*	===================================
*	==  User Routines in this file.  ==
*	===================================

*	Created  1-Nov-1991   Robert Hackenburg
*	Modified 4-Feb-1992   Robert Hackenburg
*	Full on/off capability
*	added    8-May-1992   Robert Hackenburg

*	INCLUDE files follow the STAR standard, which is that
*	they are logical names, assigned to actual files by
*	command proceedures (VMS) or script files ("UNIX").

*	For VMS, this is:  MSG_INCLUDE_DEFINE.COM

*	For "UNIX", this is:  @msg_include_define  ("@" is part of filename.)

*	STAR Standard Return Conditions defined in include file msg_ret_inc.


*                              ***
*                             ** **
*                            **   **
*                    *********     *********
*                      ****           ****
*                        ***         ***
*                        **     *     **
*                       **   *******    **
*                      **  ***     ***  **
*                     **                 **

	SUBROUTINE MESSAGE(MSG,LINES,ID)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments:  none

*   Functional Description:
*	Conditionally display a message MSG, containing LINES lines of up to
*	132 characters each on the terminal and the journal, if enabled by a
*	call to MSG_JOURNAL_ON.  The message is displayed unless disabled.
*	The message is counted unless counting is disabled.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING

	INTEGER LID !Local copy of ID.

	LID=ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_CHECK(MSG,LID,ACTIVE,COUNTING)

	IF (COUNTING) THEN !COUNTING is true only if found.
	  CALL MSG_INCR(LID)
	END IF !COUNTING

	IF (ACTIVE) THEN !Display it:
	  CALL MESSAGE_OUT(MSG,LINES)
	  CALL MSG_Abort_Check( LID ) !Check if abort limit is exceeded, and maybe abort.
	END IF

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	SUBROUTINE MESSAGE_OUT(MSG,LINES)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES !Number of lines in MSG.

*   Output arguments:  none

*   Functional Description:
*	Always display a message MSG, containing LINES lines of up to
*	132 characters each on the terminal and the journal, if
*	enabled by a call to MSG_JOURNAL_ON.  Displaying is unnaffected
*	by calls to the MSG_DISABLE command, and the message is not
*	"counted".  This is the "pure" I/O part of MESSAGE.

*   Error conditions:  none

	INCLUDE 'msg_inc'

*	Display message on terminal:
	CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_TL)

*	Display time & message on journal file:
	IF (MSG_JOURNAL_ENABLE) THEN
	  CALL MSG_TIME_STAMP(MSG_JL) !Time.
	  CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_JL) !Message.
	END IF
	RETURN
	END

	SUBROUTINE MSG_Abort_Check( ID )

	IMPLICIT NONE

*  Input argument:
	INTEGER ID !Fast-reference msg ID of a message to be checked.

*  Functional description:
*	Check the specified message-prefix's (ID) abort limit.  If it
*	has been reached, display a message, then output the summary
*	to the journal file, then abort.

	INCLUDE 'msg_inc'

	INTEGER TL, JL
	LOGICAL Void
	LOGICAL MSG_Journal_Close

	IF ( MSG_Abort_Limit( ID ) .LE. 0 ) THEN !No abort limit -- do nothing.
	
	ELSE IF ( MSG_Counts( ID ) .GE. MSG_Abort_Limit( ID ) ) THEN !Time to abort.

	  CALL MESSAGE_OUT( 'MSG_Abort_Check  Aborting on message [' // MSG_Prefix(ID)(:MSG_Length(ID)) // ']', 1 )

	  IF (MSG_JOURNAL_ENABLE) THEN !Journal is open & enabled:
	    CALL MSG_Get_LUN( TL, JL ) !Terminal LUN (don't care), Journal LUN.
	    CALL MSG_Summary( JL ) !Put an msg summary on the journal file.
	    Void = MSG_Journal_Close() !Close the file, ignore return status.
	  END IF

*	  Abort:
	  CALL EXIT

	END IF

	RETURN
	END


	SUBROUTINE MSG_COUNT(PREFIX)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*   Functional Description:
*	Enable counting, and continue displaying the message
*	recognized by PREFIX.  Has no effect if counting
*	has not been disabled.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Counting(ID)=.TRUE.
	  END DO
	  MSG_All_Nocount=.FALSE.

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Counting(ID)=.TRUE.
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_CHECK(PREFIX,ID,ACTIVE,COUNTING)
	  IF (ID.GT.0) MSG_Counting(ID)=.TRUE.

	END IF !WPT.EQ.1

	RETURN
	END

	SUBROUTINE MSG_DISABLE(PREFIX)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*   Functional Description:
*	Disable displaying, but continue counting the message
*	recognized by PREFIX.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Active(ID)=.FALSE.
	  END DO
	  MSG_ALL_DISABLE=.TRUE.

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Active(ID)=.FALSE.
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_CHECK(PREFIX,ID,ACTIVE,COUNTING)
	  IF (ID.GT.0) MSG_Active(ID)=.FALSE.

	END IF !WPT.EQ.1

	RETURN
	END

	SUBROUTINE MSG_DISPLAY(MSG,LINES,ID)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments: none

*   Functional Description:
*	Conditionally display & count a message MSG, containing LINES lines
*	of up to 132 characters each, on the terminal.

*   Return conditions: none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING

	INTEGER LID !Local copy of ID.

	LID=ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_CHECK( MSG, LID, ACTIVE, COUNTING )

	IF (COUNTING) THEN !COUNTING is true only if found.
	  CALL MSG_INCR(LID)
	END IF

	IF (ACTIVE) THEN !Display it:
	  CALL MSG_DISPLAY_OUT( MSG, LINES )
	  CALL MSG_Abort_Check( LID ) !Check if abort limit is exceeded, and maybe abort.
	END IF

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	SUBROUTINE MSG_DISPLAY_AND_ECHO(MSG,LINES,LUN,ID)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.
	INTEGER LUN  !FORTRAN logical unit number for MSG to be echoed to.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments: none

*   Functional Description:
*	Conditionally display and count a message MSG, containing LINES lines
*	of up to 132 characters each, on the terminal, and echo the display on
*	FORTRAN logical unit LUN.

*   Return conditions: none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING

	INTEGER LID !Local copy of ID.

	LID=ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_CHECK(MSG,LID,ACTIVE,COUNTING)

	IF (COUNTING) THEN !COUNTING is true only if found.
	  CALL MSG_INCR(LID)
	END IF

	IF (ACTIVE) THEN !Display it:
	  CALL MSG_DISPLAY_AND_ECHO_OUT(MSG,LINES,LUN)
	  CALL MSG_Abort_Check( LID ) !Check if abort limit is exceeded, and maybe abort.
	END IF

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	SUBROUTINE MSG_DISPLAY_AND_ECHO_OUT(MSG,LINES,LUN)

	IMPLICIT NONE

*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.
	INTEGER LUN  !FORTRAN logical unit number for MSG to be echoed to.

*   Output arguments: none

*   Functional Description:
*	Display a message MSG, containing LINES lines of up to
*	132 characters each, on the terminal, and echo the display on
*	FORTRAN logical unit LUN.

*   Return conditions: none

	INCLUDE 'msg_inc'

*	Display message on terminal:
	CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_TL)

*	Echo message onto LUN:
	CALL MSG_TO_LUN_OUT(MSG,LINES,LUN)

	RETURN
	END

	SUBROUTINE MSG_DISPLAY_OUT(MSG,LINES)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Output arguments: none

*   Functional Description:
*	Display a message MSG, containing LINES lines of up to
*	132 characters each, on the terminal.

*   Return conditions: none

	INCLUDE 'msg_inc'

*	Display message on terminal:
	CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_TL)

	RETURN
	END

	SUBROUTINE MSG_ENABLE(PREFIX)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*   Functional Description:
*	Enable message-output for the STAR-standard message with
*	stored prefix PREFIX.  Has no effect if message-display
*	has not been disabled.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Active(ID)  =.TRUE.
	    MSG_Counting(ID)=.TRUE. !Active, No-Counting is an illegal state.
	  END DO
	  MSG_ALL_DISABLE=.FALSE.

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Active(ID)  =.TRUE.
	      MSG_Counting(ID)=.TRUE. !Active, No-Counting is an illegal state.
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_CHECK(PREFIX,ID,ACTIVE,COUNTING)
	  IF (ID.GT.0) THEN
	    MSG_Active(ID)=.TRUE.
	    MSG_Counting(ID)=.TRUE. !Active, No-Counting is an illegal state.
	  END IF

	END IF !WPT.EQ.1

	RETURN
	END

	LOGICAL FUNCTION MSG_ENABLED(PREFIX,ID)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments: none

*   Functional Description:
*	Returns the message-enabled-status for the STAR-standard message with
*	stored prefix PREFIX, message-index ID.  If the message is not
*	defined, it is defined and enabled, with counting turned on.

*   Return conditions:
*	.TRUE. if the message is enabled (active),
*	.FALSE. if the message is disabled (inactive).

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING

	INTEGER LID !Local copy of ID.


	LID=ID

*	Check that the message-prefix is in the index, enter it
*	in the index if necessary, and return its ID:
	CALL MSG_CHECK(PREFIX,LID,ACTIVE,COUNTING)

	IF (ACTIVE) THEN !Message is enabled;  return .TRUE.
	  MSG_ENABLED=.TRUE.
	ELSE             !Message is disabled; return .FALSE.
	  MSG_ENABLED=.FALSE.
	  IF (COUNTING) THEN !Count here, if disabled.
	    CALL MSG_INCR(LID)
	  END IF !COUNTING
	END IF !ACTIVE

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	LOGICAL FUNCTION MSG_ENABLED_TRACE(PREFIX,ID)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments: none

*   Functional Description:
*	Returns the message-enabled-status for the STAR-standard message with
*	stored prefix PREFIX, message-index ID.  If the message is not
*	defined, it is defined, but not enabled nor does it count, until
*	explicitly requested.
*	Identical to MSG_ENABLED, except that if a message is defined here,
*	it is defined to be inactive and non-counting, regardless of whether
*	"enable *" (enable all) has been set.  "Trace" messages must be
*	specifically enabled.
*	This is intended to be used with trace messages, which normally
*	reside in code, but are intended to be inactive until explicitly
*	activated to trace the flow of the code, as in debugging.

*   Return conditions:
*	.TRUE. if the message is enabled (active),
*	.FALSE. if the message is disabled (inactive).

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING,FOUND

	INTEGER LID !Local copy of ID.

	LOGICAL MSG_FIND


	LID=ID

*	This prevents a test with MSG_ENABLED_TRACE from automatically
*	defining (& enabling!) a message:
	IF (LID.LE.0) THEN !Look up the message in the index:

	  FOUND=MSG_FIND(PREFIX,LID,ACTIVE,COUNTING) !Set the LID & flags.

	  IF (.NOT.FOUND) THEN !Enter the prefix in the index:

	    CALL MSG_ENTER(PREFIX,LID)  !Enter it, permitting faster ID-reference.

	    IF (LID.GT.0) THEN          !Check that it was entered OK.
	      MSG_Active(LID)=.FALSE.   !But don't enable a message defined here!
	      MSG_Counting(LID)=.FALSE. !And don't even let such a one count!
	    END IF !LID.GT.0

	    IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	    MSG_ENABLED_TRACE=.FALSE.        !Return, saying "message not enabled".
	    RETURN

	  END IF !.NOT.FOUND

	ELSE !Fast lookup:
	  ACTIVE = MSG_Active(LID)
	  COUNTING = MSG_Counting(LID)

	END IF !LID.LE.0

	IF (ACTIVE) THEN !Message is enabled;  return .TRUE.
	  MSG_ENABLED_TRACE=.TRUE.
	ELSE             !Message is disabled; return .FALSE.
	  MSG_ENABLED_TRACE=.FALSE.
	  IF (COUNTING) THEN !Count here, if disabled.
	    CALL MSG_INCR(LID)
	  END IF !COUNTING
	END IF !ACTIVE

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	SUBROUTINE MSG_Get_LUN( Terminal_LUN, Journal_LUN )

	IMPLICIT NONE

*   Output arguments:
	INTEGER Terminal_LUN !FORTRAN logical unit number of terminal
	                     !for output, typically 6.
	INTEGER Journal_LUN  !FORTRAN logical unit number of journal file.

*   Functional Description:
*	Get the message handler LUNs and pass them to the caller.

*   Error conditions: none

	INCLUDE 'msg_inc'

*	Get the LUNs from the COMMON:
	Terminal_LUN = MSG_TL
	Journal_LUN  = MSG_JL

	RETURN
	END

	SUBROUTINE MSG_INI(JOURNAL_LUN)

	IMPLICIT NONE

*  Input argument:
	INTEGER JOURNAL_LUN !FORTRAN logical unit number of journal file.

*  Functional description:
*	Initialize the STAR-standard message-handling package.
	
*   Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER TERMINAL_LUN
	PARAMETER (TERMINAL_LUN=6)

	MSG_Nprefixes=0
	MSG_Total_Lookups=0 !Count of all prefix (character-search) lookups.
	MSG_All_Count_Limit=0 !No count limit is the default.
	MSG_All_Disable=.FALSE.
	MSG_All_Nocount=.FALSE.
	CALL MSG_Journal_Off
	CALL MSG_Set_LUN(TERMINAL_LUN,JOURNAL_LUN)
	CALL MSG_Set_Summary_Page_Length ( 66 ) !66 lines per page -- default.
	CALL MSG_Name_Node(' ')

	CALL STRELA0 !Initialize elapsed real-time.
	CALL STRCPU0 !Initialize elapsed CPU-time.

	RETURN

	END

	LOGICAL FUNCTION MSG_JOURNAL_CLOSE()

	IMPLICIT NONE

*   Returned value:   STAR standard return condition, see below.

*   Functional Description:
*	Call this subroutine to close the message journal file.
*	Disables message-logging in the journal file.
*	Returns .TRUE. if the file was successfully closed,
*	returns .FALSE. if the file could not be closed (probably
*	because it was not opened)

*   Return conditions:
*	.TRUE. for successful close of journal file.
*	.FALSE. for close failure of journal file.

	INCLUDE 'msg_inc'

	INTEGER id1

	IF (MSG_JOURNAL_ENABLE) THEN !Journal file enabled.
	  CLOSE(UNIT=MSG_JL,ERR=1)
	END IF
	MSG_JOURNAL_CLOSE=.TRUE.
	RETURN

*	Error closing the file - it probably wasn't open.
1	CONTINUE

	CALL MSG_JOURNAL_OFF !Disable journal file.

	CALL MESSAGE(
     1	  'MSG_JOURNAL_CLOSE-E1 Unable to close journal file.',1,id1) !Display the message.

	MSG_JOURNAL_CLOSE=.FALSE.

	RETURN

	END

	SUBROUTINE MSG_JOURNAL_OFF

	IMPLICIT NONE

*   Input arguments:  none
*   Output arguments: none

*   Functional Description:
*	Call this subroutine to disable the message journal file.

*   Error conditions: none

	INCLUDE 'msg_inc'

	MSG_JOURNAL_ENABLE=.FALSE.

	RETURN
	END

	SUBROUTINE MSG_JOURNAL_ON

	IMPLICIT NONE

*   Input arguments:  none
*   Output arguments: none

*   Functional Description:
*	Call this subroutine to enable the message journal file.

*   Error conditions: none

	INCLUDE 'msg_inc'

	MSG_JOURNAL_ENABLE=.TRUE.

	RETURN
	END

	LOGICAL FUNCTION MSG_JOURNAL_OPEN(FILE_NAME)

	IMPLICIT NONE

*   Input argument:
	CHARACTER*(*) FILE_NAME !The journal file-name.

*   Returned value:   STAR standard return condition, see below.

*   Functional Description:
*	Call this subroutine to specify FILE_NAME as the
*	message journal file and to open that file.
*	Returns .TRUE. if the file was successfully opened
*	and enables message logging to the journal file;
*	returns .FALSE. if the file could not be opened, and
*	disables message-logging in the journal file.

*   Return conditions:
*	.TRUE. for successful open of journal file.
*	.FALSE. for open failure of journal file.

	INCLUDE 'msg_inc'

	INTEGER File_ver
	CHARACTER*80 Filename_ver

	CHARACTER*80 MSG(2)
	INTEGER NAME_LEN,id1
	SAVE id1

	LOGICAL STROPENVER

	DATA id1/0/

501	FORMAT('MSG_JOURNAL_OPEN-E1 Unable to open journal file:'
     1	   ,' (version 'I5')'/A)

*	First, make sure it's not already open:
	CLOSE(UNIT=MSG_JL,ERR=1)
1	CONTINUE

*	Open a journal file in a machine-independent fashion:
	IF (STROPENVER( MSG_JL, FILE_NAME
     1	              , 'STATUS=NEW, CARRIAGECONTROL=LIST'
     2	              , 32767, '.', File_ver, Filename_ver ) ) THEN

	  MSG_JOURNAL_OPEN=.TRUE.

	  CALL MSG_JOURNAL_ON !Enable journal file.

	ELSE

*	  OPEN error - probably already a file by that name if a unix.

	  CALL MSG_JOURNAL_OFF !Disable journal file.

	  NAME_LEN=LEN(FILE_NAME)
	  IF (NAME_LEN.GT.80) NAME_LEN=80 !Ensure that the name fits.

	  WRITE(MSG,501,ERR=2) File_ver,FILE_NAME(1:NAME_LEN) !Make the message.

	  CALL MESSAGE(MSG,2,id1) !Display the message.

	  MSG_JOURNAL_OPEN=.FALSE.

	END IF

	RETURN

2	CONTINUE
	CALL MESSAGE('MSG_JOURNAL_OPEN-E2  Format error',1,-1)
	MSG_JOURNAL_OPEN=.FALSE.
	RETURN

	END

	SUBROUTINE MSG_JOURNAL_PAGE

	IMPLICIT NONE
	
*   Functional Description:
*	Write a form-feed to the journal file, if open & enabled.

*   Return conditions: none

	INCLUDE 'msg_inc'

	INTEGER FF
	PARAMETER (FF='014'O) !Octal rep. of ASCII form-feed character.

	IF (MSG_JOURNAL_ENABLE) THEN
	  CALL MSG_TO_LUN_OUT(CHAR(FF),1,MSG_JL) !Route through here, with LUN=MSG_JL.
	END IF

	RETURN
	END

	SUBROUTINE MSG_LUN_PAGE(LUN)

	IMPLICIT NONE

	INTEGER LUN  !FORTRAN logical unit number.
	
*   Functional Description:
*	Write a form-feed to the device openned on LUN.

*   Return conditions: none

	INCLUDE 'msg_inc'

	INTEGER FF
	PARAMETER (FF='014'O) !Octal rep. of ASCII form-feed character.

	CALL MSG_TO_LUN_OUT(CHAR(FF),1,LUN)

	RETURN
	END

	SUBROUTINE MSG_Name_Node(Node_name)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) Node_name

*  Functional description:
*	Enters the ASCII node-name in the msg package, for use
*	in node-stamping (along with the time-stamp) journal
*	entries of message-occurrances.

	INCLUDE 'msg_inc'

	MSG_Node_name=node_name

	RETURN
	END

	SUBROUTINE MSG_NOCOUNT(PREFIX)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*   Functional Description:
*	Disable counting, but continue displaying the message
*	recognized by PREFIX (unless display is also disabled).

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Counting(ID)=MSG_Active(ID) !Can't "no-count" an active message!
	  END DO
	  MSG_All_Nocount=MSG_All_Disable   !Can't "no-count" an active message!

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Counting(ID)=MSG_Active(ID) !Can't "no-count" an active message!
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_CHECK(PREFIX,ID,ACTIVE,COUNTING)
	  IF (ID.GT.0) MSG_Counting(ID)=MSG_Active(ID) !Can't "no-count" an active message!

	END IF !WPT.EQ.1

	RETURN
	END

	INTEGER FUNCTION MSG_RC_JOURNAL_CLOSE()

	IMPLICIT NONE

*   Returned value:   STAR standard return condition, see below.

*   Functional Description:
*	Call this subroutine to close the message journal file.
*	Disables message-logging in the journal file.
*	This is the same as MSG_JOURNAL_CLOSE, but implemented
*	as an integer function returning a "code".

*   Return conditions:
*	MSG_CLOSED_RC for successful close of journal file.
*	MSG_NOT_CLOSED_RC for close failure of journal file.

	INCLUDE 'msg_ret_inc'

	LOGICAL MSG_JOURNAL_CLOSE

	IF (MSG_JOURNAL_CLOSE()) THEN !Success:
	  MSG_RC_JOURNAL_CLOSE=MSG_CLOSED_RC
	ELSE !Failure:
	  MSG_RC_JOURNAL_CLOSE=MSG_NOT_CLOSED_RC
	END IF

	RETURN

	END

	INTEGER FUNCTION MSG_RC_JOURNAL_OPEN(FILE_NAME)

	IMPLICIT NONE

*   Input argument:
	CHARACTER*(*) FILE_NAME !The journal file-name.

*   Returned value:   STAR standard return condition, see below.

*   Functional Description:
*	Call this subroutine to specify FILE_NAME as the
*	message journal file and to open that file.
*	This is the same as MSG_JOURNAL_OPEN, but implemented
*	as an integer function returning a "code".

*   Return conditions:
*	MSG_OPENED_RC for successful open of journal file.
*	MSG_NOT_OPENED_RC for open failure of journal file.

	INCLUDE 'msg_ret_inc'

	LOGICAL MSG_JOURNAL_OPEN

	IF (MSG_JOURNAL_OPEN(FILE_NAME)) THEN !Success:
	  MSG_RC_JOURNAL_OPEN=MSG_OPENED_RC
	ELSE !Failure:
	  MSG_RC_JOURNAL_OPEN=MSG_NOT_OPENED_RC
	END IF

	RETURN

	END

	SUBROUTINE MSG_Set_Abort_Limit( Prefix, Limit )

	IMPLICIT NONE

*  Input arguments:
	CHARACTER*(*) Prefix !A STAR-standard message prefix.
	INTEGER Limit !Maximum no. of times to display a message before aborting.

*   Functional Description:
*	Set the abort limit for the message recognized by Prefix;
*	Program termination results when the prefix's
*	count has exceeded Limit.  Wildcards are permitted.
*	The existing count-limit (after which message-display is disabled
*	for Prefix), is set to the same value -- aborting msg calls
*	should not be quiet without deliberate intervention (ie, re-specify
*	the count-limit after this call to obtain silent aborts -- don't
*	do it!).

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Abort_limit(ID)=LIMIT
	    MSG_Count_limit(ID)=LIMIT
	    MSG_Active  ( ID ) = .TRUE.
	    MSG_Counting( ID ) = .TRUE.
	  END DO

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Abort_limit(ID)=LIMIT
	      MSG_Count_limit(ID)=LIMIT
	      MSG_Active  ( ID ) = .TRUE.
	      MSG_Counting( ID ) = .TRUE.
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_CHECK(PREFIX,ID,ACTIVE,COUNTING)
	  IF (ID.GT.0) THEN
	    MSG_Abort_limit(ID)=LIMIT
	    MSG_Count_limit(ID)=LIMIT
	    MSG_Active  ( ID ) = .TRUE.
	    MSG_Counting( ID ) = .TRUE.
	  END IF

	END IF !WPT.EQ.1

	RETURN
	END

	SUBROUTINE MSG_Set_By_Command( COM )

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) COM !Command-string describing features to be set.

*   Functional Description:
*	Set the MSG package parameters on individual messages, referenced
*	by prefix, according to arguments contained in a single character-
*	string command contained in COM.  Commands take these forms:

*	To disable specific messages, but to continue counting their occurances:
*	DISABLE prefix-1 prefix-2 ... prefix-n

*	To enable specific messages:
*	ENABLE prefix-1 prefix-2 ... prefix-n

*	To resume counting of message occurances, whether or not disabled:
*	COUNT prefix-1 prefix-2 ... prefix-n

*	To stop counting of message occurances, whether or not disabled:
*	NOCOUNT prefix-1 prefix-2 ... prefix-n

*	To set message-limits on specific messages:
*	LIMIT prefix-1=limit-1 prefix-2=limit-2 ... prefix-n=limit-n

*	To set message-abort-limits on specific messages:
*	ABORT prefix-1=limit-1 prefix-2=limit-2 ... prefix-n=limit-n

*	To set the number of lines per page in the summary-output:
*	LINES line-count

*	Spacing is not critical in the command line, provided at least one
*	or more spaces, tabs or commas separate the arguments.
*	The "=" symbols in the LIMIT command are essential, but spaces around
*	it are optional and not at all critical.

*	Wildcards are permitted in the prefix specifications.

*   Return conditions: none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER NARGS_max
	PARAMETER (NARGS_max=80)
	INTEGER IARG(NARGS_max)
	DOUBLE PRECISION DARG(NARGS_max)
	CHARACTER*80 ARG(NARGS_max)
	LOGICAL VIARG(NARGS_max),VDARG(NARGS_max)

	INTEGER JARG,NARGS

	CHARACTER*80 M80(2)
	INTEGER L
	LOGICAL ERROR

*	Parse out the arguments:
	CALL STRPARSE(COM,NARGS_max,NARGS,ARG,DARG,IARG,VDARG,VIARG)

*	Convert first arg. to all-caps:
	CALL STRCAP(1,80,ARG(1))

	JARG=2
	DO WHILE (JARG.LE.NARGS)
	  ERROR=.FALSE.
	  IF      (ARG(1).EQ.'DISABLE') THEN
	    CALL MSG_DISABLE(ARG(JARG))
	    JARG=JARG+1
	  ELSE IF (ARG(1).EQ.'ENABLE' ) THEN
	    CALL MSG_ENABLE(ARG(JARG))
	    JARG=JARG+1
	  ELSE IF (ARG(1).EQ.'COUNT'  ) THEN
	    CALL MSG_COUNT(ARG(JARG))
	    JARG=JARG+1
	  ELSE IF (ARG(1).EQ.'NOCOUNT') THEN
	    CALL MSG_NOCOUNT(ARG(JARG))
	    JARG=JARG+1
	  ELSE IF (ARG(1).EQ.'LIMIT'  ) THEN
	    IF (JARG+2.GT.NARGS) THEN !Not enough arguments are present.
	      ERROR=.TRUE.
	    ELSE IF (ARG(JARG+1).NE.'=') THEN !Wrong format.
	      ERROR=.TRUE.
	    ELSE IF (.NOT.VIARG(JARG+2)) THEN !Not a valid integer.
	      ERROR=.TRUE.
	    ELSE !All's well -- set this limit:
	      CALL MSG_SET_LIMIT(ARG(JARG),IARG(JARG+2))
	    END IF
	    JARG=JARG+3
	  ELSE IF (ARG(1).EQ.'ABORT'  ) THEN
	    IF (JARG+2.GT.NARGS) THEN !Not enough arguments are present.
	      ERROR=.TRUE.
	    ELSE IF (ARG(JARG+1).NE.'=') THEN !Wrong format.
	      ERROR=.TRUE.
	    ELSE IF (.NOT.VIARG(JARG+2)) THEN !Not a valid integer.
	      ERROR=.TRUE.
	    ELSE !All's well -- set this abort-limit:
	      CALL MSG_Set_Abort_Limit(ARG(JARG),IARG(JARG+2))
	    END IF
	    JARG=JARG+3
	  ELSE IF (ARG(1).EQ.'LINES'  ) THEN
	    IF (JARG.GT.NARGS) THEN !Not enough arguments are present.
	      ERROR=.TRUE.
	    ELSE IF (.NOT.VIARG(JARG)) THEN !Not a valid integer.
	      ERROR=.TRUE.
	    ELSE !All's well -- set the summary page-length:
	      CALL MSG_Set_Summary_Page_Length( IARG(JARG) )
	    END IF
	    JARG=JARG+1
	  ELSE !Unrecognized command.
	    ERROR=.TRUE.
	  END IF

	  IF (ERROR) THEN
	    L=MIN(LEN(COM),58)
	    IF (L.GT.0) THEN
	      WRITE(M80,501,ERR=1) COM(:L)
	      CALL MESSAGE_OUT(M80,2) !2 lines in M80.
	    ELSE
	      CALL MESSAGE_OUT(
     1	        'MSG_SET_BY_COMMAND-E2 zero-length command given.',1)
	    END IF
	    CALL MESSAGE_OUT('    Legal commands are:'//
     1	    ' COUNT, DISABLE, ENABLE, LIMIT, NOCOUNT, ABORT, LINES.',1)
	    JARG=NARGS+1 !Force loop termination.
	  END IF

	END DO !WHILE (JARG.LE.NARGS)

	RETURN

501	FORMAT('MSG_SET_BY_COMMAND-E1 Error in command:'/
     1	       '                      'A)

1	CONTINUE
	CALL MESSAGE('MSG_SET_BY_COMMAND-E3  Format error',1,-1)
	RETURN


	END

	SUBROUTINE MSG_Set_From_File(LUN)

	IMPLICIT NONE

*  Input argument:
	INTEGER LUN !FORTRAN Logical Unit Number, on which a command (ASCII)
	            !file should be (already) open.

*  Functional description:
*	Reads lines from LUN and interprets them as MSG_SET_BY_COMMAND commands
*	until either an <EOF> or a line containing MSG_EXIT is encountered.

	CHARACTER*132 COM

	DO WHILE (.TRUE.) !Infinite loop -- exit on <EOF> or MSG_EXIT.

	  COM=' '
	  READ(LUN,100,ERR=1,END=1) COM
100	FORMAT(A132)
	  IF (COM.EQ.'MSG_EXIT') RETURN
	  IF (COM.EQ.'msg_exit') RETURN
	  CALL MSG_SET_BY_COMMAND(COM) !Set the feature.

	END DO !WHILE (.TRUE.)

*	Come here on EOF or ERR on READ:
1	CONTINUE
	RETURN
	END

	SUBROUTINE MSG_Set_Limit( PREFIX, LIMIT )

	IMPLICIT NONE

*  Input arguments:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.
	INTEGER LIMIT !Maximum no. of times to display a message.

*   Functional Description:
*	Set the count limit for the message recognized by PREFIX;
*	displays of the specified message are disabled once its
*	count has exceeded LIMIT.  Wildcards are permitted.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Count_limit(ID)=LIMIT
	  END DO
	  MSG_ALL_COUNT_LIMIT=LIMIT !And set the default, for new messages...

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Count_limit(ID)=LIMIT
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_CHECK(PREFIX,ID,ACTIVE,COUNTING)
	  IF (ID.GT.0) MSG_Count_limit(ID)=LIMIT

	END IF !WPT.EQ.1

	RETURN
	END

	SUBROUTINE MSG_SET_LUN(TERMINAL_LUN,JOURNAL_LUN)

	IMPLICIT NONE

*   Input arguments:
	INTEGER TERMINAL_LUN !FORTRAN logical unit number of terminal
	                     !for output, typically 6.
	INTEGER JOURNAL_LUN  !FORTRAN logical unit number of journal file.

*   Output arguments:  none

*   Functional Description:
*	Set the message handler LUNs.  The caller specifies
*	the FORTRAN logical unit number of the terminal in
*	TERMINAL_LUN and the journal file in JOURNAL_LUN.

*   Error conditions: none

	INCLUDE 'msg_inc'

*	Put the LUNs into the COMMON:
	MSG_TL=TERMINAL_LUN	
	MSG_JL=JOURNAL_LUN

	RETURN
	END

	SUBROUTINE MSG_Set_Summary_Page_Length( Page_Length )

	IMPLICIT NONE

*  Input argument:
	INTEGER Page_Length  !Length of summary page (lines).

*   Functional Description:
*	Set the number of lines per page for the summary output.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	MSG_Summary_Page_Length = Page_Length

	RETURN
	END

	SUBROUTINE MSG_SUMMARY(LUN)

	IMPLICIT NONE

*  Input argument:
	INTEGER LUN !Device on which the summary is output.

*   Functional Description:
*	Output a summary of the STAR-standard errors on the specified LUN.

*   Return conditions:  none

*   Error conditions:  none

	CALL MSG_SUMMARY_EVENT(LUN,0) !No column of normalized entries.

	RETURN
	END

	SUBROUTINE MSG_SUMMARY_EVENT(LUN,EVENTS)

	IMPLICIT NONE

*  Input argument:
	INTEGER LUN !Device on which the summary is output.
	INTEGER EVENTS !Number of events by which to normalize.

*   Functional Description:
*	Output a summary of the STAR-standard errors on the specified LUN.
*	Include a column of normalized occurances of each message,
*	which consists of a column of the number of occurances of each
*	message, divided by EVENTS.  If EVENTS is not positive, then the
*	column is ommitted.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER MSG_Summary_width_P
	PARAMETER (MSG_Summary_width_P=132)
	INTEGER ID
	LOGICAL SORTED
	INTEGER SID(MSG_Nprefixes_max_P)
	CHARACTER*(MSG_Summary_width_P) MSG(2)
	CHARACTER*(MSG_Summary_width_P) Ghost
	CHARACTER*(MSG_Prefix_length_P) Prefix_stripped_1,Prefix_stripped_2
	CHARACTER*(MSG_Sample_length_P) Sample
	CHARACTER*23 C23
	CHARACTER*8 State
	INTEGER Prefix_number_1,Prefix_number_2
	INTEGER I
	INTEGER LINE,EPT
	REAL Fraction

	INTEGER Header_Lines


*	For Events = 0:
100	FORMAT(T14'--- Message Accounting Summary --- ('A23')'/
     1	      'Message-Prefix & Truncated Sample of last occurance'
     1	       T66'    Counts'T77'     Limit' T90'   Lookups' T101' AbortLimit'T114'   State' )

101	FORMAT( A, T65 '|' I10, T77, I10, T90, I10, T101, I11, T114, A )



*	For No Messages:
102	FORMAT(T14'--- Message Accounting Summary --- ('A23')'/
     1	      '         ---------  No Messages  ----------')


*	For Events > 0:
103	FORMAT(T14'--- Message Accounting Summary --- ('A23')'/
     1	      'Message-Prefix & Truncated Sample of last occurance'
     2	       T66'    Counts'T77'  Counts/evt'T90'     Limit'
     1	       T101'   Lookups' T112' AbortLimit'T124'   State' )

104	FORMAT( A, T65 '|' I10, T77, F12.4, T90, I10, T101, I10, T112, I11, T124, A )


	DO ID=1,MSG_Nprefixes !Initialize the map.
	  SID(ID)=ID
	END DO

	SORTED=.FALSE. !Assume not sorted & force initial loop entry.
	DO WHILE (.NOT.SORTED)
	  SORTED=.TRUE.
	  DO ID=1,MSG_Nprefixes-1
	    CALL MSG_PARSE_PREFIX
     1	     (MSG_Prefix(SID(ID)  ),Prefix_stripped_1,Prefix_number_1)
	    CALL MSG_PARSE_PREFIX
     1	     (MSG_Prefix(SID(ID+1)),Prefix_stripped_2,Prefix_number_2)
	    IF (LGT(Prefix_stripped_1,Prefix_stripped_2)) THEN
	      SORTED=.FALSE.
	      I=SID(ID)
	      SID(ID)=SID(ID+1)
	      SID(ID+1)=I
	    ELSE IF (LLT(Prefix_stripped_1,Prefix_stripped_2)) THEN
*	      Do nothing.
	    ELSE IF (Prefix_number_1.GT.Prefix_number_2) THEN
	      SORTED=.FALSE.
	      I=SID(ID)
	      SID(ID)=SID(ID+1)
	      SID(ID+1)=I
	    END IF !LGT(Prefix_stripped_1,Prefix_stripped_2)
	  END DO !ID=1,MSG_Nprefixes-1

	END DO !WHILE (.NOT.SORTED)

	IF (MSG_Nprefixes.EQ.0) THEN !No messages.
	  CALL MSG_LUN_PAGE(LUN)
	  CALL STRASCDATETIME(C23)
	  WRITE(MSG,102) C23
	  CALL MSG_TO_LUN_OUT(MSG,2,LUN)
	END IF

	LINE=0
	DO ID=1,MSG_Nprefixes

	  IF (LINE.EQ.0) THEN !Do the header:
	    CALL MSG_LUN_PAGE(LUN)
	    CALL STRASCDATETIME(C23)
	    IF (EVENTS.LE.0) THEN !No fractional occurances:
	      WRITE(MSG,100) C23
	    ELSE !Fractional occurances:
	      WRITE(MSG,103) C23
	    END IF
	    Header_Lines = 2
	    CALL MSG_TO_LUN_OUT( MSG, Header_Lines, LUN )
	  END IF

	  Sample=MSG_Sample(SID(ID))
	  CALL STREND(Sample,EPT) !Find last non-blank.
	  IF (EPT.LT.(MSG_Sample_length_P-6)) THEN !More than 6 blanks.
	    DO I=MSG_Sample_length_P-2,EPT+2,-2 !Append alternating dots.
	      Sample(I:I)='.'
	    END DO
	  ELSE !Few or no blanks -- no adulteration necessary:
	  END IF

*	  Determine the state:
	  IF (MSG_Abort_Limit( SID(ID) ).LE.0) THEN
	    State = ' '
	  ELSE IF (MSG_Counts( SID(ID) ).GE.MSG_Abort_Limit( SID(ID) ) ) THEN
*	    This message caused a program termination -- should only be one!
	    State = ' Aborted'
	  ELSE
	    State = ' '
	  END IF

	  IF (State .NE. ' ') THEN !State already determined.
	  ELSE IF ( MSG_Active( SID(ID) ) ) THEN
	    State = '  Active'
	  ELSE IF ( MSG_Counting( SID(ID) ) ) THEN
	    State = 'Counting'
	  ELSE
	    State = 'Inactive'
	  END IF


	  IF (EVENTS.LE.0) THEN !No fractional occurances:
	    WRITE(MSG,101)
     1	          Sample,MSG_Counts(SID(ID))
     1	         ,MSG_Count_limit(SID(ID))
     1	         ,MSG_Lookups(SID(ID))
     1	         ,MSG_Abort_Limit(SID(ID))
     1	         ,State
	  ELSE !Fractional occurances:
	    Fraction=FLOAT(MSG_Counts(SID(ID)))/FLOAT(EVENTS)
	    WRITE(MSG,104)
     1	          Sample,MSG_Counts(SID(ID)),Fraction
     1	         ,MSG_Count_limit(SID(ID))
     1	         ,MSG_Lookups(SID(ID))
     1	         ,MSG_Abort_Limit(SID(ID))
     1	         ,State
	  END IF
	  CALL STREND(MSG,EPT) !Find last non-blank.
	  Ghost = ' '
	  DO I=MSG_Sample_length_P,EPT-2,2 !Fill in with alternating dots.
	    IF (MSG(1)(I-1:I+1).EQ.' ') THEN !Three blanks in a row:
	      MSG(1)(I:I)='.'
	      Ghost(I:I)='.'
	    END IF
	  END DO
	  DO I=MSG_Sample_length_P+2,EPT-4,2 !Check for lone (inserted) dots & remove them.
	    IF (Ghost(I-2:I+2) .EQ. '  .  ' ) THEN
	      MSG(1)(I:I) = ' '
	    END IF
	  END DO
	  CALL MSG_TO_LUN_OUT(MSG,1,LUN)

	  IF ( LINE + 1 + Header_Lines .GE. MSG_Summary_Page_Length) THEN
	    LINE=0 !Start a new page.
	  ELSE
	    LINE=LINE+1
	  END IF

	END DO !ID=1,MSG_Nprefixes

	RETURN
	END

	SUBROUTINE MSG_TIME_STAMP(LUN)

	IMPLICIT NONE
	INTEGER LUN

*   Input arguments:
*	LUN -- FORTRAN logical unit number for time-stamp to be written on.

*   Output arguments: none

*   Functional Description:
*	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
*	"Stamp" the node-name on the same line, if node-name has been specified
*	with a "call msg_name_node(<node_name>)".
*	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
*	as 17-character ASCII strings.

*	Produces output only if different than the previous output from
*	this subroutine.  eg, a call to this routine in a tight loop will
*	only produce an actual time-stamp once each second.

*   Return conditions: none

	INCLUDE 'msg_inc'

	CHARACTER*(MSG_Node_name_length_P) Last_Node_name
	SAVE Last_Node_name

	CHARACTER*23 Date_time, Last_Date_time
	CHARACTER*17 CPU_time
	CHARACTER*18 Elapsed_time
	SAVE Last_Date_time

	DATA Last_Node_name/' '/
	DATA Last_Date_time/' '/

	IF (MSG_Node_name.EQ.' ') THEN !No node name:
*	  Don't bother with a time-stamp if it hasn't changed:
	  CALL STRASCDATETIME(Date_time)
	  IF (Date_time.EQ.Last_Date_time) THEN !No change:
	  ELSE !It has changed:
	    CALL STRELAASC( Elapsed_time )
	    CALL STRCPUASC( CPU_time )
	    CALL MSG_TO_LUN_OUT( Date_time //
     1	                       '   Elapsed:' // Elapsed_time //
     1	                       '   CPU time:' // CPU_time
     1	                       , 1, LUN )
	    Last_Date_time=Date_time
	  END IF

	ELSE                           !Append node name:
*	  Don't bother with a time-stamp if node or time hasn't changed.
	  CALL STRASCDATETIME(Date_time)
	  IF ( (Date_time.EQ.Last_Date_time) .AND.
     1	       (MSG_Node_name.EQ.Last_Node_name)  ) THEN !No change:
	  ELSE !Time or node has changed:
	    CALL STRELAASC( Elapsed_time )
	    CALL STRCPUASC( CPU_time )
	    CALL MSG_TO_LUN_OUT( Date_time //
     1	                       ' From: ' // MSG_Node_name //
     1	                       '   Elapsed:' // Elapsed_time //
     1	                       '   CPU time:' // CPU_time
     1	                       , 1, LUN )
	    Last_Date_time=Date_time
	    Last_Node_name=MSG_Node_name
	  END IF

	END IF !MSG_Node_name.EQ.' '

	RETURN
	END

	SUBROUTINE MSG_TIME_STAMP_OUT(LUN)

	IMPLICIT NONE
	INTEGER LUN

*   Input arguments:
*	LUN -- FORTRAN logical unit number for time-stamp to be written on.

*   Output arguments: none

*   Functional Description:
*	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
*	"Stamp" the node-name on the same line, if node-name has been specified
*	with a "call msg_name_node(<node_name>)".
*	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
*	as 17-character ASCII strings.

*	Always produces output.

*   Return conditions: none

	INCLUDE 'msg_inc'

	CHARACTER*23 Date_time
	CHARACTER*17 CPU_time
	CHARACTER*18 Elapsed_time


	IF (MSG_Node_name.EQ.' ') THEN !No node name:
	  CALL STRASCDATETIME(Date_time)
	  CALL STRELAASC( Elapsed_time )
	  CALL STRCPUASC( CPU_time )
	  CALL MSG_TO_LUN_OUT( Date_time //
     1	                     '   Elapsed:' // Elapsed_time //
     1	                     '   CPU time:' // CPU_time
     1	                     , 1, LUN )

	ELSE                           !Append node name:
	  CALL STRASCDATETIME(Date_time)
	  CALL STRELAASC( Elapsed_time )
	  CALL STRCPUASC( CPU_time )
	  CALL MSG_TO_LUN_OUT( Date_time //
     1	                     ' From: ' // MSG_Node_name //
     1	                     '   Elapsed:' // Elapsed_time //
     1	                     '   CPU time:' // CPU_time
     1	                     , 1, LUN )

	END IF !MSG_Node_name.EQ.' '

	RETURN
	END

	SUBROUTINE MSG_TO_JOURNAL(MSG,LINES,ID)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments: none

*   Functional Description:
*	Conditionally display & count a message MSG, containing LINES lines
*	of up to 132 characters each, on the journal file, if open & enabled.

*   Return conditions: none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING

	INTEGER LID !Local copy of ID.

	LID=ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_CHECK(MSG,LID,ACTIVE,COUNTING)

	IF (COUNTING) THEN !COUNTING is true only if found.
	  CALL MSG_INCR(LID)
	END IF

	IF (ACTIVE) THEN !Display it on the journal file:
	  CALL MSG_TO_JOURNAL_OUT(MSG,LINES)
	  CALL MSG_Abort_Check( LID ) !Check if abort limit is exceeded, and maybe abort.
	END IF

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	SUBROUTINE MSG_TO_JOURNAL_OUT(MSG,LINES)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Output arguments: none

*   Functional Description:
*	Unconditionally display a message MSG, containing LINES lines
*	of up to 132 characters each, on the journal file, if open & enabled.

*   Return conditions: none

	INCLUDE 'msg_inc'

*	Display time & message on journal file:
	IF (MSG_JOURNAL_ENABLE) THEN
	  CALL MSG_TIME_STAMP(MSG_JL) !Time.
	  CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_JL) !Route through here, with LUN=MSG_JL.
	END IF

	RETURN
	END

	SUBROUTINE MSG_TO_LUN(MSG,LINES,LUN,ID)

	IMPLICIT NONE
	
*   Input arguments:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.
	INTEGER LUN  !FORTRAN logical unit number for MSG to be sent to.

*   Input/Output argument:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Output arguments: none

*   Functional Description:
*	Conditionally display & count a message MSG, containing LINES lines
*	of up to 132 characters each, on LUN.

*   Return conditions: none

	INCLUDE 'msg_inc'

	LOGICAL ACTIVE,COUNTING

	INTEGER LID !Local copy of ID.

	LID=ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_CHECK(MSG,LID,ACTIVE,COUNTING)

	IF (COUNTING) THEN !COUNTING is true only if found.
	  CALL MSG_INCR(LID)
	END IF

	IF (ACTIVE) THEN !Display it:
	  CALL MSG_TO_LUN_OUT(MSG,LINES,LUN)
	  CALL MSG_Abort_Check( LID ) !Check if abort limit is exceeded, and maybe abort.
	END IF

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END

	SUBROUTINE MSG_TO_LUN_OUT(MSG,LINES,LUN)

	IMPLICIT NONE
	CHARACTER*(*) MSG(*)
	INTEGER LINES,LUN

*   Input arguments:
*	MSG -- 1 or more line character-string message submitted for display.
*	LINES -- Number of lines in MSG.
*	LUN -- FORTRAN logical unit number for MSG to be written to.

*   Output arguments: none

*   Functional Description:
*	Display a message MSG, containing LINES lines of up to
*	132 characters each, on FORTRAN logical unit LUN.

*   Return conditions: none

	INCLUDE 'msg_inc'

	INTEGER LINE_LEN,I
	INTEGER LastNonBlank, Line_end


1	FORMAT(A)
*2	FORMAT(A) or FORMAT(' 'A):
	INCLUDE 'msg_native_inc' !Define a FORMAT (2) with or w/o car-cont. space.

	LINE_LEN=LEN(MSG(1))
	IF (LINE_LEN.GT.131) LINE_LEN=131

*	Display message on LUN:
	IF (LUN.EQ.MSG_TL) THEN !Message to terminal -- special for VMS:
	  DO I=1,LINES
	    CALL STREND(MSG(I),LastNonBlank) !Point at last non-blank.
	    Line_end=MIN( LINE_LEN, LastNonBlank )
	    WRITE(LUN,2,ERR=10) MSG(I)(1:Line_end)
	  END DO
	ELSE
	  DO I=1,LINES
	    CALL STREND(MSG(I),LastNonBlank) !Point at last non-blank.
	    Line_end=MIN( LINE_LEN, LastNonBlank )
	    WRITE(LUN,1,ERR=10) MSG(I)(1:Line_end)
	  END DO
*	  This seems to be necessary to get AIX (on the RS6000) and other
*	  Unix machines to keep the listing or journal file up-to-date;  without
*	  this, a crash leaves the last several lines of the listing file NOT
*	  in the listing file -- a terrible thing indeed for debugging (yuck):
	  CALL STRFLUSH( LUN )
	END IF


	RETURN

10	CONTINUE

	WRITE(MSG_TL,501)  MSG(1)(:LINE_LEN)
501	FORMAT(' MSG_TO_LUN_OUT-E1  Format error, 1st line:'/
     1	       ' 'A)
	RETURN


	END

