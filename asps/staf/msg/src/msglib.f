*	This is the STAR message/error handling package (prefix MSG_).

*	===================================
*	==  User Routines in this file.  ==
*	===================================

*	Created  1-Nov-1991   Robert Hackenburg
*	Modified 4-Feb-1992   Robert Hackenburg
*	Full on/off capability
*	added    8-May-1992   Robert Hackenburg
*	Classes
*	added   26-Jan-1994   Robert Hackenburg

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
*
	SUBROUTINE			Message( MSG, Lines, ID )

	IMPLICIT NONE
	
*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER       Lines  !Number of lines in MSG.

*   Input/Output:
	INTEGER ID     !Fast-reference message ID.  Set to zero by caller 
                       !before first call, set by Message on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of MSG message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Outputs:  none

*   Brief description:  Display and log a message.

*   Description:
*	Conditionally display a message MSG, containing Lines lines of up to
*	132 characters each on the terminal and the journal, if enabled by a
*	call to MSG_Journal_On.  The message is displayed unless disabled.
*	The message is counted unless counting is disabled.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL Active, Counting

	INTEGER LID !Local copy of ID.

	LID=ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_Check( MSG, LID, Active, Counting )

	IF ( Counting ) THEN !COUNTING is true only if found.
	  CALL MSG_Incr( LID )
	END IF

	IF ( Active ) THEN !Display it:
	  CALL Message_Out( MSG, Lines )
	  CALL MSG_Abort_Check( LID ) !Check if abort limit is exceeded, and maybe abort.
	END IF

	IF (ID.EQ.0) ID=LID !Ensure it's changed only if zero.

	RETURN
	END
*
	SUBROUTINE			Message_Out( MSG, Lines )

	IMPLICIT NONE
	
*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER        Lines !Number of lines in MSG.

*   Brief description:  Display and log a message, bypassing MSG accounting.

*   Description:
*	Always display a message MSG, containing Lines lines of up to
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
*
	SUBROUTINE			MSG_Class_Define( Class, State, Count_Limit, Abort_Limit )

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) Class       !A STAR-standard message class.
	CHARACTER*(*) State       !'Active', 'Count' or 'Inactive'.
	INTEGER       Count_Limit !Default count-limit for this class.
	INTEGER       Abort_Limit !Default count-limit for this class.

*  Brief description:  Define a new MSG class (of prefixes).

*  Description:
*	Each MSG prefix is assigned to a class according to this scheme:

*	     * If the prefix contains a "-", its class is the first character
*	       after the "-", if non-blank.

*	     * If the prefix ends with a "-" or has no "-", it is assigned
*	       to the null class.

*	The MSG class to which a prefix is assigned is used (soley) for the
*	purpose of setting the prefix's state and limits (count and abort).
*	Classes permit the assignment of a variety of default MSG states
*	and counts for diffent groups of messages.  The predefined MSG
*	classes are these:

*	Class Description            State    Count Limit    Abort Limit

*	 ""   No-class message       Active       50            None
*	  A   Abort message          Active      None        Abort on 1st (msg abort)
*	  B   Bug message            Active      None           None
*	  D   Debug message          Active      None           None
*	  E   Error message          Active       20            None
*	  F   Fatal message          Active      None           None (Application abort)
*	  I   Informative message    Active      None           None
*	  T   Trace message        Inactive      None           None
*	  W   Warning message        Active       10            None

*	These can be changed or new classes added with a call to this routine.

	INCLUDE 'msg_inc'

	CHARACTER*20 CAP_State
	LOGICAL Found
	INTEGER ID

	LOGICAL MSG_Enter_Class
	LOGICAL MSG_Find_Class

	Found = MSG_Find_Class( Class, ID ) !Look up the class in the index.
	IF ( .NOT. Found ) THEN !Didn't find it.
	  IF ( .NOT. MSG_Enter_Class( Class, ID ) ) THEN !No more room & already complained.
	    RETURN !Just get out.
	  END IF
	END IF

	CAP_state = State
	CALL STRCAPS( CAP_state ) !All caps.

	IF      ( CAP_state .EQ. 'ACTIVE' ) THEN
	  MSG_Class_Default_Active(      ID ) = .TRUE.
	  MSG_Class_Default_Counting(    ID ) = .TRUE.
	ELSE IF ( CAP_state .EQ. 'INACTIVE' ) THEN
	  MSG_Class_Default_Active(      ID ) = .FALSE.
	  MSG_Class_Default_Counting(    ID ) = .FALSE.
	ELSE IF ( CAP_state .EQ. 'COUNTING' ) THEN
	  MSG_Class_Default_Active(      ID ) = .FALSE.
	  MSG_Class_Default_Counting(    ID ) = .TRUE.
	END IF

	MSG_Class_Default_Abort_Limit( ID ) = Abort_Limit
	MSG_Class_Default_Count_Limit( ID ) = Count_Limit


	RETURN
	END
*
	SUBROUTINE			MSG_Count( PREFIX )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Brief description:  Enable counting for a prefix.

*  Description:
*	Enable counting, and continue displaying the message
*	recognized by PREFIX.  Has no effect if counting
*	has not been disabled.


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
*
	SUBROUTINE			MSG_Disable( PREFIX )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Brief description:  Disable message-display for a specified prefix.

*  Description:
*	Disable displaying, but continue counting the message
*	recognized by PREFIX.


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
*
	SUBROUTINE			MSG_Display( MSG, LINES, ID )

	IMPLICIT NONE
	
*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Input/Output:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Outputs: none

*   Brief description:  Display a message on the terminal (only).

*   Description:
*	Conditionally display & count a message MSG, containing LINES lines
*	of up to 132 characters each, on the terminal.

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
*
	SUBROUTINE			MSG_Display_Out(MSG,LINES)

	IMPLICIT NONE
	
*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*   Outputs: none

*   Brief description:  Display a message on the terminal (only);  bypass accounting.

*   Description:
*	Display a message MSG, containing LINES lines of up to
*	132 characters each, on the terminal.

	INCLUDE 'msg_inc'

*	Display message on terminal:
	CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_TL)

	RETURN
	END
*
	SUBROUTINE			MSG_Display_and_Echo( MSG, LINES, LUN, ID )

	IMPLICIT NONE
	
*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.
	INTEGER LUN  !FORTRAN logical unit number for MSG to be echoed to.

*   Input/Output:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*   Outputs: none

*   Brief description:  Display a message on the terminal and echo on a FORTRAN LUN.

*   Description:
*	Conditionally display and count a message MSG, containing LINES lines
*	of up to 132 characters each, on the terminal, and echo the display on
*	FORTRAN logical unit LUN.

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
*
	SUBROUTINE			MSG_Display_and_Echo_Out( MSG, LINES, LUN )

	IMPLICIT NONE

*   Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.
	INTEGER LUN  !FORTRAN logical unit number for MSG to be echoed to.

*   Outputs: none

*   Brief description:  Display a message, echo on a FORTRAN LUN, and bypass accounting.

*   Description:
*	Display a message MSG, containing LINES lines of up to
*	132 characters each, on the terminal, and echo the display on
*	FORTRAN logical unit LUN.


	INCLUDE 'msg_inc'

*	Display message on terminal:
	CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_TL)

*	Echo message onto LUN:
	CALL MSG_TO_LUN_OUT(MSG,LINES,LUN)

	RETURN
	END
*
	SUBROUTINE			MSG_Enable( PREFIX )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Brief description:  Enable message-display for a specified prefix.

*  Description:
*	Enable message-output for the STAR-standard message with
*	stored prefix PREFIX.  Has no effect if message-display
*	has not been disabled.

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do them all:

	  DO ID=1,MSG_Nprefixes
	    MSG_Active(      ID ) = .TRUE.
	    MSG_Counting(    ID ) = .TRUE. !Active, No-Counting is an illegal state.
	    MSG_Count_limit( ID ) = 0 !Always reset counting limit here, too.
	  END DO
	  MSG_ALL_DISABLE=.FALSE.

	ELSE IF (WPT.GT.1) THEN !There's a wildcard.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      MSG_Active(      ID ) =.TRUE.
	      MSG_Counting(    ID ) =.TRUE. !Active, No-Counting is an illegal state.
	      MSG_Count_limit( ID ) = 0 !Always reset counting limit here, too.
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_Check( Prefix, ID, Active, Counting )
	  IF (ID.GT.0) THEN
	    MSG_Active(      ID ) = .TRUE.
	    MSG_Counting(    ID ) = .TRUE. !Active, No-Counting is an illegal state.
	    MSG_Count_limit( ID ) = 0 !Always reset counting limit here, too.
	  END IF

	END IF !WPT.EQ.1

	RETURN
	END
*
	LOGICAL FUNCTION		MSG_Enabled( PREFIX, ID )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Input/Output:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Outputs: none

*  Brief description:  Return enabled-status for a prefix;  define new prefix "enabled".

*  Description:
*	Returns the message-display-enabled-status for the STAR-standard message
*	with stored prefix PREFIX, message-index ID.  If the message is not
*	defined, it is defined and enabled, with counting turned on.

*  Return conditions:
*	.TRUE. if the message is enabled (active),
*	.FALSE. if the message is disabled (inactive).

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
*
	LOGICAL FUNCTION		MSG_Enabled_Trace( PREFIX, ID )

	IMPLICIT NONE

*	This routine is obviated by the introduction of classes,
*	but is retained for back-compatibility.

*  Input:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Input/Output:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Outputs: none

*  Brief description:  Return enabled-status for a prefix;  define new prefix "disabled".

*  Description:
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

	    CALL MSG_Enter(PREFIX,LID)  !Enter it, permitting faster ID-reference.

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
*
	SUBROUTINE			MSG_Get_LUN( Terminal_LUN, Journal_LUN )

	IMPLICIT NONE

*  Outputs:
	INTEGER Terminal_LUN !FORTRAN logical unit number of terminal
	                     !for output, typically 6.
	INTEGER Journal_LUN  !FORTRAN logical unit number of journal file.

*  Brief description:  Get in-use terminal and journal FORTRAN LUNs.

*  Description:
*	Get the message handler terminal and journal LUNs and pass them
*	to the caller.

*  Error conditions: none

	INCLUDE 'msg_inc'

*	Get the LUNs from the COMMON:
	Terminal_LUN = MSG_TL
	Journal_LUN  = MSG_JL

	RETURN
	END
*
	SUBROUTINE			MSG_Ini( Journal_LUN )

	IMPLICIT NONE

*  Input:
	INTEGER Journal_LUN !FORTRAN logical unit number of journal file.

*  Brief description:  MSG package initializion and journal-LUN specification.

*  Description:
*	Initialize the STAR-standard message-handling package and set the
*	journal LUN.
	
*  Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER     Terminal_LUN
	PARAMETER ( Terminal_LUN = 6 )

	INTEGER ID

	LOGICAL MSG_Initialized
	SAVE    MSG_Initialized

	DATA    MSG_Initialized / .FALSE. /

	IF ( MSG_Initialized ) THEN !Already initialized -- don't hose the existing prefixes:
	  MSG_Total_Lookups   = 0 !Count of all prefix (character-search) lookups.
	  DO ID = 1, MSG_Nprefixes
	    CALL MSG_ResetID( ID ) !Reset counters, flags and limits to the defaults.
	  END DO
 	ELSE                        !Cold-start initialization -- get everything:

	  MSG_Nprefixes       = 0
	  MSG_Nclasses        = 0
	  MSG_Total_Lookups   = 0 !Count of all prefix (character-search) lookups.
	  MSG_All_Count_Limit = 50 !Default for no-class messages.
	  MSG_TimeStamp_CPU   = .FALSE. !If true, time-stamp on changed CPU time.
	  MSG_All_Disable     = .FALSE.
	  MSG_All_Nocount     = .FALSE.
	  MSG_Sorted          = .FALSE.
	  CALL MSG_Journal_Off
	  CALL MSG_Set_LUN( Terminal_LUN, Journal_LUN )
	  CALL MSG_Name_Node( ' ' )

*	  MSG Summary defaults:
	  CALL MSG_Set_Summary_Page_Length (   60 )     !60 lines per page.
	  CALL MSG_Set_Summary_Mode_Active(   .TRUE.  ) !List active messages.
	  CALL MSG_Set_Summary_Mode_Counting( .TRUE.  ) !List counting (but not displaying) messages.
	  CALL MSG_Set_Summary_Mode_Inactive( .FALSE. ) !Do not list inactive messages.
	  CALL MSG_Set_Summary_Mode_Aborted(  .TRUE. )  !List the aborted message.

*	  Define the predefined classes:
	  CALL MSG_Class_Define( ' ',   'Active', 50, 0 ) !Null or blank class.
	  CALL MSG_Class_Define( 'A',   'Active',  0, 1 ) !Abort message class (MSG abort).
	  CALL MSG_Class_Define( 'B',   'Active',  0, 0 ) !Bug message class.
	  CALL MSG_Class_Define( 'C', 'Counting',  0, 0 ) !Counting (CPU-measuring) class.
	  CALL MSG_Class_Define( 'D',   'Active',  0, 0 ) !Debug message class.
	  CALL MSG_Class_Define( 'E',   'Active', 20, 0 ) !Error message class.
	  CALL MSG_Class_Define( 'F',   'Active',  0, 0 ) !Fatal message class (application abort).
	  CALL MSG_Class_Define( 'I',   'Active',  0, 0 ) !Informative message class.
	  CALL MSG_Class_Define( 'O',   'Active',  1, 0 ) !Once-only message class.
	  CALL MSG_Class_Define( 'T', 'Inactive',  0, 0 ) !Trace (silient debug) message class.
	  CALL MSG_Class_Define( 'W',   'Active', 10, 0 ) !Warning message class.

	END IF ! MSG_Initialized

	CALL STRELA0 !Initialize elapsed real-time.
	CALL STRCPU0 !Initialize elapsed CPU-time.

	RETURN

	END
*
	LOGICAL FUNCTION		MSG_Journal_Close()

	IMPLICIT NONE

*  Brief description:  Close the MSG journal file.

*  Description:
*	Call this subroutine to close the message journal file.
*	Disables message-logging in the journal file.
*	Returns .TRUE. if the file was successfully closed,
*	returns .FALSE. if the file could not be closed (probably
*	because it was not opened)

*  Return conditions:
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
*
	SUBROUTINE			MSG_Journal_Off

	IMPLICIT NONE

*  Inputs:  none
*  Outputs: none

*  Brief description:  Disable journal logging of messages.

*  Description:
*	Call this subroutine to disable the message journal file.

*  Error conditions: none

	INCLUDE 'msg_inc'

	MSG_JOURNAL_ENABLE=.FALSE.

	RETURN
	END
*
	SUBROUTINE			MSG_Journal_On

	IMPLICIT NONE

*  Inputs:  none
*  Outputs: none

*  Brief description:  (Re)enable journal logging of messages.

*  Description:
*	Call this subroutine to (Re)enable the message journal file.

*  Error conditions: none

	INCLUDE 'msg_inc'

	MSG_JOURNAL_ENABLE=.TRUE.

	RETURN
	END
*
	LOGICAL FUNCTION		MSG_Journal_Open( FILE_NAME )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) FILE_NAME !The journal file-name.

*  Brief description:  Open a message-logging journal file.

*  Description:
*	Call this subroutine to specify FILE_NAME as the
*	message journal file and to open that file.
*	Returns .TRUE. if the file was successfully opened
*	and enables message logging to the journal file;
*	returns .FALSE. if the file could not be opened, and
*	disables message-logging in the journal file.

*  Return conditions:
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
*
	SUBROUTINE			MSG_Journal_Page

	IMPLICIT NONE

*  Brief description:  Send a form-feed to the journal file.
	
*  Description:
*	Write a form-feed to the journal file, if open & enabled.

	INCLUDE 'msg_inc'

	INTEGER FF
	PARAMETER (FF='014'O) !Octal rep. of ASCII form-feed character.

	IF (MSG_JOURNAL_ENABLE) THEN
	  CALL MSG_TO_LUN_OUT(CHAR(FF),1,MSG_JL) !Route through here, with LUN=MSG_JL.
	END IF

	RETURN
	END
*
	SUBROUTINE			MSG_LUN_Page( LUN )

	IMPLICIT NONE

	INTEGER LUN  !FORTRAN logical unit number.

*  Brief description:  Send a form-feed to a FORTRAN LUN.
	
*  Description:
*	Write a form-feed to the device openned on LUN.

	INCLUDE 'msg_inc'

	INTEGER FF
	PARAMETER (FF='014'O) !Octal rep. of ASCII form-feed character.

	CALL MSG_TO_LUN_OUT(CHAR(FF),1,LUN)

	RETURN
	END
*
	SUBROUTINE			MSG_Mark( Prefix, ID )

	IMPLICIT NONE
	
*  Inputs:
	CHARACTER*(*) Prefix !MSG prefix.

*  Input/Output:
	INTEGER ID     !Fast-reference MSG ID.  Uniquely assigned on first call.

	INCLUDE 'msg_inc'

*  Description:
*	Set the given Prefix's "Marked CPU time" to current.  When this prefix
*	occurs in a call to an MSG routine which does MSG accounting (eg, Message),
*	and when that Prefix has had its "Marked CPU time" set (ie, to non-zero),
*	that prefix's "total CPU time" is incremented by Current CPU time, less
*	the "Marked CPU time".  The Marked CPU time is then zeroed.

	LOGICAL Active, Counting
	INTEGER LID !Local copy of ID.
	INTEGER CPU_Time(2)

	LID = ID

*	Check or set the ID & flags.
*	Enter MSG's prefix in the index if needed.
	CALL MSG_Check( Prefix, LID, Active, Counting )

	IF ( Counting ) THEN !Counting is true only if found.
*	  Prefix was found or defined, counting is enabled for it, and LID is now valid.
	  CALL STRCPU( CPU_Time ) !Get "absolute" CPU time, since last STRCPU0 call.
	  MSG_CPU_Mark( LID ) = CPU_Time(1) ! 2nd element is unused.
	END IF !Counting

	IF ( ID .EQ. 0 ) ID = LID !Ensure it's changed only if zero.

	RETURN
	END
*
	SUBROUTINE			MSG_Name_Node( Node_name )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Node_name

*  Brief description:  Specify the local (ASCII) node name for node-time stamping.

*  Description:
*	Enters the ASCII node-name in the msg package, for use
*	in node-stamping (along with the time-stamp) journal
*	entries of message-occurrances.

	INCLUDE 'msg_inc'

	MSG_Node_name=node_name

	RETURN
	END
*
	SUBROUTINE			MSG_NoCount( PREFIX )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Brief description:  Disable MSG counting of a previously disabled prefix.

*  Description:
*	Disable counting for PREFIX if displaying for PREFIX is
*	already disabled.  Otherwise do nothing.

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
*
	INTEGER FUNCTION		MSG_RC_Journal_Close()

	IMPLICIT NONE

	INCLUDE 'msg_ret_inc'

*  Brief description:  Return-code version of MSG_Journal_Close.

*  Description:
*	Call this subroutine to close the message journal file.
*	Disables message-logging in the journal file.
*	This is the same as MSG_JOURNAL_CLOSE, but implemented
*	as an integer function returning a "code".

*  Returned value:
*	MSG_CLOSED_RC for successful close of journal file.
*	MSG_NOT_CLOSED_RC for close failure of journal file.

	LOGICAL MSG_JOURNAL_CLOSE

	IF (MSG_JOURNAL_CLOSE()) THEN !Success:
	  MSG_RC_JOURNAL_CLOSE=MSG_CLOSED_RC
	ELSE !Failure:
	  MSG_RC_JOURNAL_CLOSE=MSG_NOT_CLOSED_RC
	END IF

	RETURN

	END
*
	INTEGER FUNCTION		MSG_RC_Journal_Open( FILE_NAME )

	IMPLICIT NONE

	INCLUDE 'msg_ret_inc'

*  Input:
	CHARACTER*(*) FILE_NAME !The journal file-name.

*  Brief description:  Return-code version of MSG_Journal_Open

*  Description:
*	Call this subroutine to specify FILE_NAME as the
*	message journal file and to open that file.
*	This is the same as MSG_JOURNAL_OPEN, but implemented
*	as an integer function returning a "code".

*  Returned value:
*	MSG_OPENED_RC for successful open of journal file.
*	MSG_NOT_OPENED_RC for open failure of journal file.

	LOGICAL MSG_JOURNAL_OPEN

	IF (MSG_JOURNAL_OPEN(FILE_NAME)) THEN !Success:
	  MSG_RC_JOURNAL_OPEN=MSG_OPENED_RC
	ELSE !Failure:
	  MSG_RC_JOURNAL_OPEN=MSG_NOT_OPENED_RC
	END IF

	RETURN

	END
*
	SUBROUTINE			MSG_Set_Abort_Limit( Prefix, Limit )

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) Prefix !A STAR-standard message prefix.
	INTEGER Limit !Maximum no. of times to display a message before aborting.

*  Brief description:  Set abort limit for a prefix.

*  Description:
*	Set the abort limit for the message recognized by Prefix;
*	Program termination results when the prefix's
*	count has exceeded Limit.  Wildcards are permitted.
*	The existing count-limit (after which message-display is disabled
*	for Prefix), is set to the same value -- aborting msg calls
*	should not be quiet without deliberate intervention (ie, re-specify
*	the count-limit after this call to obtain silent aborts -- don't
*	do it!).


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
*
	SUBROUTINE			MSG_Set_By_Command( COM )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) COM !Command-string describing features to be set.

*  Brief description:  ASCII-string command interface to control MSG.

*  Description:
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

*	To have time-stamps occur on a changed CPU time (in addition to
*	getting them on changed real-time or node-name):
*	TIMESTAMP CPU

*	To have time-stamps not occur on a changed CPU time (ie, only
*	getting them on changed real-time or node-name):
*	TIMESTAMP NOCPU

*	Spacing is not critical in the command line, provided at least one
*	or more spaces, tabs or commas separate the arguments.
*	The "=" symbols in the LIMIT command are essential, but spaces around
*	it are optional and not at all critical.

*	Wildcards are permitted in the prefix specifications.


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
	CALL STRCAPS(ARG(1))

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
	      CALL MSG_Set_Limit( ARG(JARG), IARG(JARG+2) )
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
	  ELSE IF (ARG(1).EQ.'TIMESTAMP') THEN
	    IF (ARG(JARG).EQ.'CPU') THEN
	      CALL MSG_Set_TimeStamp_CPU( .TRUE. )
	    ELSE IF (ARG(2).EQ.'NOCPU') THEN
	      CALL MSG_Set_TimeStamp_CPU( .FALSE. )
	    ELSE
	      ERROR=.TRUE.
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
     1	    ' COUNT, DISABLE, ENABLE, LIMIT, NOCOUNT, ABORT, LINES, TIMESTAMP.',1)
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
*
	SUBROUTINE			MSG_Set_From_File( LUN )

	IMPLICIT NONE

*  Input:
	INTEGER LUN !FORTRAN Logical Unit Number, on which a command (ASCII)
	            !file should be (already) open.

*  Brief description:  ASCII-file command interface to control MSG.

*  Description:
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
*
	SUBROUTINE			MSG_Set_Limit( Prefix, Limit )

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) Prefix !A message prefix.
	INTEGER       Limit  !Maximum no. of (additional) times to display a message.

*  Brief description:  Set auto-disable count limit for a prefix.

*  Description:
*	Set the count limit for the message recognized by PREFIX;
*	displays of the specified message are disabled once its
*	count has exceeded LIMIT.  Wildcards are permitted.

	INCLUDE 'msg_inc'


	INTEGER ID,WPT,SPT
	LOGICAL ACTIVE,COUNTING

*	First check to see if there is a wildcard in the specified prefix:
	WPT=INDEX(PREFIX,'*')

	IF (WPT.EQ.1) THEN !Wildcard in first position -- do all enabled ones:

	  DO ID=1,MSG_Nprefixes
	    IF ( MSG_Active( ID ) ) THEN
	      IF ( Limit .EQ. 0 ) THEN !No limit is different:
	        MSG_Count_limit( ID ) = 0
	      ELSE                     !Add limit to existing counts:
	        MSG_Count_limit( ID ) = MSG_Counts( ID ) + Limit
	      END IF
	      MSG_Counting(    ID ) = .TRUE. !... can't be active with no-count!
	    END IF
	  END DO
	  MSG_ALL_COUNT_LIMIT=LIMIT !And set the default, for new messages...

	ELSE IF (WPT.GT.1) THEN !There's a wildcard -- do selected, enabled ones.

*	  Point to last character before the wildcard; protect against too-long:
	  SPT=MIN(WPT-1,MSG_Prefix_length_P)
	  DO ID=1,MSG_Nprefixes
	    IF ( PREFIX(:SPT).EQ.MSG_Prefix(ID)(:SPT) ) THEN	    
	      IF ( MSG_Active( ID ) ) THEN
	        IF ( Limit .EQ. 0 ) THEN !No limit is different:
	          MSG_Count_limit( ID ) = 0
	        ELSE                     !Add limit to existing counts:
	          MSG_Count_limit( ID ) = MSG_Counts( ID ) + Limit
	        END IF
	        MSG_Counting(    ID ) = .TRUE. !... can't be active with no-count!
	      END IF
	    END IF
	  END DO !ID=1,MSG_Nprefixes

	ELSE !No wildcard.

	  ID=0 !Clear this to do a lookup-by-prefix.
*	  Check that the message-prefix is in the index, enter it
*	  in the index if necessary, and return its ID:
	  CALL MSG_Check(Prefix,ID,Active,Counting)
	  IF (ID.GT.0) THEN
	    IF ( Limit .EQ. 0 ) THEN !No limit is different:
	      MSG_Count_limit( ID ) = 0
	    ELSE                     !Add limit to existing counts:
	      MSG_Count_limit( ID ) = MSG_Counts( ID ) + Limit
	    END IF
	    MSG_Active(      ID ) = .TRUE. !Always enable on set-limit.
	    MSG_Counting(    ID ) = .TRUE. !... can't be active with no-count!
	  END IF

	END IF !WPT.EQ.1

	RETURN
	END
*
	SUBROUTINE			MSG_Set_LUN( TERMINAL_LUN, JOURNAL_LUN )

	IMPLICIT NONE

*  Inputs:
	INTEGER TERMINAL_LUN !FORTRAN logical unit number of terminal
	                     !for output, typically 6.
	INTEGER JOURNAL_LUN  !FORTRAN logical unit number of journal file.

*  Outputs:  none

*  Brief description:  Set the terminal and journal LUNs for MSG.

*  Description:
*	Set the message handler LUNs.  The caller specifies
*	the FORTRAN logical unit number of the terminal in
*	TERMINAL_LUN and the journal file in JOURNAL_LUN.

	INCLUDE 'msg_inc'

*	Put the LUNs into the COMMON:
	MSG_TL=TERMINAL_LUN	
	MSG_JL=JOURNAL_LUN

	RETURN
	END
*
	SUBROUTINE			MSG_Set_Summary_Mode_Aborted( Mode )

	IMPLICIT NONE

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Brief description:  Set the MSG summary mode for "Aborted" messages.

	INCLUDE 'msg_inc'

	MSG_Summary_Mode_Aborted = Mode

	RETURN
	END
*
	SUBROUTINE			MSG_Set_Summary_Mode_Active( Mode )

	IMPLICIT NONE

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Brief description:  Set the MSG summary mode for "Active" messages.

	INCLUDE 'msg_inc'

	MSG_Summary_Mode_Active = Mode

	RETURN
	END
*
	SUBROUTINE			MSG_Set_Summary_Mode_Counting( Mode )

	IMPLICIT NONE

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Brief description:  Set the MSG summary mode for "Counting" messages.

	INCLUDE 'msg_inc'

	MSG_Summary_Mode_Counting = Mode

	RETURN
	END
*
	SUBROUTINE			MSG_Set_Summary_Mode_Inactive( Mode )

	IMPLICIT NONE

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Brief description:  Set the MSG summary mode for "Inactive" messages.

	INCLUDE 'msg_inc'

	MSG_Summary_Mode_Inactive = Mode

	RETURN
	END
*
	SUBROUTINE			MSG_Set_Summary_Page_Length( Page_Length )

	IMPLICIT NONE

*  Input:
	INTEGER Page_Length  !Length of summary page (lines).

*  Brief description:  Set the MSG summary table page length.

	INCLUDE 'msg_inc'

	MSG_Summary_Page_Length = Page_Length

	RETURN
	END
*
	SUBROUTINE			MSG_Set_TimeStamp_CPU( Mode )

	IMPLICIT NONE

*  Input:
	LOGICAL Mode !Whether the mode is to be set TRUE or FALSE.

*  Brief description:  Set the MSG time-stamp mode for stamping CPU changes.

	INCLUDE 'msg_inc'

	MSG_TimeStamp_CPU = Mode

	RETURN
	END
*
	SUBROUTINE			MSG_Summary( LUN )

	IMPLICIT NONE

*  Input:
	INTEGER LUN !Device on which the summary is output.

*  Brief description:  Generate an MSG summary table on a FORTRAN LUN.

	CALL MSG_Summary_Event( LUN, 0 ) !"0" ==> no column of normalized entries.

	RETURN
	END
*
	SUBROUTINE			MSG_Summary_CPU( LUN )

	IMPLICIT NONE

*  Input:
	INTEGER LUN !Device on which the summary is output.

*  Brief description:  Generate a CPU-usage MSG-summary table.

*  Description:
*	Output a CPU-usage summary of the MSG-handled & marked messages
*	on the specified LUN.  "Marked" messages are messages whose occurances
*	were preceded, one-for-one, by calls to MSG_Mark, with the same
*	MSG prefix.

	INCLUDE 'msg_inc'

	INTEGER MSG_Summary_width_P
	PARAMETER (MSG_Summary_width_P=132)
	INTEGER ID

	CHARACTER*(MSG_Summary_width_P) MSG(2)
	CHARACTER*(MSG_Summary_width_P) Ghost
	CHARACTER*(MSG_Sample_length_P) Sample
	CHARACTER*23 C23
	CHARACTER*15 Cavg, Ctot
	INTEGER I
	INTEGER Page_line, EPT
	REAL CPU_avg, CPU_tot, Ticks_per_Second
	LOGICAL List_this_Message
	INTEGER Header_Lines

	INTEGER STRCPUTPS


100	FORMAT(T14'--- CPU Usage Summary --- ('A23')'/
     1	      'Message-Prefix & Truncated Sample of last occurance'
     1	       T66 'Total CPU Usage' T83 '  Avg CPU Usage'
     1	       T100 '    Counts' T111 '   Lookups' )

101	FORMAT( A, T65 '|' A, T83, A, T100, I10, T111, I10 )



*	For No Messages:
102	FORMAT(T14'--- CPU Usage Summary --- ('A23')'/
     1	      '         ---------  No Measurements  ----------')



	Ticks_per_Second = FLOAT( STRCPUTPS() ) !Get platform-dependent ticks-per-second.

	CALL MSG_Sort

	IF (MSG_Nprefixes.EQ.0) THEN !No messages.
	  CALL MSG_LUN_PAGE(LUN)
	  CALL STRASCDATETIME(C23)
	  WRITE(MSG,102) C23
	  CALL MSG_TO_LUN_OUT(MSG,2,LUN)
	END IF

	Page_line = 0
	DO ID = 1, MSG_Nprefixes

	  IF ( Page_line .EQ. 0 ) THEN !Do the header:
	    CALL MSG_LUN_PAGE(LUN)
	    CALL STRASCDATETIME(C23)
	    WRITE(MSG,100) C23
	    Header_Lines = 2
	    Page_line = Header_Lines
	    CALL MSG_to_LUN_Out( MSG, Header_Lines, LUN )
	  END IF

	  Sample = MSG_Sample( MSG_SID(ID) )

*	  Sometimes a \n (newline) character is embedded in a message, or even a \f, \b or \0:
	  I = MIN( INDEX( Sample, '\n' ), INDEX( Sample, '\f' ), INDEX( Sample, '\b' ), INDEX( Sample, '\0' ) )
	  IF      ( I .GE. 1 ) THEN !Blan out \n etc. and all after.
	    Sample(I:) = ' '
	  END IF

	  CALL STRClean( Sample, MSG_Sample_length_P, Sample ) !Clean out any non-printables.

	  CALL STREND(Sample,EPT) !Find last non-blank.
	  IF (EPT.LT.(MSG_Sample_length_P-6)) THEN !More than 6 blanks.
	    DO I=MSG_Sample_length_P-2,EPT+2,-2 !Append alternating dots.
	      Sample(I:I)='.'
	    END DO
	  ELSE !Few or no blanks -- no adulteration necessary:
	  END IF

	  IF      ( MSG_Counts(    MSG_SID(ID) ) .LE. 0 ) THEN !This one never happened.
	    List_this_Message = .FALSE.
	  ELSE IF ( MSG_CPU_Total( MSG_SID(ID) ) .LE. 0 ) THEN !This one never got marked.
	    List_this_Message = .FALSE.
	  ELSE                                             !List this one.
	    List_this_Message = .TRUE.
	  END IF


	  IF ( List_this_message ) THEN !List only those selected by the mode flags:

	    CPU_tot = FLOAT( MSG_CPU_Total( MSG_SID(ID) ) ) / Ticks_per_Second
	    CPU_avg = CPU_tot / FLOAT( MSG_Counts( MSG_SID(ID) ) )

	    IF ( CPU_tot .GT. 1000000. ) THEN
	      WRITE( Ctot, '(E15.7)' ) CPU_tot
	    ELSE IF ( CPU_tot .LT. 0.001 ) THEN
	      WRITE( Ctot, '(E15.7)' ) CPU_tot
	    ELSE
	      WRITE( Ctot, '(F15.6)' ) CPU_tot
	    END IF

	    IF ( CPU_avg .GT. 1000000. ) THEN
	      WRITE( Cavg, '(E15.7)' ) CPU_avg
	    ELSE IF ( CPU_avg .LT. 0.001 ) THEN
	      WRITE( Cavg, '(E15.7)' ) CPU_avg
	    ELSE
	      WRITE( Cavg, '(F15.6)' ) CPU_avg
	    END IF

	    WRITE( MSG, 101 )
     1	           Sample
     1	         , Ctot
     1	         , Cavg
     1	         , MSG_Counts(    MSG_SID(ID) )
     1	         , MSG_Lookups(   MSG_SID(ID) )
	    CALL STREND( MSG, EPT ) !Find last non-blank.
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

	    Page_line = Page_line + 1
	    IF ( Page_line .GE. MSG_Summary_Page_Length) THEN
	      Page_line = 0 !Start a new page.
	    END IF

	  END IF ! List_this_message

	END DO ! ID = 1, MSG_Nprefixes

	RETURN
	END
*
	SUBROUTINE			MSG_Summary_Event( LUN, EVENTS )

	IMPLICIT NONE

*  Input:
	INTEGER LUN !Device on which the summary is output.
	INTEGER EVENTS !Number of events by which to normalize.

*  Brief description:  Generate an MSG summary table with a frequency column on a FORTRAN LUN.

*  Description:
*	Output a summary of the MSG-handled messages on the specified LUN.
*	Include a column of normalized occurances of each message,
*	which consists of a column of the number of occurances of each
*	message, divided by EVENTS.  If EVENTS is not positive, then the
*	column is ommitted.

	INCLUDE 'msg_inc'

	INTEGER MSG_Summary_width_P
	PARAMETER (MSG_Summary_width_P=132)
	INTEGER ID
	CHARACTER*(MSG_Summary_width_P) MSG(2)
	CHARACTER*(MSG_Summary_width_P) Ghost
	CHARACTER*(MSG_Sample_length_P) Sample
	CHARACTER*23 C23
	CHARACTER*8 State
	INTEGER I
	INTEGER Page_line, EPT
	REAL Fraction
	LOGICAL List_this_Message

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


	CALL MSG_Sort

	IF (MSG_Nprefixes.EQ.0) THEN !No messages.
	  CALL MSG_LUN_PAGE(LUN)
	  CALL STRASCDATETIME(C23)
	  WRITE(MSG,102) C23
	  CALL MSG_TO_LUN_OUT(MSG,2,LUN)
	END IF

	Page_line = 0
	DO ID = 1, MSG_Nprefixes

	  IF ( Page_line .EQ. 0 ) THEN !Do the header:
	    CALL MSG_LUN_PAGE(LUN)
	    CALL STRASCDATETIME(C23)
	    IF (EVENTS.LE.0) THEN !No fractional occurances:
	      WRITE(MSG,100) C23
	    ELSE !Fractional occurances:
	      WRITE(MSG,103) C23
	    END IF
	    Header_Lines = 2
	    Page_line = Header_Lines
	    CALL MSG_to_LUN_Out( MSG, Header_Lines, LUN )
	  END IF

	  Sample = MSG_Sample( MSG_SID(ID) )

*	  Sometimes a \n (newline) character is embedded in a message, or even a \f, \b or \0:
	  I = MIN( INDEX( Sample, '\n' ), INDEX( Sample, '\f' ), INDEX( Sample, '\b' ), INDEX( Sample, '\0' ) )
	  IF      ( I .GE. 1 ) THEN !Blan out \n etc. and all after.
	    Sample(I:) = ' '
	  END IF

	  CALL STRClean( Sample, MSG_Sample_length_P, Sample ) !Clean out any non-printables.

	  CALL STREND(Sample,EPT) !Find last non-blank.
	  IF (EPT.LT.(MSG_Sample_length_P-6)) THEN !More than 6 blanks.
	    DO I=MSG_Sample_length_P-2,EPT+2,-2 !Append alternating dots.
	      Sample(I:I)='.'
	    END DO
	  ELSE !Few or no blanks -- no adulteration necessary:
	  END IF

*	  Determine the state:
	  List_this_Message = .FALSE. !Set true if the state is selected for listing.
	  IF (MSG_Abort_Limit( MSG_SID(ID) ).LE.0) THEN
	    State = ' '
	  ELSE IF (MSG_Counts( MSG_SID(ID) ).GE.MSG_Abort_Limit( MSG_SID(ID) ) ) THEN
*	    This message caused a program termination -- should only be one!
	    State = ' Aborted'
	    IF ( MSG_Summary_Mode_Aborted ) List_this_Message = .TRUE.
	  ELSE
	    State = ' '
	  END IF

	  IF (State .NE. ' ') THEN !State already determined.
	  ELSE IF ( MSG_Active( MSG_SID(ID) ) ) THEN
	    State = '  Active'
	    IF ( MSG_Summary_Mode_Active ) List_this_Message = .TRUE.
	  ELSE IF ( MSG_Counting( MSG_SID(ID) ) ) THEN
	    State = 'Counting'
	    IF ( MSG_Summary_Mode_Counting ) List_this_Message = .TRUE.
	  ELSE
	    State = 'Inactive'
	    IF ( MSG_Summary_Mode_Inactive ) List_this_Message = .TRUE.
	  END IF


	  IF ( List_this_message ) THEN !List only those selected by the mode flags:

	    IF (EVENTS.LE.0) THEN !No fractional occurances:
	      WRITE(MSG,101)
     1	            Sample,MSG_Counts(MSG_SID(ID))
     1	           ,MSG_Count_limit(MSG_SID(ID))
     1	           ,MSG_Lookups(MSG_SID(ID))
     1	           ,MSG_Abort_Limit(MSG_SID(ID))
     1	           ,State
	    ELSE !Fractional occurances:
	      Fraction=FLOAT(MSG_Counts(MSG_SID(ID)))/FLOAT(EVENTS)
	      WRITE(MSG,104)
     1	            Sample,MSG_Counts(MSG_SID(ID)),Fraction
     1	           ,MSG_Count_limit(MSG_SID(ID))
     1	           ,MSG_Lookups(MSG_SID(ID))
     1	           ,MSG_Abort_Limit(MSG_SID(ID))
     1	           ,State
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

	    Page_line = Page_line + 1
	    IF ( Page_line .GE. MSG_Summary_Page_Length) THEN
	      Page_line = 0 !Start a new page.
	    END IF

	  END IF ! List_this_message

	END DO ! ID = 1, MSG_Nprefixes

	RETURN
	END
*
	SUBROUTINE			MSG_Time_Stamp( LUN )

	IMPLICIT NONE
	INTEGER LUN

*  Inputs:
*	LUN -- FORTRAN logical unit number for time-stamp to be written on.

*  Outputs: none

*  Brief description:  Write a node and time (real and CPU) stamp to a FORTRAN LUN, if new.

*  Description:
*	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
*	"Stamp" the node-name on the same line, if node-name has been specified
*	with a "call msg_name_node(<node_name>)".
*	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
*	as 17-character ASCII strings.

*	Produces output only if different than the previous output from
*	this subroutine.  eg, a call to this routine in a tight loop will
*	only produce an actual time-stamp once each second.



	INCLUDE 'msg_inc'

	LOGICAL TimeStamp
	LOGICAL NodeName

	CHARACTER*(MSG_Node_name_length_P) Last_Node_name
	SAVE Last_Node_name

	CHARACTER*23 Date_time, Last_Date_time
	CHARACTER*17 CPU_time
	CHARACTER*17 Last_CPU_time
	CHARACTER*18 Elapsed_time
	SAVE Last_Date_time
	SAVE Last_CPU_time

	DATA Last_Node_name/' '/
	DATA Last_Date_time/' '/
	DATA Last_CPU_time /' '/


	TimeStamp = .FALSE.
	CALL STRASCDATETIME(Date_time)
	CALL STRCPUASC( CPU_time )

	IF (MSG_Node_name.EQ.' ') THEN !No node name:
	  NodeName = .FALSE.
*	  Don't bother with a time-stamp if it hasn't changed:
	  IF (Date_time.EQ.Last_Date_time) THEN !No change:
	  ELSE !It has changed:
	    TimeStamp = .TRUE.
	  END IF
	ELSE                           !Append node name:
	  NodeName = .TRUE.
*	  Don't bother with a time-stamp if node or time hasn't changed.
	  IF ( (Date_time.EQ.Last_Date_time) .AND.
     1	       (MSG_Node_name.EQ.Last_Node_name)  ) THEN !No change:
	  ELSE !Time or node has changed:
	    TimeStamp = .TRUE.
	  END IF
	END IF !MSG_Node_name.EQ.' '

	IF ( MSG_TimeStamp_CPU ) THEN !Time Stamp if CPU is different:
	  IF ( Last_CPU_time .NE. CPU_TIME ) THEN
	    TimeStamp = .TRUE.
	  END IF
	END IF

	IF ( .NOT. TimeStamp ) THEN
	ELSE IF ( .NOT. NodeName ) THEN
	  CALL STRELAASC( Elapsed_time )
	  CALL MSG_TO_LUN_OUT( Date_time //
     1	                     '   Elapsed:' // Elapsed_time //
     1	                     '   CPU time:' // CPU_time
     1	                     , 1, LUN )
	  Last_Date_time=Date_time
	  Last_CPU_time=CPU_time
	ELSE
	  CALL STRELAASC( Elapsed_time )
	  CALL MSG_TO_LUN_OUT( Date_time //
     1	                     ' From: ' // MSG_Node_name //
     1	                     '   Elapsed:' // Elapsed_time //
     1	                     '   CPU time:' // CPU_time
     1	                     , 1, LUN )
	  Last_Date_time=Date_time
	  Last_Node_name=MSG_Node_name
	  Last_CPU_time=CPU_time
	END IF

	RETURN
	END
*
	SUBROUTINE			MSG_Time_Stamp_Out( LUN )

	IMPLICIT NONE
	INTEGER LUN

*  Inputs:
*	LUN -- FORTRAN logical unit number for time-stamp to be written on.

*  Outputs: none

*  Brief description:  Write a node and time (real and CPU) stamp to a FORTRAN LUN, always.

*  Description:
*	"Stamp" the date and time on a line on FORTRAN logical unit LUN.
*	"Stamp" the node-name on the same line, if node-name has been specified
*	with a "call msg_name_node(<node_name>)".
*	Also stamps an elapsed real-time counter and an elapsed CPU-time counter,
*	as 17-character ASCII strings.

*	Always produces output.


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
*
	SUBROUTINE			MSG_to_Journal( MSG, LINES, ID )

	IMPLICIT NONE
	
*  Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*  Input/Output:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Outputs: none

*  Brief description:  Write a message to the journal (only), if enabled.

*  Description:
*	Conditionally display & count a message MSG, containing LINES lines
*	of up to 132 characters each, on the journal file, if open & enabled.


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
*
	SUBROUTINE			MSG_to_Journal_Out( MSG, LINES )

	IMPLICIT NONE
	
*  Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.

*  Outputs: none

*  Brief description:  Write a message to the journal (only);  bypass accounting.

*  Description:
*	Unconditionally display a message MSG, containing LINES lines
*	of up to 132 characters each, on the journal file, if open & enabled.


	INCLUDE 'msg_inc'

*	Display time & message on journal file:
	IF (MSG_JOURNAL_ENABLE) THEN
	  CALL MSG_TIME_STAMP(MSG_JL) !Time.
	  CALL MSG_TO_LUN_OUT(MSG,LINES,MSG_JL) !Route through here, with LUN=MSG_JL.
	END IF

	RETURN
	END
*
	SUBROUTINE			MSG_to_LUN( MSG, LINES, LUN, ID )

	IMPLICIT NONE
	
*  Inputs:
	CHARACTER*(*) MSG(*) !1 or more line character-string message
	                     !submitted for display.
	INTEGER LINES  !Number of lines in MSG.
	INTEGER LUN  !FORTRAN logical unit number for MSG to be sent to.

*  Input/Output:
	INTEGER ID     !STAR-standard message ID.  Set to zero by caller 
                       !before first call, set by MESSAGE on first call by
                       !looking up or entering the prefix contained in MSG
                       !(prefix is everything before the first space) in
	               !the index of STAR-standard message prefixes.
	               !If ID is negative, ID remains unchanged, and lookup
	               !is then always by prefix.

*  Outputs: none

*  Brief description:  Write a message to a FORTRAN LUN, if enabled.

*  Description:
*	Conditionally display & count a message MSG, containing LINES lines
*	of up to 132 characters each, on LUN.


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
*
	SUBROUTINE			MSG_to_LUN_Out( msg, Nlines, LUN )

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) msg(*) !1 or more line character-string message submitted for display.
	INTEGER       Nlines !Number of lines in MSG.
	INTEGER       LUN    !FORTRAN logical unit number for MSG to be written to.

*  Brief description:  Write a message to a FORTRAN LUN;  bypass accounting.

*  Description:
*	Display a message, containing one or more lines of no particular
*	length (or length-limit), on FORTRAN logical unit LUN.

*	This routine ends with a call to STRFlush( LUN ), flushing the output
*	output buffer.  This seems to be necessary to get some machines to
*	keep the LUN file (or terminal) up-to-date;  without this, a crash
*	leaves the last several lines of the output to LUN not in the listing
*	file -- a terrible thing indeed for debugging:


	INCLUDE 'msg_inc'

	INTEGER Line_len, I
	INTEGER LastNonBlank, Line_end


1	FORMAT(A)
*2	FORMAT(A) or FORMAT(' 'A):
	INCLUDE 'msg_native_inc' !Define a FORMAT (2) with or w/o car-cont. space.

	Line_len = LEN( msg(1) )

*	IF ( Line_len .GT. 131 ) Line_len=131  !This ruins in-line new-lines -- disabled!

*	Display message on LUN:
	IF ( LUN .EQ. MSG_TL ) THEN !Message to terminal -- special for VMS:
*	  This differs from the bottom block in the FORMAT statement used,
*	  which differs only under VMS (which still uses old FORTRAN carriage
*	  control -- ie, 1st column does print, it is a printer command).
	  DO I = 1, Nlines
	    CALL STREnd( msg(I), LastNonBlank ) !Point at last non-blank.
	    Line_end = MIN( Line_len, LastNonBlank )
	    WRITE( LUN, 2, ERR=10 ) msg(I)(1:Line_end)
	  END DO
	  CALL STRFlush( LUN )
	ELSE
	  DO I = 1, Nlines
	    CALL STREnd( msg(I), LastNonBlank ) !Point at last non-blank.
	    Line_end = MIN( Line_len, LastNonBlank )
	    WRITE( LUN, 1, ERR=10 ) msg(I)(1:Line_end)
	  END DO
	  CALL STRFlush( LUN )
	END IF


	RETURN

10	CONTINUE

	WRITE( MSG_TL, 501 )  msg(1)(:Line_len)
501	FORMAT( ' MSG_to_LUN_Out-E1  Format error, 1st line:' /
     1	        ' ' A )
	RETURN


	END
*
