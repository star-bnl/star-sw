*	This is the STAR message/error handling package (prefix MSG_).

*	=======================================
*	==  Internal Routines in this file.  ==
*	=======================================

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

	SUBROUTINE MSG_CHECK(MSG,ID,ACTIVE,COUNTING)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) MSG !A STAR-standard message, with prefix.
	                  !MSG can be just a prefix.

*  Input/output argument:
	INTEGER ID !STAR-standard message ID.  Set to zero by caller 
                   !before first call, set by MESSAGE on first call by
                   !looking up or entering the prefix contained in MSG
                   !(prefix is everything before the first space) in
	           !the index of STAR-standard message prefixes.

*  Output arguments:
	LOGICAL ACTIVE !Flag set to .TRUE. if the message selected
                       !(by prefix and/or ID) is active.
	LOGICAL COUNTING !Flag set to .TRUE. if the message selected
                         !(by prefix and/or ID) is enabled for counting.

*   Functional Description:
*	Find the character string consisting of MSG's prefix in the
*	index of STAR-standard message prefixes (sic).  If found
*	in the index, return the index ID and the flags ACTIVE
*	and COUNTING.  If ID is specified as positive,
*	a lookup in the index is not preformed, but a
*	check that ID is in the index and a check that
*	MSG's prefix agrees with the stored STAR-standard message
*	prefix selected by ID are made.  If the stored prefix
*	does not agree with MSG's prefix, MSG_CHECK issues an error
*	message (which indicates a program bug!), then zeros ID and starts
*	again, with ID set to the correct index ID if MSG's prefix is in the
*	index, and entering MSG's prefix if it is not in the index.
*	If not found, the prefix is entered in the index.
*	The flags ACTIVE and COUNTING are set according to the whether the
*	message is active (output enabled) and enabled for counting.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL FOUND
	LOGICAL E_FOUND, E_ACTIVE, E_COUNTING, CALLER_BUG
	INTEGER MID, MLEN
	CHARACTER*(MSG_Prefix_length_P) PREFIX

	CHARACTER*132 M132(4)
	CHARACTER*(MSG_Prefix_length_P) Prefix_given,Prefix_stored

	INTEGER I1, L, LID
	INTEGER LastNonBlank

	INTEGER EID
	SAVE EID

	CHARACTER*5 seplist
	SAVE seplist
	LOGICAL seplist_initialized
	SAVE seplist_initialized

	LOGICAL MSG_FIND

	DATA seplist_initialized/.FALSE./
	DATA EID/0/


*	This is a nice long message -- it should only occur if someone misuses
*	the message library (msglib) routines in code, and as such should only
*	occur "once", after which the programmer should fix the problem (he
*	should feel glad the thing didn't crash!  So don't complain.)
501	FORMAT('MSG_CHECK-B1 Program bug in caller.  The given ID:'I11/
     2	       '             doesn''t select the given prefix: ['A']'/
     3         '             The prefix stored as that ID is: ['A']'/
     4	       '             Note that each message needs to have'
     4	  ,' its own ID variable.')



	IF (.NOT.seplist_initialized) THEN
	  seplist_initialized=.TRUE.
	  seplist(1:1) = ' '        !Space
	  seplist(2:2) = '	'   !Tab
	  seplist(3:3) = CHAR( 10 ) !Newline
	  seplist(4:4) = CHAR( 12 ) !Form-feed
	  seplist(3:3) = CHAR(  0 ) !Null
	END IF

	LID=ID !Make a local copy.
	CALLER_BUG=.FALSE.

	IF (LID.LE.0) THEN !Look up the message in the index:
	  CALL MSG_GET_PREFIX(MSG,PREFIX)
	  FOUND=MSG_FIND(PREFIX,LID,ACTIVE,COUNTING) !Set the ID & flags.
	ELSE IF (LID.LE.MSG_Nprefixes) THEN !It's in the index:
	  FOUND=.TRUE.
	  ACTIVE=MSG_Active(LID)
	  COUNTING=MSG_Counting(LID)
	  IF (.NOT.ACTIVE) THEN !Waste no more time unless active.
	    RETURN
	  ELSE !It's active -- it's OK to spend some time:
	    CALL MSG_GET_PREFIX(MSG,PREFIX)
	    L=MIN(MSG_Length(LID),MSG_Prefix_length_P)
	    IF (PREFIX(:L).EQ.MSG_Prefix(LID)(:MSG_Length(LID))) THEN !It's the right one:
	    ELSE !It's wrong -- this is a caller-bug:
	      CALLER_BUG=.TRUE.
*	      First check if this message is disabled:
	      E_FOUND=MSG_FIND('MSG_CHECK-B1',EID,E_ACTIVE,E_COUNTING)
	      IF (.NOT.E_FOUND) THEN !Enter the prefix in the index.
	        CALL MSG_ENTER('MSG_CHECK-B1',EID)
	        E_ACTIVE=.TRUE.
	        E_COUNTING=.TRUE.
	        MSG_Active(EID)=E_ACTIVE !Overwrite usual defaults.
	        MSG_Counting(EID)=E_COUNTING
	      END IF
	      IF (E_ACTIVE) THEN
	        Prefix_given =PREFIX          !Truncate to MSG_Prefix_length_P.
	        Prefix_stored=MSG_Prefix(LID) !Truncate to MSG_Prefix_length_P.
	        WRITE(M132,501) LID,Prefix_given,Prefix_stored
	        CALL MESSAGE_OUT(M132,4) !4 lines in M132.
	      END IF
	      IF (E_COUNTING) THEN
	        CALL MSG_INCR(EID)
	      END IF
	      FOUND=MSG_FIND(PREFIX,LID,ACTIVE,COUNTING) !Set the ID & flags.
	    END IF !PREFIX(:L).EQ.MSG_Prefix(LID)(:MSG_Length(LID))
	  END IF !ACTIVE
	ELSE !It's not in the index:
	  FOUND=.FALSE.
	END IF !LID.LE.0

	IF (.NOT.FOUND) THEN !Enter the prefix in the index.
	  CALL MSG_ENTER(PREFIX,LID)
	  ACTIVE=.NOT.MSG_ALL_DISABLE
	  COUNTING=.NOT.MSG_ALL_NOCOUNT
	END IF

*	Check whether it's exceeded its counting limit:
	IF (LID.GT.0) THEN
	  IF (.NOT.MSG_Active(LID)) THEN
	  ELSE IF (MSG_Count_limit(LID).LE.0) THEN !No counting limit.
	  ELSE IF (MSG_Counts(LID).GE.MSG_Count_limit(LID)) THEN
*	    Turn off messages -- continue counting, of course:
	    MSG_Active(LID)=.FALSE.
	    ACTIVE=.FALSE.
*	    And display a message-shut-off warning:
	    CALL STREND(MSG_Prefix(LID),LastNonBlank)
	    CALL MESSAGE_OUT(
     1	         MSG_Prefix(LID)(:LastNonBlank)//
     1	        ' Disabled -- count limit reached',1)
	  END IF !.NOT.ACTIVE
	END IF

	IF (FOUND.AND.ACTIVE) THEN !Make the latest message this ones sample:
*	  Left-justify the message-sample -- don't want indentations in the table.
*	  Scan for first non-separator:
	  I1=1
	  DO WHILE (  ( INDEX( seplist, MSG(I1:I1) ) .GT. 0 )
     1	      .AND.   ( I1 .LT. (LEN(MSG)-1) )  )
	    I1=I1+1
	  END DO

	  MSG_Sample(LID)=MSG(I1:)

	END IF

	IF (CALLER_BUG) THEN !Don't set the given ID.
	ELSE                 !Set it.
	  ID=LID
	END IF


	RETURN
	END

	SUBROUTINE MSG_ENTER(PREFIX,ID)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Output argument:
	INTEGER ID !STAR-standard message ID of the newly-entered
	           !message-prefix PREFIX, or zero if no more room.

*   Functional Description:

*	Enter the STAR-standard message prefix into the index.
*	If there is no more room in the index, display a message
*	and return ID=0.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	CHARACTER*132 M132(3)
	INTEGER EID
	INTEGER L
	LOGICAL ACTIVE,COUNTING,FOUND
	LOGICAL MSG_FIND

	IF (MSG_Nprefixes.LT.MSG_Nprefixes_max_P) THEN !There's room:
	  MSG_Nprefixes=MSG_Nprefixes+1
	  ID=MSG_Nprefixes
	  CALL STREND(PREFIX,L) !Find last non-blank.
	  MSG_Length(ID)=L
	  MSG_Prefix(ID)=' ' !Make sure it's blanked out first.
	  MSG_Prefix(ID)=PREFIX(1:MSG_Length(ID))
	  MSG_Sample(ID)=' ' !Make sure it's blanked out first.
	  MSG_Sample(ID)=PREFIX(1:MSG_Length(ID)) !Initialize sample with prefix.
	  MSG_Active(ID)=.NOT.MSG_All_Disable
	  MSG_Counting(ID)=.NOT.MSG_All_Nocount
	  MSG_Counts(ID)=0
	  MSG_Lookups(ID)=0
	  MSG_Count_limit(ID)=MSG_ALL_COUNT_LIMIT !The default.
	  MSG_Abort_limit(ID)=0 !The default -- no abort limit.
	ELSE !There's no room -- message:
	  ID=0
	  WRITE(M132,501) PREFIX
501	FORMAT('MSG_ENTER-E1 No room left for messages, prefix not entered:'/
     1	       '             ['A']'/
     1	       '             Change parameter MSG_Nprefixes_max_P in'
     1    ,' msg_inc.')
	  FOUND=MSG_FIND('MSG_ENTER-E1',EID,ACTIVE,COUNTING)
	  IF (ACTIVE) CALL MESSAGE_OUT(M132,3)
	  IF (COUNTING) CALL MSG_INCR(EID)
	END IF !MSG_Nprefixes.LT.MSG_Nprefixes_max_P

	RETURN
	END

	LOGICAL FUNCTION MSG_FIND(PREFIX,ID,ACTIVE,COUNTING)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) PREFIX !A STAR-standard message prefix.

*  Output arguments:
	INTEGER ID       !STAR-standard message ID.
	LOGICAL ACTIVE   !Flag set to .TRUE. if the message selected
                         !(by PREFIX and/or ID) is active.
	LOGICAL COUNTING !Flag set to .TRUE. if the message selected
                         !(by PREFIX and/or ID) is enabled for counting.

*   Functional Description:

*	Find the character string PREFIX in the index of
*	STAR-standard message prefixes (sic).  If found
*	in the index, return the index ID and return
*	MSG_FIND true.  The ID is never used to find PREFIX.
*	If PREFIX is not found, returns .FALSE..  The flags ACTIVE
*	and COUNTING are set according to the whether the message
*	is active (output enabled) and enabled for counting.

*   Return conditions:
*	.TRUE. if PREFIX is found in the index.
*	.FALSE. if PREFIX is not found in the index.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL FOUND
	INTEGER MID,MLEN
	INTEGER L, LID

	FOUND=.FALSE.
	CALL STREND(PREFIX,L) !Find last non-blank.
	IF (L.LE.0) THEN
	  MSG_FIND=FOUND
	  RETURN
	END IF

	MSG_TOTAL_LOOKUPS = MSG_TOTAL_LOOKUPS + 1

	LID=1
	DO WHILE (.NOT.FOUND .AND. (LID.LE.MSG_Nprefixes))
	  IF (MSG_Prefix(LID)(1:MSG_Length(LID)).EQ.PREFIX(:L)) THEN
	    FOUND=.TRUE.
	    MSG_Lookups(LID) = MSG_Lookups(LID) + 1 !Record this lookup.
	  ELSE
	    LID=LID+1
	  END IF
	END DO !WHILE (.NOT.FOUND .AND. (LID.LE.MSG_Nprefixes))

	IF (.NOT.FOUND) THEN
*	  Not in index -- set these like this:
	  ACTIVE=.FALSE.
	  COUNTING=.FALSE.
	  LID=0
	ELSE !In index -- look up the flags:
	  IF (.NOT.MSG_Active(LID)) THEN
	  ELSE IF (MSG_Count_limit(LID).LE.0) THEN !No counting limit.
	  ELSE IF (MSG_Counts(LID).GE.MSG_Count_limit(LID)) THEN
*	    Turn off messages -- continue counting, of course:
	    MSG_Active(LID)=.FALSE.
*	    And display a message-shut-off warning:
	    CALL MESSAGE_OUT(
     1	         MSG_Prefix(LID)//' Disabled -- count limit reached',1)
	  END IF !.NOT.ACTIVE
	  ACTIVE=MSG_Active(LID)
	  COUNTING=MSG_Counting(LID)
	END IF

	ID = LID
	MSG_FIND=FOUND

	RETURN
	END

	SUBROUTINE MSG_GET_PREFIX(MSG,PREFIX)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) MSG !A STAR-standard message, with prefix.

*  Output argument:
	CHARACTER*(*) PREFIX !The STAR-standard message-prefix from MSG.

*   Functional Description:
*	Extract the STAR-standard prefix from the character string MSG
*	and return it in PREFIX.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER I1,In,L

	CHARACTER*5 seplist
	SAVE seplist
	LOGICAL seplist_initialized
	SAVE seplist_initialized

	DATA seplist_initialized/.FALSE./


	IF (.NOT.seplist_initialized) THEN
	  seplist_initialized=.TRUE.
	  seplist(1:1) = ' '        !Space
	  seplist(2:2) = '	'   !Tab
	  seplist(3:3) = CHAR( 10 ) !Newline
	  seplist(4:4) = CHAR( 12 ) !Form-feed
	  seplist(3:3) = CHAR(  0 ) !Null
	END IF

	L=LEN(MSG)

*	Scan for first non-separator:
	I1=1
	DO WHILE (  ( INDEX( seplist, MSG(I1:I1) ) .GT. 0 )
     1	    .AND.   ( I1 .LT. (L-1) )  )
	  I1=I1+1
	END DO


*	Scan for the next separator:
	In=I1+1
	DO WHILE (  ( INDEX( seplist, MSG(In:In) ) .LE. 0 )
     1	    .AND.   ( In .LT. L )  )
	  In=In+1
	END DO


	IF ( In .GT. L ) THEN !Blank line, or zero-length line -- blank prefix:
	  PREFIX=' '
	ELSE
*	  If terminated by a separator, then subtract one from In to make it
*	  the last character in the prefix:
	  IF (INDEX( seplist, MSG(In:In) ).GT.0) THEN !The trailing separator is here.
	    In=In-1 !Back off one, from the trailing separator.
	  END IF
	  IF (In.LT.I1) THEN !Blank line, or zero-length line -- blank prefix:
	    PREFIX=' '
	  ELSE
*	    Prefix is everything from first non-separator to one-before next separator:
	    PREFIX=MSG(I1:In)
	  END IF
	END IF

	RETURN
	END

	SUBROUTINE MSG_INCR(ID)

	IMPLICIT NONE

*  Input argument:
	INTEGER ID !STAR-standard message-ID -- must be valid.

*  Functional description:
*	Increment the counter for the STAR-standard-message indexed
*	by ID.

	INCLUDE 'msg_inc'

	MSG_Counts(ID)=MSG_Counts(ID)+1

	RETURN
	END

	SUBROUTINE MSG_PARSE_PREFIX
     1	          (Prefix,Prefix_stripped,Prefix_number)

	IMPLICIT NONE

*  Input argument:
	CHARACTER*(*) Prefix !STAR-standard messsage prefix.

*  Output arguments:
	CHARACTER*(*) Prefix_stripped !Prefix stripped of trailing digits.
	INTEGER Prefix_number !Prefix trailing digits, decoded as integer.
	                      !If there are no trailing digits, set to zero.

*  Functional description:
*	Parse the specified STAR-standard message prefix "Prefix" into two
*	components:
*	1)	The prefix without trailing digits,
*	2)	The trailing digits, converted to an integer.
*	The principal use of this is for sorting, to preserve the sequence
*	1,2,3...8,9,10,11, etc., instead of the straight ASCII sequence,
*	which would go as 1,10,11,...,2,20,21,... etc.


	LOGICAL DONE
	INTEGER First_digit,Last_digit
	INTEGER I
	CHARACTER*132 C132,M132

	LOGICAL STRINT


	CALL STREND(Prefix,Last_digit) !Find last non-blank.
	First_digit=0
	I=Last_digit
	DONE=.FALSE.
	DO WHILE (.NOT.DONE)
	  IF (INDEX('0123456789',Prefix(I:I)).GT.0) THEN !It's a digit:
	    First_digit=I
	  ELSE !Ran past the digits, if any:
	    DONE=.TRUE.
	  END IF
	  I=I-1
	  IF (I.LE.0) THEN !No more characters to test.
	    DONE=.TRUE.
	  END IF
	END DO !WHILE (.NOT.DONE)

	IF (First_digit.GT.0) THEN !Found some trailing digits:
	  C132=Prefix(First_digit:Last_digit)//' '
	  IF (.NOT.STRINT(C132,10,0,0,I,M132)) THEN !Bogus -- shouldn't happen:
	    Prefix_number=0
	  ELSE !Good number.
	    Prefix_number=I
	  END IF
	  IF (First_digit.EQ.1) THEN !Wierd case -- all digits:
	    Prefix_stripped=' ' !Blank it out.
	  ELSE
	    Prefix_stripped=Prefix(1:First_digit-1)
	  END IF
	ELSE !No trailing digits:
	  Prefix_stripped=Prefix
	  Prefix_number=0
	END IF

	RETURN
	END

