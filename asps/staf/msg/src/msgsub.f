*	This is the STAR message/error handling package (prefix MSG_).

*	=======================================
*	==  Internal Routines in this file.  ==
*	=======================================

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

	SUBROUTINE MSG_Check( MSG, ID, Active, Counting )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) MSG !A STAR-standard message, with prefix.
	                  !MSG can be just a prefix.

*  Input/output:
	INTEGER ID !STAR-standard message ID.  Set to zero by caller 
                   !before first call, set by Message on first call by
                   !looking up or entering the prefix contained in MSG
                   !(prefix is everything before the first space) in
	           !the index of STAR-standard message prefixes.

*  Outputs:
	LOGICAL Active !Flag set to .TRUE. if the message selected
                       !(by prefix and/or ID) is active.
	LOGICAL Counting !Flag set to .TRUE. if the message selected
                         !(by prefix and/or ID) is enabled for counting.

*  Brief description:  Lookup a prefix, define new ones, return fast-ref ID.

*   Description:
*	Find the character string consisting of MSG's prefix in the
*	index of STAR-standard message prefixes (sic).  If found
*	in the index, return the index ID and the flags Active
*	and Counting.  If ID is specified as positive,
*	a lookup in the index is not preformed, but a
*	check that ID is in the index and a check that
*	MSG's prefix agrees with the stored STAR-standard message
*	prefix selected by ID are made.  If the stored prefix
*	does not agree with MSG's prefix, MSG_Check issues an error
*	message (which indicates a program bug!), then zeros ID and starts
*	again, with ID set to the correct index ID if MSG's prefix is in the
*	index, and entering MSG's prefix if it is not in the index.
*	If not found, the prefix is entered in the index.
*	The flags Active and Counting are set according to the whether the
*	message is active (output enabled) and enabled for counting.

*   Return conditions:  none

*   Error conditions:  none

*	Called in msglib.f .

	INCLUDE 'msg_inc'

	LOGICAL Found
	LOGICAL E_Found, E_Active, E_Counting, CALLER_BUG
	CHARACTER*(MSG_Prefix_length_P) Prefix

	CHARACTER*132 M132(4)
	CHARACTER*(MSG_Prefix_length_P) Prefix_given,Prefix_stored

	INTEGER I1, L, LID
	INTEGER LastNonBlank

	INTEGER EID
	SAVE EID

	CHARACTER*5 seplist
	SAVE        seplist
	LOGICAL     seplist_initialized
	SAVE        seplist_initialized

	LOGICAL MSG_Find

	DATA    seplist_initialized/.FALSE./
	DATA    EID / 0 /


*	This is a nice long message -- it should only occur if someone misuses
*	the message library (msglib) routines in code, and as such should only
*	occur "once", after which the programmer should fix the problem (he
*	should feel glad the thing didn't crash!  So don't complain.)
501	FORMAT('MSG_Check-B1 Program bug in caller.  The given ID:'I11/
     2	       '             doesn''t select the given prefix: ['A']'/
     3         '             The prefix stored as that ID is: ['A']'/
     4	       '             Note that each message needs to have'
     4	  ,' its own ID variable.')



	IF ( .NOT. seplist_initialized ) THEN
	  seplist_initialized = .TRUE.
	  seplist(1:1) = ' '        !Space
	  seplist(2:2) = '	'   !Tab
	  seplist(3:3) = CHAR( 10 ) !Newline
	  seplist(4:4) = CHAR( 12 ) !Form-feed
	  seplist(3:3) = CHAR(  0 ) !Null
	END IF

	LID = ID !Make a local copy.
	CALLER_BUG = .FALSE.

	IF      ( LID .LE. 0 )             THEN !Look up the message in the index:
	  CALL MSG_Get_Prefix( MSG, Prefix )
	  Found = MSG_Find( Prefix, LID, Active, Counting ) !Set the ID & flags.
	ELSE IF ( LID .LE. MSG_Nprefixes ) THEN !It's in the index:
	  Found    = .TRUE.
	  Active   = MSG_Active( LID )
	  Counting = MSG_Counting( LID )
	  IF ( .NOT. Active ) THEN !Waste no more time unless active.
	    RETURN
	  ELSE !It's active -- it's OK to spend some time:
	    CALL MSG_Get_Prefix( MSG, Prefix )
	    L = MIN( MSG_Length( LID ), MSG_Prefix_length_P )
	    IF ( Prefix(:L) .EQ. MSG_Prefix(LID)(:MSG_Length(LID)) ) THEN !It's the right one:
	    ELSE !It's wrong -- this is a caller-bug:
	      CALLER_BUG = .TRUE.
*	      First check if this message is disabled:
	      E_Found = MSG_Find( 'MSG_Check-B1', EID, E_Active, E_Counting )
	      IF ( .NOT. E_Found ) THEN !Enter the prefix in the index.
	        CALL MSG_Enter( 'MSG_Check-B1', EID )
	        E_Active   = .TRUE.
	        E_Counting = .TRUE.
	        MSG_Active(   EID ) = E_Active !Overwrite usual defaults.
	        MSG_Counting( EID ) = E_Counting
	      END IF
	      IF ( E_Active ) THEN
	        Prefix_given  = Prefix          !Truncate to MSG_Prefix_length_P.
	        Prefix_stored = MSG_Prefix(LID) !Truncate to MSG_Prefix_length_P.
	        WRITE( M132, 501 ) LID, Prefix_given, Prefix_stored
	        CALL Message_Out( M132, 4 ) !4 lines in M132.
	      END IF
	      IF ( E_Counting ) THEN
	        CALL MSG_INCR( EID )
	      END IF
	      Found = MSG_Find( Prefix, LID, Active, Counting ) !Set the ID & flags.
	    END IF !Prefix(:L) .EQ. MSG_Prefix(LID)(:MSG_Length(LID))
	  END IF !Active
	ELSE !It's not in the index:
	  Found = .FALSE.
	END IF !LID.LE.0

	IF ( .NOT. Found ) THEN !Enter the prefix in the index.
	  CALL MSG_Enter( Prefix, LID )
	END IF

*	Check whether it's exceeded its counting limit:
	IF ( LID .GT. 0 ) THEN
	  Active   = MSG_Active(LID)
	  Counting = MSG_Active(LID)
	  IF      ( .NOT. MSG_Active(LID) )                     THEN
	  ELSE IF ( MSG_Count_limit(LID) .LE. 0 )               THEN !No counting limit.
	  ELSE IF ( MSG_Counts(LID) .GE. MSG_Count_limit(LID) ) THEN
*	    Turn off messages -- continue counting, of course:
	    MSG_Active(LID) = .FALSE.
	    Active          = .FALSE.
*	    And display a message-shut-off warning:
	    CALL STRend( MSG_Prefix(LID), LastNonBlank )
	    CALL Message_Out( MSG_Prefix(LID)(:LastNonBlank)//
     1	        ' Disabled -- count limit reached',1)
	  END IF !.NOT.Active
	ELSE !Not found, not entered -- total failure.  Just set these false:
	  Active   = .FALSE.
	  Counting = .FALSE.
	END IF

	IF ( Found .AND. Active ) THEN !Make the latest message this ones sample:
*	  Left-justify the message-sample -- don't want indentations in the table.
*	  Scan for first non-separator:
	  I1 = 1
	  DO WHILE (  ( INDEX( seplist, MSG(I1:I1) ) .GT. 0 )
     1	      .AND.   ( I1 .LT. (LEN(MSG)-1) )  )
	    I1 = I1 + 1
	  END DO

	  MSG_Sample(LID) = MSG(I1:)

	END IF

	IF ( CALLER_BUG ) THEN !Don't set the given ID.
	ELSE                   !Set it.
	  ID = LID
	END IF


	RETURN
	END

	SUBROUTINE MSG_Enter( Prefix, ID )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Prefix !A STAR-standard message prefix.

*  Output:
	INTEGER ID !Fast-reference ID of the newly-entered
	           !message-prefix, or zero if no more room.

*  Brief description:  Define new prefixes and assign fast-ref IDs.

*   Description:

*	Enter the STAR-standard message prefix into the index.
*	If there is no more room in the index, display a message
*	and return ID=0.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	CHARACTER*132 M132(3)
	CHARACTER*(MSG_class_length_P) Class
	INTEGER CID
	INTEGER EID
	INTEGER L
	LOGICAL Active, Counting, Found
	LOGICAL MSG_Find
	LOGICAL MSG_Find_Class

	IF (MSG_Nprefixes.LT.MSG_Nprefixes_max_P) THEN !There's room:
	  MSG_Nprefixes=MSG_Nprefixes+1
	  ID=MSG_Nprefixes
	  CALL STREND(Prefix,L) !Find last non-blank.
	  MSG_Length(ID)=L
	  MSG_Prefix(ID)=' ' !Make sure it's blanked out first.
	  MSG_Prefix(ID)=Prefix(1:MSG_Length(ID))
	  MSG_Sample(ID)=' ' !Make sure it's blanked out first.
	  MSG_Sample(ID)=Prefix(1:MSG_Length(ID)) !Initialize sample with prefix.
	  MSG_Counts(ID)=0
	  MSG_Lookups(ID)=0
	  CALL MSG_Get_Class( MSG_Prefix(ID), Class ) !Get the class, from the prefix.
	  IF ( MSG_Find_Class( Class, CID ) ) THEN !Found the class -- set class defaults:
	    MSG_Active(      ID ) = MSG_Class_Default_Active(      CID )
	    MSG_Counting(    ID ) = MSG_Class_Default_Counting(    CID )
	    MSG_Count_limit( ID ) = MSG_Class_Default_Count_Limit( CID )
	    MSG_Abort_limit( ID ) = MSG_Class_Default_Abort_Limit( CID )
	  ELSE                                     !Didn't find the class:
	    MSG_Active(      ID ) = .NOT. MSG_All_Disable
	    MSG_Counting(    ID ) = .NOT. MSG_All_Nocount
	    MSG_Count_limit( ID ) = MSG_All_Count_Limit !The default.
	    MSG_Abort_limit( ID ) = 0 !The default -- no abort limit.
	  END IF
	ELSE !There's no room -- message:
	  ID=0
	  WRITE(M132,501) Prefix
501	FORMAT('MSG_Enter-E1 No room left for messages, prefix not entered:'/
     1	       '             ['A']'/
     1	       '             Change parameter MSG_Nprefixes_max_P in'
     1    ,' msg_inc.')
	  Found=MSG_FIND('MSG_Enter-E1',EID,Active,Counting)
	  IF (Active) CALL MESSAGE_OUT(M132,3)
	  IF (Counting) CALL MSG_INCR(EID)
	END IF !MSG_Nprefixes.LT.MSG_Nprefixes_max_P

	RETURN
	END

	LOGICAL FUNCTION MSG_Enter_Class( Class, ID )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Class !A prefix class.

*  Output:
	INTEGER ID !Class ID of the newly-entered
	           !prefix-class Class;  zero for the
	           !null class.

*  Brief description:  Define new class and assign an ID.

*   Description:

*	Enter the prefix-class into the index.
*	If there is no more room in the index, display a message
*	and return false.

*   Error conditions:  none

*  Returns:
*	.TRUE.  If Class is entered in the index.
*	.FALSE. If there is no more room in the index.

	INCLUDE 'msg_inc'

	CHARACTER*132 M132(3)
	INTEGER EID
	INTEGER L
	LOGICAL Found, Active, Counting
	LOGICAL MSG_Find

	IF ( MSG_Nclasses .LT. MSG_Nclasses_max_P ) THEN !There's room:
	  CALL STRend( Class, L ) !Find last non-blank.
	  L = MIN( L, MSG_class_length_P )
	  IF ( L .LE. 0 ) THEN !Null class -- don't actually enter it.
	    ID = 0
	  ELSE
	    MSG_Nclasses = MSG_Nclasses + 1
	    ID = MSG_Nclasses
	    MSG_Class( ID ) = ' ' !Make sure it's blanked out first.
	    MSG_Class( ID ) = Class(:L)
	  END IF
	  MSG_Enter_Class = .TRUE.
	ELSE !There's no room -- message:
	  WRITE( M132, 501 ) Class
501	FORMAT('MSG_Enter_Class-E1 No room left for new classes; class not entered:'/
     1	       '             ['A']'/
     1	       '             Change parameter MSG_Nclasses_max_P in msg_inc.')
	  Found = MSG_Find( 'MSG_Enter_Class-E1', EID, Active, Counting )
	  IF ( Active ) CALL Message_Out( M132, 3 )
	  IF ( Counting ) CALL MSG_Incr( EID )
	  MSG_Enter_Class = .FALSE.
	END IF !MSG_Nprefixes .LT. MSG_Nprefixes_max_P

	RETURN
	END

	LOGICAL FUNCTION MSG_Find( Prefix, ID, Active, Counting )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Prefix !A STAR-standard message prefix.

*  Outputs:
	INTEGER ID       !STAR-standard message ID.
	LOGICAL Active   !Flag set to .TRUE. if the message selected
                         !(by Prefix and/or ID) is active.
	LOGICAL Counting !Flag set to .TRUE. if the message selected
                         !(by Prefix and/or ID) is enabled for counting.

*  Brief description:  Lookup a prefix in the MSG prefix index, return fast-ref ID.

*   Description:

*	Find the character string Prefix in the index of
*	STAR-standard message prefixes (sic).  If found
*	in the index, return the index ID and return
*	MSG_Find true.  The ID is never used to find Prefix.
*	If Prefix is not found, returns .FALSE..  The flags Active
*	and Counting are set according to the whether the message
*	is active (output enabled) and enabled for counting.

*   Return conditions:
*	.TRUE. if Prefix is found in the index.
*	.FALSE. if Prefix is not found in the index.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL Found
	INTEGER L, LID

	Found=.FALSE.
	CALL STREND(Prefix,L) !Find last non-blank.
	IF (L.LE.0) THEN
	  MSG_Find=Found
	  RETURN
	END IF

	MSG_TOTAL_LOOKUPS = MSG_TOTAL_LOOKUPS + 1

	LID=1
	DO WHILE (.NOT.Found .AND. (LID.LE.MSG_Nprefixes))
	  IF (MSG_Prefix(LID)(1:MSG_Length(LID)).EQ.Prefix(:L)) THEN
	    Found=.TRUE.
	    MSG_Lookups(LID) = MSG_Lookups(LID) + 1 !Record this lookup.
	  ELSE
	    LID=LID+1
	  END IF
	END DO !WHILE (.NOT.Found .AND. (LID.LE.MSG_Nprefixes))

	IF (.NOT.Found) THEN
*	  Not in index -- set these like this:
	  Active=.FALSE.
	  Counting=.FALSE.
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
	  END IF !.NOT.Active
	  Active=MSG_Active(LID)
	  Counting=MSG_Counting(LID)
	END IF

	ID = LID
	MSG_Find=Found

	RETURN
	END

	LOGICAL FUNCTION MSG_Find_Class( Class, ID )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Class !A prefix-class.

*  Output:
	INTEGER ID       !ID of Class.

*  Brief description:  Lookup a class in the MSG class index, return ID.

*   Description:

*	Find the character string Class in the index of
*	Prefix Classes.  If found in the index, return
*	the index ID and return MSG_Find_Class true.
*	If Class is null or blank, then ID = 0 is returned,
*	but MSG_Find_Class still returns true.
*	The ID is never used to find Class.
*	If Class is not found, returns .FALSE. .

*   Return conditions:
*	.TRUE. if Class is found in the index or is the null class.
*	.FALSE. if Class is not found in the index.

*   Error conditions:  none

	INCLUDE 'msg_inc'

	LOGICAL Found
	INTEGER L, LID

	Found = .FALSE.
	CALL STRend( Class, L ) !Find last non-blank.
	IF ( L .LE. 0 ) THEN !Null.
	  ID = 0 !This is a class unto itself.
	  MSG_Find_Class = .TRUE.
	  RETURN
	END IF

	LID = 1
	DO WHILE ( .NOT. Found .AND. ( LID .LE. MSG_Nclasses ) )
	  IF ( MSG_Class( LID )(:MSG_class_length_P) .EQ. Class(:MSG_class_length_P) ) THEN
	    Found = .TRUE.
	  ELSE
	    LID = LID + 1
	  END IF
	END DO !WHILE ( .NOT. Found .AND. ( LID .LE. MSG_Nclasses ) )

	IF ( .NOT. Found ) THEN
*	  Not in index -- set this like this:
	  LID = 0
	END IF

	ID = LID
	MSG_Find_Class = Found

	RETURN
	END

	SUBROUTINE MSG_Get_Class( Prefix, Class )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Prefix !A STAR-standard message-prefix.

*  Output:
	CHARACTER*(*) Class !The STAR-standard prefix-class from Prefix.

*  Brief description:  Strip the class off of a prefix and return it.

*   Description:
*	Extract the STAR-standard class from the character string Prefix
*	and return it in Class.

*   Return conditions:  none

*   Error conditions:  none

	INCLUDE 'msg_inc'

	INTEGER I, L

	CHARACTER*1 seplist
	SAVE        seplist
	LOGICAL     seplist_initialized
	SAVE        seplist_initialized

	DATA seplist_initialized / .FALSE. /


	IF ( .NOT. seplist_initialized ) THEN
	  seplist_initialized=.TRUE.
	  seplist(1:1) = '-'        !Dash
	END IF

	L = LEN( Prefix )

*	Scan for the separator:
	I = 1
	DO WHILE (  ( INDEX( seplist, Prefix(I:I) ) .LE. 0 )
     1	    .AND.   ( I .LT. L )  )
	  I=I+1
	END DO

	I=I+1 !Point one-past the separator.

	IF ( I .GT. L ) THEN !Blank prefix, zero-length prefix or prefix ending in "-"  -- blank class:
	  Class = ' '
	ELSE
*	  Class is everything after separator to end-of-prefix, up to MSG_class_length_P:
	  L = MIN( L, I + MSG_class_length_P - 1 )
	  Class = Prefix(I:L)
	END IF

	RETURN
	END

	SUBROUTINE MSG_Get_Prefix( MSG, Prefix )

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) MSG !A STAR-standard message, with prefix.

*  Output:
	CHARACTER*(*) Prefix !The STAR-standard message-prefix from MSG.

*  Brief description:  Strip the prefix off a message and return it.

*   Description:
*	Extract the STAR-standard prefix from the character string MSG
*	and return it in Prefix.

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
	  Prefix=' '
	ELSE
*	  If terminated by a separator, then subtract one from In to make it
*	  the last character in the prefix:
	  IF (INDEX( seplist, MSG(In:In) ).GT.0) THEN !The trailing separator is here.
	    In=In-1 !Back off one, from the trailing separator.
	  END IF
	  IF (In.LT.I1) THEN !Blank line, or zero-length line -- blank prefix:
	    Prefix=' '
	  ELSE
*	    Prefix is everything from first non-separator to one-before next separator:
	    Prefix=MSG(I1:In)
	  END IF
	END IF

	RETURN
	END

	SUBROUTINE MSG_Incr( ID )

	IMPLICIT NONE

*  Input:
	INTEGER ID !STAR-standard message-ID -- must be valid.

*  Brief description:  Increment a prefix's counter, referenced by ID.

*  Description:
*	Increment the counter for the STAR-standard-message indexed
*	by ID.

*	Called in msglib.f .

	INCLUDE 'msg_inc'

	MSG_Counts(ID)=MSG_Counts(ID)+1

	RETURN
	END

	SUBROUTINE MSG_Parse_Prefix( Prefix, Prefix_stripped, Prefix_number)

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) Prefix !STAR-standard messsage prefix.

*  Outputs:
	CHARACTER*(*) Prefix_stripped !Prefix stripped of trailing digits.
	INTEGER Prefix_number !Prefix trailing digits, decoded as integer.
	                      !If there are no trailing digits, set to zero.

*  Brief description:  Parse a prefix into a string and a number.

*  Description:
*	Parse the specified STAR-standard message prefix "Prefix" into two
*	components:
*	1)	The prefix without trailing digits,
*	2)	The trailing digits, converted to an integer.
*	The principal use of this is for sorting, to preserve the sequence
*	1,2,3...8,9,10,11, etc., instead of the straight ASCII sequence,
*	which would go as 1,10,11,...,2,20,21,... etc.

*	Note that classes are irrelevant to this purpose, and are ignored.

*	Called in msglib.f in MSG_Summary.



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

