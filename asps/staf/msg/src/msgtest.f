	PROGRAM msgTest

	IMPLICIT NONE

*  Description:
*	Test bed for msg routines, and their underlying str calls.

	INTEGER     MSG_Journal_LUN_P
	PARAMETER ( MSG_Journal_LUN_P = 11 )

	CHARACTER*132 M132

	INTEGER TL, JL
	INTEGER I

	INTEGER ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8
	SAVE    ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8

	EXTERNAL MSG_TestAlarm
	LOGICAL MSG_Journal_Close
	LOGICAL MSG_Journal_Open

	DATA    ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8 / 0, 0, 0, 0, 0, 0, 0, 0 /


	CALL MSG_Ini( MSG_Journal_LUN_P )
	IF ( .NOT. MSG_Journal_Open( 'msg.jou' ) ) THEN
	  CALL EXIT
	END IF

	CALL MSGAlarmRegister( MSG_TestAlarm )

	CALL MSG_Mark( 'msgTest-O4', ID4 )
	DO I = 1, 1024
	  CALL Message( 'msgTest-O6  Once    #6', 1, ID6 )
	END DO
	CALL MSG_EnableAlarm( 'msgTest-I1' )
	CALL Message( 'msgTest-I1  Message #1', 1, ID1 )
	CALL Message( 'msgTest-T2  Trace   #2', 1, ID2 )
	CALL Message( 'msgTest-C3  Count   #3', 1, ID3 )
	CALL Message( 'msgTest-O4  Once    #4', 1, ID4 )
	CALL Message( 'msgTest-T5  Trace   #5', 1, ID5 )
	CALL Message( 'msgTest-O6  Once    #6', 1, ID6 )
	CALL Message( 'msgTest-I7  Message #7', 1,  -1 )

	CALL MSG_Mark( 'msgTest-O4', ID4 )
	CALL MSG_Enable( 'msgTest-T2' )
	CALL Message( 'msgTest-I1  Message #1', 1, ID1 )
	CALL Message( 'msgTest-T2  Trace   #2', 1, ID2 )
	CALL Message( 'msgTest-C3  Count   #3', 1, ID3 )
	CALL Message( 'msgTest-O4  Once    #4', 1, ID4 )
	CALL Message( 'msgTest-T5  Trace   #5', 1, ID5 )
	CALL Message( 'msgTest-O6  Once    #6', 1, ID6 )
	CALL Message( 'msgTest-I7  Message #7', 1,  -1 )

	CALL MSG_Mark( 'msgTest-O4', ID4 )
	CALL MSG_Disable( 'msgTest-T2' )
	CALL Message( 'msgTest-I1  Message #1', 1, ID1 )
	CALL Message( 'msgTest-T2  Trace   #2', 1, ID2 )
	CALL Message( 'msgTest-C3  Count   #3', 1, ID3 )
	CALL Message( 'msgTest-O4  Once    #4', 1, ID4 )
	CALL Message( 'msgTest-T5  Trace   #5', 1, ID5 )
	CALL Message( 'msgTest-O6  Once    #6', 1, ID6 )
	CALL Message( 'msgTest-I7  Message #7', 1,  -1 )

	CALL MSG_Get_LUN(  TL,  JL )    !Get terminal and journal LUNs.

	CALL MSG_Summary_Event( TL, 3 ) !List them all, to the terminal.
	CALL MSG_Summary_CPU(   TL )    !CPU usage measurements.

	CALL MSG_Summary_Event( JL, 3 ) !List them all, in the journal.
	CALL MSG_Summary_CPU(   JL )    !CPU usage measurements.

	IF ( .NOT. MSG_Journal_Close() ) THEN !This doesn't really have any failure modes.
	END IF

	CALL EXIT
	END

	SUBROUTINE	MSG_TestAlarm( MSG, Level )

	IMPLICIT NONE

	CHARACTER*(*) MSG
	INTEGER       Level

	CALL Message_Out( 'MSG_TestAlarm-I1   Simulated Alarm!!!!!!!!!!!!!', 1 )
	CALL Message_Out( MSG(:8), 1 )
	CALL Message_Out( '                   Simulated Alarm!!!!!!!!!!!!!', 1 )

	RETURN
	END
