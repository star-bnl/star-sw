	program msgTest

	implicit none

*  Description:
*	Test bed for msg routines, and their underlying str calls.

	integer     MSG_Journal_LUN_P
	parameter ( MSG_Journal_LUN_P = 11 )

	integer TL, JL
	integer I

	integer ID1, ID2, ID3, ID4, ID5, ID6
	save    ID1, ID2, ID3, ID4, ID5, ID6

	external MsgAlarmRoutineSample
	logical Msg_Journal_Close
	logical Msg_Journal_Open

	data    ID1, ID2, ID3, ID4, ID5, ID6
     1	       / 0, 0, 0, 0, 0, 0 /


	call Msg_Ini( MSG_Journal_LUN_P )
	if ( .not. Msg_Journal_Open( 'msg.jou' ) ) then
	  call exit
	end if

	call Message_Out(
     1	'msgTest-I0  Testing Fortran-LUN to C file-stream conversion:'
     1	  , 1 )
	open( unit=12, file='msgTest.dat', status='UNKNOWN'
     1	  , carriagecontrol='LIST' )
	write( 12,
     1	  '(''Test to Fortran unit 12 with a Fortran write'')' )
	call Msg_Display_and_Echo_Out(
     1	  'Test to Fortran unit 12 through msg (non-Fortran)', 1, 12 )
	call Msg_To_Journal_Out(
     1	  'Test to the Journal following test to unit 12', 1 )
	close( unit=12 )
	call Message_Out(
     1	'       Done Testing Fortran-LUN to C file-stream conversion.'
     1	  , 1 )

	call Msg_Get_LUN(  TL,  JL )    !Get terminal and journal LUNs.

	call MsgAlarmRegister( MsgAlarmRoutineSample )

	call Msg_Mark( 'msgTest-O4', ID4 )
	do I = 1, 1024
	  call Message( 'msgTest-O6  Once    #6', 1, ID6 )
	end do
	call MSG_EnableAlarm( 'msgTest-I1' )
	call Message( 'msgTest-I1  Message #1', 1, ID1 )
	call Message( 'msgTest-T2  Trace   #2', 1, ID2 )
	call Message( 'msgTest-C3  Count   #3', 1, ID3 )
	call Message( 'msgTest-O4  Once    #4', 1, ID4 )
	call Message( 'msgTest-T5  Trace   #5', 1, ID5 )
	call Message( 'msgTest-O6  Once    #6', 1, ID6 )
	call Message( 'msgTest-I7  Message #7', 1,  -1 )

	call MSG_Mark( 'msgTest-O4', ID4 )
	call MSG_Enable( 'msgTest-T2' )
	call Message( 'msgTest-I1  Message #1', 1, ID1 )
	call Message( 'msgTest-T2  Trace   #2', 1, ID2 )
	call Message( 'msgTest-C3  Count   #3', 1, ID3 )
	call Message( 'msgTest-O4  Once    #4', 1, ID4 )
	call Message( 'msgTest-T5  Trace   #5', 1, ID5 )
	call Message( 'msgTest-O6  Once    #6', 1, ID6 )
	call Message( 'msgTest-I7  Message #7', 1,  -1 )

	call MSG_Mark( 'msgTest-O4', ID4 )
	call MSG_Disable( 'msgTest-T2' )
	call Message( 'msgTest-I1  Message #1', 1, ID1 )
	call Message( 'msgTest-T2  Trace   #2', 1, ID2 )
	call Message( 'msgTest-C3  Count   #3', 1, ID3 )
	call Message( 'msgTest-O4  Once    #4', 1, ID4 )
	call Message( 'msgTest-T5  Trace   #5', 1, ID5 )
	call Message( 'msgTest-O6  Once    #6', 1, ID6 )
	call Message( 'msgTest-I7  Message #7', 1,  -1 )


	call Msg_Summary_Event( TL, 3 ) !List them all, to the terminal.
	call Msg_Summary_CPU(   TL )    !CPU usage measurements.

	call Msg_Summary_Event( JL, 3 ) !List them all, in the journal.
	call Msg_Summary_CPU(   JL )    !CPU usage measurements.

	if ( .not. Msg_Journal_Close() ) then !This doesn't really have any failure modes.
	end if

	call exit
	end



*
	subroutine	Msg_TestAlarm( Prefix, sansPrefix, Level )

	implicit none

	character*(*) Prefix
	character*(*) sansPrefix
	integer       Level

	call Message_Out(
     1	 'MSG_TestAlarm-I1   Simulated Alarm!!!!!!!!!!!!!', 1 )
	call Message_Out( Prefix, 1 )
	call Message_Out( sansPrefix, 1 )
	call Message_Out(
     1	 '                   Simulated Alarm!!!!!!!!!!!!!', 1 )

	return
	end
