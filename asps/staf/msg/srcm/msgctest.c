
#include <stdlib.h>
#include "msg.h"

	void main()
{

/*  Description:
	Test bed for msg "c" routines, and their underlying str calls.
*/

#define MSG_Journal_LUN_P 11

	int i;
	static int  negOne=-1;
	static int *NegOne=&negOne;
	static int JL, TL;
	static int ID1=0;
	static int ID2=0;
	static int ID3=0;
	static int ID4=0;
	static int ID5=0;
	static int ID6=0;

	MsgIni( MSG_Journal_LUN_P );
	if ( !MsgJournalOpen( "msg.jou" ) )
	{
	  exit(1);
	}

	MsgMark( "msgTest-O4", &ID4 );
	for ( i=0; i<1024; i++ )
	{
	  Message( "msgTest-O6  Once    #6", &ID6 );
	}
	Message( "msgTest-I1  Message #1", &ID1 );
	Message( "msgTest-T2  Trace   #2", &ID2 );
	Message( "msgTest-C3  Count   #3", &ID3 );
	Message( "msgTest-O4  Once    #4", &ID4 );
	Message( "msgTest-T5  Trace   #5", &ID5 );
	Message( "msgTest-O6  Once    #6", &ID6 );
	Message( "msgTest-I7  Message #7", NegOne );

	MsgMark( "msgTest-O4", &ID4 );
	MsgEnable( "msgTest-T2" );
	Message( "msgTest-I1  Message #1", &ID1 );
	Message( "msgTest-T2  Trace   #2", &ID2 );
	Message( "msgTest-C3  Count   #3", &ID3 );
	Message( "msgTest-O4  Once    #4", &ID4 );
	Message( "msgTest-T5  Trace   #5", &ID5 );
	Message( "msgTest-O6  Once    #6", &ID6 );
	Message( "msgTest-I7  Message #7", NegOne );

	MsgMark( "msgTest-O4", &ID4 );
	MsgDisable( "msgTest-T2" );
	Message( "msgTest-I1  Message #1", &ID1 );
	Message( "msgTest-T2  Trace   #2", &ID2 );
	Message( "msgTest-C3  Count   #3", &ID3 );
	Message( "msgTest-O4  Once    #4", &ID4 );
	Message( "msgTest-T5  Trace   #5", &ID5 );
	Message( "msgTest-O6  Once    #6", &ID6 );
	Message( "msgTest-I7  Message #7", NegOne );

	MsgGetLUN( &TL,  &JL );   /* Get terminal and journal LUNs.   */

	MsgSummaryEvent( TL, 3 ); /* List them all, to the terminal.  */
	MsgSummaryCPU(   TL );    /* CPU usage measurements.          */

	MsgSummaryEvent( JL, 3 ); /* List them all, in the journal.   */
	MsgSummaryCPU(   JL );    /* CPU usage measurements.          */
	if ( !MsgJournalClose() )
	{
	  exit(1);
	}

	exit(0);
}
