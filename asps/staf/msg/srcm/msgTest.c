
#include <stdlib.h>
#include "msg.h"


void    main( int argc, char*argv[] )

{

/*  Description:
	Test bed for new msg "c" routines.
*/

	int i;
	static int ID1=0;
	static int ID2=0;
	static int ID3=0;
	static int ID4=0;
	static int ID5=0;
	static int ID6=0;

	FILE *fid;

	MsgInit( "hs" );
	if ( !MsgJournalOpen( "msg.jou" ) ) {
	  exit(1);
	}

	MsgAlarmRegister( (funcPoint) MsgAlarmRoutineSample );

	MsgEnableAlarm( "msgTest-I1" );

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
	Message( "msgTest-I7  Message #7", NULL );

	MsgMark( "msgTest-O4", &ID4 );
	MsgEnable( "msgTest-T2" );
	Message( "msgTest-I1  Message #1", &ID1 );
	Message( "msgTest-T2  Trace   #2", &ID2 );
	Message( "msgTest-C3  Count   #3", &ID3 );
	Message( "msgTest-O4  Once    #4", &ID4 );
	Message( "msgTest-T5  Trace   #5", &ID5 );
	Message( "msgTest-O6  Once    #6", &ID6 );
	Message( "msgTest-I7  Message #7", NULL );

	MsgMark( "msgTest-O4", &ID4 );
	MsgDisable( "msgTest-T2" );
	Message( "msgTest-I1  Message #1", &ID1 );
	Message( "msgTest-T2  Trace   #2", &ID2 );
	Message( "msgTest-C3  Count   #3", &ID3 );
	Message( "msgTest-O4  Once    #4", &ID4 );
	Message( "msgTest-T5  Trace   #5", &ID5 );
	Message( "msgTest-O6  Once    #6", &ID6 );
	Message( "msgTest-I7  Message #7", NULL );

	MsgFinish( "s", 3 );

	exit(0);
}

