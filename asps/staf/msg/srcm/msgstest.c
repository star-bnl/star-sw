
#include <unistd.h>
#include <stdlib.h>
#include <msg.h>

	void main( int argc, char*argv[] )
{

/*  Description:
	Test bed for msg "c" routines, and their underlying str calls.
*/

#define MSG_Journal_LUN_P 11

	int i;
	static int  negOne=-1;
	static int *NegOne=&negOne;
	static int ID1=0;
	static int ID2=0;
	static int ID3=0;
	static int ID4=0;
	static int ID5=0;
	static int ID6=0;
	static int ID8=0;
	static int ID9=0;
	static int ID10=0;


	MsgInit( "s" );
	if ( !MsgJournalOpen( "msg.jou" ) ) {
	  exit(1);
	}

	MsgMark( "msgTest-O4", &ID4 );
	for ( i=0; i<1024; i++ ) {
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

	i = 0;
	while ( TRUE ) {
	  Message( "msgTest-I8  Looping endlessly, testing shared memory", &ID8 );
	  Message( "msgTest-T9  Tracing endlessly, testing shared memory", &ID9 );
	  sleep( 1 );
	  if ( MsgEnabled( "msgTest-T10  Enable this message to quit.", &ID10 ) ) {
	    MsgFinish( "s", i );
	    exit(0);
	  }
	  i++;
	}

	exit(1);
}
