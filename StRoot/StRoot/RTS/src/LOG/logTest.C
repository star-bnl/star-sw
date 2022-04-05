// This snippet is used for testing the log to the localhost
#include <stdio.h>
#include <stdlib.h>

#include <rtsLog.h>

int main(int argc, char *argv[])
{
	int port ;
	const char *dest ;
	int i ;

	if(argc == 1) {
		port = 8006 ;
		dest = "172.16.0.1" ;
	}
	else {
		port = atoi(argv[1]) ;
		dest = RTS_LOG_HOST ;
	}


	
	printf("Default destination is -%s-, port %d\n",RTS_LOG_HOST,RTS_LOG_PORT) ;

	rtsLogLevel(DBG) ;
	rtsLogAddDest(dest,port) ;
	printf("Destination -%s-, port %d\n",dest,port) ;

	for(i=0;i<10000;i++) {
		LOG(DBG,"Testing log facility %u",i) ;
	}

	LOG(WARN,"This should be warning...",0,0,0,0,0) ;
	LOG(CRIT,"This is really critical...",0,0,0,0,0) ;

	return 0 ;
}

