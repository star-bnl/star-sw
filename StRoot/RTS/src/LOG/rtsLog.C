/*
	Logs into the rts.log
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <getopt.h>

#include <rtsLog.h>

int main(int argc, char *argv[])
{
	extern char *optarg ;
	extern int optind ;

	int c;

	char logdest[128] ;
	short logport ;
	char loglevel[128] ;
	char logstr[1024] ;


	logport = 8000 ;	// rts.log
	strcpy(logdest,"172.16.0.1") ;	// daqman on local net
	strcpy(loglevel,INFO);

	rtsLogLevel(DBG) ;
	rtsLogOutput(RTS_LOG_NET) ;

	memset(logstr,0,sizeof(logstr)) ;

	while((c = getopt(argc,argv,"c:d:p:w:h:")) != EOF) {
	switch(c) {
	case 'c' :
		rtsLogAddCmd(optarg) ;
		break ;
	case 'd' :	// loglevel
		strcpy(loglevel,optarg) ;
		break ;
	case 'w' :
		rtsLogOutput(atoi(optarg)) ;
		break ;
	case 'p' :
		logport = atoi(optarg) ;
		break ;
	case 'h' :
		strcpy(logdest,optarg) ;
		break ;
	case '?' :
	default :
		fprintf(stderr,"Usage %s: [-d loglevel] [-p port] [-w output] [-h log host] [-c cmd]\n",argv[0]) ;
		return -1 ;
	}
	}
		
	rtsLogAddDest(logdest,logport) ;

	sprintf(logstr,"COLOR%s: %s [line 0]:",loglevel,__FILE__) ;

	while(optind < argc) {
		strcat(logstr," ") ;
		strcat(logstr,argv[optind]) ;
		optind++ ;
	}

	strcat(logstr,"\n") ;

	rtsLogUnix_v(logstr) ;

	return 0 ;
}

