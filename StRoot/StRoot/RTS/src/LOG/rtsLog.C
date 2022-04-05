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
	char filearg[128] ;
	char cmdarg[128] ;
	int linenum ;
	int use_stdin ;
	char buff[256] ;

	logport = RTS_LOG_PORT_TEST ;	// test.log
	strcpy(logdest,"172.16.0.1") ;	// daqman on local net
	strcpy(loglevel,INFO);
	strcpy(filearg,"shell.sh") ;
	strcpy(cmdarg,"shell") ;
	linenum = 0 ;
	use_stdin = 0 ;

	rtsLogLevel(DBG) ;
	rtsLogOutput(RTS_LOG_NET) ;

	memset(logstr,0,sizeof(logstr)) ;

	while((c = getopt(argc,argv,"c:d:p:w:h:f:l:i")) != EOF) {
	switch(c) {
	case 'c' :
		strcpy(cmdarg,optarg) ;
		break ;
	case 'd' :	// loglevel as string
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
	case 'f' :
		strcpy(filearg,optarg) ;
		break ;
	case 'l' :
		linenum = atoi(optarg) ;
		break ;
	case 'i' :
		use_stdin = 1 ;
		break ;
	case '?' :
	default :
		fprintf(stderr,"Usage %s: [-d loglevel] [-p port] [-w output] [-h log host] [-c cmd]\n",argv[0]) ;
		return -1 ;
	}
	}
		
	rtsLogAddCmd(cmdarg) ;
	rtsLogAddDest(logdest,logport) ;

	if(use_stdin) {
		while(!feof(stdin)) {
			if(fgets(buff,sizeof(buff),stdin)==0) continue ;

			sprintf(logstr,"COLOR%s: %s [line %d]: %s",loglevel,filearg,linenum,buff) ;
			rtsLogUnix_v(logstr) ;
		}
	}
	else {

		sprintf(logstr,"COLOR%s: %s [line %d]:",loglevel,filearg,linenum) ;

		while(optind < argc) {
			strcat(logstr," ") ;
			strcat(logstr,argv[optind]) ;
			optind++ ;
		}

		strcat(logstr,"\n") ;

		rtsLogUnix_v(logstr) ;
	}

	return 0 ;
}

