#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#define MAX_LOGFILES 10


static char buff[1024] ;
static struct stat statb ;

#ifndef PROJDIR
#warning "PROJDIR not defined! Assuming /RTS"
#define PROJDIR "/RTS"
#endif

static char logfiles[MAX_LOGFILES][128] = {
	PROJDIR"/log/rts.log",
	PROJDIR"/log/trigger.log",
	PROJDIR"/log/evp.log",
	PROJDIR"/log/det.log",
	PROJDIR"/log/daq.log",
} ;

static FILE *files[MAX_LOGFILES] ;

static int oldsizes[MAX_LOGFILES] ;




int main(int argc, char *argv[])
{
	char *fret ;
	int ret ;
	int i ;
	int data_in ;
	u_int last_flush ;

#ifdef OLD_TEST
	for(i=0;i<MAX_LOGFILES;i++) {	
		printf("%3d: %d %c -%s-\n",i,logfiles[i][0],logfiles[i][0],logfiles[i]) ;
	}
	return 0 ;
#endif

	i = 0 ;

	while(logfiles[i][0] != 0) {
		
		files[i] = fopen(logfiles[i],"r") ;

		if(files[i] == NULL) {
			perror(logfiles[i]) ;
		}
		i++ ;
	} ;


	last_flush = 0 ;

	for(;;) {

	u_int last_delta = time(NULL) - last_flush ;

	data_in = 0 ;
	for(i=0;i<MAX_LOGFILES;i++) {

		if(logfiles[i][0]==0) continue ;



		errno = 0 ;
		fret = fgets(buff,sizeof(buff),files[i]) ;
		if(fret == NULL) {
			if(last_delta >= 1) {
				last_flush = time(NULL) ;
				last_delta = 0 ;
				fflush(stdout) ;
			}

			if(errno) {
				perror(logfiles[i]) ;
				sleep(1) ;
				continue ;
			}
			// let's check the file
			ret = stat(logfiles[i],&statb) ;
			if(ret < 0) {
				perror(logfiles[i]) ;
				sleep(1) ;
				continue ;
			}

			if(statb.st_size < oldsizes[i]) {
				fclose(files[i]) ;
				files[i] = fopen(logfiles[i],"r") ;
				oldsizes[i] = 0 ;
				continue ;
				// reopen
			}
			oldsizes[i] = statb.st_size ;
			continue ;	// no data...
		}

		data_in++ ;	// we have something

		if(((strstr(buff,"OPERATOR") != NULL) || 
		    (strstr(buff,"CRITICAL") != NULL) ||
		    (strstr(buff,"SHIFTLOG") != NULL))) {
			printf("%s",buff) ;
		}


	}

	if(!data_in) sleep(1) ;

	}	// FOREVER


	return -1 ;	// UNREACHABLE
}

		
