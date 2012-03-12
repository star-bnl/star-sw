#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <syslog.h>
#include <time.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <signal.h>
#include <errno.h>


#define CONNECT_WAIT 5

static char tbuff[10240] ;






#ifdef __sun
typedef int socklen_t ;
#endif

int main(int argc, char *argv[])
{
	int s ;
	int ret ;
//	int i ;
	int port ;
	int fd = 0;
	static struct sockaddr_in cAddr ;
	int cSize ;
	static char *buffer ;
	time_t t ;
	struct tm *tm ;
	static char hname[256], hname_short[256] ;
	struct hostent *hent ;
	int testing = 0 ;

	if(argc > 3) {
		fprintf(stderr,"Usage: %s <file> [port]\n",argv[0]) ;	
		syslog(LOG_ERR,"Usage: %s <file> [port]\n",argv[0]) ;	
		return -1 ;
	}


	if(argc==3) {
		port = atoi(argv[2]) ;
	}
	else port = 0 ;

	if(port) syslog(LOG_INFO,"Started on port %d, file %s\n",port,argv[1]) ;
	else syslog(LOG_INFO,"Started on stdin, file %s\n",argv[1]) ;

	cSize = sizeof(cAddr) ;

	if(strcmp(argv[1],"dummy") == 0) {
		testing = 1 ;
	}
	else {
		errno = 0 ;
		fd = open(argv[1],O_CREAT | O_APPEND | O_WRONLY,0666) ;
		if(fd < 0) {
			syslog(LOG_ERR,"File %s open: %s\n",argv[1],strerror(errno)) ;
			return -1 ;
		}
	}
	
	for(;;) {
		int sz ;
		int cou = 0 ;

		s = 0 ;	// stdin
		

		
		sz = 16*1024*1024 ;
		for(;;) {

			ret = setsockopt(s,SOL_SOCKET,SO_RCVBUF,(char *)&sz,sizeof(int)) ;
			if(ret == 0) break ;
			sz /= 2 ;
		} ;

		syslog(LOG_INFO,"RCVBUFF set to %d bytes...\n",sz) ;

		t = time(NULL) ;
		tm = localtime(&t) ;

		memset(tbuff,'*',sizeof(tbuff)) ;
		sprintf(tbuff,"[%-8s %02d:%02d:%02d %03d] ","STARTUP",tm->tm_hour,tm->tm_min,tm->tm_sec,tm->tm_yday+1) ;
		int pre_len = strlen(tbuff) ;
		buffer = tbuff + pre_len - 4;

		int first = 1 ;

		for(;;) {
			int i, j ;
//			int *counter = (int *)buffer ;
 
			ret = recvfrom(s,buffer,sizeof(tbuff)-pre_len-10,0,
				       (struct sockaddr *)&cAddr, (socklen_t *)&cSize) ;

			i = (inet_lnaof(cAddr.sin_addr) & 0xFF00) >> 8 ;
			j = inet_lnaof(cAddr.sin_addr) & 0xFF ;

			// sanity...
			tbuff[sizeof(tbuff)-1] = 0 ;	// just to make sure that strlen will work!
			

			hent = gethostbyaddr((char *)&cAddr.sin_addr,4,AF_INET) ;
			// testing!
			//hent = 0 ;
			if(!hent) {
				sprintf(hname_short,"%03d.%03d",i,j) ;
				//syslog(LOG_ERR,"gethostbyname -%s- error\n",hname_short) ;

			}
			else {
				strcpy(hname,hent->h_name) ;
				strcpy(hname_short,hent->h_name) ;
				char *dot = index(hname_short,'.') ;
				if(dot) {	// zap all after first dot...
					*dot = 0 ;
				}
				// and zap after 8 chars anyway!
				*(hname_short+8) = 0 ;

				//syslog(LOG_ERR,"Connection from -%s-   -%s-\n",hname,hname_short) ;
			}

			//fprintf(stderr,"Received %d bytes from %s [%d:%d] - sequence %d\n",ret,inet_ntoa(cAddr.sin_addr),i,j,*(int *)buffer) ;

			if(ret == 0) {
				syslog(LOG_ERR,"Connection closed by host. On UDP???\n") ;
			}
			if(ret < 0) {
				syslog(LOG_ERR,"Read failed [%m] - reconnecting... On UDP???\n") ;
				close(s) ;
				s = -1 ;
				break ;
			}

			t = time(NULL) ;
			tm = localtime(&t) ;

			char tmp = *(buffer+4) ;
			sprintf(tbuff,"[%-8s %02d:%02d:%02d %03d] ",hname_short,tm->tm_hour,tm->tm_min,tm->tm_sec,tm->tm_yday+1) ;
			*(buffer+4) = tmp ;

			// skip the first INT which has the counter in it and for NL

			*(buffer+ret) = 0 ;

//			syslog(LOG_ERR,"Last char is -%40s-, len %d, chars %d %d %d %d\n",(buffer+4),strlen(buffer+4),
//			       *(buffer+1+strlen(buffer+4)),*(buffer+2+strlen(buffer+4)),*(buffer+3+strlen(buffer+4)),*(buffer+4+strlen(buffer+4))) ;

			int len ;
			if(*(buffer+3+strlen(buffer+4)) == '\n') {
//				syslog(LOG_ERR,"Has newline...") ;
				len = (buffer+3+strlen(buffer+4))-tbuff ;
			}
			else {	// add a NL
				*(buffer+4+strlen(buffer+4)) = (char) '\n' ;
				len = (buffer+4+strlen(buffer+4))-tbuff ;
			}

			cou++ ;
			if(testing) {
				if((cou%1000)==0) {
					syslog(LOG_INFO,"%s : Got %u packets...\n",argv[1],cou) ;
				}
			}
			else {
				if(first) {
					syslog(LOG_INFO,"Doing write of %d bytes\n",len+1) ;
				}
				first = 0;
				write(fd,tbuff,len+1) ;
			}

		}
	}

	if(testing) ;
	else close(fd) ;
	return 0 ;
}

