#include <stdio.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <arpa/inet.h>
#include <string.h>
#include <syslog.h>
#include <stdlib.h>
#include <time.h>

#if defined(__linux__) || defined(__APPLE__)	
#else	/* Solaris */
#include <procfs.h>	
#endif


#include <rtsLog.h>


#ifdef __cplusplus
extern "C" {
#endif

volatile int tonkoLogLevel = 2 ;	


static char *getCmd(void) ;
static int  odesc = -1 ;
static int handchange ;
static FILE *fdesc = 0 ;

/* defaults */
#ifdef RTS_PROJECT_PP
static char servER[80] = "130.199.20.88" ;
#else
static char servER[80] = RTS_LOG_HOST ;
#endif

static int port = RTS_LOG_PORT ;
static int output_flag = RTS_LOG_NET ;
static char cmd[1024] ;

/*
	Thread issues:

	The routine itself should be thread safe however no guarantees
	are made to the operating system calls:
		sendto
		sprintf
		strlen
		perror
	The current (2.6) Solaris mentions that they should be.

	Additionally, the destination  node:port as well as the log severity
	level are COMMON (by design) to all threads.
*/

int rtsLogOutput(int flag)
{
	output_flag = flag ;
	return flag ;
}

void rtsLogAddCmd(const char *cmd_in)
{
	strncpy(cmd,cmd_in,sizeof(cmd)-1) ;
	return ;
}

int rtsLogAddDest(const char *host, int newport)
{
	/* mark as changed by hand */
	handchange = 1 ;

	/* set the statics */
	strncpy(servER,host,sizeof(servER)-1) ;
	port = newport ;
	/* mark as not connected so that next send reconnects
	with the new values */
	if(odesc >= 0) close(odesc) ;	
	odesc = -1 ;


	return 0 ;
}

int rtsLogAddFile(char *fname)
{
	if(fdesc) fclose(fdesc) ;
	fdesc = 0 ;

	if(fname) {
		fdesc = fopen(fname,"w") ;
	}

	if(fdesc) return 0 ;
	
	return -1 ;

}


static char _g_fname[256];
static char *jml_fname = NULL;
void rtsLogAddJmlFile (char *fname)
{
	strcpy(_g_fname, fname);
	jml_fname = _g_fname;
}


int rtsLogUnix_v(const char *str, ...) 
{
	/* common to all threads */
	static int sockAddrSize ;
	static struct sockaddr_in serverAddr ;
	static char *cmd_l ;
	/* thread dependant */
	int ret ;
	char buffer[10240] ;
	char *string = buffer ;
	unsigned int *cou = (unsigned int *)buffer ;
	int len ;
	int err = 0 ;
	va_list ap ;
	int colored ;
	int to_file = 0 ;

	retry_connect: ;

	if((output_flag & RTS_LOG_NET) && (odesc<0)) {	/* set the socket up */
		char *rts_host ;
		int bufsize ;

//		if(port==0) port = RTS_LOG_PORT ;

		if(!handchange && (rts_host=getenv("RTS_LOG_HOST"))) {
			strncpy(servER,rts_host,sizeof(servER)-1) ;
		}

	
		cmd_l = 0 ;

		sockAddrSize = sizeof(struct sockaddr_in) ; 
		memset((char *)&serverAddr,0,sockAddrSize) ;
		serverAddr.sin_family = AF_INET ;
		serverAddr.sin_port = htons(port) ;

		/* hostname */
		if((serverAddr.sin_addr.s_addr = inet_addr(servER)) == (unsigned int)-1) {
			perror(servER);
			return -1 ;
		}

		odesc = socket(AF_INET, SOCK_DGRAM,0) ;
		if(odesc < 0) {
			perror("socket") ;
			return -1 ;
		}

		bufsize = 8*1024*1024 ;
		for(;;) {
			ret = setsockopt(odesc,SOL_SOCKET,SO_SNDBUF,(char *)&bufsize,sizeof(bufsize)) ;
			if(ret == 0) break ;
			bufsize /= 2 ;
		}
		//printf("2: set sockbuff to %d bytes\n",bufsize) ;

		cmd_l = getCmd() ;	// get the name of the running executable
	}

	len = 4 ;	/* to account for the counter */

	colored = 0 ;
	if(strncmp(str,"COLOR",5)==0) {	// COLORED output!

		str += 5 ;	

		if(strncmp(str,DBG,strlen(DBG))==0) {
			;	// don't color
		}
		else if(strncmp(str,NOTE,strlen(NOTE))==0) {
			;	// don't color
		}
		else if(strncmp(str,WARN,strlen(WARN))==0) {
			colored = 1 ;
			sprintf(string+len,"%s",ANSI_CYAN) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,CRIT,strlen(CRIT))==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s%s%s",ANSI_RED,ANSI_BOLD,ANSI_REVERSE) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,ERR,strlen(ERR))==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s",ANSI_RED) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,CAUTION,strlen(CAUTION))==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s%s",ANSI_MAGENTA,ANSI_REVERSE) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,TERR,strlen(TERR))==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s",ANSI_GREEN) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,OPER,strlen(OPER))==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s%s",ANSI_BLUE,ANSI_REVERSE) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,"U_",2)==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s",ANSI_REVERSE) ;
			len += strlen(string+len) ;
		}
		else if(strncmp(str,INFO,strlen(INFO))==0) {
			colored = 1 ;
			to_file = 1 ;
			sprintf(string+len,"%s",ANSI_BLUE) ;
			len += strlen(string+len) ;
		}
		else  {
			//colored = 1 ;
			//sprintf(string+len,"%s",ANSI_BLUE) ;
			//len += strlen(string+len) ;
		}

	}

	if(cmd[0]) {
		sprintf(string+len,"(%s): ",cmd) ;
		len += strlen(string+len) ;
	}
	else if(cmd_l) {
		sprintf(string+len,"(%s): ",cmd_l) ;
		len += strlen(string+len) ;
	}


		

	va_start(ap, str) ;
	vsprintf(string+len,str,ap) ;
	len += strlen(string+len) ;

	if(colored) {
		char *end ;
		int nl_cou = 0 ;

		end = string + len  ;	// points at 0 now...
		while(*(end-1) == '\n') {
			nl_cou++ ;
			len-- ;
			*(end-1) = 0 ;
			end-- ;
		}
		sprintf(end,"%s",ANSI_RESET) ;
		len += strlen(end) ;

		while(nl_cou) {
			strcat(string+len,"\n") ;
			len += strlen(string+len) ;
			nl_cou-- ;
		}
	}
	
	//fprintf(stderr,"********** len from calc %d, len now %d\n",len,strlen(buffer+4)+4) ;
	//len = strlen(buffer+4)+4 ;

	*cou = 0xFFFFFFFF ;	/* special handling by server! */

	/* send it to the right place via UDP */

	if((odesc >= 0) && (output_flag & RTS_LOG_NET)) {
		ret = sendto(odesc,(char *)buffer,len,0,
			     (struct sockaddr *)&serverAddr,sockAddrSize) ;
		//fprintf(stderr,"**** sendto returns %d, should %d\n",ret,len) ;

		if(ret < 0) {
			syslog(LOG_USER|LOG_ERR,"LOG sendto returned %d [%m]\n",ret) ;
			close(odesc) ;
			odesc = -1 ;
			if(err == 0) {	// reconnect only once!
				err = 1 ;
				goto retry_connect ;
			}
		}
	}

	/* and/or to stderr */
	if(output_flag & RTS_LOG_STDERR) {
		fprintf(stderr,"%s",(char *)buffer+4) ;
	}


	if(fdesc && to_file && (output_flag & RTS_LOG_FILE)) {
		time_t t = time(0) ;
		struct tm *tm = localtime(&t) ;

		if(fdesc) fprintf(fdesc,"%02d:%02d:%02d: %s",tm->tm_hour,tm->tm_min,tm->tm_sec,(char*)buffer+4) ;
	}

	if(jml_fname) {
		time_t t = time(0) ;
		struct tm *tm = localtime(&t) ;

		FILE *f = fopen(jml_fname, "a");
		if(f) fprintf(f,"%02d:%02d:%02d: %s",tm->tm_hour,tm->tm_min,tm->tm_sec,(char*)buffer+4) ;
		fclose(f);

	}

	return 0 ;
}



static char *getCmd(void)
{


	static char *str = "(no-name)" ;
#if defined(__linux__) || defined(__APPLE__)	
	FILE *file ;
	static char name[128] ;
	char *ptr ;

	file = fopen("/proc/self/cmdline","r") ;
	if(file==NULL) return str ;

	fscanf(file,"%120s",name) ;	/* make sure we don't overwrite name */
	fclose(file) ;

	if((ptr = strrchr(name,'/'))) {
		return ptr+1 ;
	}
	else {
		return name ;
	}
#else	// solaris
	int fd, ret ;
	static struct psinfo ps ;

	fd = open("/proc/self/psinfo",O_RDONLY,0666) ;
	if(fd < 0) return str ;

	ret = read(fd,(char *)&ps,sizeof(ps)) ;
	close(fd) ;

	if(ret != sizeof(ps)) {
		return str ;
	}

	return ps.pr_fname ;
#endif
}

#ifdef __cplusplus
}
#endif

