#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/time.h>
#include <utime.h>

#define MAX_LOGFILES 10
#define LOGLINES	400 

#define DATA_1	"/online/tonko/log_content_old.html"
#define DATA_2	"/online/tonko/log_content.html"
#define TOUCH	"/online/tonko/log_touch.html"
//#define DAQ_TOUCH	"/online/tonko/daq_touch.html"

int parseLog(char *b, u_int file, u_int line) ;
int dumpLines(void) ;

static char buff[1024] ;
static struct stat statb ;

#ifndef PROJDIR
#warning "PROJDIR not defined! Assuming /RTS"
#define PROJDIR "/RTS"
#endif

//#define LOG_SUFFIX ".0"
#define LOG_SUFFIX ""

static char logfiles[MAX_LOGFILES][128] = {
	PROJDIR"/log/rts.log"LOG_SUFFIX,
	PROJDIR"/log/trigger.log"LOG_SUFFIX,
	PROJDIR"/log/evp.log"LOG_SUFFIX,
	PROJDIR"/log/det.log"LOG_SUFFIX,
	PROJDIR"/log/daq.log"LOG_SUFFIX,
	PROJDIR"/log/tpx.log"LOG_SUFFIX,
	PROJDIR"/log/esb.log"LOG_SUFFIX,

} ;

// took out	PROJDIR"/log/reader.log"LOG_SUFFIX,

static FILE *files[MAX_LOGFILES] ;

static int oldsizes[MAX_LOGFILES] ;


static char data_preamble[] = { " \
<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"> \
<html> \
<head> \
<link rel=\"stylesheet\" type=\"text/css\" href=\"daq.css\"> \
<title>STAR DAQ Logging</title> \
</head> \
<body> \
" };



static struct logline {
	u_int sec ;
	char tm[24] ;
	u_int day ;	// day of the year...

	u_int repeat ;

	u_int file ;
	u_int seq ;

	char node[10] ;
	char rb[10] ;

	char sev[32] ;

	char task[16] ;
	char src[128] ;
	u_int line ;

	char msg[256] ;
} loglines[LOGLINES] ;

int last_logline = 0 ;


int main(int argc, char *argv[])
{
	char *fret ;
	int ret ;
	int i ;
	int data_in, new_lines ;
	u_int last_flush ;
	u_int lines[MAX_LOGFILES] ;

//#define OLD_TEST
#ifdef OLD_TEST
	for(i=0;i<MAX_LOGFILES;i++) {	
		printf("%3d: %d %c -%s-\n",i,logfiles[i][0],logfiles[i][0],logfiles[i]) ;
	}
	return 0 ;
#endif

	i = 0 ;


	FILE *ff = fopen(TOUCH,"w") ;
	fprintf(ff,"%u\n",(u_int)time(NULL)) ;
	fclose(ff) ;

	while(logfiles[i][0] != 0) {
		
		files[i] = fopen(logfiles[i],"r") ;

		if(files[i] == NULL) {
			perror(logfiles[i]) ;
		}
		i++ ;
	} ;


	last_flush = 0 ;

	memset(lines,0,sizeof(lines)) ;

	new_lines = 0 ;

	for(;;) {

	u_int last_delta = time(NULL) - last_flush ;

	data_in = 0 ;

	for(i=0;i<MAX_LOGFILES;i++) {

		if(logfiles[i][0]==0) continue ;


//		fprintf(stderr,"Here 1: %d\n",i) ;

		errno = 0 ;
		fret = fgets(buff,sizeof(buff),files[i]) ;
		if(fret == NULL) {

			if(errno) {
				perror(logfiles[i]) ;
				sleep(1) ;
				continue ;
			}
			// let's check the file
			errno = 0 ;
			ret = stat(logfiles[i],&statb) ;
			if(ret < 0) {
				perror(logfiles[i]) ;
				sleep(1) ;
				continue ;
			}

			if(statb.st_size < oldsizes[i]) {
				// reopen
				fclose(files[i]) ;
				files[i] = fopen(logfiles[i],"r") ;
				oldsizes[i] = 0 ;
				lines[i] = 0 ;
				continue ;
			}

			oldsizes[i] = statb.st_size ;

		}
		else {
			data_in++ ;	// we have something

			lines[i]++ ;

			if((strstr(buff,"CRITICAL") != NULL) ||\
			   (strstr(buff,"OPERATOR") != NULL) ||\
			   (strstr(buff,"CAUTION") != NULL)) {
				new_lines++ ;
				parseLog(buff,i,lines[i]) ;
				//printf("%s",buff) ;
			}
		}

	}


//	fprintf(stderr,"Ending...\n") ;


	if((last_delta >= 60) || (new_lines && (last_delta >= 5))) {	// dump every 1 if there is any new content
		last_flush = time(NULL) ;
		last_delta = 0 ;
		dumpLines() ;
		printf("Dumping %d lines\n",new_lines) ;
		new_lines = 0 ;
	}

	if(!data_in) {	// pause if no data...
		printf("No data...\n") ;
		sleep(1) ;
	}
	else {
		static u_int tot_data ;
		tot_data += data_in ;

		if((tot_data % 10000)==0) {
			printf("read %d lines\n",tot_data) ;
		}
	}


	}	// FOREVER


	return -1 ;	// UNREACHABLE
}

int parseLog(char *b, u_int file, u_int seq)
{
	u_int cou ;
	int i ;
	static char tm[16] ;
	static char task[128] ;
	static char line[128] ;
	static char day[8] ;

	int use = 0 ;
	u_int min_sec = 0xffffffff ;

	for(i=0;i<LOGLINES;i++) {
		if(loglines[i].sec == 0) {	// unassigned - use immediately
			use = i ;
			break ;
		}

		if(loglines[i].sec <= min_sec) {
			min_sec = loglines[i].sec ;
			use = i ;
		}
	}


	loglines[use].repeat = 1 ;	// clear to 1...


	// skip [
	b++ ;


	cou = 0 ;
	while(*b != ' ') {
		loglines[use].node[cou++] = *b++ ;
	}
	loglines[use].node[cou] = 0 ;

	while(*(++b) == ' ') ;	// skip spaces

	cou = 0 ;
	while(*b != ' ') {
		tm[cou++] = *b++ ;
	}
	tm[cou] = 0 ;

	b++ ;	// skip space

	// get day
	strncpy(day,b,3) ;
	b += 3 ;
	loglines[use].day = atoi(day) ;


	b++ ;	// skip ]
	b++ ;	// skip space

	cou = 0 ;
	while(*b != ':') {
		task[cou++] = *b++ ;
	}
	task[cou] = 0 ;


	b++ ;	// skip :
	b++ ;	// skip space

	cou = 0 ;
	while(*b != ':') {
		loglines[use].sev[cou++] = *b++ ;
	}
	loglines[use].sev[cou] = 0 ;


	b++ ;	// skip :
	b++ ;	// skip space

	cou = 0 ;
	while(*b != ' ') {
		loglines[use].src[cou++] = *b++ ;
	}
	loglines[use].src[cou] = 0 ;

	b += 7 ;	// skip space,[,line,space
	
	cou = 0 ;
	while(*b != ']') {
		line[cou++] = *b++ ;
	}
	line[cou] = 0 ;

	loglines[use].line = atoi(line) ;

	b += 3 ;	// skip ],:,space

	*(b+strlen(b)-1) = 0 ;

	strncpy(loglines[use].tm,tm,sizeof(loglines[use].tm)-1) ;

	char itm[sizeof(tm)] ;
	strcpy(itm,tm) ;

	for(cou=0;cou<strlen(tm);cou++) {
		if(itm[cou] == ':') itm[cou] = 0 ;
	}

	
	loglines[use].sec = loglines[use].day*24*3600 + atoi(itm)*3600 + atoi(itm+3)*60 + atoi(itm+6) ;


	char *the_task ;
	char rb[10] ;
	if((task[1] == 'R') && (task[2] == 'B') && (task[5] == '_') && (task[7] == ']')) {	// Receiver Board
		strncpy(rb+1,task+1,6) ;
		rb[0] = ':' ;
		rb[7] = 0 ;

		cou = 0 ;

		the_task = &task[9] ;
		if((task[9] == '0') && (task[10]=='x')) {
			*(task+strlen(task)-1)=0 ;	// kill the last ")"
			while(*the_task++ != '(') ;			
		}

	}
	else {
		rb[0] = 0 ;
		
		the_task = task+1 ;	// skip "("
		*(the_task + strlen(the_task) -1) = 0 ;	// kill ")"
	}

	if(strlen(the_task) == 0) {
		strcpy(the_task,"???") ;
	}

	strncpy(loglines[use].task,the_task,sizeof(loglines[use].task)-1) ;
	strncpy(loglines[use].rb,rb,sizeof(loglines[use].rb)-1) ;
	strncpy(loglines[use].msg,b,sizeof(loglines[use].msg)-1) ;

	printf("Adding %d, last %d: file %d, line %d....\n",use,last_logline,file,seq) ;

	loglines[use].seq = seq ;
	loglines[use].file = file ;

	for(i=0;i<LOGLINES;i++) {
		if(i==use) continue ;
		if(loglines[i].sec == 0) continue ;

		if(loglines[i].line == loglines[use].line) {
		if(loglines[i].file == loglines[use].file) {
		if(loglines[i].sec == loglines[use].sec) {
		if(strcmp(loglines[i].node,loglines[use].node)==0) {
		if(strcmp(loglines[i].src,loglines[use].src) == 0) {
		if(strcmp(loglines[i].task,loglines[use].task) == 0) {
			loglines[use].repeat = loglines[i].repeat + 1 ;
			loglines[i].sec = 0 ;
		}}}}}}
	}


	last_logline = use ;

	return use ;
}


int compare(const void *s1, const void *s2)
{
	struct logline *l1 = (struct logline *)s1 ;
	struct logline *l2 = (struct logline *)s2 ;

	if(l1->sec < l2->sec) return 1 ;
	if(l1->sec > l2->sec) return -1 ;

	if(l1->file == l2->file) {
		if(l1->seq < l2->seq) return 1 ;
		if(l1->seq > l2->seq) return -1 ;
		return 0 ;
	}

	// not from the same file...
	if(l1->file < l2->file) return 1 ;
	if(l1->file > l2->file) return -1 ;

	else return 0 ;
}

int dumpLines(void)
{
	int i ;



	time_t now = time(NULL) ;
	struct tm *stm = localtime(&now) ;

	errno = 0 ;
	FILE *o = fopen(DATA_1,"w") ;
	if(o==NULL) {
		perror(DATA_1) ;
		return -1 ;
	}

	fprintf(o,"%s\n",data_preamble) ;

	fprintf(o,"<table width=100%% border=1 cellspacing=0>\n") ;
	fprintf(o,"<tr %s><th>%s</th><th>%s</th><th>%s</th><th>%s</th><th>%s</th><th>%s</th><th>%s</th></tr>\n",
	       "",
	       "Time","#","Node","Severity","Task","Source#line","Msg") ;




	int sec = stm->tm_yday*24*3600 + stm->tm_hour*3600 + stm->tm_min*60 + stm->tm_sec ;


	qsort(loglines,LOGLINES,sizeof(loglines[0]),compare) ;


	for(i=0;i<LOGLINES;i++) {
		if(loglines[i].sec == 0) continue ;

		int delta = sec - loglines[i].sec ;

		char *color ;

		if(delta > 120) {	// older than 20 minutes...
			if(strncmp(loglines[i].sev,"CRIT",4)==0) color = "style=color:#FF3333" ;
			else if(strncmp(loglines[i].sev,"CAUT",4)==0) color = "style=color:#FF33FF" ;
			else if(strncmp(loglines[i].sev,"OPER",4)==0) color = "style=color:#3333FF" ;
			else color = "style=color:#333333" ;

		}
		else {			
			if(strncmp(loglines[i].sev,"CRIT",4)==0) color = "style=color:#FF0000;font-weight:bold" ;
			else if(strncmp(loglines[i].sev,"CAUT",4)==0) color = "style=color:#FF00FF;font-weight:bold" ;
			else if(strncmp(loglines[i].sev,"OPER",4)==0) color = "style=color:#0000FF;font-weight:bold" ;
			else color = "style=color:#000000;font-weight:bold" ;
		}


		fprintf(o,"<tr %s><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s%s</td><td>%s#%d</td><td>%s</td></tr>\n",
		       color,
		       loglines[i].tm,loglines[i].repeat,
		       loglines[i].node,loglines[i].sev,loglines[i].task,loglines[i].rb,loglines[i].src,loglines[i].line,loglines[i].msg) ;
		
//		printf("%7d [%s] (%d) repeat %d:%s:%s:%s%s:%s:%d:%s\n",sec-loglines[i].sec,loglines[i].tm,loglines[i].day,loglines[i].repeat,
//		       loglines[i].node,loglines[i].sev,loglines[i].rb,loglines[i].task,loglines[i].src,loglines[i].line,loglines[i].msg) ;
	}

	fprintf(o,"</table>\n") ;

	fprintf(o,"\n</body></html>\n") ;
	fclose(o) ;

	rename(DATA_1,DATA_2) ;

	// touch(TOUCH) ;
	now = time(NULL) ;
	struct utimbuf utim ;
	utim.actime = now ;
	utim.modtime = now ;

	utime(DATA_2,&utim) ;
	utime(TOUCH,&utim) ;
//	utime(DAQ_TOUCH,&utim) ;

	return 0 ;
}

