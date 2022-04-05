//
//   $Id: logBrowser.C,v 1.2 2008/02/27 19:12:21 fine Exp $
//
//  Based on critServer.C written by Tonko Ljubicic
//  Curses functionality and command line args added by M.J. LeVine 01/2003
//  Age limits on displayed log records added M.J. LeVine 01/2003
//
//  14-Jan-2003 MJL Added time calculation per file, corrected for midnight effect
//  14-Jan-2003 MJL Unlabelled records now have the highest priority
//  14-Jan-2003 MJL If no absolute path specified, make it /RTS/log/
//  14-Jan-2003 MJL Fixed bug where all files were scanned even if one was specified
//  14-Jan-2003 MJL Added trigger string (-s)
//  14-Jan-2003 MJL Added termination string (-f)
//  14-Jan-2003 MJL Added ignore string (-v)
//  14-Jan-2003 MJL Added multiple required strings (-g)
//  15-Jan-2003 MJL Added absolute time option for -t
//  15-Jan-2003 MJL Added -e scan from EOF
//  15-Jan-2003 MJL Added -m monochrome version (no escape sequences)
//  16-Jan-2003 MJL Moved parse command line to separate fcn for readability
//  17-Jan-2003 MJL Created adjust_time() to move code out of main() for readability
//  17-Jan-2003 MJL Made find_starting_point() to perform binary search for first record
//  21-Jan-2003 MJL changed printing of scanned file names for clarity
//  21-Jan-2003 MJL fix many logical errors in checking for start string
//  21-Jan-2003 MJL fix errors in adjusting times for the date change
//  22-Jan-2003 MJL remove all absolute time, just use hours and minutes
//  22-Jan-2003 MJL assume file starts at 08:00

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <curses.h>

#define MAX_LOGFILES 128
#define MAX_GREPSTRINGS 10

//#define DEBUG

static char buff[1024] ;
static struct stat statb ;
static time_t mytime, earliest;
static int time_requested = 0;
static int triggered, last_file=0;
static int got_finished = 0;
static int scan_from_end = 0, monochrome = 0;
static char start[128] = "", finish[128]="", suppress[128]="";


static struct tm *filemodtime, *wtime;
static char *timestr;
static int level=0, mins, first_buff;
static int  ngrep ;
static int in_hour, in_min, in_sec;
static int req_hour, req_min, start_time;;
static char level_str[] = "dummy string";

static char logfiles[MAX_LOGFILES][128] = {
	"/RTS/log/rts.log",
	"/RTS/log/trigger.log",
	"/RTS/log/evp.log",
	"/RTS/log/det.log",
} ;

static char in_buff[MAX_LOGFILES][1024] ;
static int  in_buff_full[MAX_LOGFILES];

static int daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

static char grep_string[MAX_GREPSTRINGS][128] = {
  "","","","","","",
} ;


static FILE *files[MAX_LOGFILES] ;
static int rectime[MAX_LOGFILES] ;
static int found_start[MAX_LOGFILES];
static int oldsizes[MAX_LOGFILES] ;

// we define the colors here
// put near top of file for visibility
//============================================================================
  // palette:
#define COL_NONE    30
#define COL_BLACK   40 
#define COL_RED     31
#define COL_GREEN   32
#define COL_YELLOW  33 
#define COL_BLUE    34
#define COL_MAGENTA 35 
#define COL_CYAN    36 
#define COL_WHITE   37
#define BOLD         1
#define NORMAL       0
//============================================================================
// used for DEBUG
#define PRINT0 printf("\033[%d;%d;%dm%s\033[0m",NORMAL,COL_NONE,COL_NONE,in_buff[first_buff])
// used for  NOTICE
#define PRINT1 printf("\033[%d;%d;%dm%s\033[0m",NORMAL,COL_NONE,COL_NONE,in_buff[first_buff])
// used for WARNING
#define PRINT2 printf("\033[%d;%d;%dm%s\033[0m",NORMAL,COL_NONE,COL_CYAN,in_buff[first_buff])
// used for ERROR
#define PRINT3 printf("\033[%d;%d;%dm%s\033[0m",BOLD,COL_NONE,COL_MAGENTA,in_buff[first_buff])
// used for OPERATOR
#define PRINT4 printf("\033[%d;%d;%dm%s\033[0m",BOLD,COL_YELLOW,COL_MAGENTA,in_buff[first_buff])
// used for CRITICAL & TERR
#define PRINT5 printf("\033[%d;%d;%dm%s\033[0m",BOLD,COL_BLACK,COL_RED,in_buff[first_buff])
//============================================================================

void printusage();
void parse_arguments(char **argv, int argc);
void find_starting_point(int i, int start_time);

int main(int argc, char *argv[])
{
  char *fret ;
  int ret;
  
  int i,j;
  int data_in ;
  off_t bytes_left[MAX_LOGFILES];
  int tries = 0;

//   for (i=0; i<argc; i++) {
//     printf("argv[%d]  :%s:\n",i,argv[i]);
//   }

  parse_arguments(argv, argc);

  i = 0;

  printf("files to be scanned: \n");

  while(logfiles[i][0] != 0) {
		
    files[i] = fopen(logfiles[i],"r") ; //get a FILE

    if(files[i] == NULL) {
      perror(logfiles[i]) ;
      exit(1);
    }

    printf("\t%s\n",logfiles[i++]);
  }
  last_file = i;

  for (i=0; i<last_file; i++) {
    found_start[i]=0;
    if (scan_from_end) fseek(files[i], 0, SEEK_END);
    else if (time_requested != 0) {
      start_time = 3600*((req_hour+16)%24) + 60*req_min;
      find_starting_point(i,start_time);
    }
    // will move pointer to appropriate place for search

    ret = stat(logfiles[i],&statb) ;

    bytes_left[i] = (statb.st_size - ftello(files[i]));
    in_buff_full[i] = 0;
  } 

  //  printf("number of files: %d\n",last_file);
  sleep(3);

  if (!monochrome) {
    initscr();
		
    if(has_colors() == FALSE) {
      endwin();
      printf("This terminal does not support color: \n\
use /usr/dt/bin/dtterm -background white\n");
      exit(1);
    }
    endwin();
  }

  for(;;) {

    off_t new_bytes_left, rate;
    int inlevel, ib, still_looking;
    int eof_found;

    data_in = 0 ;

    for (i=0; i<last_file;i++) {

      if (in_buff_full[i]) continue;
      
      still_looking = 1;
      eof_found = 0;

      do {
	errno = 0 ;
	fret = fgets(in_buff[i],sizeof(in_buff[i]),files[i]) ;
	if(fret == NULL) {
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
	  // has the file we are scanning been replaced by a newer version?      
	  if(statb.st_size < oldsizes[i]) {
	    fclose(files[i]) ;
	    files[i] = fopen(logfiles[i],"r") ; // reopen
	    oldsizes[i] = 0 ;
	    continue ;
	  }
	  if ((eof_found = feof(files[i]))==1) clearerr(files[i]);
	  oldsizes[i] = statb.st_size ;
	  tries++;
	  if ((tries%10000) == 0) {
	    fflush(stdout);
	    printf("\nwaiting for data..%s \r",logfiles[i]);
	    fflush(stdout);
	  }
	  continue ;	// no data...
	}

	data_in++ ;	// we have something
	sscanf(in_buff[i],"%*s %d:%d:%d",&in_hour,&in_min,&in_sec);
	if ((data_in%2000) == 0) {
	  fflush(stdout);
	  printf("\nscanning records:  %s   %02d:%02d \t\t\t\r",
		 logfiles[i],in_hour,in_min);
	  fflush(stdout);
	}
	ret =  stat(logfiles[i],&statb) ;

	// Do we have to skip some records to catch up?
	new_bytes_left = (statb.st_size - ftello(files[i]));
	rate = new_bytes_left - bytes_left[i];
	if (rate >2000) { // are we losing ground? Skip some records
	  printf("skipping records to catch up\n");
	  fseeko(files[i], new_bytes_left/2 , SEEK_CUR);
	  //	  printf("skipping ahead to try to catch up\n");
	  new_bytes_left = (statb.st_size - ftello(files[i]));
	}
	bytes_left[i] = new_bytes_left; //update bytes_left


	rectime[i] = 3600*((in_hour +16)%24) + in_min*60 +in_sec;

	// compare absolute time
	
	if (time_requested && !found_start[i])
	  if (rectime[i] < start_time) {
// 	    printf(" record: %02d:%02d  rectime %d, start_time %d\n",
// 		   in_hour, in_min, rectime[i], start_time);
	    continue; 
	  }
	  else {
	    found_start[i] = 1;
	    //	    printf(" found start for %s\n",logfiles[i]);
	  }

	//check for trigger string, if one is defined
	if (!triggered) {
	  if (strstr(in_buff[i],start) != NULL) triggered = 1;
	  else {
	    in_buff_full[i] = 0; //throw this one away
	    continue;
	  }
	}
	//	printf(" just after triggered comparison\n");
	// check for finish string, set flag
	if (finish[0]) {
	  //	  printf(" here I am in finish compare loop %d\n",i);
	  if (strstr(in_buff[i],finish) != NULL) got_finished = 1;
	}
	// check for suppression string
	else if (suppress[0]) {
	  //	  printf(" here I am in suppress compare loop\n");
	  if (strstr(in_buff[i],suppress) != NULL) {
	    in_buff_full[i] = 0; //throw this one away
	    continue;
	  }
	}
	// check for grep string(s)
	if (ngrep) {
	  int g=0, compare=1;
	  //	  printf(" here I am in grep compare loop\n");
	  while (grep_string[g][0]) {
	    if (strstr(in_buff[i],grep_string[g]) == NULL) compare=0;
	    g++;
	  }
	  if ((!compare)  && (!got_finished)) {
	    in_buff_full[i] = 0; //throw this one away
	    continue;
	  }
	}

	if     (strstr(in_buff[i],": DEBUG:"   ) != NULL) inlevel=0;
	else if(strstr(in_buff[i],": NOTICE:"  ) != NULL) inlevel = 1;
	else if(strstr(in_buff[i],": WARNING:" ) != NULL) inlevel = 2;
	else if(strstr(in_buff[i],": ERROR:"   ) != NULL) inlevel = 3;
	else if(strstr(in_buff[i],": OPERATOR:") != NULL) inlevel = 4;
	else if(strstr(in_buff[i],": TERR:"    ) != NULL) inlevel = 5;
	else if(strstr(in_buff[i],": CRITICAL:") != NULL) inlevel = 5;
	else inlevel = 5; // unlabeled messages get highest priority

	if (inlevel<level)  {
	    in_buff_full[i] = 0; //throw this one away
	    continue;
	  }

	still_looking = 0;
	in_buff_full[i] = 1; // something useful in in_buff[i]

      } while ((!eof_found) && still_looking );

      //      printf(" terminated do ..while() loop for %s\n",logfiles[i]);

    } // loop over input files


    first_buff = -1;
    earliest = 100000; // impossibly in the future
    for (ib=0; ib<last_file; ib++) {
      if (in_buff_full[ib]) { 
	if  (earliest > rectime[ib]) {
	  earliest = rectime[ib]; 
	  first_buff = ib;
	}
      }
    }

    if (first_buff < 0) continue;

    if (monochrome) {
#ifdef DEBUG
      printf("%d: %s",first_buff,in_buff[first_buff]);
#else
      printf("%s",in_buff[first_buff]);
#endif
      fflush(stdout);
    }
    else {
      switch (inlevel) {
      case 0:
	PRINT0;
	break;
      case 1:
	PRINT1;
	break;
      case 2:
	PRINT2;
	break;
      case 3:
	PRINT3;
	break;
      case 4:
	PRINT4;
	break;
      case 5:
	PRINT5;
	break;
      default:
	break;
      }
	fflush(stdout);
    }

    in_buff_full[first_buff] = 0; // can re-use buffer


    // check for termination string
    
    if (got_finished)  {
      printf("finish string encountered...exiting\n");
      exit(0);
    }
    if(!data_in)   sleep(1) ;
    
  }	// for (;;)
  
  return -1 ;	// UNREACHABLE
}


void parse_arguments(char **argv, int argc)
{
  int i,j;
  time_t abstime;
  struct tm *wtime;

  for (i=2; i<=argc; i++) {
    if (strncmp(argv[i-1],"-h",2)==0) {
      printusage();
    }
  }
  mins = 5;
  ngrep = 0;
  time(&mytime);   // get wall time
  
  for (j=0,i=2; i<=argc; i++) {
    if (strncasecmp(argv[i-1],"-t",2)==0) {
      if (argc == i) printusage();
      if (strstr(argv[i],":") == NULL) {//no ':' in string
	sscanf(argv[i],"%d",&mins);
	abstime = mytime - 60*mins;
	wtime = localtime(&abstime);
	req_hour = wtime->tm_hour;
	req_min = wtime->tm_min;
      }
      else { // absolute time
	sscanf(argv[i],"%d:%d",&req_hour,&req_min);
      }
      time_requested = 1;
      printf("display records starting from %02d:%02d\n", req_hour, req_min);
      i++;
    }
    else if (strncasecmp(argv[i-1],"-n",2)==0) {
      if (argc == i) printusage();
      strcpy(level_str,argv[i++]);
      if (strncasecmp(level_str,"DEB",3)==0) level = 0;
      else if (strncasecmp(level_str,"NOT",3)==0) level = 1;
      else if (strncasecmp(level_str,"WARN",4)==0) level = 2;
      else if (strncasecmp(level_str,"ERR",3)==0) level = 3;
      else if (strncasecmp(level_str,"OPER",4)==0) level = 4;
      else if (strncasecmp(level_str,"CRIT",4)==0) level = 5;
      else if (strncasecmp(level_str,"TERR",4)==0) level = 5;
      else printusage();
    }
    else if (strncasecmp(argv[i-1],"-e",2)==0) {
      // start scanning at current EOF
      scan_from_end = 1;
      printf("scanning from current end(s) of file\n");
    }
    else if (strncasecmp(argv[i-1],"-m",2)==0) {
      // monochrome version (no control strings
      monochrome = 1;
      printf("monochrome version - no escape sequences\n");
    }
    else if (strncasecmp(argv[i-1],"-s",2)==0) {
      if (argc == i) printusage();
      strcat(start,argv[i++]);
    }
    else if (strncasecmp(argv[i-1],"-f",2)==0) {
      if (argc == i) printusage();
      strcat(finish,argv[i++]);
    }
    else if (strncasecmp(argv[i-1],"-g",2)==0) {
      if (argc == i) printusage();
      if (ngrep==MAX_GREPSTRINGS) printusage();
      strcat(grep_string[ngrep++],argv[i++]);
      grep_string[ngrep][0] = 0;
    }
    else if (strncasecmp(argv[i-1],"-v",2)==0) {
      if (argc == i) printusage();
      strcat(suppress,argv[i++]);
    }
    else {
      strcpy(logfiles[j],argv[i-1]);
      if (strstr(logfiles[j],"/")==NULL) {
	//if absolute path not specified, prepend "/RTS/log/"
	char temp[128];
	strcpy(temp, logfiles[j]);
	strcpy(logfiles[j],"/RTS/log/");
	strcat(logfiles[j],temp);
      }
      j++;
    }
  }

  if (j>0) logfiles[j][0] = 0;
  triggered = (start[0]) ? 0:1; //start string => triggered = 0

  printf("level=%d\n",level);
  if (start[0]) printf("start \"%s\"\n",start);
  if (finish[0]) printf("finish \"%s\"\n",finish);
  j=0;
  if (ngrep) {
    printf("strings required:\n");
    while (grep_string[j][0]) printf("\t\"%s\"\n",grep_string[j++]);
  }
  if (suppress[0]) printf("suppress \"%s\"\n",suppress);

}

// finds appropriate offset into files[i] to quickly find a timestamped
// record close to (earlier than) the time specified
void find_starting_point(int i, int start_time)
{
  int ret, niter=0;
  char * fret;
  off_t fsize, fpos, lastpos, bytes_rem;
  int ltime;
  int req_hour, req_min;

  req_hour = (start_time/3600+8)%24;
  req_min = (start_time%3600)/60;
  printf("find_starting_point(): file %s: start time requested %02d:%02d\n",
     	 logfiles[i], req_hour, req_min);

  lastpos = fpos = ftello(files[i]);

  ret =  stat(logfiles[i],&statb) ;
  fsize = statb.st_size;
  bytes_rem = fsize - fpos;

  while (bytes_rem > 10000) {
    fret = fgets(in_buff[i],sizeof(in_buff[i]),files[i]) ;
    if(fret == NULL) {
      if(errno) {
	perror(logfiles[i]) ;
	return;
      }
      fseeko(files[i], 0, SEEK_SET); // reset pointer to BOF
      break ;	// no data...
    }
    // reading twice is required after an arbitrary jump, to make sure
    // that we get an entire record, on the 2nd try
    fret = fgets(in_buff[i],sizeof(in_buff[i]),files[i]) ;
    if(fret == NULL) {
      if(errno) {
	perror(logfiles[i]) ;
	return;
      }
      fseeko(files[i], 0, SEEK_SET); // reset pointer to BOF
      break ;	// no data...
    }

    sscanf(in_buff[i],"%*s %d:%d:%d",&in_hour,&in_min,&in_sec);

//           printf(" find_starting_point(): %d iters record time:  %02d:%02d\n",
//      	     niter, in_hour, in_min);
    ltime = in_sec + 60*in_min + 3600*((in_hour+16)%24);

//     printf("find_starting_point(): ltime %d\t\tstart_time %d\n",
// 	   ltime, start_time);
    if (ltime < start_time) { // must jump forward
      lastpos = ftello(files[i]);// keep track of last posn before forward step:
      if (fsize -  lastpos <= bytes_rem/2 ) break; // too close to EOF
      fseeko(files[i], bytes_rem/2, SEEK_CUR); 
      bytes_rem = bytes_rem/2;
    }
    else if (ltime > start_time) { // must step backward
      if (ftello(files[i]) < bytes_rem/2 ) { // safe to step back?
	fseeko(files[i], 0, SEEK_SET); // no, go to BOF and return
	break;
      }
      fseeko(files[i], -bytes_rem/2, SEEK_CUR); // yes
      bytes_rem = bytes_rem/2;
    }
    niter++;
  }
  // if search terminates with file pointer set following the
  // desired timestamp, set pointer to last position before a
  // forward step was made
  if (ltime >= start_time) fseeko(files[i],lastpos,SEEK_SET);

  printf(" starting %s at time %02d:%02d after %d iterns\n",
	 logfiles[i], in_hour, in_min, niter);

  return;
}
		
void printusage() {
  printf("usage: logBrowser [-n ALARM_LEVEL] [-t time] [-s \"start string\"] 
\t[-f \"stop string\"] [-g \"grep string\"] [-v \"suppress string\"] 
\t[file1 [,file2,....]]

ALARM_LEVEL={DEBug,NOTice,WARNing,ERRor,OPERator,CRITical,TERR}
specifies the lowest level to be printed. Values of ALARM_LEVEL 
are not case sensitive. Values may be abbreviated as indicated 
in upper case. If no alarm level specified all messages will be 
displayed.

Log files specified by  filename[s] will be displayed. If none are 
specified, all log files will be shown. Default path is /RTS/log/.

    -t\t max age (minutes) of message to be displayed.
    -t\t hour:min absolute time to start displaying messages.
\tDefault value = all messages (beginning of file(s)).
\tArguments containing ':' will be treated as absolute time 
\tusing 24 hour clock. logBrowser assumes that a file starts at 08:00  
\tand lasts no more than 24 hours. If a start string [-g] is specified,
\tthe default time is beginning of file.

    -s\tstart string:  no lines will be shown until this string
\tencountered. Quotes are required if whitespace is part of string.

    -f\tstop string: program will print this string, then terminate.
\tQuotes are required if whitespace is part of string.

    -g\tgrep string: only lines containing (all of) these strings will 
\tbe displayed. Multiple [up to %d] grep strings may be specified. 
\tQuotes are required if whitespace is part of string. To specify 
\tadd'l strings use 

\t\t-g string1 -g string2 -g \"string 3\".

    -v\tsuppress string: lines containing this string will not 
\tbe displayed. Quotes are required if whitespace is part of string.

    -e\tstart scanning at the end of file(s).

    -m\tmonochrome version (no escape strings sent to terminal).


HINT: In order to get the records interleaved in correct time order from 
more than one file, it is necessary to wait for a reocrd from each file 
which meets all of the qualifications (-g, for example). If this is not what 
you want, then open a separate logBrowser window for each file.\n\n",MAX_GREPSTRINGS);

  exit(0);
}
