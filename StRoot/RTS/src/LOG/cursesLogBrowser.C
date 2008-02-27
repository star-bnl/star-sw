//
//   $Id: cursesLogBrowser.C,v 1.2 2008/02/27 19:12:21 fine Exp $
//
//  Based on critServer.C written by Tonko Ljubicic
//  Curses functionality and command line args added by M.J. LeVine 01/2003
//

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <curses.h>

#define MAX_LOGFILES 128


//#define DEBUG
static char buff[1024] ;
static struct stat statb ;

static char level_str[] = "dummy string";

static char logfiles[MAX_LOGFILES][128] = {
	"/RTS/log/rts.log",
	"/RTS/log/trigger.log",
	"/RTS/log/evp.log",
	"/RTS/log/det.log",
} ;

static FILE *files[MAX_LOGFILES] ;

static int oldsizes[MAX_LOGFILES] ;

// we define the colors here
// put near top of file for visibility
void define_colors(){
  // palette:
  // COLOR_BLACK 
  // COLOR_RED 
  // COLOR_GREEN 
  // COLOR_YELLOW 
  // COLOR_BLUE 
  // COLOR_MAGENTA 
  // COLOR_CYAN 
  // COLOR_WHITE

  // init_pair(refno,foreground color, background color)
  init_pair(1,COLOR_BLACK,COLOR_WHITE);      // used for DEBUG & NOTICE
  init_pair(2,COLOR_CYAN,COLOR_WHITE);       // used for WARNING
  init_pair(3,COLOR_MAGENTA,COLOR_WHITE);    // used for ERROR
  init_pair(4,COLOR_MAGENTA,COLOR_YELLOW);    // used for OPERATOR
  init_pair(5,COLOR_RED,COLOR_BLACK);        // used for CRITICAL & TERR

  attron(COLOR_PAIR(1));
  bkgd(' ');
}

int main(int argc, char *argv[])
{
	char *fret ;
	int ret ;
	int i,j, level ;
	int data_in ;

	for (i=2; i<=argc; i++) {
	  if (strncmp(argv[i-1],"-h",2)==0) {
	    initscr();
	    printw("usage: logBrowser [-n ALARM_LEVEL] [file1 [,file2....]]\n\n\
 ALARM_LEVEL={DEBug,NOTice,WARNing,ERRor,OPERator,CRITical,TERR}\n\
 specifies the lowest level to be printed.\n\
 Values of ALARM_LEVEL are not case sensitive.\n\
 Values may be abbreviated as indicated in upper case.\n\
 If no alarm level specified all messages will be displayed.\n\n\
 Log files specified by  filename[s] will be displayed.\n\
 If none are specified, all log files will be shown.\n");
	    getch();  //wait for keyboard input - avoids window disappearing
	    exit(0);
	  }
	}
	for (j=0,i=2; i<=argc; i++) {
	  if (strncasecmp(argv[i-1],"-n",2)==0) {
	    strcpy(level_str,argv[i++]);
	    if (strncasecmp(level_str,"DEB",3)==0) level = 0;
	    else if (strncasecmp(level_str,"NOT",3)==0) level = 1;
	    else if (strncasecmp(level_str,"WARN",4)==0) level = 2;
	    else if (strncasecmp(level_str,"ERR",3)==0) level = 3;
	    else if (strncasecmp(level_str,"OPER",4)==0) level = 4;
	    else if (strncasecmp(level_str,"CRIT",4)==0) level = 5;
	    else if (strncasecmp(level_str,"TERR",4)==0) level = 5;
	    else level = 0;
	  }
	  else {
	    strcpy(logfiles[j++],argv[i-1]);
	  }
	}

#ifdef DEBUG
	if (j>0) logfiles[j][0] = 0;

	printf("level=%d\n",level);
	j = 0;
	while (logfiles[j][0] != 0) {
	  printf("filename[%d]: %s\n",j,logfiles[j++]);
	  } 
	printf("\n");
#endif
//==================finished parsing command line =================

	i = 0;
	while(logfiles[i][0] != 0) {
		
		files[i] = fopen(logfiles[i],"r") ;

		if(files[i] == NULL) {
			perror(logfiles[i]) ;
		}
		i++ ;
	} ;


	initscr();

	if(has_colors() == FALSE) {
	  endwin();
	  printf("This terminal does not support color: \n\
use /usr/dt/bin/dtterm -background white\n");
	  exit(1);
	}

	scrollok(stdscr,TRUE);
	start_color();
	define_colors();
	intrflush(stdscr,TRUE); // flush output after keyboard interrupt
       
	for(;;) {

	  int inlevel;

	  data_in = 0 ;
	  for(i=0;i<MAX_LOGFILES;i++) {

	    if(logfiles[i][0]==0) continue ;
	    
	    errno = 0 ;
	    fret = fgets(buff,sizeof(buff),files[i]) ;
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
	    
	    inlevel = 0;
	    if(strstr(buff,": DEBUG:") != NULL) inlevel=0;
	    else if(strstr(buff,": NOTICE:") != NULL) inlevel = 1;
	    else if(strstr(buff,": WARNING:") != NULL) inlevel = 2;
	    else if(strstr(buff,": ERROR:") != NULL) inlevel = 3;
	    else if(strstr(buff,": OPERATOR:") != NULL) inlevel = 4;
	    else if(strstr(buff,": TERR:") != NULL) inlevel = 5;
	    else if(strstr(buff,": CRITICAL:") != NULL) inlevel = 5;
	    
	    if (inlevel<level) continue;

	    switch (inlevel) {
	    case 0:
	      attron(COLOR_PAIR(1));
	      break;
	    case 1:
	      attron(COLOR_PAIR(1));
	      break;
	    case 2:
	      attron(COLOR_PAIR(2));
	      break;
	    case 3:
	      attron(COLOR_PAIR(3));
	      attron(A_BOLD);
	      break;
	    case 4:
	      attron(COLOR_PAIR(4));
	      attron(A_BOLD);
	      break;
	    case 5:
	      attron(COLOR_PAIR(5));
	      attron(A_BOLD);
	      break;
	    default:
	      break;
	    }
	    
	    printw("%s",buff) ;    // write the line
	    attron(COLOR_PAIR(1)); // back to black on white
	    attroff(A_BOLD);       // turn off the bold
	    refresh();             // paint the screen
	  }

	  if(!data_in) sleep(1) ;
	  
	}	// FOREVER

	endwin();
	return -1 ;	// UNREACHABLE
}

		


