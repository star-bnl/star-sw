// $Id: SpinDbFunctions.C,v 1.1 2005/09/30 23:47:48 balewski Exp $
// Description: SPIN DB functions
// Author: Jan Balewski
// 

 
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <getopt.h>

#ifndef __USE_XOPEN
#define __USE_XOPEN
#endif
#include <time.h> 

#include "spinDbAPI.h"



//extern "C" char *strptime (const char *s, const char *fmt, struct tm *tp);
int    verboseMode = false;
int    quietMode   = false;


// ===============================================================
// time stamp function
// ===============================================================
time_t 
getTimeStamp(const char  *timeformat[] , const char *timestr) 
{
  time_t    ts=0;
  struct tm tm;

  ts = time(NULL);
  localtime(&ts);

  if(strncmp(timestr,"now",3)) { 
    memset(&tm,0x0,sizeof(struct tm));  
    for(int i=0; timeformat[i]!=NULL ; i++ ) {
      //dprintf("trying %s\n",timeformat[i]);
      if(strptime(timestr,timeformat[i], &tm)!=NULL) { 
	//dprintf("using date/time format: %s\n",timeformat[i]);
	break;
      }
    }
    ts = mktime(&tm);
  }
  if(ts<0) {
    fprintf(stderr,"time stamp %s not understood\n",timestr);
    fprintf(stderr,"\tvalid formats are:\n");
    for(int i=0; timeformat[i]!=NULL ; i++ ) 
      fprintf(stderr,"\t%s\n",timeformat[i]);
    return(-1);
  }

  // now print the "decoded" times
  char *tstr=NULL,*nline=NULL;

  dprintf("TIME STAMP: ");
  tstr=asctime(localtime(&ts));  
  nline=strrchr(tstr,'\n');
  if(nline!=NULL) *nline=0x00; // get rid of '\n' returned by asctime
  dprintf("%s (%s)\t",tstr,tzname[daylight]);

  tstr=asctime(gmtime(&ts));    
  nline=strrchr(tstr,'\n');
  if(nline!=NULL) *nline=0x00; // get rid of '\n' returned by asctime
  dprintf("%s (GMT)\n",tstr);
  return ts;
}


// ===============================================================
// CONVERT TIME FROM <-> SQL
// ===============================================================
static void
timeStringConv1(const char *a, char *b) {
      memcpy(b+0,a+0,4); // yyyy
      b[4]='-';
      memcpy(b+5,a+4,2); // mm
      b[7]='-';
      memcpy(b+8,a+6,2); // dd
      b[10]=' ';      
      memcpy(b+11,a+8,2); // hh
      b[13]=':';
      memcpy(b+14,a+10,2); // mm
      b[16]=':';
      memcpy(b+17,a+12,2); // ss
      b[19]=0;
      //fprintf(out,"=%s=%s=\n",a,b);
}


char *
fmtSqlTime(const char* sqltime) {
  static char stringTime[1024];
  timeStringConv1(sqltime,stringTime);
  return stringTime;
}




// ===============================================================
// format function
// ===============================================================
static inline char* 
fmt(const char *s, const int len) {
  static char fmtstr[SpinDbMaxDbPathLen];
  sprintf(fmtstr,"%s %%%d[A-z0-9 \\t]\n",s,len-1);
  return fmtstr;
}



// $Log: SpinDbFunctions.C,v $
// Revision 1.1  2005/09/30 23:47:48  balewski
// start
//

