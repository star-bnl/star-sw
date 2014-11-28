// $Id: eemcDbFunctions.C,v 1.1 2013/01/25 16:46:49 stevens4 Exp $
// Description: EEMC DB functions
// Author: Piotr A. Zolnierczuk 
// Contributions Jan Balewski
// 
// $Log: eemcDbFunctions.C,v $
// Revision 1.1  2013/01/25 16:46:49  stevens4
// Scripts used to upload EEMC tables to the DB
//
// Revision 1.6  2003/09/12 16:38:36  zolnie
// *** empty log message ***
//
// Revision 1.5  2003/08/29 17:07:22  zolnie
// added: flavor and expiration time flag
//
// Revision 1.4  2003/08/07 16:33:23  zolnie
// replaced confusing --noWrite/-w option with a clearer one: --dataonly/-d
//
// Revision 1.3  2003/06/03 06:29:15  zolnie
// fixed time zone problem
//
// Revision 1.2  2003/04/11 18:27:58  balewski
// add -w option to prohibit writing of the '#node/table' string to the output
//
// Revision 1.1  2003/01/28 23:22:18  balewski
// start
//
// Revision 1.24  2003/01/13 18:20:49  zolnie
// modified history time to be local rather than GMT
//
// Revision 1.23  2003/01/10 04:52:05  zolnie
// updates to Tcl/Tk interface (czyli Zadana Pana Jana)
//
// Revision 1.22  2003/01/08 21:58:27  zolnie
// history clean-up
// tk interface updates
//
// Revision 1.21  2003/01/03 16:40:16  zolnie
// added comment field
//
// Revision 1.20  2003/01/02 16:31:06  zolnie
// added comment field to all the c-structs
//
// Revision 1.19  2002/12/03 19:30:46  zolnie
// polished stuff
//
// Revision 1.18  2002/12/03 17:58:44  balewski
// added PMTped , VerC
//
// Revision 1.17  2002/11/14 19:22:27  zolnie
// working version of mkHVDB
//
// Revision 1.16  2002/11/13 20:12:35  zolnie
// towards (semi)(stable?) final solution
//
// Revision 1.15  2002/11/11 22:54:02  zolnie
// fixed a number of bugs in perl scripts (mainly in digestPmtConf.pl)
// patchy version of mkHVDB
// updated *.hh files
//
// Revision 1.14  2002/11/05 17:51:04  zolnie
// "characterization" updates
//
// Revision 1.13  2002/11/01 23:29:34  zolnie
// *** empty log message ***
//
// Revision 1.12  2002/11/01 20:47:39  zolnie
// added: file parameter added
// help updated
//
// Revision 1.11  2002/11/01 19:47:45  zolnie
// fixed bug: logical conditions in read/write_db were reversed
//
// Revision 1.10  2002/11/01 19:23:19  zolnie
// use templates in eemcDbOp
//
// Revision 1.9  2002/11/01 14:49:48  balewski
//
// ADCconf works
//
// Revision 1.8  2002/11/01 14:34:06  zolnie
// shorten EEMCDbMaxName from 32 to 16
//
// Revision 1.7  2002/11/01 14:18:13  balewski
// walka z ADC conf
//
// Revision 1.6  2002/10/31 22:13:25  zolnie
// bug fixed in eemcConstDB.hh (algebra problem)
//
// Revision 1.5  2002/10/31 20:44:31  zolnie
// for Jas
//
// Revision 1.4  2002/10/31 15:58:28  zolnie
// structure field naming bug fixed in eemcDbPMTcal
//
// Revision 1.3  2002/10/30 22:48:33  zolnie
// new dbase manipulation updates
//
// Revision 1.2  2002/10/30 15:39:44  zolnie
// updated eemcDb and Readme files
//
// Revision 1.1  2002/10/29 23:00:50  zolnie
// chyba dziala (ze sciezkami)
//


 
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <getopt.h>

#ifndef __USE_XOPEN
#define __USE_XOPEN
#endif
#include <time.h> 

#include "eemcDb.h"



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
  static char fmtstr[EEmcDbMaxDbPathLen];
  sprintf(fmtstr,"%s %%%d[A-z0-9 \\t]\n",s,len-1);
  return fmtstr;
}




