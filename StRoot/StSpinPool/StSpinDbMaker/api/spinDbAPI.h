#ifndef __SPIN_DB_API_H_
#define __SPIN_DB_API_H_

#include "spinConstDB.hh" 

#include "spinDbV124.hh"

//what a piece of scheisse .......
static const time_t SpinDbMaxUnixTime   = 1988150400; // Jan 1,2033 00:00:00 UTC

static const int    SpinDbMaxDbPathLen  = 1024;
static const int    SpinDbMaxKeyLength  =  256;
static const char   SpinDbKeyFormat[]   = "#%s/%s"; //dbNode->printName()/tableNodeName


enum SpinDbAction { GetDB , SetDB , PrintTree , PrintHistory, PrintConfig };

#ifdef DEBUG
#define dprintf(str...) if(!quietMode) fprintf(stderr,str)
#else
#define dprintf(str...) ; /* nothing */
#endif

time_t getTimeStamp(const char  *dbTimeFormat[] , const char *timestr);
char  *fmtSqlTime(const char* sqltime);

extern int    verboseMode;
extern int    quietMode  ;
#endif







