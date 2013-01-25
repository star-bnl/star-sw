#ifndef __EEMC_DB_H_
#define __EEMC_DB_H_

#include "eemcConstDB.hh" 

#include "eemcDbPMTconf.hh"
#include "eemcDbADCconf.hh"
#include "eemcDbBoxconf.hh"

#include "eemcDbPMTcal.hh"  // will retire next time, JB
#include "eemcDbPIXcal.hh"
#include "eemcDbPMTname.hh" // will retire next time, JB
#include "eemcDbPIXname.hh"
#include "eemcDbPMTped.hh"
#include "eemcDbPMTstat.hh"


#include "eemcDbPMTchar.hh" 
#include "eemcDbCWchar.hh"

#include "eemcDbHVsys.hh"
#include "eemcDbHVtemp.hh"

#include "eemcDbXMLdata.hh"

//#include "kretDbRingS.hh"
//#include "kretDbWCM.hh"

//what a piece of scheisse .......
static const time_t EEmcDbMaxUnixTime   = 1988150400; // Jan 1,2033 00:00:00 UTC

static const int    EEmcDbMaxDbPathLen  = 1024;
static const int    EEmcDbMaxKeyLength  =  256;
static const char   EEmcDbKeyFormat[]   = "#%s/%s"; //dbNode->printName()/tableNodeName


enum EEmcDbAction { GetDB , SetDB , PrintTree , PrintHistory, PrintConfig };


#ifdef DEBUG
#define dprintf(str...) if(!quietMode) fprintf(stderr,str)
#else
#define dprintf(str...) ; /* nothing */
#endif

//
time_t getTimeStamp(const char  *dbTimeFormat[] , const char *timestr);
char  *fmtSqlTime(const char* sqltime);

//extern int quietMode;
extern int    verboseMode;
extern int    quietMode  ;
#endif







