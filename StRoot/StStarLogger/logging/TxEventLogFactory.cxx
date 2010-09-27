#include <sstream>
#include <iostream>
#include <fstream>
#include <regex.h>
#include <cstring>

#include "TxEventLogFactory.h"
#include "TxEventLogWeb.h"
#include "TxEventLogCollector.h"

using namespace TxLogging;

//_________________________________________________________________
TxEventLog* TxEventLogFactory::create(const char *technology)
{
   TxEventLog *log = 0;
   if (technology && (*technology=='w' || *technology=='W' ))
      log= new TxEventLogWeb;
   else if (technology && (*technology=='c' || *technology=='C'|| *technology=='U'  || *technology=='u' ))
      log= new TxEventLogCollector;
   else
      log=new TxEventLogFile;
   return  log;
}

using namespace std;
namespace {
/**
 *
 * @author Valeri Fine (from  lbhajdu's Java version)
 */

    /**
     * valid input formats can be:
     *
     *      -key [value] -value [value] //Note: as of yet this one can not have spaces
     *      [value]=[value]
     *      [value] = [value]
     *      [value] =[value]
     *      [value]= [value]
     *      [value]     =[value]
     *      [value]=     [value]
     *
     * @param args the command line arguments
     */
     
 /**
  * Call UCM logger via JNI and pass logging data
  *
  * @param key the key string
  * @param value the value string for the key
  */
 //________________________________________________________________________
static void log(const char *key, const char *value)
{
  using namespace TxLogging;
  printf("\" %s \"=\"%s\"", key, value);

  const char * ucmCollector = "ucm";
  TxEventLog *eventLog = TxEventLogFactory::create(ucmCollector);

  //Find magic keys
  if(! strcmp(key,"JobSubmitState")){

      if(!strcmp(value, "STAGEIN")){
          eventLog->setJobSubmitState(TxEventLog::STAGEIN);
      }else if(!strcmp(value,"ACTIVE")){
          eventLog->setJobSubmitState(TxEventLog::ACTIVE);
      }else if(!strcmp(value, "DONE")){
          eventLog->setJobSubmitState(TxEventLog::DONE);
       }

   }else{
     //If it's not a magic work, just add it as a key value pair
        eventLog->logStart(key, value);
   }
}
//________________________________________________________________________
int  Main(int argc, const char *argv[])
{
   string arg;
   while (--argc) arg += *(++argv);
   regex_t rgex1;
   regex_t rgex2;
   char errtext[512]={0};
   int errcode = 0;

   const char *regX1 = "^ *([^ ]+) *= *([^ ]+.*[^ ]*) *$";
   const char *regX2 = "^ *-key *([^ ]+) *-value *([^ ]+.*[^ ]*) *$";
   regcomp(&rgex1,regX1,REG_ICASE | REG_EXTENDED);
   regcomp(&rgex2,regX2,REG_ICASE | REG_EXTENDED );
   regmatch_t matchptr [3]; 
   size_t nmatch = sizeof(matchptr)/sizeof(regmatch_t);
   if (! (errcode = regexec(&rgex1,arg.c_str(),nmatch,matchptr,0) ) )  {
      string key   = arg.substr(matchptr[1].rm_so, matchptr[1].rm_eo-matchptr[1].rm_so );
      string value = arg.substr(matchptr[2].rm_so, matchptr[2].rm_eo-matchptr[2].rm_so ); 
      log(key.c_str(), value.c_str());
    } else if (! regexec(&rgex2,arg.c_str(),nmatch,matchptr,0) ) {
      string key   = arg.substr(matchptr[1].rm_so, matchptr[1].rm_eo-matchptr[1].rm_so );
      string value = arg.substr(matchptr[2].rm_so, matchptr[2].rm_eo-matchptr[2].rm_so ); 
      log(key.c_str(), value.c_str());
    } else{
       const char *usage = " Input format error\n"
                       "\n"
                       "Usage:\n"
                       "\n"
                       "      ulog -key [key] -value [value]\n"
                       "or\n"
                       "      ulog [key]=[value]\n"
                       "\n";
            printf("\n%s: %s\n",arg.c_str(),usage);
            return 1;
    }
    regfree(&rgex1);
    regfree(&rgex2);
    return 0;
 }
}

//_________________________________________________________________
int TxEventLogFactory::main(int argc, const char *argv[])
{
    return Main(argc, argv);
}
