/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

#include <cstring>
#include <string>
#include <stdio.h>
#include "TxEventLogFactory.h"
#include "TxEventLog.h"
#include <regex.h>

using namespace std;

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

   const char *regX1 = "^ *([^ ]+) *= *([^ ]+.*[^ ]*) *$";
   const char *regX2 = "^ *-key *([^ ]+) *-value *([^ ]+.*[^ ]*) *$";
   regcomp(&rgex1,regX1,REG_ICASE);
   regcomp(&rgex2,regX2,REG_ICASE);
   regmatch_t matchptr [3]; 
   size_t nmatch = sizeof(matchptr)/sizeof(regmatch_t);

   if (! regexec(&rgex1,arg.c_str(),nmatch,matchptr,0) )  {
      string key   = arg.substr(matchptr[1].rm_so, matchptr[1].rm_eo-matchptr[1].rm_so+1 );
      string value = arg.substr(matchptr[2].rm_so, matchptr[2].rm_eo-matchptr[2].rm_so+1 ); 
      log(key.c_str(), value.c_str());
    }  else if (! regexec(&rgex2,arg.c_str(),nmatch,matchptr,0) ) {
      string key   = arg.substr(matchptr[1].rm_so, matchptr[1].rm_eo-matchptr[1].rm_so+1 );
      string value = arg.substr(matchptr[2].rm_so, matchptr[2].rm_eo-matchptr[2].rm_so+1 ); 
      log(key.c_str(), value.c_str());
    } else{
       const char *usage = "\nInput format error\n"
                       "\n"
                       "Usage:\n"
                       "\n"
                       "      java -jar logger -key [key] -value [value]\n"
                       "or\n"
                       "      java -jar logger [key]=[value]\n"
                       "\n";
            printf("%s\n",usage);
            return 1;
    }
    regfree(&rgex1);
    regfree(&rgex2);
    return 0;
 }
