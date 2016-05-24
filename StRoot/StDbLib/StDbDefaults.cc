#include "StDbDefaults.hh"
#include "stdb_streams.h"
#include <dirent.h>
#include <stdlib.h>
using namespace std;
StDbDefaults* StDbDefaults::mInstance=0;

StDbDefaults::StDbDefaults(){

 strncpy(mversion,"default",sizeof(mversion));
 strncpy(mflavor,"ofl",sizeof(mflavor));
 mprodTime = 0;
 mendTime =2145873600; // Dec 31, 2037
 strncpy(mdbServerVar,"STDB_SERVERS",sizeof(mdbServerVar));
 strncpy(mdbServerFile,"dbServers.xml",sizeof(mdbServerFile));
                
}

////////////////////////////////////////////////////////////////////
char* StDbDefaults::getServerFileName(const dbFindServerMode mode){

  if(mode==userHome) return getFileName("HOME");
  if(mode==serverEnvVar) return getFileName(mdbServerVar);
  if(mode==starDefault) return getFileName("STAR");

  return NULL;
};


////////////////////////////////////////////////////////////////////
char* StDbDefaults::getFileName(const char* fileName){

  char* nullReturn=0;
  if(!fileName) return nullReturn;
 
  StString fn;

  if(strcmp(fileName,"HOME")==0){
    fn << ( getenv("HOME") ? getenv("HOME") : "" ) << "/" << mdbServerFile;
  } else if(strcmp(fileName,"STAR")==0){
    fn << ( getenv("STAR") ? getenv("STAR") : "" ) << "/" << "StDb/servers/" <<mdbServerFile;
  } else {
    char* fname = getenv(fileName);
    if ( fname == NULL || !fname ) { return nullReturn; }
    fn << fname;
    if ( opendir(fname) ) { fn << "/" << mdbServerFile; }
  }

  string fns=fn.str();
  char* retVal= new char[fns.length()+1];
  strcpy(retVal,fns.c_str());

  return retVal;
}






