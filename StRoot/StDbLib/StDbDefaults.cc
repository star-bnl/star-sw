#include "StDbDefaults.hh"
#include <strstream.h>
#include <dirent.h>
#include <stdlib.h>

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
char*
StDbDefaults::getServerFileName(const dbFindServerMode mode){

  char* retVal=0;
  switch (mode) {
  case userHome:
    {
      retVal=getFileName("HOME");
      break;
    }
  case serverEnvVar:
    {
      retVal=getFileName(mdbServerVar);
      break;
    }
  case starDefault:
    {
      retVal=getFileName("STAR");
      break;
    }
  default:
    {
      retVal=getFileName("STAR");
      break;
    }
  }

  return retVal;
};

////////////////////////////////////////////////////////////////////

char*
StDbDefaults::getFileName(const char* fileName){

  char* nullReturn=0;
  if(!fileName) return nullReturn;

 
  ostrstream fn;

  if(strcmp(fileName,"HOME")==0){
    fn<<getenv("HOME")<<"/"<<mdbServerFile<<ends;
  } else if(strcmp(fileName,"STAR")==0){
    fn<<getenv("STAR")<<"/"<<"StDb/servers/"<<mdbServerFile<<ends;
  } else {
    char* fname=getenv(fileName);
    if(!fname)return nullReturn;
    fn<<fname;
    if(opendir(fname))fn<<"/"<<mdbServerFile;  
    fn<<ends;
  }

  char* retVal= new char[strlen(fn.str())+1];
  strcpy(retVal,fn.str());
  fn.freeze(0);

  return retVal;
}
