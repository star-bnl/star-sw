#include "StDbDefaults.hh"
#include <Stsstream.h>
#include <Stiostream.h>
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

  const char* tmpString = fn.str();
  char* retVal= new char[strlen(tmpString)+1];
  strcpy(retVal,tmpString);
  fn.freeze(0);

  return retVal;
}



