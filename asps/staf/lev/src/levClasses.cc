//:Copyright 1996, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        levClasses.C
//:DESCRIPTION: LEV Classes
//:AUTHOR:      hjw - Herb Ward, ward@physics.utexas.edu
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     01jul96-v000a-hjw- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#define USE_TDM
#define VSIZE 40
#include <stdio.h>
#include <string.h>
#ifndef WIN32
# include <unistd.h>
# include <sys/utsname.h>
#else
#  include <windows.h>
#  include <process.h>
#  include <time.h>
#  include <winsock.h>
typedef int pid_t;
#endif
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>

#include "lev_macros.h"
#include "asuAlloc.h"
#include "levClasses.hh"
#include "tdm_globals.h"
#include "dui_globals.h"
#include "emlLib.h"

//:----------------------------------------------- MACROS             --
#define LEV_ENV_TABLE "config/levEnv"
#define LEV_VER_TABLE "config/levVer"
/* HACK These should be from idl*.idl: */
#define LEV_ENV_SPEC "struct levEnvTab { char name[32],value[128]; };"
#define LEV_VER_SPEC "struct levVerTab { char name[128],\
  type[32],version[256]; };"
#define LEV_ENV_MAX_ROWS 40
#define LEV_VER_MAX_ROWS 350
#define PP printf(
//:----------------------------------------------- PROTOTYPES         --

#if defined(SUN)
extern "C" int gethostname(char *name, int namelen);
#endif

extern CC_P void LogEnvInfo();
void levConvertToDigits(char *xx) {
  char *junk,*month,*day,*hour,*minute,*second,*year,copy[55];
  strncpy(copy,xx,53); 
  junk=strtok(copy," :");
  month=strtok(NULL," :");
  day=strtok(NULL," :");
  hour=strtok(NULL," :");
  minute=strtok(NULL," :");
  second=strtok(NULL," :");
  year=strtok(NULL," :");
  if(junk==NULL ||month==NULL ||day==NULL ||hour==NULL ||minute==NULL
  ||second==NULL ||year==NULL) return;
       if(!strcmp(month,"Jan")) strcpy(month,"1");
  else if(!strcmp(month,"Feb")) strcpy(month,"2");
  else if(!strcmp(month,"Mar")) strcpy(month,"3");
  else if(!strcmp(month,"Apr")) strcpy(month,"4");
  else if(!strcmp(month,"May")) strcpy(month,"5");
  else if(!strcmp(month,"Jun")) strcpy(month,"6");
  else if(!strcmp(month,"Jul")) strcpy(month,"7");
  else if(!strcmp(month,"Aug")) strcpy(month,"8");
  else if(!strcmp(month,"Sep")) strcpy(month,"9");
  else if(!strcmp(month,"Oct")) strcpy(month,"10");
  else if(!strcmp(month,"Nov")) strcpy(month,"11");
  else if(!strcmp(month,"Dec")) strcpy(month,"12");
  else return;
  sprintf(xx,"%04d.%02d.%02d %02d:%02d:%02d",
  atoi(year), atoi(month), atoi(day), atoi(hour), atoi(minute), atoi(second));
}
/*---------------------------------------------- Later
STAFCV_T levLogStopTime(DS_DATASET_T *pEnv,const char *comment) {
  char stopTime[152]; int ii,len; time_t calTime;
  calTime=time(NULL); strncpy(stopTime,ctime(&calTime),50); 
  len=strlen(stopTime);
  for(ii=len-1;ii>=0;ii--) if(stopTime[ii]=='\n') stopTime[ii]='\0';
  levConvertToDigits(stopTime);
  if(!levRegisterEnvInfo(&gNumRowEnv,"stopTime",stopTime,pEnv)) 
      EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo(&gNumRowEnv,"stopComment",comment,pEnv)) 
      EML_ERROR(TABLE_INSERTION_FAILED);
  EML_SUCCESS(STAFCV_OK);
}
----------------------------------------------*/
STAFCV_T levRegisterEnvInfo(char *name,char *value) {
  /* Eg, name="start time", and value="July 4, 1996 17:04". */
  TDM_COLUMN_T col; TDM_CELLDATA_T cellData; DS_TYPE_CODE_T tcode;
  int column; char *colName,*val,*v;
  tdmTable* tab; long ncol,nrow;
  if(NULL == (tab = tdm->findTable(LEV_ENV_TABLE))) {
     EML_ERROR(ENV_TBL_NOT_FOUND);
  }
  nrow=(long)tab->rowCount();
  if(nrow>=LEV_ENV_MAX_ROWS) EML_ERROR(TOO_MANY_ROWS);
  tab->rowCount((long)(1+nrow));
  for(column=0;column<2;column++) {
    if(column==0) {
      val=name; colName="name";
      if(strlen(val)>31) EML_ERROR(INFO_NAME_TOO_BIG);
    } else if(column==1) {
      val=value; colName="value";
      if(strlen(val)>255) EML_ERROR(INFO_VALUE_TOO_BIG);
    }
    if(!tab->findColumn(col,colName)) {
      EML_ERROR(FIND_COL_FAILED);
    }
    ncol = (int)(col.nCol); tcode = col.code; cellData._d = tcode;
    cellData.data.v = (void*)MALLOC(sizeof(double)); // BUG? FIXED?
    v=(char*)MALLOC(strlen(val) +1);
    strcpy(v,val); cellData.data.c = (char*)v;
    if(!tab->putCell(cellData,nrow,ncol)) {
      PP"putCell fail\n"); EML_ERROR(PUT_CELL_FAILED);
    }
  }
  FREE(cellData.data.v);
  EML_SUCCESS(STAFCV_OK);
} /* nrow val .v .c */
void levExeName(char *out) {
  pid_t pid,myPid; char *cc,com[200],fn[100],buf[200]; int fo=0; FILE *ff;
  strcpy(out,"lev.c 1fail");
  myPid=getpid(); sprintf(fn,"/var/tmp/lev.%d",myPid);
  sprintf(com,"ps -e > %s",fn); system(com);
  ff=fopen(fn,"r"); if(ff==NULL) return;
  while(fgets(buf,198,ff)) {
    pid=atol(strtok(buf," \t\n")); if(pid==myPid) { fo=7; break; }
  } fclose(ff); sprintf(com,"rm %s 2> /dev/null",fn); system(com);
  if(!fo) { strcpy(out,"lev.c 2fail"); return; }
  strtok(NULL," \t\n"); strtok(NULL," \t\n"); cc=strtok(NULL," \t\n");
  if(cc==NULL) { strcpy(out,"lev.c 3fail"); return; }
  strncpy(out,cc,VSIZE); 
}
void levFactory::levUpdate() {
  socObject *obj;
  for( int i=0; i<soc->count();i++ ){
     if(NULL != (obj = soc->getObject(i))) {
        registerVersion(obj->Name(),obj->Type(),obj->Version());
     }
  }
}
STAFCV_T levFactory::update() {
  levUpdate();
  return TRUE; // HACK, meaningless return value
}
STAFCV_T levFactory::levRegisterEnvironment() {

#ifndef WIN32
  struct utsname osys;
#endif /* WIN32 */ 

  time_t calTime; int ii,len;
  char nameOfExe[VSIZE+2],hostname[VSIZE+2],user[VSIZE+2],*cc,startTime[152];
  PP"This is levRegisterEnvironment().\n");
  /******************  user name ****************************/
  cc=getenv("USER");
  if(cc==NULL) strcpy(user,"getenv-failed"); else strncpy(user,cc,VSIZE); 
  /******************  hostname ****************************/
  if(gethostname(hostname,VSIZE)) {
    strncpy(hostname,"gethostname-failed-lev.c",VSIZE); 
  }
  /******************  operating system ****************************/
#ifndef WIN32
  uname(&osys);
#endif
  /******************  start time ****************************/
  calTime=time(NULL); strncpy(startTime,ctime(&calTime),50); 
  len=strlen(startTime);
  for(ii=len-1;ii>=0;ii--) if(startTime[ii]=='\n') startTime[ii]='\0';
  levConvertToDigits(startTime);
  /*----------------------------------------
  mytmstruct=localtime(&calTime);
  if((mytmstruct->tm_isdst)>0) strcat(startTime," daylight savings time");
  else if((mytmstruct->tm_isdst)==0) {
    strcat(startTime," not daylight savings time");
  } else strcat(startTime," daylight savings time not available");
  ------------------------------------------------------------*/
  /******************  name of executable ****************************/
  levExeName(nameOfExe);
  /******************  output to tables ****************************/
  if(!levRegisterEnvInfo("user",user)) EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("hostname",hostname))
	EML_ERROR(TABLE_INSERTION_FAILED);
#ifndef WIN32
  if(!levRegisterEnvInfo("sysname",osys.sysname))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("node",osys.nodename))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("release",osys.release))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("version",osys.version))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("machine",osys.machine))
	EML_ERROR(TABLE_INSERTION_FAILED);
#else
  if(!levRegisterEnvInfo("sysname","Windows NT"))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("node","UNKOWN"))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("release","4"))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("version","0"))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("machine","UNKOWN"))
	EML_ERROR(TABLE_INSERTION_FAILED);
#endif
  if(!levRegisterEnvInfo("nameOfExe",nameOfExe))
	EML_ERROR(TABLE_INSERTION_FAILED);
  if(!levRegisterEnvInfo("startTime",startTime))
	EML_ERROR(TABLE_INSERTION_FAILED);
  EML_SUCCESS(STAFCV_OK);
}
//:=============================================== CLASS              ==
// levFactory

//:----------------------------------------------- CTORS & DTOR       --
levFactory:: levFactory(const char *name)
	: socFactory()
	, socObject(name, "levFactory") {
  myPtr = (SOC_PTR_T)this;
  lock(TRUE);
printf("DEBUG HACK 1a \n"); fflush(0);
  tdm->newDataset("config",20);
printf("DEBUG HACK 1b \n"); fflush(0);
  tdm->newTable(LEV_ENV_TABLE,LEV_ENV_SPEC,LEV_ENV_MAX_ROWS);
printf("DEBUG HACK 1c \n"); fflush(0);
  tdm->newTable(LEV_VER_TABLE,LEV_VER_SPEC,LEV_VER_MAX_ROWS);
printf("DEBUG HACK 1d \n"); fflush(0);
  this->levRegisterEnvironment();
  // Craig tdm->findTable(LEV_ENV_TABLE,myEnvironment);
  // Craig tdm->findTable(LEV_VER_TABLE,myVersions);
}
levFactory:: ~levFactory() { }

//:----------------------------------------------- ATTRIBUTES         --

//:----------------------------------------------- PUB FUNCTIONS      --
int levCellAlreadyIn(const char *table,const char *value,long col) {
  /* returns true if value is represented anywhere in table in column col */
  long i,nrow;
  tdmTable* tab;
  TDM_CELLDATA_T cellData;
  if(NULL == (tab = tdm->findTable(table))) {
     EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",table);
     EML_ERROR(TABLE_NOT_FOUND);
  }
  nrow=tab->rowCount();
  for(i=nrow-1;i>=0;i--) {
    tab->getCell(cellData,i,col);
    if(!strcmp(cellData.data.c,value)) return TRUE;
  }
  return FALSE;
}
STAFCV_T levFactory:: registerVersion(const char *name,
  const char *type, const char *version) {
  TDM_COLUMN_T col; TDM_CELLDATA_T cellData; DS_TYPE_CODE_T tcode;
  int column; char *colName,*v; char colAlreadyIn0,colAlreadyIn1;
  const char *val;
  tdmTable* tab; long ncol,nrow;
  /*------------------------------------------- */
  if(NULL == (tab = tdm->findTable(LEV_VER_TABLE))) {
     EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
     EML_ERROR(TABLE_NOT_FOUND);
  }
  nrow=(long)tab->rowCount();
  if(nrow>=LEV_ENV_MAX_ROWS) EML_ERROR(TOO_MANY_ROWS);
  colAlreadyIn0=levCellAlreadyIn(LEV_VER_TABLE,name,(long)0);
  colAlreadyIn1=levCellAlreadyIn(LEV_VER_TABLE,type,(long)1);
  if(colAlreadyIn0&&colAlreadyIn1) {
    PP"Already registered: '%s', '%s'\n",name,type); return TRUE;
  } else {
    PP"Registering:        '%s', '%s', '%s'\n",name,type,version);
  }
  tab->rowCount((long)(1+nrow));
  for(column=0;column<3;column++) {
    if(column==0) {
      val=name; colName="name";
      if(strlen(val)>31) EML_ERROR(NAME_TOO_LONG);
    } else if(column==1) {
      val=type; colName="type"; 
      if(strlen(val)>31) EML_ERROR(TYPE_TOO_LONG);
    } else if(column==2) {
      val=version; colName="version";
      if(strlen(val)>255) EML_ERROR(VALUE_TO_LONG);
    }
    if(!tab->findColumn(col,colName)) {
      EML_ERROR(FIND_COL_FAILED);
    }
    ncol = (int)(col.nCol); tcode = col.code; cellData._d = tcode;
    cellData.data.v = (void*)MALLOC(1000);  // BUG? does this mem get used?
    v=(char*)MALLOC(strlen(val) +1);
    strcpy(v,val); cellData.data.c = (char*)v;
    if(!tab->putCell(cellData,nrow,ncol)) {
      PP"putCell fail\n"); EML_ERROR(PUT_CELL_FAILED);
    }
    FREE(cellData.data.v);
  }
  /* -------------------------------------------------------*/
  EML_SUCCESS(STAFCV_OK);
} /* nrow val */
char *levFactory:: version() {
  char *c=NULL;
  char *v="$Header: /scratch/smirnovd/cvs2git_readonly/cvs/star-sw/asps/staf/lev/src/Attic/levClasses.cc,v 1.12 1998/08/13 02:08:27 perev Exp $";
  c=(char*)MALLOC(strlen(v)+1);
  strcpy(c,v);
  return c;
}
//:---------------------------------------------------------------------
tdmTable * levFactory:: environment() 
{
   return myEnvironment;
}
tdmTable * levFactory:: versions() 
{
   return myVersions;
}
//:---------------------------------------------------------------------
unsigned char levFactory:: implementsInterface (const char * iface) {
   if( 0 == strcmp("levFactory",iface)
   ||  socFactory::implementsInterface(iface)
   ){
      return TRUE;
   }
   return FALSE;
}
