/* This is code for the lev pkg (Logging of Environment and Version info). 
** Begun Aug 18 1995 by Herb Ward. */
/*********************************************************  INCLUDES  **/
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/utsname.h>
#include <time.h>
#include <sys/types.h>
#include "dstype.h"
#include "dsxdr.h"
/*********************************************************  TYPEDEFS  **/
typedef char boolean; /* bbb Is this consistent with rest of STAR? */
/*********************************************************  GLOBALS  **/
DS_DATASET_T *gConfigBranch=NULL;
DS_DATASET_T *gPTabVer=NULL,*gPTabEnv=NULL;
int gNumRowEnv=0;
/*********************************************************  DEFINES  **/
#define TRUE 1  /* bbb Is this consistent with rest of STAR? */
#define FALSE 0 /* bbb Is this consistent with rest of STAR? */
#define CONFIG "config"
#define ENVNAME "environment"
#define VERNAME "versions"
#define PP printf(
#define VSIZE 40
#define DDIM 6
#define TYPEENV "struct lev_table_env { char label[%d],info[%d]; }"
#define TYPEVER "struct lev_table_ver { char label[%d],info[%d]; }"
#define NROWVER 400
#define NROWENV 15
#define CHARSIZE 25
/*********************************************************  FUNCTIONS  **/
void levOse(void) {
  PP"----------------------------------------------------------- pkg lev.\n");
}
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
boolean levFindVersionTable(DS_DATASET_T **ppVer) {
  *ppVer=gPTabVer;
  if(gPTabVer==NULL) return FALSE;
  return TRUE;
}
boolean levFindEnvironmentTable(DS_DATASET_T **ppEnv) {
  *ppEnv=gPTabEnv;
  if(gPTabEnv==NULL) return FALSE;
  return TRUE;
}
boolean levNewTable(DS_DATASET_T *parent,char *name,char *colType,size_t nRow,
  DS_DATASET_T **handleOut) {
  *handleOut=NULL;
  if(!dsAddTable(parent,name,colType,nRow,NULL)) {
    PP"dsAddTable(%s) failed in lev.c.\n",name); return FALSE;
  } else PP"lev.c, dsAddTable1() successful.\n");
  if(!dsFindEntry(handleOut,parent,name)) {
    PP"dsFindEntry(%s) failed in lev.c.\n",name); return FALSE;
  }
  if(!dspAddDTable(*handleOut)) { /* bbb TAS, not MOAST */
    PP"dspAddDTable(%s) failed in lev.c.\n",name); return FALSE;
  }
  return TRUE;
}
void levErrorMess(int xx) {
  Ose();
  PP"Failure of dsPutCell in lev.c of pkg lev (location %d).\n",xx);
  dsPerror("dsPerror:\n");
}
boolean levAddOneRow(int *nRow,const char *col0,const char *col1,
  DS_DATASET_T *pTable) {
  char cop0[260],cop1[260];
  strcpy(cop0,col0); cop0[CHARSIZE]='\0';
  strcpy(cop1,col1); cop1[CHARSIZE]='\0';
  dsSetTableRowCount(pTable,(size_t)((*nRow)+1));
  if(!dsPutCell(cop0,pTable,(size_t)(*nRow),0)) {
    levErrorMess(0); return FALSE;
  }
  if(!dsPutCell(cop1,pTable,(size_t)(*nRow),1)) {
    levErrorMess(1); return FALSE;
  }
  (*nRow)++;
  PP"Added to table: %20s %s\n",cop0,cop1);
  return TRUE;
}
void ConvertToDigits(char *xx) {
  int ii,len; char *junk,*month,*day,*hour,*minute,*second,*year,copy[55];
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
  else if(!strcmp(month,"Oct")) strcpy(month,"0");
  else if(!strcmp(month,"Nov")) strcpy(month,"11");
  else if(!strcmp(month,"Dec")) strcpy(month,"12");
  else return;
  sprintf(xx,"%04d.%02d.%02d %02d:%02d:%02d",
  atoi(year), atoi(month), atoi(day), atoi(hour), atoi(minute), atoi(second));
}
boolean levStopTime(DS_DATASET_T *pEnv,const char *comment) {
  char stopTime[152]; int ii,len; time_t calTime;
  calTime=time(NULL); strncpy(stopTime,ctime(&calTime),50);
  len=strlen(stopTime);
  for(ii=len-1;ii>=0;ii--) if(stopTime[ii]=='\n') stopTime[ii]='\0';
  ConvertToDigits(stopTime);
  if(!levAddOneRow(&gNumRowEnv,"stopTime",stopTime,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"stopComment",comment,pEnv)) return FALSE;
  return TRUE;
} /* this line has trailing spaces */    
/* bbb used to be enum type below */
boolean levRegisterVersion(DS_DATASET_T *pVer,char *name,int type,char *ver) {
  /* bbb type not used */
  static int nrows=0;
  if(!levAddOneRow(&nrows,name,ver,pVer)) return FALSE;
  return TRUE;
}
boolean levMake_gConfigBranch(void) {
  /******************  create dataset branch and tables ****************/
  if(!dsNewDataset(&gConfigBranch,CONFIG,DDIM)) { /* can add DDIM-1 entries */
    PP"dsNewDataset failed in lev.c.\n"); return FALSE;
  } else {
    PP"lev.c: created branch %s with tables %s & %s.\n",
    CONFIG,ENVNAME,VERNAME);
  }
  return TRUE;
}
boolean levAddEnvironmentTable(DS_DATASET_T **ppEnv) {
  /* bbb comment not used */
  char colType[200];
  if(*ppEnv==NULL) {
    if(gConfigBranch==NULL) { if(!levMake_gConfigBranch()) return FALSE; }
    *ppEnv=gConfigBranch;
  }
  /* We use a sprintf to colType so CHARSIZE will be available easily as
  ** an integer */
  sprintf(colType,TYPEENV,CHARSIZE,CHARSIZE);
  if(!levNewTable((*ppEnv),ENVNAME,colType,NROWENV,&gPTabEnv)) return FALSE;
  *ppEnv=gPTabEnv;
  return TRUE;
}
boolean levAddVersionTable(DS_DATASET_T **ppVer) {
  /* bbb comment not used */
  char colType[200];
  if(*ppVer==NULL) {
    if(gConfigBranch==NULL) { if(!levMake_gConfigBranch()) return FALSE; }
    *ppVer=gConfigBranch;
  }
  /* We use a sprintf to colType so CHARSIZE will be available easily as
  ** an integer */
  sprintf(colType,TYPEVER,CHARSIZE,CHARSIZE);
  if(!levNewTable((*ppVer),VERNAME,colType,NROWVER,&gPTabVer)) return FALSE;
  *ppVer=gPTabVer;
  return TRUE;
}
boolean levRegisterEnvironment(DS_DATASET_T *pEnv,const char *comment) {
  struct tm *mytmstruct;
  struct utsname osys; time_t calTime; int ii,len;
  char nameOfExe[VSIZE+2],hostname[VSIZE+2],user[VSIZE+2],*cc,startTime[152];
  PP"This is lev_log_environment() in lev.c.\n");
  /******************  user name ****************************/
  cc=getenv("USER");
  if(cc==NULL) strcpy(user,"getenv-failed"); else strncpy(user,cc,VSIZE);
  /******************  hostname ****************************/
  if(gethostname(hostname,VSIZE)) {
    strncpy(hostname,"gethostname-failed-lev.c",VSIZE);
  }
  /******************  operating system ****************************/
  uname(&osys);
  /******************  start time ****************************/
  calTime=time(NULL); strncpy(startTime,ctime(&calTime),50);
  len=strlen(startTime);
  for(ii=len-1;ii>=0;ii--) if(startTime[ii]=='\n') startTime[ii]='\0';
  ConvertToDigits(startTime);
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
  levOse();
  PP"This is lev.c.  I am putting the following information into\n");
  PP"table \"/%s/%s\".\n",CONFIG,ENVNAME);
  gNumRowEnv=0;
  if(!levAddOneRow(&gNumRowEnv,"user",user,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"hostname",hostname,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"sysname",osys.sysname,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"node",osys.nodename,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"release",osys.release,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"version",osys.version,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"machine",osys.machine,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"nameOfExe",nameOfExe,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"startTime",startTime,pEnv)) return FALSE;
  if(!levAddOneRow(&gNumRowEnv,"startComment",comment,pEnv)) return FALSE;
  return TRUE;
}
/* void main(void) {
  lev.c("Standalone test.");
} */
