#include <stdio.h> 
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "emlLib.h"
#include "asuAlloc.h"

int 
emlMessage(char *fmt, ...)
{
	int status;

	va_list args;
	va_start(args, fmt);

	status = vfprintf(stdout, fmt, args);
        if(!strstr(fmt,"\n")) fprintf(stdout,"\n");

	va_end(args);

	return status;
}

char * emlContext(char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	vsprintf(eml_context, fmt, args);

	va_end(args);

	return eml_context;
}

int eml_strcat(char *tgt,char *src,size_t len) {
  if(strlen(tgt)+strlen(src)>len-3) return 0;
  strcat(tgt,src);
  return 7;
}

#define ASU_PATH_SIZE 149
#define ASU_BUF       249

/* This looks complicated, but most of the complication is an
** elaborate fail-safe scheme. */
void emlPrettifyErrorMessage(char *errmsg,int maxlen) {

  char buf[ASU_BUF+1],foundOne,whichErr=0,*x,*copy,*buf1,*buf2;
  char pathsave[ASU_PATH_SIZE+1],*errorCode,*path,*fileName,*linenumber;
  size_t len,imaxlen;
  int j,totalLen,i,lineNum;

  imaxlen=maxlen;
  if(eml_beep_on) { fprintf(stderr,"\007"); fflush(stderr); }
  if(!eml_pretty_on) return;
  pathsave[0]=0;
  len = strlen(errmsg);
  if (len <= 0) return;

  copy=(char*)MALLOC(len+1); if(!copy) return; strcpy(copy,errmsg);
  buf1=(char*)MALLOC(len+1); if(!buf1) { FREE(copy);             return; }
  buf2=(char*)MALLOC(len+1); if(!buf2) { FREE(copy); FREE(buf1); return; }
  buf1[0]=0; buf2[0]=0; x=strtok(copy,"\n");
  path = NULL;
  while(x) {
    if(x[0]=='E'&&x[1]=='R'&&x[2]=='R'&&x[3]=='O'&&x[4]=='R') {
      if(!eml_strcat(buf1,x,len)) { whichErr=__LINE__; break; }
      if(!eml_strcat(buf1,"\n",len)) { whichErr=__LINE__; break; }
    } else {            /* we are in the error trace */
      foundOne=0;
      errorCode=x;
      for(i=0;x[i];i++) { if(x[i]=='-'&&x[i+1]=='/') { foundOne=7; break; } }
      if(!foundOne) { whichErr=__LINE__; break; } x[i]=0;
      path=x+i+1;
      fileName=strstr(path,"/src/");
      if(!fileName) { whichErr=__LINE__; break; }
      fileName-=4; fileName[0]=0; fileName++;
      foundOne=0;
      for(j=strlen(fileName)-1;j>=0;j--) { /* find beginning of line number */
        if(fileName[j]>'9'||fileName[j]<'0') { foundOne=7; break; }
      }
      if(!foundOne) { whichErr=__LINE__; break; }
      if(fileName[j]!='.') { whichErr=__LINE__; break; }
      fileName[j]=0; linenumber=fileName+j+1;
      lineNum=atoi(linenumber);
      if(lineNum<5||lineNum>10000) { whichErr=__LINE__; break; }
      if(pathsave[0]) { /* chk all files have the same path up to ???/src  */
        if(strcmp(pathsave,path)) { whichErr=__LINE__; break; }
      } else {
        if(strlen(path)>ASU_PATH_SIZE) { whichErr=__LINE__; break; }
        strcpy(pathsave,path);
      }
      totalLen =strlen(errorCode);
      totalLen+=strlen(fileName);
      totalLen+=strlen(linenumber);
      totalLen+=30;
      if(totalLen<100) totalLen=100;
      if(totalLen>ASU_BUF) { whichErr=__LINE__; break; }
      sprintf(buf,"  %-25s   vi +%-4s %s\n",errorCode,linenumber,fileName);
      if(!eml_strcat(buf2,buf,len)) { whichErr=__LINE__; break; }
    }
    x=strtok(NULL,"\n");
  }

  if((path==NULL) || ( strlen(buf1)+ strlen(path)+ strlen(buf2)+30 > maxlen))
    whichErr=__LINE__;

  if(whichErr) {
    fprintf(stderr,"Prettification of error message failed (%d),\n",whichErr);
    fprintf(stderr,"printing raw error message:\n");
  } else {
    errmsg[0]=0;
    eml_strcat(errmsg,buf1,imaxlen);
    eml_strcat(errmsg,"  cd ",imaxlen);
    eml_strcat(errmsg,path,imaxlen);
    eml_strcat(errmsg,"\n",imaxlen);
    eml_strcat(errmsg,buf2,imaxlen);
    for(i=1;;i++) {
      if(errmsg[i-1]=='\n'&&errmsg[i]==0) errmsg[i-1]=0;
      if(errmsg[i]==0) break;
    }
  }

  FREE(copy); FREE(buf1); FREE(buf2);
}

/*
INVALID_SELECT_SPEC-/star/u2/ward/Staf/pkgs/top/src/topClasses.cc.810
ERROR: Invalid selection spec '{forgotTheRightCurlyBracket'.
KAM_METHOD_FAILURE-/star/u2/ward/Staf/pkgs/top/src/top_kam.cc.88
*/
