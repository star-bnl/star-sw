#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define SIZE 200
void Err(int);
#define PP printf(
#define EE fprintf(stderr,
#define FF fprintf(gOut,
#define TAB 52
char gExe[58];
char gCurrOutFile[120];
char gCurrPam[80];
char gTab[TAB][70];
char gType[TAB][20];
int gCnt,gNTab;
FILE *gOut;
int gLine;
void Ose(void) {
  PP"--------------------------------------------------------------------\n");
}
void DelNL(char *x) {
  int ii,len; len=strlen(x);
  for(ii=len-1;ii>=0;ii--) if(x[ii]=='\n') x[ii]='\0';
}
void Usage(void) {
  EE"Usage: %s \n",gExe); exit(2);
}
int Shell(char *com,char *results,int max,int big) {
  FILE *ff; char line[SIZE],comm[199]; int ii;
  sprintf(comm,"%s > junk.yyuuix6",com);
  system(comm);
  ff=fopen("junk.yyuuix6","r"); if(ff==NULL) Err(__LINE__);
  for(ii=0;ii<max;ii++) {
    if(!fgets(line,SIZE,ff)) break; DelNL(line);
    if(big<SIZE) line[big]='\0';
    strcpy(results+ii*big,line);
  }
  fclose(ff); system("rm junk.yyuuix6"); return ii;
}
void Err(int errnum) {
  fprintf(stderr,"Fatal error line number %d of %s. \n",errnum,__FILE__);
  PP"line %d of MODULES.OUT\n",gLine);
  exit(1);
}
void DelTrailingWhite(char *x) {
  int ii,len; char a;
  len=strlen(x);
  for(ii=len-1;ii>=0;ii--) {
    a=x[ii]; if(a!='\n'&&a!='\t'&&a!=' ') break; x[ii]=0;
  }
}
int FirstSixMod(char *x) {
  if(x[0]!='m') return 0; if(x[1]!='o') return 0; if(x[2]!='d') return 0;
  if(x[3]!='u') return 0; if(x[4]!='l') return 0; if(x[5]!='e') return 0;
  return 7;
}
int IsWhite(char a) {
  if(a!='\n'&&a!='\t'&&a!=' ') return 7;
  return 0;
}
int IsDig(char a) {
  if(a<'0') return 0;
  if(a>'9') return 0;
  return 7;
}
int NumAtEnd(char *x) {
  int rv=7,ii,len; char *pp,last[100];
  len=strlen(x); for(ii=len-1;ii>=0;ii--) { if(!IsWhite(x[ii])) break; }
  if(ii<0) Err(__LINE__);
  strcpy(last,x+ii+1);
  pp=last;
  while(*pp) { if(!IsDig((*pp))) rv=0; pp++; }
  return rv;
}
int LineType(char *x) {
  int rw,sum,hu,nae;
  if(!FirstSixMod(x)) Err(__LINE__);
  nae=NumAtEnd(x);
  if(x[6]=='_') hu=7; else hu=0;
  if(strstr(x," read ")||strstr(x," write ")||strstr(x," update ")) rw=7;
  else rw=0;
  sum=rw+nae+hu;
  if(sum!=0&&sum!=21) {
    PP"Baa,d number %d. numatend=%d, readwrite=%d, hasundersocre=%d.\n",
    gLine,nae,rw,hu);
    PP"'%s'\n",x);
    exit(2);
  }
  if(nae) return 2;
  return 1;
}
void TabsToSpaces(char *x) {
  int ii;
  for(ii=strlen(x)-1;ii>=0;ii--) { if(x[ii]=='\t') x[ii]=' '; }
}
void Mk_gCurrOutFile_gCurrPam(char *xx) {
  char *one,*two,*thr,cp[111];
  strcpy(cp,xx);
  one=strtok(cp," \t\n"); two=strtok(NULL," \t\n"); thr=strtok(NULL," \t\n");
  if(!one||!two||!thr) Err(__LINE__);
  if(strcmp(one,"module")) Err(__LINE__);
  strcpy(gCurrPam,thr);
  sprintf(gCurrOutFile,"modules/%s.idl",gCurrPam);
}
void HeaderBlurb(void) {
  FF"/* Filename = %s.idl */\n",gCurrPam);
  FF"#include \"ASU.idl\"\n");
}
void WriteMainPart(void) {
  char punc[30]; int ii;
  for(ii=0;ii<gNTab;ii++) FF"#include \"%s.idl\"\n",gTab[ii]);
  FF"interface %s {\n",gCurrPam);
  FF"  STAFCV_T call(\n");
  for(ii=0;ii<gNTab;ii++) {
    if(ii<gNTab-1) strcpy(punc,","); else *punc=0;
    FF"         %9s %13s t%d %s\n",gType[ii],gTab[ii],ii+1,punc);
  }
  FF"  );\n");
  FF"};\n");
}
void HandleTableLine(char *line) {
  char *a,*b,*c,*d,*e;
  if(gNTab>=TAB) Err(__LINE__);
  a=strtok(line," \n"); b=strtok(NULL," \n"); c=strtok(NULL," \n");
  d=strtok(NULL," \n"); e=strtok(NULL," \n");
  if(!a||!b||!c||!d||!e) Err(__LINE__);
  if(strcmp(a,"module_table")) Err(__LINE__);
  strcpy(gTab[gNTab],c);
  if(!strcmp(d,"read")) strcpy(gType[gNTab],"in");
  else if(!strcmp(d,"write")) strcpy(gType[gNTab],"out");
  else if(!strcmp(d,"update")) strcpy(gType[gNTab],"inout");
  else Err(__LINE__);
  gNTab++;
}
main(int nnn, char *aaa[]) {
  char line[SIZE];
  FILE *in;
  int lt;
  gLine=0; strncpy(gExe,aaa[0],55); if(nnn!=1) Usage(); gCnt=0;
  in=fopen("MODULES.OUT","r"); if(in==NULL) Err(__LINE__); gOut=NULL;
  while(fgets(line,SIZE,in)) {
    gLine++; DelTrailingWhite(line); TabsToSpaces(line);
    lt=LineType(line);
    switch(lt) {
      case 1:
        if(gOut) { WriteMainPart(); fclose(gOut); }
        Mk_gCurrOutFile_gCurrPam(line);
        gOut=fopen(gCurrOutFile,"w"); if(gOut==NULL) Err(__LINE__);
        gCnt++; HeaderBlurb();
        gNTab=0; break;
      case 2:
        HandleTableLine(line);
        break;
      default: Err(__LINE__);
    }
  }
  if(gOut) fclose(gOut);
  PP"Total lines read from input file = %d\n",gLine);
  fclose(in);
  exit(0);
}
