/* Translates informix reports (.h) into idl files for tas/moast transition */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define SIZE 200
void Err(int);
#define PP printf(
#define EE fprintf(stderr,
#define FF fprintf(out,
#define SS fprintf(ss,
char gExe[58];
void Ose(void) {
  PP"--------------------------------------------------------------------\n");
}
void DelNL(char *x) {
  int ii,len; len=strlen(x);
  for(ii=len-1;ii>=0;ii--) if(x[ii]=='\n') x[ii]='\0';
}
void Usage(void) {
  EE"Usage: %s dothfilefrominformix\n",gExe); exit(2);
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
  fprintf(stderr,"Fatal error number %d.  Exiting....\n",errnum);
  exit(1);
}
void Outname(char*aaa,char*outname) {
  char *aa; int pos,ii;
  aa=strstr(aaa,"_"); if(!aa) Err(__LINE__);
  sprintf(outname,"tables/%s",aa+1); pos=0;
  for(ii=0;ii<strlen(outname);ii++) if(outname[ii]=='_') pos=ii;
  strcpy(outname+pos,".idl");
}
int AllWhite(char *x) {
  int ii,len; char xx; len=strlen(x);
  for(ii=len-1;ii>=0;ii--) {
    xx=x[ii]; if(xx!=' '&&xx!='\t'&&xx!='\n') return 0;
  }
  return 7;
}
int Toss(int lineNum,char *x) {
  if(AllWhite(x)) return 7;
  if(strstr(x,"descrip struct name:")) return 7;
  if(strstr(x,"row struct name:")) return 7;
  return 0;
}
void Modify(int *srs,char *xx,char *outname) {
  char *cc,buf[SIZE];
  if(*srs) {
    cc=strstr(xx," int ");
    if(cc) {
      if(strlen(cc)>6) {
        cc[1]='l'; cc[2]='o'; cc[3]='n'; cc[4]='g'; return;
      }
    }
  }
  if(strstr(xx,"*             more..:")) {
    sprintf(buf,"*  %s",strstr(xx,":")+1); strcpy(xx,buf); return;
  }
  if(strstr(xx,"*        description:")) {
    sprintf(buf,"*  %s",strstr(xx,":")+1); strcpy(xx,buf); return;
  }
  if(strstr(xx," typedef struct {")) {
    sprintf(xx,"  struct %s {\n",strstr(outname,"/")+1); cc=strstr(xx,".idl");
    if(!cc) Err(__LINE__); cc[0]=0; strcat(xx," {\n");
    (*srs)=7; return;
  }
}
int FirstNonwhiteIsCurly(char *x) {
  int rv=0; int ii,len; char a;
  len=strlen(x);
  for(ii=0;ii<len;ii++) {
    a=x[ii];
    if(a!=' '&&a!='\n'&&a!='\t') { if(a=='}') rv=7; break; }
  }
  return rv;
}
main(int nnn, char *aaa[]) {
  char *cc,line[SIZE],outname[100];
  FILE *ss,*in; FILE *out;
  int saelc=0,startedRowStruct=0,lcnt=0; /* saelc=SkipAllExceptLastComment */
  strncpy(gExe,aaa[0],55); if(nnn!=2) Usage();
  if(strlen(aaa[1])==20&&strstr(aaa[1],"_table_st.h")) {
    PP"Skipping %s, it is not a normal INFORMIX .h report.\n",aaa[1]);
    exit(0);
  }
  ss=fopen("st.scr","a"); if(!ss) Err(__LINE__);
  in=fopen(aaa[1],"r"); if(in==NULL)   Err(__LINE__);
  Outname(aaa[1],outname); out=fopen(outname,"w");
  if(out==NULL) { PP"no write %s\n",outname); exit(2); }
  while(fgets(line,SIZE,in)) {
    lcnt++;
    if(lcnt==1) { FF"/* %s\n",strstr(outname,"/")+1); continue; }
    if(lcnt==5&&!strstr(line,"Table:")) {
      PP"Warning on file %s\n",aaa[1]);
    }
    Modify(&startedRowStruct,line,outname);
    if(saelc&&!strstr(line,"Last mod. for ")) continue;
    if(startedRowStruct&&FirstNonwhiteIsCurly(line)) {
      saelc=7;  FF"  };\n"); continue;
    }
    if(Toss(lcnt,line)) continue;
    FF"%s",line);
  } fclose(in); fclose(out);
  SS"echo \"%24s %19s xiooooooooooooooooooooooooooooooo\"\n",
  aaa[1],outname);
  SS"diff       %25s %25s\n",aaa[1],outname);
  PP"%s %s\n",aaa[1],outname);
  fclose(ss);
  exit(0);
}
