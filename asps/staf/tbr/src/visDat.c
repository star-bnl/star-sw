/* This reads files from the STAF table browser, and produces
  an input file for the visualizer STARGL.
** Herbert Ward --- started Sept 1 1996 --- ward@moore.ph.utexas.edu */
/****************************************************  DEFINES    ***/
#define HALF_TPC_LEN 2.10   /* meters */
#define TPC_RAD      2.00   /* meters */
#define ERR Err(__LINE__)
#define RADIANS_PER_DEGREE 0.0174532
#define POINTSPERTRACK 50
#define STEP 8.5
#define LSIZE 300
#define MAX_PTS_PER_TRACK 10
#define PP printf(
/****************************************************  PROTO   ***/
void DelNL(char *cc);
void Err(int x);
/****************************************************  INCLUDES   ***/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
/****************************************************  GLOBALS    ***/
FILE *gDataOut;
/****************************************************  FUNCTIONS  ***/
void CalcCenterAndRadiusOfCurv(int qq,            /* { makeproto */
  float *rc,float *xc,float *yc,float r0,
  float phi0,float ptrans,float mag,float psi) {
  float xx,yy;
  xx=r0*cos(phi0*RADIANS_PER_DEGREE); yy=r0*sin(phi0*RADIANS_PER_DEGREE);
  if(qq==0)  *rc=1e20; else *rc=ptrans/(qq*mag*0.002998);
  *xc=xx+(*rc)*cos((psi-90)*RADIANS_PER_DEGREE);
  *yc=yy+(*rc)*sin((psi-90)*RADIANS_PER_DEGREE);
}
int HasLowerCaseLetters(char *cc) {
  int ii;
  for(ii=(int)strlen(cc)-1;ii>=0;ii--) if(cc[ii]>='a'&&cc[ii]<='z') return 7;
  return 0;
}
void WriteOneTrack(int qq,int id, float r0,float phi0,float z0,
  float tanl,float psi,float ptrans,float mag) {
  float step,firstAngle,rc,xx,yy,xc,yc,angle,zz;
  int cntr=0; char scr[100];
  CalcCenterAndRadiusOfCurv(qq,&rc,&xc,&yc,r0,phi0,ptrans,mag,psi);
  if(rc>=0) { firstAngle=psi+90; step = -STEP;           }
  else      { firstAngle=psi-90; step =  STEP; rc = -rc; }
  for(angle=firstAngle;;angle+=step) { /* in degrees */
    zz=z0+rc*fabs(angle-firstAngle)*RADIANS_PER_DEGREE*tanl;
    xx=xc+rc*cos(angle*RADIANS_PER_DEGREE);
    yy=yc+rc*sin(angle*RADIANS_PER_DEGREE);
    sprintf(scr,"%7.4f %7.4f %7.4f",xx/100,yy/100,zz/100);
    if(HasLowerCaseLetters(scr)) {
      fprintf(gDataOut,"Error 96x in file %s.\n",__FILE__);
    } else {
      fprintf(gDataOut,"%s\n",scr);
    }
    if(++cntr>=MAX_PTS_PER_TRACK) break;
    if(fabs(zz)/100>HALF_TPC_LEN) break;
    if(sqrt(xx*xx+yy*yy)/100>TPC_RAD) break;
  }
}
void DelNL(char *cc) {
  int ii; ii=strlen(cc)-1; if(cc[ii]=='\n') cc[ii]='\0';
}
int Egr_globtrk(char *color,char *line) {
  char  *icharge,*id,*invpt,*phi0,*psi,*r0,*tanl,*z0,*xlast1,*xlast2,*xlast3;
  float              finvpt,fphi0,fpsi,fr0,ftanl,fz0;
  char *junk;
  int  ficharge,fid;
  icharge=strtok(line," "); if(!icharge) return 1;
  id=strtok(NULL," "); if(!id) return 2;
  invpt=strtok(NULL," "); if(!invpt) return 3;
  phi0=strtok(NULL," "); if(!phi0) return 4;
  psi=strtok(NULL," "); if(!psi) return 5;
  r0=strtok(NULL," "); if(!r0) return 6;
  tanl=strtok(NULL," "); if(!tanl) return 7;
  xlast1=strtok(NULL," "); if(!xlast1) return 8;
  xlast2=strtok(NULL," "); if(!xlast2) return 9;
  xlast3=strtok(NULL," "); if(!xlast3) return 10;
  z0=strtok(NULL," "); if(!z0) return 11;
  junk=strtok(NULL," "); if(junk) return 12;

  ficharge=atof(icharge);
  fid=atof(id);
  finvpt=atof(invpt);
  fphi0=atof(phi0);
  fpsi=atof(psi);
  fr0=atof(r0);
  ftanl=atof(tanl);
  fz0=atof(z0);
  if(finvpt==0) finvpt=1e-20;
  fprintf(gDataOut,"%s %d %d\n",color,fid,fid);
  WriteOneTrack(ficharge,fid,fr0,fphi0,fz0,ftanl,fpsi,1/finvpt,0.5);
  return 0;
}
int Tphit(char *color,char *line) {
  char *x,*y,*z,*id,*j;
  id=strtok(line," "); if(!id) return 1;
  x=strtok(NULL," "); if(!x) return 2;
  y=strtok(NULL," "); if(!y) return 3;
  z=strtok(NULL," "); if(!z) return 4;
  j=strtok(NULL," "); if( j) return 5;
  fprintf(gDataOut,"%s %d %d\n",color,atoi(id),atoi(id));
  fprintf(gDataOut,"%g %g %g\n", atof(x)/100.0, atof(y)/100.0, atof(z)/100.0);
  return 0;
}
int Tpt_track(char *color,char *line) {
  char  *icharge,*id,*invpt,*phi0,*psi,*r0,*tanl,*z0;
  float              finvpt,fphi0,fpsi,fr0,ftanl,fz0;
  char *junk;
  int  ficharge,fid;
  id=strtok(line," "); if(!id) return 1;
  icharge=strtok(NULL," "); if(!icharge) return 2;
  invpt=strtok(NULL," "); if(!invpt) return 3;
  phi0=strtok(NULL," "); if(!phi0) return 4;
  psi=strtok(NULL," "); if(!psi) return 5;
  r0=strtok(NULL," "); if(!r0) return 6;
  tanl=strtok(NULL," "); if(!tanl) return 7;
  z0=strtok(NULL," "); if(!z0) return 8;
  junk=strtok(NULL," "); if(junk) return 9;

  fid=atof(id);
  ficharge=atof(icharge);
  finvpt=atof(invpt);
  fphi0=atof(phi0);
  fpsi=atof(psi);
  fr0=atof(r0);
  ftanl=atof(tanl);
  fz0=atof(z0);
  if(finvpt==0) finvpt=1e-20;
  fprintf(gDataOut,"%s %d %d\n",color,fid,fid);
  WriteOneTrack(ficharge,fid,fr0,fphi0,fz0,ftanl,fpsi,1/finvpt,0.5);
  return 0;
}
int ConvertToVisData(char *table,int nlines,int count) {
  FILE *ff; char *ss,color[17],*mode,line[LSIZE+1]; int rv,tab=0,nn=0,error=0;
  ff=fopen(".tbr.temp","r"); if(!ff) return 1;
  if(count>0) mode="a"; else mode="w";
  gDataOut=fopen("vis.dump",mode); if(!gDataOut) { fclose(ff); return 2; }
  while(fgets(line,LSIZE,ff)) {
    nlines--;
    if(nlines%1000==0&&nlines>0)
        PP"Rows to go = %d , phase II, %s\n",nlines,table);
    if(strlen(line)>LSIZE-5) { fclose(ff); fclose(gDataOut); return 3; } nn++;
    DelNL(line);
    if(nn==1) {
      strncpy(color,line+4,15); color[0]+='a'-'A';
      ss=strstr(color,","); if(!ss) return 11; ss[0]=0;
      if(strstr(line,"table egr_globtrk")) tab=11;
      if(strstr(line,"table tpt_track"))   tab=22;
      if(strstr(line,"table tphit"))       tab=33;
    }
    if(nn<=3) continue;
    if(tab==11) {
      rv=Egr_globtrk(color,line);
      if(rv) { PP"Error, Egr_globtrk=%d.\n",rv); error=5; break; }
    } else if(tab==22) {
      rv=Tpt_track(color,line);
      if(rv) { PP"Error, Tpt_track=%d.\n",rv); error=6; break; }
    } else if(tab==33) {
      rv=Tphit(color,line);
      if(rv) { PP"Error, Tphit=%d.\n",rv); error=7; break; }
    } else { error=4; break; }
  }
  fclose(ff); fclose(gDataOut);
  if(error) return error;
  return 0;
}
#ifdef DEBUG_STANDALONE
void Err(int x) {
  PP"error %d\n",x); exit(2);
}
main() {
  int rv;
  PP"calling ConvertToVisData.\n");
  rv=ConvertToVisData("xxxx",0,0);
  PP"rv=%d (zero=no error).\n",rv);
}
#endif
