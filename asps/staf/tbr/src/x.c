/* To do:
*/
/* Motif (gui) part of the STAR browser.  Begun April 22 1995, Herb Ward. */
/***********************************************************  INCLUDES  **/
#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <stdlib.h>
#include "dstype.h"
#include "dsxdr.h"
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/Form.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/Frame.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/ToggleBG.h>
#include <Xm/ToggleB.h>
#include <Xm/LabelG.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#define EXTERN extern
#include "brow.h"
#include "x.h"
/******************************************************  PROTOTYPES  **/
void AuxOutputFromOneLn(int whWin,int whAct);
void DrawLineXOnly(int x1,int y1,int x2,int y2);
void DrawLine(float x1,float y1,float x2,float y2);
void DrawString(float xf,float yf,char *cc);
void DrawStringXOnly(int x,int y,char *cc);
void AddPs(char *x);
void RunTheRows(myBool,int whWin,void (*fnct)());
void MakeWindow(int,int);
Widget Column(Widget parent);
Widget Row(Widget parent);
void CreateMenuItems(Widget mbar,int whichWindow);
XtCP TextCB(Widget w,caddr_t cld,caddr_t cad);
void Say(char *mess);
/***********************************************************  FUNCTIONS  **/
void DrawXaxisTicks(float minn,float maxx,float ts) {
  float frac,xx;  int ii,cc;
  xx=minn; ii=0;
  while(xx<maxx+0.1*ts) { /* tick marks */
    cc=-15;
    frac=(xx-minn)/(maxx-minn);
    DrawLine(frac,0.0,frac,TICKSIZEX);
    DrawLine(frac,1.0,frac,1.0-TICKSIZEX);
    frac+=XTICKADJUSTX;
    sprintf(gPass,"%g",xx); DrawString(frac,XTICKADJUSTY,gPass);
    xx=++ii*ts+minn;
  }
}
void DrawYaxisTicks(float minn,float maxx,float ts) {
  float frac,xx;  int ii;
  xx=minn; ii=0;
  while(xx<maxx+0.1*ts) { /* tick marks */
    frac=(xx-minn)/(maxx-minn);
    DrawLine(TICKSIZEY,frac,0.0,frac);
    DrawLine(1-TICKSIZEY,frac,1.0,frac);
    frac+=YTICKADJUSTY;
    sprintf(gPass,"%g",xx); DrawString(YTICKADJUSTX,frac,gPass);
    xx=++ii*ts+minn;
  }
}
void SendMail(void) {
  FILE *ff; char ln[105];
  system("echo $USER > junk.OoEEeFc77 2> /dev/null");
  ff=fopen("junk.OoEEeFc77","r"); if(ff==NULL) return;
  fgets(ln,100,ff); fclose(ff); system("rm junk.OoEEeFc77 2> /dev/null");
  /* if(strstr(ln,"ward")) return; */
  system("echo $USER browser `date` > junk.OoEEeFc77 2> /dev/null");

  ff=fopen("junk.OoEEeFc77","a"); if(ff==NULL) return;
  fprintf(ff,"\n%s\n",gInFile); fclose(ff);

  system("cat junk.OoEEeFc77 | mail ward@physics.utexas.edu 2> /dev/null");
  system("rm junk.OoEEeFc77 2> /dev/null");
}
void DrawLinePsOnly(float x1,float y1,float x2,float y2) {
  float xx1,yy1,xx2,yy2; char buf[150];
  xx1=LEFTPS+x1*WIDTH; xx2=LEFTPS+x2*WIDTH;
  yy1=BOTTPS+y1*HEIGH; yy2=BOTTPS+y2*HEIGH;
  sprintf(buf,"newpath\n%5.1f %5.1f moveto\n%5.1f %5.1f lineto\nstroke\n",
  xx1,yy1,xx2,yy2);
  AddPs(buf);
}
void DrawLine(float x1,float y1,float x2,float y2) {
  int ix1,iy1,ix2,iy2;
  DrawLinePsOnly(x1,y1,x2,y2);
  ix1=gLeft+(gRight-gLeft)*x1+0.5;
  ix2=gLeft+(gRight-gLeft)*x2+0.5;
  iy1=gDown-(gDown-gUp)*y1+0.5;
  iy2=gDown-(gDown-gUp)*y2+0.5;
  DrawLineXOnly(ix1,iy1,ix2,iy2);
}
void Progress(int x) {
  /* compare to numRows in DoCuts BBB */
}
Widget Column(Widget parent) {
  Widget rv; Arg args[59]; register int nn;
  nn=0;
  XtSetArg(args[nn],XmNorientation,XmVERTICAL); nn++;
  rv=XmCreateRowColumn(parent,"lrc",args,nn); XtManageChild(rv);
  return rv;
}
Widget Row(Widget parent) {
  Widget rv; Arg args[59]; register int nn;
  nn=0;
  XtSetArg(args[nn],XmNorientation,XmHORIZONTAL); nn++;
  rv=XmCreateRowColumn(parent,"lrc",args,nn); XtManageChild(rv);
  return rv;
}
void DoOnce2(void) {
  gCalculateAverages=TRUE;
  SendMail();
  gLast=-1;
  gNGraphicsUp=0;
  gNWin=0;
}
void Say(char *mess) {
  XmString messWidget,okWidget,titleWidget;
  char *title="STAR table browser";
  register int n;
  Arg args[25];
  Widget localShell,button,mess_box;
  char name[90];
  sprintf(name,title);
  n=0;
  localShell=XtCreatePopupShell(name,
  transientShellWidgetClass,gMainWindow,args,n);

  messWidget=XmStringCreateLtoR(mess,XmSTRING_DEFAULT_CHARSET);
  titleWidget=XmStringCreateLtoR(name,XmSTRING_DEFAULT_CHARSET);
  okWidget=XmStringCreateLtoR("Dismiss",XmSTRING_DEFAULT_CHARSET);
  n=0;
  XtSetArg(args[n],XmNdialogTitle,titleWidget); n++;
  XtSetArg(args[n],XmNokLabelString,okWidget); n++;
  XtSetArg(args[n],XmNmessageString,messWidget); n++;

  mess_box=XmCreateMessageDialog(localShell,name,args,n);
  button=XmMessageBoxGetChild(mess_box,XmDIALOG_CANCEL_BUTTON);
  XtUnmanageChild(button);
  button=XmMessageBoxGetChild(mess_box,XmDIALOG_HELP_BUTTON);
  XtUnmanageChild(button);
  XmStringFree(messWidget); XmStringFree(titleWidget); XmStringFree(okWidget);
  XtManageChild(mess_box);
}
void MakeMenuItem(int actionNumber,Widget mpane,char *item,XtCP cb) {
  Widget but;
  but=XmCreatePushButton(mpane,item,NULL,0);
  XtManageChild(but);
  XtAddCallback(but,XmNactivateCallback,cb,(void*)(1000*actionNumber+gNWin));
}
void FinishThisMenu(Widget mbar,Widget mpane,char *menuName) {
  ARGS
  Widget cas;
  nn=0;
  XtSetArg(args[nn],XmNsubMenuId,mpane); nn++;
  cas=XmCreateCascadeButton(mbar,menuName,args,nn);
  XtManageChild(cas);
}
void QuitCB(Widget w,caddr_t cld,caddr_t cad) {
  PP"Normal end of STAR table browser.\n"); exit(0);
}
void Complain5(void) {
 Say("\
 You wrote too much to the bottom part of\n\
 the window.\n\
 Try restricting the amount you write with\n\
 the upper right part of the window.\n\
 \n\
 Sorry for this clunky programming.  Extra\n\
 rows should scroll off the top.\n\
 ");
}
void Write(char *mess,int whWin) {
  if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(108);
  if(strlen(gWin[whWin]->textOutput)+strlen(mess)>TEXT_SIZE_PART_3-5) {
    Complain5(); gWin[whWin]->textOutput[0]='\0'; gBreakRowsLoop=TRUE;
  }
  strcat(gWin[whWin]->textOutput,mess);
  /*-------------------------------------------------------------
  XmTextSetString(gWin[whWin]->txtWidWrite,gWin[whWin]->textOutput);
  -------------------------------------------------------------*/
  XmTextInsert(gWin[whWin]->txtWidWrite,
      (XmTextPosition)strlen(gWin[whWin]->textOutput),mess);
  XmTextSetInsertionPosition(gWin[whWin]->txtWidWrite,
      (XmTextPosition)strlen(gWin[whWin]->textOutput)-1);
  XmUpdateDisplay(gWin[whWin]->txtWidWrite);
}
myBool GetTheCuts(char *out,char *tableName) {
 FILE *ff; char *tok,fn[150],line[MAX_CUTS_STRING+5]; myBool fo;
  sprintf(fn,"stargl.cuts"); ff=fopen(fn,"r");
  if(ff==NULL) { Say("You have no cuts file.\nSee \"Help\"."); return FALSE; }
  fo=FALSE;
  while(fgets(line,MAX_CUTS_STRING,ff)) {
    if(*line=='#') continue;
    tok=strtok(line," \t");
    if(!strcmp(tableName,tok)) { fo=TRUE; break; }
  } fclose(ff);
  if(!fo) {
    Say("Cuts file has no line for this table \nSee \"Help\"."); return FALSE;
  }
  tok=strtok(NULL," \n\t"); strcpy(out,tok);
  return TRUE;
}
char *ColName(int ln,int whWin) {
  int ii,nl=0,len; char *cc,*rr;
  len=strlen(gWin[whWin]->textClickPart);
  for(ii=0;ii<len;ii++) {
    if(nl>=ln) break; if(gWin[whWin]->textClickPart[ii]=='\n') nl++;
  }
  strncpy(gPass,(gWin[whWin]->textClickPart)+ii,40); gPass[38]='\0';
  rr=strstr(gPass,"( "); if(rr!=NULL) rr[1]='@';
  cc=strtok(gPass," ");
  for(ii=strlen(cc)-1;ii>=0;ii--) if(cc[ii]=='@') cc[ii]=' ';
  return cc;
}
char *RadStr(int whWin) {
  switch(gWin[whWin]->whichRadio) {
    case 0: strcpy(gPass,"AllRow"); break;
    case 1: strcpy(gPass,"Range"); break;
    case 2: strcpy(gPass,"Cuts"); break;
    case 3: strcpy(gPass,"Next10"); break;
    default: Err(121);
  }
  return gPass;
}
void AddPs(char *x) {
  gNPs += strlen(x); if(gNPs<PS-4) strcat(gPs,x);
  else Err(131);
}
void InitPs(void) {
  *gPs='\0'; gNPs=0;
  AddPs("%!\n/Times-Roman findfont\n15 scalefont\nsetfont\n");
}
void DrawStringPsOnly(float xf,float yf,char *cc) {
  float xx1,yy1; char buf[150],copy[310];
  int jj,len,ii;
  strncpy(copy,cc,147); copy[149]='\0'; ii=0; len=strlen(copy);
  while(ii<len) {
    if(copy[ii]=='('||copy[ii]==')') {
        for(jj=len;jj>=ii;jj--) copy[jj+1]=copy[jj]; copy[ii]='\\'; ii++;
    }
    ii++;
  }
  xx1=LEFTPS+xf*WIDTH; yy1=BOTTPS+yf*HEIGH;
  sprintf(buf,"%5.1f %5.1f moveto\n(%s) show\n",xx1,yy1,copy); AddPs(buf);
}
void DrawString(float xf,float yf,char *cc) {
  /* tees to both postscript and to X */
  int x,y;
  yf -= 0.04;
  DrawStringPsOnly(xf,yf,cc);
  x=(xf*(GRAPHWIDTH-RIGHTMARGIN-LEFTMARGIN))+LEFTMARGIN;
  y=-( (yf*(GRAPHHITE-BOTTOMMARGIN-TOPMARGIN))+BOTTOMMARGIN-GRAPHHITE );
  DrawStringXOnly(x,y,cc);
}
void DoBox(void) {
  DrawLine(0.0,0.0,0.0,1.0); DrawLine(0.0,0.0,1.0,0.0);
  DrawLine(0.0,1.0,1.0,1.0); DrawLine(1.0,1.0,1.0,0.0);
}
void Complain1(void) {
  Say("You can only highlight\none column for 1D histograms.");
}
void Complain2(void) {
  /* Two reasons for this: gHist[] only 1d, and draw.c not coded for >1. */
  Say("Only one graphics window at a time.\nPlease close the one you have.");
}
char *Date(void) {
  FILE *ff; int len,ii;
  system("date '+%D %T' > brow.temp0");
  ff=fopen("brow.temp0","r"); if(ff==NULL) *gPass='\0';
  else {
    if(!fgets(gPass,50,ff)) *gPass='\0';
    else { len=strlen(gPass); gPass[len-1]='\0'; }
  }
  len=strlen(gPass);
  for(ii=len-1;ii>=0;ii--) if(gPass[ii]=='/') gPass[ii]='-';
  for(ii=len-1;ii>=0;ii--) if(gPass[ii]==':') { gPass[ii]='\0'; break; }
  return gPass;
}
float TickSize(float min,float max) {
  /* space between clicks in same units as the axis */
  int theLog,ii; double iid; float try; theLog=log10(max-min); try=1e25;
  for(ii=theLog-5;ii<theLog+5;ii++) {
    iid=ii;
    try=1.0*pow(10.0,iid); if((max-min)/try<MAXTICKS) break;
    try=5.0*pow(10.0,iid); if((max-min)/try<MAXTICKS) break;
  }
  return try;
}
float Tick1(float min,float ticksize) { /* value at beginning of axis */
  int xx;  float rv;
  xx=(min/ticksize); if(xx<=0) xx--;
  rv=1.0*xx*ticksize;
  return rv;
}
float Tick2(float max,float ticksize) { /* value at end of axis */
  int xx; float rv;
  xx=(max/ticksize)+1; if(xx<0) xx++; rv=1.0*xx*ticksize;
  return rv;
}
void DrawHist(int whWin) {
  Widget junk1; caddr_t junk2,junk3;
  float maxF; int maxI,ii,jj;
  float offX,offY,deltaX1,deltaY1,deltaY2,deltaX2;
  float x2c,x1c,x1,y1,x2,y2,tsY,tsX,minnX,maxxY,minnY,maxxX;
  float y1c,y2c;
  char label[111],name[40],buf[40];
  if(gNGraphicsUp>0) { Complain2(); return; } MakeWindow(0,WIN_TYPE_GRAPHICS);
  InitPs();
  maxI=0; for(ii=HIST-1;ii>=0;ii--) if(maxI<gHist[ii]) maxI=gHist[ii];
  gUp=TOPMARGIN; gDown=GRAPHHITE-BOTTOMMARGIN; gLeft=LEFTMARGIN;
  gRight=GRAPHWIDTH-RIGHTMARGIN; maxF=maxI;
  tsX=TickSize(gMin,gMax); minnX=Tick1(gMin,tsX); maxxX=Tick2(gMax,tsX);
  tsY=TickSize(0.0,maxF);  minnY=0.0;             maxxY=Tick2(maxF,tsY);
  strcpy(name,ColName(gHistWhLine,whWin));
  strcpy(buf,RadStr(whWin));
  sprintf(label,"%s(%s).%s",gWin[whWin]->tableName,buf,name);
  offX=gMin-minnX; deltaX1=gMax-gMin; deltaX2=maxxX-minnX;
  offY=0.0;        deltaY1=maxI;      deltaY2=maxxY-minnY;
  DoBox();
  for(ii=0;ii<=HIST;ii++) { /* the hist, see comment 91t about x1c and x2c */
    jj=ii-1;
                 x1=(1.0*(0.5+jj))/(HIST-1) + 0.5/HIST;
                 x2=(1.0*(0.5+ii))/(HIST-1) + 0.5/HIST;
    if(jj>=   0) y1=(1.0*gHist[jj])/maxI; else y1=0.0;
    if(ii< HIST) y2=(1.0*gHist[ii])/maxI; else y2=0.0;
    x1c=((deltaX1*x1)+offX)/deltaX2; x2c=((deltaX1*x2)+offX)/deltaX2;
    y1c=((deltaY1*y1)+offY)/deltaY2; y2c=((deltaY1*y2)+offY)/deltaY2;
    DrawLine(x1c,y1c,x1c,y2c); DrawLine(x1c,y2c,x2c,y2c);
  }
  DrawString(MAINLABELX,MAINLABELY+0.05,label);
  DrawString(MAINLABELX,MAINLABELY-0.05,Date());
  DrawXaxisTicks(minnX,maxxX,tsX);
  DrawYaxisTicks(minnY,maxxY,tsY);
  ExposeCB(junk1,junk2,junk3);
}
void SayError0(void) {
 Say("\
 You have to highlight at least one column\n\
 so I'll have something to output.\n\
 See \"Using this window\" under \"Help\".\n\
 ");
}
void BotBigCB(Widget ww,caddr_t cld,caddr_t cad) {
  ARGS
  short rr;
  int whWin; whWin=(int)cld;
  rr=gWin[whWin]->nRowsWritePart; rr+=4; gWin[whWin]->nRowsWritePart=rr;
  nn=0;
  XtSetArg(args[nn],XmNrows,rr); nn++;
  XtSetValues(gWin[whWin]->txtWidWrite,args,nn);
}
void CloseThisWindowCB(Widget w,caddr_t cld,caddr_t cad) {
  int whWin; whWin=(int)cld;
  if(gWin[whWin]->win_type==WIN_TYPE_GRAPHICS) gNGraphicsUp--;
  XtPopdown(gWin[whWin]->shell); XtDestroyWidget(gWin[whWin]->shell);
}
void RunNull(size_t row) { /* Menu item just counts selected rows. */
}
void RunHistMinMax(size_t row) {
  int irow=row; float val;
  val=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
      gWin[gRunWhWin]->tlm[gRunWhichHilitedLine],irow,
      gWin[gRunWhWin]->subscript[gRunWhichHilitedLine]);
  if(gTableValueError) {
    Say("Table has unsupported data type"); gBreakRowsLoop=TRUE;
  }
  if(gMin>val) gMin=val; if(gMax<val) gMax=val;
}
void RunHistFill(size_t row) {
  int hist,irow=row; float val;
  val=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
      gWin[gRunWhWin]->tlm[gRunWhichHilitedLine],irow,
      gWin[gRunWhWin]->subscript[gRunWhichHilitedLine]);
  if(gTableValueError) {
    Say("Table has unsupported data type"); gBreakRowsLoop=TRUE;
  }
  hist=(HIST-1)*(val-gMin)/(gMax-gMin);
  if(hist>=HIST||hist<0) {
    PP"hist=%d, HIST=%d, gMax=%e, gMin=%e\n",hist,HIST,gMax,gMin);
    exit(2);
  }
  gHist[hist]++;
}
void RunAverage(size_t row) {
  int irow=row;
  gRunTotal+=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
      gWin[gRunWhWin]->tlm[gRunWhichHilitedLine],irow,
      gWin[gRunWhWin]->subscript[gRunWhichHilitedLine]);
  if(gTableValueError) {
    Say("Table has unsupported data type"); gBreakRowsLoop=TRUE;
  }
}
void RunValue(size_t row) {
  int fo=0,ii,irow=row; float val;
  char tt2[60],format[22],tmp[100],buf[422];
  sprintf(buf,"%s%6d",gSumCol,row+1);
  if(strlen(buf)!=EXT) {
    PP"buf=%s, len should be %d.\n",buf,EXT); exit(2);
  }
  for(ii=0;ii<gWin[gRunWhWin]->nLineClickPart;ii++) { /* user-selected cols */
    if(!gWin[gRunWhWin]->isHilited[ii]) continue; fo++;
    val=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
          gWin[gRunWhWin]->tlm[ii],irow,gWin[gRunWhWin]->subscript[ii]);
    if(gTableValueError) {
      Say("Table has unsupported data type"); gBreakRowsLoop=TRUE; break;
    }
    Format(WIDE-1,tmp,val);
    sprintf(format,"%%%ds",WIDE); sprintf(tt2,format,tmp);
    strcat(buf,tt2);
  }
  if(fo>0) { strcat(buf,"\n"); Write(buf,gRunWhWin);  }
  else if(gRunNRowsDone==0) SayError0();
}
void OneLnPerRowCB(Widget w,caddr_t cld,caddr_t cad) { /* see Comment 8b */
  int whAct,whWin; char act[111];
  void (*runFunc)();
  whWin=((int)cld)%1000; whAct=((int)cld)/1000;
  gRunWhWin=whWin; if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(128);
  switch(whAct) {
    case 0: strcpy(act,"Value"); runFunc=RunValue; break;
    default: Err(129);
  }
  sprintf(gSumCol,"%6s ",act); /* for passing to runFunc */
  gRunNRowsDone=0;
  RunTheRows(FALSE,whWin,runFunc);
  if(gRunNRowsDone==0) Say("No rows were\nselected by \"ROW SELECTION\".");
}
/* Comment 8b: OneLnAllRowsCB is the callback for "half" the Action menu
   items.  It decodes the client data into whWin and whAct, selects which
   function pointer should be passed to RunTheRows, handles auxiliary output
   like writing the number of rows used and the cuts string, and loops over
   the selected columns. */
void OneLnAllRowsCB(Widget w,caddr_t cld,caddr_t cad) { /* see Comment 8b */
  int cnt,whAct,nCol=0,fo=0,ii,whWin; char tt[111],sumCol[211];
  float summary;
  myBool firstTime=TRUE,dalo; /* dalo=doAtLeastOne */
  char act[30],tt2[45],format[30];
  void (*runFunc)();
  whWin=((int)cld)%1000; whAct=((int)cld)/1000;
  gRunWhWin=whWin; if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(120);
  /*Write("------------------------------------------------\n",whWin);*/
  switch(whAct) {
    case 0: strcpy(act,"Ave."); runFunc=RunAverage; dalo=FALSE; break;
    case 1: strcpy(act,"Bug1"); runFunc=RunNull; dalo=TRUE; break;
    case 2: strcpy(act,"Bug2"); runFunc=RunHistMinMax; dalo=FALSE;
      gMin=1e25; gMax=-1e25; break;
    default: Err(125);
  }
  sprintf(sumCol,"%6s %6s",act,RadStr(whWin));
  if(strlen(sumCol)!=EXT) Err(122); cnt=0;
  for(ii=0;ii<gWin[whWin]->nLineClickPart;ii++) {
    /* over user-selected cols */
    if(!gWin[whWin]->isHilited[ii] && !(ii==0&&dalo) ) continue;
    if(whAct==2&&++cnt>1) { Complain1(); return; } gHistWhLine=ii;
    if(++nCol>MCOL) Err(123); /*ShouldBeProtectedInSetHiliteWIN_TYPE_TABLE*/
    fo++; gRunTotal=0; gRunNRowsDone=0; gRunWhichHilitedLine=ii;
    if(firstTime) { firstTime=FALSE; RunTheRows(FALSE,whWin,runFunc); }
    else RunTheRows(TRUE,whWin,runFunc);
    if(gRunNRowsDone>0) {
      switch(whAct) {
        case 0: summary=gRunTotal/gRunNRowsDone;
          Format(WIDE-1,tt,summary); sprintf(format,"%%%ds",WIDE);
          sprintf(tt2,format,tt); strcat(sumCol,tt2); break;
        case 1: case 2: break;
        default: Err(130);
      }
    }
    else { Write("\"ROW SELECTION\" selected zero rows.\n",whWin); break; }
  }
  if(gMax==gMin) { gMax=gMin+0.1*fabs(gMin); gMin=gMin-0.1*fabs(gMin); }
  if(gMax==gMin) { gMax+=1; gMin-=1; }
  switch(whAct) {
    case 0: if(fo==0) SayError0();
      else { strcat(sumCol,"\n"); Write(sumCol,whWin); } break;
    case 1: break;
    case 2: for(ii=HIST-1;ii>=0;ii--) gHist[ii]=0;
      if(fo==0) { SayError0(); return; }
      RunTheRows(FALSE,whWin,RunHistFill); break;
    default: Err(126);
  }
  AuxOutputFromOneLn(whWin,whAct);
}
void AuxOutputFromOneLn(int whWin,int whAct) { /* includes histogramming */
  char bf[222],car[30];
  myBool reportCuts,reportRows;
  switch(gWin[whWin]->whichRadio) {
    case 0: case 3: reportRows=FALSE; reportCuts=FALSE; break;
    case 1:         reportRows=TRUE;  reportCuts=FALSE; break;
    case 2:         reportRows=TRUE;  reportCuts=TRUE;  break;
    default: Err(124);
  }
  switch(strlen(RadStr(whWin))) {
    case 4: strcpy(car,"^^^^"); break; case 7: strcpy(car,"^^^^^^^"); break;
    case 5: strcpy(car,"^^^^^"); break; case 6: strcpy(car,"^^^^^^"); break;
    default: Err(127);
  }
  switch(whAct) {
    case 0:
      if(reportRows) { sprintf(bf,"%6s %6s This represents %d row(s).\n","",
        car,gRunNRowsDone); Write(bf,whWin);
      } break;
    case 1:
      sprintf(bf,"Your \"ROW SELECTION\" (top right) selects %d row(s).\n",
      gRunNRowsDone); Write(bf,whWin); break;
    case 2: DrawHist(whWin); break;
    default: Err(133);
  }
  if(reportCuts) { Write(gCuts,whWin); Write("\n",whWin); }
} /* end of OneLnAllRowsCB() */
void Complain6(void) {
 Say("\
 You did not type properly in the box labeled\n\
 \"Type range\".   Example: you want rows 12 to 24,\n\
 type \"12-24\" or \"12:24\" or \"12 24\"\n\
 or \"12 to 24\".\n\
 ");
}
void RunTheRows(myBool skipInit,int whWin,void (*fnct)()) {
  size_t row,start,end; char cuts[MAX_CUTS_STRING];
  static char ba[MAXROW_DIV_BY_8];
  if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(109);
  start=FirstRow(whWin); end=LastRow(skipInit,whWin);
  if(start>end) { Complain6(); return; }
  if(!skipInit&&gWin[whWin]->useCuts) {
    if(!GetTheCuts(cuts,gWin[whWin]->tableName)) return;
    if(!DoCutsWrapper(MAXROW_DIV_BY_8,ba,cuts,gWin[whWin]->wh_gDs)) {
      Say("We have error 51r.\nCheck your cuts string."); return;
    }
    strncpy(gCuts,cuts,COL);
    strcpy(gCuts+COL-8," etc...");
  }
  gBreakRowsLoop=FALSE;
  for(row=start;row<=end;row++) {
    /* ActionCB/RunAverage are a simple example of how to use this */
    if(gWin[whWin]->useCuts) { if(!RowPassedCuts(ba,(long)row)) continue; }
    fnct(row);
    gRunNRowsDone++;
    if(gBreakRowsLoop) break;
  }
}
void DumpPsCB(Widget w,caddr_t cld,caddr_t cad) {
  FILE *ff; char fn[111];
  strcpy(fn,"plot.ps");
  ff=fopen(fn,"w");
  if(ff==NULL) { Say("I can't write in this directory."); return; }
  fprintf(ff,"%s",gPs);
  fprintf(ff,"\nshowpage\n");
  fclose(ff);
  Say("Your file is named plot.ps.");
}
void DoCB(Widget w,caddr_t cld,caddr_t cad) {
  gCalculateAverages=TRUE;
}
void SkipCB(Widget w,caddr_t cld,caddr_t cad) {
  gCalculateAverages=FALSE;
}
void SigFigCB(Widget w,caddr_t cld,caddr_t cad) { /* pops it up */
  Say("This does not work yet.");
  /* XtPopup(gSigFigScalePopup,XtGrabNone); */
}
void HelpSelTabCB(Widget w,caddr_t cld,caddr_t cad) {
 Say("\
 The top part of this window gives info on the\n\
 data set you selected earlier.  The bottom part\n\
 is a list of the tables which are available in\n\
 this dataset.\n\
 \n\
 Click on the table that you want to look at.\n\
 A new window will pop up.  You can pop up as\n\
 many such windows as you like.  Further help\n\
 is given there.\n\
 ");
}
void HelpUtwCB(Widget w,caddr_t cld,caddr_t cad) {
 Say("\
 This window represents one table.\n\
 \n\
 In the middle is a list of the table's columns.\n\
 \n\
 Highlight a column by clicking in this list.\n\
 Un-highlight by clicking again.\n\
 \n\
 CRITICAL:\n\
 Items from the \"Action\" menu only work on rows\n\
 selected in the top right corner of the window, so be care-\n\
 ful with row-wise wholesale actions like averaging and his-\n\
 tograms when you have a subtle \"ROW SELECTION\" like \"Cuts\".\n\
 ");
}
void HelpCutsCB(Widget w,caddr_t cld,caddr_t cad) {
  *gPass='\0';
  strcat(gPass,"Cuts are held in file stargl.cuts.\n");
  strcat(gPass,"A sample five-line cuts file is:\n\n");
  strcat(gPass,"# Comment line\n");
  strcat(gPass,"globtrk  id.gt.10.and.id.le.110\n");
  strcat(gPass,"# globtrk  (id.eq.1.or.icharge.eq.1).and.(invpt.gt.7)\n");
  strcat(gPass,"tphit    id_globtrk.gt.10.and.id_globtrk.le.110\n");
  strcat(gPass,"tptrack    cov[5].gt.3.12\n");
  strcat(gPass,"Each non-comment line begins with a table name.\n");
  strcat(gPass,"Specifiers in the conditionals (eg, id_globtrk) are\n");
  strcat(gPass,"column names.  Note the use of\n");
  strcat(gPass,"square brackets (NOT PARENTHESES, AS IN TAS)\n");
  strcat(gPass,"for columns whose data types are arrays.\n");
  strcat(gPass,"\n");
  strcat(gPass,"\n");
  strcat(gPass,"You can alter your cuts while running this table brow-\n");
  strcat(gPass,"ser by opening the cuts file with an editor, making your\n");
  strcat(gPass,"changes, and saving the changes (without quitting the\n");
  strcat(gPass,"editor, if you desire).\n");
  Say(gPass);
}
void HelpBugRptCB(Widget w,caddr_t cld,caddr_t cad) {
  Say("Email ward@physics.utexas.edu");
}
void CreateMenuItems(Widget mbar,int type) {
  register int nn; Arg args[19]; Widget mpane;
  mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
  if(type!=WIN_TYPE_PRIMARY) {
    MakeMenuItem(NOTUSED,mpane,"Close this window",(XtCP)CloseThisWindowCB);
  }
  MakeMenuItem(NOTUSED,mpane,"Quit",(XtCP)QuitCB);
  FinishThisMenu(mbar,mpane,"File ");
  if(type==WIN_TYPE_TABLE) {
    mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
    MakeMenuItem(0,mpane,"Show value(s).",             (XtCP)OneLnPerRowCB);
    MakeMenuItem(0,mpane,"Show average.",              (XtCP)OneLnAllRowsCB);
    MakeMenuItem(1,mpane,"How many rows are selected?",(XtCP)OneLnAllRowsCB);
    MakeMenuItem(2,mpane,"Show 1d histogram.",         (XtCP)OneLnAllRowsCB);
    FinishThisMenu(mbar,mpane,"Action ");
  }
  if(type==WIN_TYPE_GRAPHICS) {
    mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
    MakeMenuItem(NOTUSED,mpane,"Write .ps file of histogram",(XtCP)DumpPsCB);
    FinishThisMenu(mbar,mpane,"WritePostScript ");
  }
  if(type==WIN_TYPE_TABLE||type==WIN_TYPE_DATASET) {
    mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
    if(type==WIN_TYPE_DATASET) {
      MakeMenuItem(NOTUSED,mpane,"Skip averages (faster)",(XtCP)SkipCB);
      MakeMenuItem(NOTUSED,mpane,"Calc averages (slower)",(XtCP)DoCB);
    }
    if(type==WIN_TYPE_TABLE) {
      MakeMenuItem(NOTUSED,mpane,"Format of numerical output",(XtCP)SigFigCB);
      MakeMenuItem(NOTUSED,mpane,"Bottom part bigger (may be iterated)",
        (XtCP)BotBigCB);
    }
    FinishThisMenu(mbar,mpane,"Preferences ");
  }
  mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
  if(type==WIN_TYPE_DATASET) {
    MakeMenuItem(NOTUSED,mpane,"Using this window",(XtCP)HelpSelTabCB);
  }
  if(type==WIN_TYPE_TABLE) {
    MakeMenuItem(NOTUSED,mpane,"Using this window",(XtCP)HelpUtwCB);
    MakeMenuItem(NOTUSED,mpane,"Cuts",(XtCP)HelpCutsCB);
  }
  MakeMenuItem(NOTUSED,mpane,"Talk to programmer",(XtCP)HelpBugRptCB);
  FinishThisMenu(mbar,mpane,"Help ");

}
void SetToPrimaryInfo(char *xx,int max) {
 strncpy(xx,"\
 ** TOP WINDOW: CHOOSE DATASET(S) **\n\
 Below is a list of the datasets.\n\
 Hierarchy is shown with indentation.\n\
 Click on one (or more) of the datasets\n\
 which are labeled as containing tables.\
 ",max-4);
}
Position Pos(int whWin,Position x,Position y) {
  Widget text;
  Position rv;
  if(whWin<0||whWin>=gNWin) Err(102);
  text=gWin[whWin]->txtWidClick;
  rv=XmTextXYToPos(text,x,y);
  return rv;
}
int LineNumber(int whWin,Position pos) {
  int rv,ii,len; char *text;
  if(whWin<0||whWin>=gNWin) Err(103);
  text=gWin[whWin]->textClickPart;
  len=strlen(text); if(pos>len) Err(107);
  rv=0;
  for(ii=pos-1;ii>=0;ii--) if(text[ii]=='\n') rv++;
  return rv;
}
void Complain(void) {
  char buf[200];
  sprintf(buf,"You can't select any more columns.\nMax = %d.",
  MCOL); Say(buf);
}
void Abbr(char *x) {
  char *pp,first[66],parens[22]; int ii,len,lenPar,lenForRest;
  if(strstr(x,"(")) {
    pp=strstr(x,"( "); if(pp!=NULL) {
      len=strlen(pp); for(ii=1;ii<len;ii++) pp[ii]=pp[ii+1];
    }
    strcpy(parens,strstr(x,"(")); strcpy(first,x); strtok(first,"()");
    lenPar=strlen(parens); lenForRest=WIDE-1-lenPar; first[lenForRest-1]='\0';
    sprintf(x,"%s%s",first,parens);
  } else {
    x[WIDE-2]='\0'; /* 1 for null byte offset, and 1 for space between cols */
  }
}
void SetHilite(int control,int whWin,int lineNum) {
  /* What the user sees is set in Loop A, what program sees is
  ** gWin[]->isHilited[].  Up to programmer to keep these two equal. */
  char buf[WIDE*MCOL+EXT+1]; /* see comments in x.h for meaning of MCOL etc */
  char tmp[111],format[22];
  char cp[43],*otext; int num=0,len,ii,ln; myBool doReturn=FALSE;
  XmTextPosition left,right;
  Widget txtWid;
  if(gWin[whWin]->win_type==WIN_TYPE_TABLE) {
    /* erase write part and adjust its header txtWidWriteH */
    sprintf(format,"%%%ds",WIDE); /* -1 for space between columns */
    strcpy(buf,"  What RowNum"); /* EXT = size of this string */
    for(ii=0;ii<gWin[whWin]->nLineClickPart;ii++) { /* Loop A */
      if((ii!=lineNum&& gWin[whWin]->isHilited[ii]) ||
      (ii==lineNum&&!gWin[whWin]->isHilited[ii])) {
        if(num++>=MCOL) { Complain(); doReturn=TRUE; break; }
        strcpy(cp,ColName(ii,whWin)); Abbr(cp); sprintf(tmp,format,cp);
        if(strlen(tmp)+strlen(buf)>WIDE*MCOL+EXT) {
          PP"This is error 91P. %d + %d > %d, MCOL = %d.\n",
          strlen(tmp),strlen(buf),WIDE*MCOL+EXT,MCOL); exit(2);
        } strcat(buf,tmp);
      }
    }
    if(doReturn) return; gWin[whWin]->textOutput[0]='\0';
    XmTextSetString(gWin[whWin]->txtWidWrite,gWin[whWin]->textOutput);
    XmTextSetString(gWin[whWin]->txtWidWriteH,buf);
  }
  txtWid=gWin[whWin]->txtWidClick;
  otext=gWin[whWin]->textClickPart;
  len=strlen(otext); left=0; right=len-1; ln=0;
  for(ii=0;ii<len;ii++) {
    if(otext[ii]=='\n') { right=ii; if(ln==lineNum) break; left=ii+1; ln++; }
  }
  if(control==HILITE_TURN_ON||!gWin[whWin]->isHilited[lineNum]) {
    gWin[whWin]->isHilited[lineNum]=TRUE;
    XmTextSetHighlight(txtWid,left,right,XmHIGHLIGHT_SELECTED);
  } else {
    gWin[whWin]->isHilited[lineNum]=FALSE;
    XmTextSetHighlight(txtWid,left,right,XmHIGHLIGHT_NORMAL);
  }
  XmUpdateDisplay(txtWid);
}
XtCP TextCB(Widget w,caddr_t cld,caddr_t cad) { /* user click in text window */
  int whWin,whatThisLinePointsTo;
  /* whatThisLinePointsTo is either an index for gDs
  or it is an arg (cast to size_t) for dsColumnName (column number). */
  XmAnyCallbackStruct *xx; Position pos;
  XButtonEvent *bev; int lineNumber;
  whWin=(int)cld;
  if(whWin<0||whWin>=gNWin) Err(104);
  xx=(XmAnyCallbackStruct*)cad;
  bev=(XButtonEvent*)(xx->event); /* Vol1 p512 */
  pos=Pos(whWin,(Position)(bev->x),(Position)(bev->y));
  lineNumber=LineNumber(whWin,pos);
  if(lineNumber<0||lineNumber>=gWin[whWin]->nLineClickPart) return 0;
  whatThisLinePointsTo=gWin[whWin]->tlm[lineNumber];
  switch(gWin[whWin]->win_type) {
    case WIN_TYPE_PRIMARY: /* user clicked in primary window */
      MakeWindow(whatThisLinePointsTo,WIN_TYPE_DATASET);
      SetHilite(HILITE_TURN_ON,whWin,lineNumber); /*BBB un-hilite when closed*/
      break;
    case WIN_TYPE_DATASET: /* user clicked in dataset window, chose a table */
      SetHilite(HILITE_TURN_ON,whWin,lineNumber); /*BBB un-hilite when closed*/
      MakeWindow(whatThisLinePointsTo,WIN_TYPE_TABLE);
      break;
    case WIN_TYPE_TABLE: /* user has selected a column */
      SetHilite(HILITE_TOGGLE,whWin,lineNumber); /*tlmUsedInRunAverage()Etc*/
      break;
    default: Err(106);
  }
}
Widget TxtWid(Widget par,char *itxt,int topMidBot,int nRow,int nCol) {
  ARGS
  String translations = "<Btn1Down>: activate()";
  Widget rv;
  nn=0;
  XtSetArg(args[nn],XmNresizeWidth, False);  nn++;
  XtSetArg(args[nn],XmNresizeHeight, False);  nn++;
  XtSetArg(args[nn],XmNeditMode, XmMULTI_LINE_EDIT);  nn++;
  XtSetArg(args[nn],XmNeditable,False); nn++;
  XtSetArg(args[nn],XmNscrollHorizontal, False);  nn++;
  switch(topMidBot) {
    case 2: case 1:
      XtSetArg(args[nn],XmNscrollVertical, True);  nn++;
      break;
    case 0:
      break;
    default: Err(111);
  }
  XtSetArg(args[nn],XmNrows,nRow);  nn++;
  XtSetArg(args[nn], XmNcolumns, nCol);  nn++;
  XtSetArg(args[nn], XmNcursorPositionVisible,False); nn++;
  if(topMidBot==2) {
    XtSetArg(args[nn], XmNautoShowCursorPosition, True);  nn++;
  }
  if(topMidBot==2||topMidBot==1) {
    rv=XmCreateScrolledText(par,"TxtWid",args,nn);
  } else {
    rv=XmCreateText(par,"TxtWid",args,nn);
  }
  XtManageChild(rv);
  XmTextSetString(rv,itxt);
  XtOverrideTranslations(rv,XtParseTranslationTable(translations));
  if(topMidBot==1)
      XtAddCallback(rv,XmNactivateCallback,(XtCP)TextCB,(void*)gNWin);
  return rv;
}
Widget LabelWidget(Widget parent,char *xx) {
  Arg args[5]; register int nn; XmString string; Widget rv;
  string=XmStringCreateLtoR(xx,XmSTRING_DEFAULT_CHARSET);
  nn=0;
  XtSetArg(args[nn],XmNlabelString,string); nn++;
  rv=XmCreateLabel(parent,"xw",args,nn);
  XtManageChild(rv);
  XmStringFree(string);
  return rv;
}
Widget RBut(Widget par,char *lab,XtCP cb,int cbData,myBool on) {
  Widget scratch;
  scratch=XtVaCreateManagedWidget(lab,
      xmToggleButtonGadgetClass,par,NULL);
  XtAddCallback(scratch,XmNvalueChangedCallback,cb,(void*)cbData);
  if(on) XmToggleButtonSetState(scratch,True,False);
  else XmToggleButtonSetState(scratch,False,False);
  return scratch;
}
void RowCB(Widget w,caddr_t cld,caddr_t cad) {
  int ii,whWin;
  whWin=((int)cld)/10;
  for(ii=NUM_RADIO-1;ii>=0;ii--) {
    if(XmToggleButtonGadgetGetState(gWin[whWin]->rad[ii])) break;
  }
  if(ii<0) Err(112);
  gWin[whWin]->whichRadio=ii;
  if(ii==1) XtSetSensitive(gVer2,True); else XtSetSensitive(gVer2,False);
}
size_t FirstRow(int whWin) {
  size_t rv; char *scratch;
  if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(113);
  gWin[whWin]->useCuts=FALSE;
  switch(gWin[whWin]->whichRadio) { /* see MakeRowSelectionWidget() */
    case 0: rv=0; break;
    case 2: rv=0; gWin[whWin]->useCuts=TRUE; break;
    case 1:
      scratch=XmTextGetString(gWin[whWin]->rowWidget);
      if(atoi(scratch)!=0) rv=atoi(scratch)-1;
      else rv=100; /* see comments 88u */
      XtFree(scratch);
      break;
    case 3: rv=gLast+1; break;
    default: Err(114);
  }
  if(rv<0||rv>=gWin[whWin]->nRow) rv=0;
  return rv;
}
size_t LastRow(myBool skipInit,int whWin) {
  size_t rv; char *scratch,scr[100],*pp;
  if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(115);
  switch(gWin[whWin]->whichRadio) { /* see MakeRowSelectionWidget() */
    case 2: case 0: rv=gWin[whWin]->nRow-1; break;
    case 1: scratch=XmTextGetString(gWin[whWin]->rowWidget);
      strcpy(scr,scratch); strtok(scr," :-to,"); pp=strtok(NULL," :-to,");
      if(atoi(pp)!=0) rv=atoi(pp)-1;
      else rv=99; /* see comments 88u */
      XtFree(scratch); break;
    case 3: rv=gLast+10; break;
    default: Err(116);
  }
  if(rv<0||rv>=gWin[whWin]->nRow) rv=gWin[whWin]->nRow-1;
  if(!skipInit) {
    gLast=rv; if(gWin[whWin]->whichRadio==2) gLast=0; /* 2 -> cuts */
  }
  return rv;
}
void MakeText(Widget par) {
  ARGS
  Widget rv;
  nn=0;
  XtSetArg(args[nn],XmNresizeWidth, False);  nn++;
  XtSetArg(args[nn],XmNresizeHeight, False);  nn++;
  XtSetArg(args[nn],XmNeditMode, XmSINGLE_LINE_EDIT);  nn++;
  XtSetArg(args[nn],XmNeditable,True); nn++;
  XtSetArg(args[nn],XmNscrollHorizontal, False);  nn++;
  XtSetArg(args[nn],XmNscrollVertical, False);  nn++;
  XtSetArg(args[nn],XmNrows,1);  nn++;
  XtSetArg(args[nn], XmNcolumns, 13);  nn++;
  XtSetArg(args[nn], XmNcursorPositionVisible,True); nn++;
  rv=XmCreateText(par,"TxtWid",args,nn);
  XtManageChild(rv);
  gWin[gNWin]->rowWidget=rv;
}
void MakeRowSelectionWidget(Widget parent) {
  Widget ver2,hor,rb,ver; int Q=0;
  hor=Row(parent);
  ver2=Column(hor);
  LabelWidget(ver2,"ROW SELECTION:");
  gVer2=Column(ver2);
  LabelWidget(gVer2,"");
  LabelWidget(gVer2,"Type range:");
  MakeText(gVer2);
  XtSetSensitive(gVer2,False);
  ver=Column(hor);
  rb=XmCreateRadioBox(ver,"rb",NULL,0);
  gWin[gNWin]->rad[Q]=RBut(rb,"All",(XtCP)RowCB,10*gNWin+Q, TRUE); Q++;
  gWin[gNWin]->rad[Q]=RBut(rb,"Range",(XtCP)RowCB,10*gNWin+Q,FALSE); Q++;
  gWin[gNWin]->rad[Q]=RBut(rb,"Cuts",(XtCP)RowCB,10*gNWin+Q,FALSE); Q++;
  gWin[gNWin]->rad[Q]=RBut(rb,"Next 10",(XtCP)RowCB,10*gNWin+Q,FALSE); Q++;
  if(Q!=NUM_RADIO) Err(117);
  XtManageChild(rb);
}
void MakeWindow(int wh_gDs,int type) { /* one of WIN_TYPE_XXX */
  ARGS
  Widget hor,ls,ver,mbar,mw; int ii,nLines; size_t nRow;
  char name[NAME+2],header[100];
  nn=0;
  if(type==WIN_TYPE_PRIMARY) {
    gMainWindow=XmCreateMainWindow(gAppShell,"0hhh",args,nn); mw=gMainWindow;
  } else {
    if(type==WIN_TYPE_DATASET) strcpy(name,"Dataset Browser");
    else if(type==WIN_TYPE_TABLE) strcpy(name,"Table Browser");
    else if(type==WIN_TYPE_GRAPHICS) strcpy(name,"Table Browser Graphics");
    else Err(134);
    ls=XtCreatePopupShell(name,transientShellWidgetClass,gAppShell,args,0);
    mw=XmCreateMainWindow(ls,"0hhh",args,nn);
  }
  XtManageChild(mw);
  mbar=XmCreateMenuBar(mw,"mbar",args,0); XtManageChild(mbar);
  CreateMenuItems(mbar,type);
  gWin[gNWin]=malloc(sizeof(WINDOW_INFO)); if(gWin[gNWin]==0) Err(100);
  for(ii=MAX_LINES_CLICK_PART-1;ii>=0;ii--) gWin[gNWin]->isHilited[ii]=FALSE;
  gWin[gNWin]->win_type=type; ver=Column(mw);
  switch(type) {
    case WIN_TYPE_PRIMARY: /* make primary window (for choosing a dataset) */
      gWin[gNWin]->rowSel=ROW_SEL_NOT_USED;
      SetToPrimaryInfo(gWin[gNWin]->textTop,TEXT_SIZE_PART_1);
      DatasetList(&nLines,gWin[gNWin]->tlm,MAX_LINES_CLICK_PART,
              gWin[gNWin]->textClickPart,TEXT_SIZE_PART_2);
      gWin[gNWin]->txtWidTop=TxtWid(ver,gWin[gNWin]->textTop,0,5,48);
      gWin[gNWin]->txtWidClick=TxtWid(ver,gWin[gNWin]->textClickPart,1,8,46);
      gWin[gNWin]->txtWidWrite=NULL;
      break;
    case WIN_TYPE_DATASET: /* we are making a window for choosing a table */
      gWin[gNWin]->rowSel=ROW_SEL_NOT_USED;
      SetToDatasetInfo(wh_gDs,gWin[gNWin]->textTop,TEXT_SIZE_PART_1); /*BBB*/
      TableList(header,wh_gDs,&nLines,gWin[gNWin]->tlm,MAX_LINES_CLICK_PART,
              gWin[gNWin]->textClickPart,TEXT_SIZE_PART_2);
      gWin[gNWin]->txtWidTop=TxtWid(ver,gWin[gNWin]->textTop,0,5,43);
      gWin[gNWin]->txtWidClickH=TxtWid(ver,header,0,1,43);
      gWin[gNWin]->txtWidClick=TxtWid(ver,gWin[gNWin]->textClickPart,1,6,41);
      gWin[gNWin]->txtWidWrite=NULL;
      break;
    case WIN_TYPE_TABLE: /* we are making a window for browsing a TAS table */
      gWin[gNWin]->rowSel=ROW_SEL_ALL; gWin[gNWin]->wh_gDs=wh_gDs;
      SetToTableInfo(name,&nRow,wh_gDs,gWin[gNWin]->textTop,TEXT_SIZE_PART_1);
      gWin[gNWin]->nRow=nRow; strncpy(gWin[gNWin]->tableName,name,NAME);
      gWin[gNWin]->tableName[NAME-1]='\0';
      ColumnList(header,wh_gDs,&nLines,gWin[gNWin]->tlm,MAX_LINES_CLICK_PART,
          gWin[gNWin]->textClickPart,TEXT_SIZE_PART_2,
          gWin[gNWin]->subscript);
      gWin[gNWin]->textOutput=malloc(TEXT_SIZE_PART_3);
      if(gWin[gNWin]->textOutput==NULL) Err(118);
      strcpy(gWin[gNWin]->textOutput,"");
      hor=Row(ver);
      gWin[gNWin]->txtWidTop=TxtWid(hor,gWin[gNWin]->textTop,0,7,30);
      MakeRowSelectionWidget(hor);
      gWin[gNWin]->txtWidClickH=TxtWid(ver,header,0,1,COL);
      gWin[gNWin]->txtWidClick=TxtWid(ver,gWin[gNWin]->textClickPart,1,6,COL);
      gWin[gNWin]->txtWidWriteH=TxtWid(ver,"",0,1,47);
      gWin[gNWin]->txtWidWrite=TxtWid(ver,gWin[gNWin]->textOutput,2,RW,COL);
      gWin[gNWin]->nRowsWritePart=RW; /* num Rows in Write part init */
      break;
    case WIN_TYPE_GRAPHICS:
      gWin[gNWin]->rowSel=ROW_SEL_NOT_USED;
      MakeDrawingArea(ver,GRAPHWIDTH,GRAPHHITE); gNGraphicsUp++;
      break;
    default: Err(101);
  }
  gWin[gNWin]->nLineClickPart=nLines; gWin[gNWin]->shell=ls;
  gNWin++; /*BBB chk init */
  if(type!=WIN_TYPE_PRIMARY) XtPopup(ls,XtGrabNone);
  if(type==WIN_TYPE_GRAPHICS) Clear();
}
void DoXStuff(void) {
  ARGS
  XEvent event; /* XButtonEvent *bev; */
  Widget j1; caddr_t j2,j3; int zero=0;
  DoOnce2();
  XtToolkitInitialize();
  gAppCon=XtCreateApplicationContext();
  gDisplay=XtOpenDisplay(gAppCon,NULL,"Dataset Catalog Browser",
      "1hhh",(XrmOptionDescRec*)NULL,0,&zero,NULL);
  if(!gDisplay) {
    PP"STAR Table Browser is an X-windows program.\n");
    PP"I can't open a display.\n");
    PP"Have you set your DISPLAY env var?\n"); exit(2);
  }
  nn=0;
  gAppShell=XtAppCreateShell("Dataset Catalog Browser",
      "starbrowser",applicationShellWidgetClass,gDisplay,args,nn);
  MakeWindow(-10,WIN_TYPE_PRIMARY); /* -10 not used */
  XtRealizeWidget(gAppShell);
  for(;;) {
    XtAppNextEvent(gAppCon,&event);
    /*---------------------------------------
    if(event.type==ButtonPress) {
      bev=(XButtonEvent*)&event; * Vol1 p512 *
      if(bev->button==1) {
        PP"gFocus=%d. (%03d,%03d)  (%03d,%03d)\n",gFocus,
        bev->x_root,bev->y_root, bev->x,bev->y);
      }
    }
    ---------------------------------------*/
    XtDispatchEvent(&event);    /* Vol1 p509 */
  }
}
