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
#include <X11/cursorfont.h>
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
#define TXTOUT "table.dump"
#define EXTERN extern
#include "brow.h"
#include "x.h"
#define CUTS_WIDTH 50
/******************************************************  GLOBALS  **/
extern int gNDs;
int gToFile,gTruncateStrings=7;
FILE *gDump;
int gLastWhWin=-10,gDone2,gDone;
myBool gBlurb2,gBlurb1;
char *gBlurb7="Click on the abbreviation (to\n\
expand/contract) to the left.  Do not click\n\
directly on the names of datasets.  (You can\n\
can click on names of tables.)";
char *gMess0=
"Do not click on this line:\n\
 1. Columns cannot be expanded or contracted.\n\
 2. Columns are not individually viewable.\n\
 3. Perhaps you want to click on the name (not\n\
    on the abbreviation \"TB\") of the table\n\
    which contains this column.";
Widget gSecondLabel,gCutsPopup,gProgressPopup,gCutsText,gProgressScale;
/******************************************************  PROTOTYPES  **/
void HelpCutsCB(Widget w,caddr_t cld,caddr_t cad);
myBool TableHasMoreThanZeroCols(int tlm);
XtCP ExpandCB(Widget w,caddr_t cld,caddr_t cad);
XtCP ExpandExceptCB(Widget w,caddr_t cld,caddr_t cad);
XtCP ContractCB(Widget w,caddr_t cld,caddr_t cad);
myBool ToggleCase(int wds);
myBool ATable(int x);
void Blurb(void),AuxOutputFromOneLn(int whWin,int whAct);
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
void SaveValueOfCalcAve(void) {
  FILE *ff;
  ff=fopen("browser.cDef","w");
  if(ff==NULL) return;
  fprintf(ff,"%d\n",gCalculateAverages); fclose(ff);
}
void Ose(void) {
  PP"------------------------------------------------  STAR TABLE BROWSER\n");
}
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
  fprintf(ff,"\n%s\n","We don't know whether in tas or not."); fclose(ff);

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
void TimeoutCursors(int on) {
    static int locked;
    static Cursor cursor;
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay(gAppShell);

    /* "locked" keeps track if we've already called the function.
     * This allows recursion and is necessary for most situations.
     */
    on? locked++ : locked--;
    if ( (locked > 1) || (locked == 1 && on == 0) )
        return; /* already locked and we're not unlocking */

    if (!cursor) /* make sure the timeout cursor is initialized */
        cursor = XCreateFontCursor(dpy, XC_watch);

    /* if "on" is true, then turn on watch cursor, otherwise, return
     * the shell's cursor to normal.
     */
    attrs.cursor = on? cursor : None;

    /* change the main application shell's cursor to be the timeout
     * cursor (or to reset it to normal).  If other shells exist in
     * this application, they will have to be listed here in order
     * for them to have timeout cursors too.
     */
    XChangeWindowAttributes(dpy, XtWindow(gAppShell), CWCursor, &attrs);
    XFlush(dpy);
}
void Progress(int x,int max,char *label,char *label2) {
  ARGS
  int val; XmString title;
  if(x==-5) {
    TimeoutCursors(TRUE);
    title=XmStringCreateLtoR(label2,XmSTRING_DEFAULT_CHARSET);
    nn=0;
    XtSetArg(args[nn],XmNlabelString,title); nn++;
    XtSetValues(gSecondLabel,args,nn);

    title=XmStringCreateLtoR(label,XmSTRING_DEFAULT_CHARSET);
    nn=0;
    XtSetArg(args[nn],XmNtitleString,title); nn++;
    XtSetValues(gProgressScale,args,nn);

    XtPopup(gProgressPopup,XtGrabNone);
    XmStringFree(title); x=0;
  }
  if(x==-10) {
    TimeoutCursors(FALSE);
    XtPopdown(gProgressPopup); return;
  }
  val=(100.0*x)/max+0.5;
  /* PP"val=%d.  x=%d, max=%d.\n",val,x,max); */
  XmScaleSetValue(gProgressScale,val);
  XmUpdateDisplay(gProgressPopup);
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
void ReadCalcAve(void) {
  FILE *ff; char line[53];
  ff=fopen("browser.cDef","r");
  if(ff==NULL) { gCalculateAverages=TRUE; return; }
  if(!fgets(line,50,ff)) { gCalculateAverages=TRUE; return; }
  gCalculateAverages=atoi(line);
}
void DoOnce2(void) {
  ReadCalcAve(); /* SendMail(); */
  gLast=-1; gNGraphicsUp=0; gNWin=0;
}
XtCP CutsCancelCB(Widget w,caddr_t cld,caddr_t cad) {
  gDone2=63;
}
XtCP CutsOkCB(Widget w,caddr_t cld,caddr_t cad) {
  gDone2=7;
}
void PrepareCutsPopup() {
  Arg args[24];
  int nn;
  Widget butBox,button,theLabel,rrr,theWindow;
  XmString title;
  nn=0;
  gCutsPopup=XtCreatePopupShell("",transientShellWidgetClass,gMainWindow,
  args,nn);
  nn=0;
  theWindow=XmCreateMainWindow(gCutsPopup,"",args,nn);
  XtManageChild(theWindow);
  nn=0;
  XtSetArg(args[nn], XmNorientation, XmVERTICAL); nn++;
  rrr=XmCreateRowColumn(theWindow,"",args,nn);
  XtManageChild(rrr);

  title=XmStringCreateLtoR("Cuts Editor",XmSTRING_DEFAULT_CHARSET);
  nn=0;
  XtSetArg(args[nn],XmNlabelString,title); nn++;
  theLabel=XmCreateLabel(rrr,"",args,nn);
  XtManageChild(theLabel);
  XmStringFree(title);

  title=XmStringCreateLtoR(
  "If you have a big cuts string, you can use several lines.",
  XmSTRING_DEFAULT_CHARSET);
  nn=0;
  XtSetArg(args[nn],XmNlabelString,title); nn++;
  theLabel=XmCreateLabel(rrr,"",args,nn);
  XtManageChild(theLabel);
  XmStringFree(title);

  nn=0;
  XtSetArg(args[nn],XmNrows,5);  nn++;
  XtSetArg(args[nn], XmNcolumns, CUTS_WIDTH);  nn++;
  XtSetArg(args[nn],XmNresizeWidth, False);  nn++;
  XtSetArg(args[nn],XmNresizeHeight, False);  nn++;
  XtSetArg(args[nn],XmNeditMode, XmMULTI_LINE_EDIT);  nn++;
  XtSetArg(args[nn],XmNscrollHorizontal, False);  nn++;
  gCutsText=XmCreateText(rrr,"TxtWid",args,nn);
  butBox=Row(rrr);
  XtManageChild(gCutsText);
  button=XmCreatePushButton(butBox," OK ",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,(XtCP)CutsOkCB,NULL);
  button=XmCreatePushButton(butBox," Cancel ",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,(XtCP)CutsCancelCB,NULL);
  button=XmCreatePushButton(butBox," Help ",NULL,0);
  XtManageChild(button);
  XtAddCallback(button,XmNactivateCallback,(XtCP)HelpCutsCB,NULL);
}
void PrepareProgressPopup() {
  Arg args[14];
  int n;
  Widget junk,rrr,theWindow;
  XmString title;
  n=0;
  gProgressPopup=XtCreatePopupShell("",transientShellWidgetClass,gMainWindow,
  args,n);
  n=0;
  theWindow=XmCreateMainWindow(gProgressPopup,"",args,n);
  XtManageChild(theWindow);
  n=0;
  XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
  rrr=XmCreateRowColumn(theWindow,"",args,n);
  XtManageChild(rrr);
  title=XmStringCreateLtoR(" ",XmSTRING_DEFAULT_CHARSET);
  n=0;
  XtSetArg(args[n],XmNlabelString,title); n++;
  junk=XmCreateLabel(rrr,"",args,n);
  XtManageChild(junk);
  n=0;
  XtSetArg(args[n],XmNlabelString,title); n++;
  gSecondLabel=XmCreateLabel(rrr,"",args,n);
  XtManageChild(gSecondLabel);
  n=0;
  XtSetArg(args[n],XmNtitleString,title); n++;
  XtSetArg(args[n],XmNheight,400); n++;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNshowValue,FALSE); n++;
  gProgressScale=XmCreateScale(rrr,"",args,n);
  XtManageChild(gProgressScale);
  n=0;
  XtSetArg(args[n],XmNlabelString,title); n++;
  junk=XmCreateLabel(rrr,"",args,n);
  XtManageChild(junk);
  XmStringFree(title);
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
  gDone=7;
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
myBool IsOperator(char *xx,int pp) {
  if(xx[pp]=='.'&&(xx[pp+1]=='g'||xx[pp+1]=='G')) return TRUE;
  if(xx[pp]=='.'&&(xx[pp+1]=='l'||xx[pp+1]=='L')) return TRUE;
  if(xx[pp]=='.'&&(xx[pp+1]=='e'||xx[pp+1]=='E')) return TRUE;
  if(xx[pp]=='='&&xx[pp+1]=='=') return TRUE;
  if(xx[pp]=='<') return TRUE;
  if(xx[pp]=='>') return TRUE;
  return FALSE;
}
void BreakIntoLines(char *xx) {
  int len,ii,chos=0,bufPos=0,lastPrint=-1;
  char buf[MAX_CUTS_STRING],save;
  len=strlen(xx); if(len>800) Err(770); *buf='\0';
  for(ii=0;ii<len;ii++) {
    if(IsOperator(xx,ii)&&chos>CUTS_WIDTH-15) {
      chos=0; save=xx[ii]; xx[ii]='\0';
      strcat(buf,xx+lastPrint+1); strcat(buf,"\n");
      lastPrint=ii-1; xx[ii]=save;
    }
    chos++;
  }
  strcat(buf,xx+lastPrint+1); 
  strcpy(xx,buf);
}
void GetRidOfNewlines(char *xx) {
  int len,ii,bufPos=0,lastPrint=-1;
  char buf[MAX_CUTS_STRING];
  len=strlen(xx); if(len<0||len>800) Err(770); *buf='\0';
  for(ii=0;ii<len;ii++) {
    if(xx[ii]=='\n') {
      xx[ii]='\0';
      strcat(buf,xx+lastPrint+1);
      lastPrint=ii;
    }
  }
  strcat(buf,xx+lastPrint+1); strcpy(xx,buf);
}
void SaveCutsInFile(char *tableName,char *xx) {
  FILE *ff,*temp; char *tok,copy[MAX_CUTS_STRING+5],line[MAX_CUTS_STRING+5];
  temp=fopen("bRoWsER.tmp","w");
  if(temp==NULL) { Say("I don't have write permission here."); return; }
  ff=fopen("browser.cuts","r");
  if(ff!=NULL) {
    while(fgets(line,MAX_CUTS_STRING,ff)) {
      strcpy(copy,line);
      tok=strtok(line," \t"); if(tok==NULL) continue;
      if(!strcmp(tableName,tok)) continue;
      if(strlen(line)<5) continue;
      fprintf(temp,"%s",copy);
    } fclose(ff);
  }
  fprintf(temp,"\n%s %s\n",tableName,xx); fclose(temp);
  system("mv bRoWsER.tmp browser.cuts");
}
myBool UserMod(char *tableName,char *xx) {
  XEvent event; XmString cuts;
  BreakIntoLines(xx); XmTextSetString(gCutsText,xx);
  XtPopup(gCutsPopup,XtGrabNone);
  gDone2=0;
  for(;;) { /* un-main loop */
    XtAppNextEvent(gAppCon,&event); XtDispatchEvent(&event);
    if(gDone||gDone2) break;
  }
  XtPopdown(gCutsPopup);
  if(gDone2>10) return FALSE; /* User cancelled. */
  strcpy(xx,XmTextGetString(gCutsText));
  GetRidOfNewlines(xx);
  SaveCutsInFile(tableName,xx);
  return TRUE; /* User said OK. */
}
myBool GetCuts(char *out,char *tableName) {
  FILE *ff; char *tok,fn[150],line[MAX_CUTS_STRING+5]; myBool fo;
  tok=NULL;
  sprintf(fn,"browser.cuts"); ff=fopen(fn,"r");
  if(ff!=NULL) {
    fo=FALSE;
    while(fgets(line,MAX_CUTS_STRING,ff)) {
      if(*line=='#') continue;
      tok=strtok(line," \t"); if(tok==NULL) continue;
      if(!strcmp(tableName,tok)) { fo=TRUE; break; }
    } fclose(ff);
    if(!fo) tok=""; else tok=strtok(NULL," \n\t");
  }
  if(tok!=NULL) strcpy(out,tok); else *out='\0';
  return UserMod(tableName,out); /* whether user cancels */
}
char *ColName(int ln,int whWin) {
  int ii,nl=0,len; char *cc,*rr;
  if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(828);
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
void WrBotCB(Widget ww,caddr_t cld,caddr_t cad) {
  ARGS
  FILE *ff; XmString scratch;
  int whWin; whWin=(int)cld;
  ff=fopen("browDump.txt","w"); if(ff==NULL) {
    Say("I can't write on browDump.txt."); return;
  }
  scratch=XmTextGetString(gWin[whWin]->txtWidWriteH);
  fprintf(ff,"%s\n",scratch); XtFree(scratch);
  fprintf(ff,"%s\n",gWin[whWin]->textOutput);
  fclose(ff); Say("Your file is named \"browDump.txt\".");
}
void BotLongerCB(Widget ww,caddr_t cld,caddr_t cad) {
  ARGS
  short rr;
  int whWin; whWin=(int)cld;
  /* if(!gBlurb1) Blurb(); gBlurb1=TRUE; */
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
  if(gWin[gRunWhWin]->tlm[gRunWhichHilitedLine]<0) Err( 48);
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
  if(gWin[gRunWhWin]->tlm[gRunWhichHilitedLine]<0) Err( 49);
  val=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
      gWin[gRunWhWin]->tlm[gRunWhichHilitedLine],irow,
      gWin[gRunWhWin]->subscript[gRunWhichHilitedLine]);
  if(gTableValueError) {
    Say("Table has unsupported data type"); gBreakRowsLoop=TRUE;
  }
  hist=(HIST-1)*(val-gMin)/(gMax-gMin);
  if(hist>=HIST||hist<0) {
    PP"Table browser fatal error 711p.\n");
    PP"hist=%d, HIST=%d, gMax=%e, gMin=%e\n",hist,HIST,gMax,gMin);
    gDone=7; return;
  }
  gHist[hist]++;
}
void RunAverage(size_t row) {
  int irow=row;
  if(gWin[gRunWhWin]->tlm[gRunWhichHilitedLine]<0) Err( 50);
  gRunTotal+=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
      gWin[gRunWhWin]->tlm[gRunWhichHilitedLine],irow,
      gWin[gRunWhWin]->subscript[gRunWhichHilitedLine]);
  if(gTableValueError) {
    Say("Table has unsupported data type"); gBreakRowsLoop=TRUE;
  }
}
void ConvertToHex(char *out,float val) {
  /* Is this the only function in the world that translates floats to hex? */
  int ii,hh; char buf[WIDE+15];
  hh=val; if(hh<0) Err(559); if(hh>255) Err(558);
  sprintf(out,"0x%02x",hh);
  for(ii=WIDE+14;ii>=0;ii--) buf[ii]=' ';
  buf[WIDE-4]='\0'; strcat(buf,out); strcpy(out,buf);
}
void RunValue(size_t row) {
  int fo=0,tt,hlLstIx,lnfhl,irow=row; float val;
  char tt2[60],format[22],tmp[100],buf[422];
  sprintf(buf,"%s%6d",gSumCol,row+1);
  if(strlen(buf)!=EXT) {
    PP"Table browser fatal error 611p.\n");
    PP"buf=%s, len should be %d.\n",buf,EXT); gDone=7; return;
  }
  if(!gTruncateStrings) strcat(buf," ");
  for(hlLstIx=0;hlLstIx<gWin[gRunWhWin]->nhlLst;hlLstIx++) {
    lnfhl=gWin[gRunWhWin]->hlLst[hlLstIx];
    if(!gWin[gRunWhWin]->isHilited[lnfhl]) Err( 60);
    fo++;
    if(gWin[gRunWhWin]->tlm[lnfhl]<0) Err( 51);
    val=ValueWrapper(gWin[gRunWhWin]->wh_gDs,
          gWin[gRunWhWin]->tlm[lnfhl],irow,gWin[gRunWhWin]->subscript[lnfhl]);
    if(gTableValueError) {
      Say("Table has unsupported data type"); gBreakRowsLoop=TRUE; break;
    }
    sprintf(format,"%%%ds",WIDE);
    if(gVWType==VWSTRING) {
      if(gTruncateStrings) {
        gStr[WIDE-1]='\0';
        for(tt=WIDE-2;tt>=0;tt--) { if(gStr[tt]!=' ') break; gStr[tt]='\0'; }
      } else {
        for(tt=strlen(gStr)-1;tt>=0;tt--) 
              { if(gStr[tt]!=' ') break; gStr[tt]='\0'; }
        strcat(gStr," ");
      }
      sprintf(tt2,format,gStr);
    } else if(gVWType==VWNUMBER) {
      Format(WIDE-1,tmp,val); sprintf(tt2,format,tmp);
    } else if(gVWType==VWHEX) {
      ConvertToHex(tt2,val);
    }
    strcat(buf,tt2);
  }
  if(fo>0) {
    strcat(buf,"\n");
    if(gToFile) { if(gDump) fprintf(gDump,"%s",buf); }
    else Write(buf,gRunWhWin);
  }
  else if(gRunNRowsDone==0) SayError0();
}
void OneLnPerRowCB(Widget w,caddr_t cld,caddr_t cad) { /* see Comment 8b */
  int whAct,whWin; char act[111];
  static int whActS,whWinS;
  void (*runFunc)();
  if((int)w==2&&(int)cld==5&&(int)cad==14) { whAct=whActS; whWin=whWinS; }
  else { whWin=((int)cld)%1000; whAct=((int)cld)/1000; }
  gLastWhWin=whWin;
  gRunWhWin=whWin; if(gWin[whWin]->win_type!=WIN_TYPE_TABLE) Err(128);
  switch(whAct) {
    case 0: strcpy(act,"Value"); runFunc=RunValue; gToFile=0;
      gDump=NULL; break;
    case 1: strcpy(act,"Value"); runFunc=RunValue; gToFile=7; 
      gDump=fopen(TXTOUT,"w");
      if(gDump)
          fprintf(gDump,"%s\n",XmTextGetString(gWin[whWin]->txtWidWriteH));
      break;
    default: Err(129);
  }
  sprintf(gSumCol,"%6s ",act); /* for passing to runFunc */
  gRunNRowsDone=0;
  RunTheRows(FALSE,whWin,runFunc);
  if(gDump) fclose(gDump);
  if(gRunNRowsDone==0&&gDone2<10) {
    Say("No rows were\nselected by \"ROW SELECTION\".");
  } else {
    if(whAct==1) {
      sprintf(act,"Your output file is %s.\n",TXTOUT); Say(act);
    }
  }
  whActS=whAct; whWinS=whWin;
}
/* Comment 8b: OneLnAllRowsCB is the callback for many of the Action menu
   items.  It decodes the client data into whWin and whAct, selects which
   function pointer should be passed to RunTheRows, handles auxiliary output
   like writing the number of rows used and the cuts string, and loops over
   the selected columns. */
void IncrementByOneCol(int whWin) {
  ARGS
  short rr;
  gWin[whWin]->ncol++;
  gWin[whWin]->width+=WIDE;
  /* if(!gBlurb2) Blurb(); gBlurb2=TRUE; */
  rr=gWin[whWin]->width;
  nn=0;
  XtSetArg(args[nn],XmNcolumns,rr); nn++;
  XtSetValues(gWin[whWin]->txtWidWrite,args,nn);
  XtSetValues(gWin[whWin]->txtWidWriteH,args,nn);
}
void OneLnAllRowsCB(Widget w,caddr_t cld,caddr_t cad) { /* see Comment 8b */
  int ii,cnt,whAct,nCol=0,fo=0,lnfhl,hlLstIx,whWin; char tt[111],sumCol[211];
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
  for(hlLstIx=0; (dalo&&hlLstIx==0) || (hlLstIx<gWin[whWin]->nhlLst);
        hlLstIx++) {
    if(gWin[whWin]->nhlLst>0) lnfhl=gWin[whWin]->hlLst[hlLstIx];
    else lnfhl=0;  /* see the complicated for() which governs this loop */
    if(!dalo&&!gWin[whWin]->isHilited[lnfhl]) Err( 61);
    if(whAct==2&&++cnt>1) { Complain1(); return; } gHistWhLine=lnfhl;
    if(++nCol>MCOL) Err( 44); /*ShouldBeProtectedInSetHiliteWIN_TYPE_TABLE*/
    fo++; gRunTotal=0; gRunNRowsDone=0; gRunWhichHilitedLine=lnfhl;
    if(firstTime) { firstTime=FALSE; RunTheRows(FALSE,whWin,runFunc); }
    else RunTheRows(TRUE,whWin,runFunc);
    if(gVWType!=VWNUMBER) {
      Say("You can't do that with octet or strings."); return;
    } else {
      if(gRunNRowsDone>0) {
        switch(whAct) {
          case 0: summary=gRunTotal/gRunNRowsDone;
            Format(WIDE-1,tt,summary); sprintf(format,"%%%ds",WIDE);
            sprintf(tt2,format,tt); strcat(sumCol,tt2); break;
          case 1: case 2: break;
          default: Err(130);
        }
      } else if(gDone2<10) { /* User did not cancel. */
        Write("\"ROW SELECTION\" selected zero rows.\n",whWin); break;
      } else break; /* User may have canceled in cuts dialog. */
    }
  }	/* loop hlLstIx */
  if(gMax==gMin) { gMax=gMin+0.1*fabs(gMin); gMin=gMin-0.1*fabs(gMin); }
  if(gMax==gMin) { gMax+=1; gMin-=1; }
  switch(whAct) {
    case 0: if(fo==0) SayError0();
      else { strcat(sumCol,"\n"); Write(sumCol,whWin); } break;
    case 1: break;
    case 2:
      if(gDone2<11) {
        for(ii=HIST-1;ii>=0;ii--) gHist[ii]=0;
        if(fo==0) { SayError0(); return; } RunTheRows(TRUE,whWin,RunHistFill);
      }
      break; /* 950709, FALSE->TRUE */
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
      if(gDone2<10) { /* User did not cancel in cuts dialog. */
        sprintf(bf,"Your \"ROW SELECTION\" (top right) selects %d row(s).\n",
        gRunNRowsDone); Write(bf,whWin);
      }
      break;
    case 2: if(gDone2<11) DrawHist(whWin); break;
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
    if(!GetCuts(cuts,gWin[whWin]->tableName)) return;
    if(!DoCutsWrapper(MAXROW_DIV_BY_8,ba,cuts,gWin[whWin]->wh_gDs)) {
      Say("We have error 51r.\nCheck your cuts string."); return;
    }
    strncpy(gCuts,cuts,COL);
    strcpy(gCuts+COL-8," etc...");
  }
  gBreakRowsLoop=FALSE;
  for(row=start;row<=end;row++) {
    /* ActionCB/RunAverage are a simple example of how to use this */
    if(gWin[whWin]->useCuts) { if(!dsuRowPassedCuts(ba,(long)row)) continue; }
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
  SaveValueOfCalcAve();
}
void SkipCB(Widget w,caddr_t cld,caddr_t cad) {
  gCalculateAverages=FALSE;
  SaveValueOfCalcAve();
}
void NotTruncCB(Widget w,caddr_t cld,caddr_t cad) {
 gTruncateStrings=0;
 Say("\
 Warning: This destroys alignment of the columns\n\
 in all browser windows.\n");
}
void TruncCB(Widget w,caddr_t cld,caddr_t cad) {
  gTruncateStrings=7;
}
void SigFigCB(Widget w,caddr_t cld,caddr_t cad) { /* pops it up */
  Say("This does not work yet.");
  /* XtPopup(gSigFigScalePopup,XtGrabNone); */
}
void HelpSelTabCB(Widget w,caddr_t cld,caddr_t cad) {
 Say("\
 Indentation indicates hierarchy.\n\
 \n\
 STEP 1. Click on the two-letter abbrevi-\n\
 ations until you see a table (abbrevi-\n\
 ation=TB) that you are interested in.\n\
 \n\
 STEP 2. When you see a table you like, click\n\
 on its name (not the abbreviation).\n\
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
 Then select an item from the \"Action\" menu.\n\
 \n\
 CRITICAL:\n\
 Items from the \"Action\" menu only work on rows\n\
 selected in \"ROW SELECTION\", so be careful with\n\
 row-wise wholesale actions like averaging and his-\n\
 tograms when you have a subtle \"ROW SELECTION\"\n\
 like \"Cuts\".\n\
 ");
}
void HelpCutsCB(Widget w,caddr_t cld,caddr_t cad) {
  *gPass='\0';
  strcat(gPass,"Cuts specification follows Fortran 90 guidelines, except\n");
  strcat(gPass,"that the \".ne.\" and \"/=\" operators are not supported.\n");
  strcat(gPass,"-------- Examples:\n");
  strcat(gPass,"id.gt.10.and.id<=110\n");
  strcat(gPass,"id_globtrk.gt.10.and.id_globtrk.le.110\n");
  strcat(gPass,"cov[5]>3.12\n");
  strcat(gPass,"--------\n");
  strcat(gPass,"Specifiers in the conditionals (eg, id_globtrk) are\n");
  strcat(gPass,"column names.  Note the use of\n");
  strcat(gPass,"square brackets (NOT PARENTHESES, AS IN TAS)\n");
  strcat(gPass,"for columns whose data types are arrays.\n");
  strcat(gPass,"\n");
  Say(gPass);
}
void HelpBugRptCB(Widget w,caddr_t cld,caddr_t cad) {
  Say("Email ward@physics.utexas.edu");
}
#ifdef STANDALONE
#define QUITSTRING "Quit"
#else
#define QUITSTRING "Close all browser windows"
#endif
void CreateMenuItems(Widget mbar,int type) {
  register int nn; Arg args[19]; Widget mpane;
  mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
  if(type==WIN_TYPE_PRIMARY) {
    MakeMenuItem(NOTUSED,mpane,QUITSTRING,(XtCP)QuitCB);
  } else {
    MakeMenuItem(NOTUSED,mpane,"Close this window",(XtCP)CloseThisWindowCB);
  }
  if(type==WIN_TYPE_TABLE) {
    MakeMenuItem(NOTUSED,mpane,"Dump this window to .txt file",(XtCP)WrBotCB);
  }
  FinishThisMenu(mbar,mpane,"File ");
  if(type==WIN_TYPE_PRIMARY) {
    mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
    MakeMenuItem(0,mpane,"Expand",    (XtCP)ExpandCB);
    MakeMenuItem(0,mpane,"Expand except for columns",(XtCP)ExpandExceptCB);
    MakeMenuItem(0,mpane,"Contract",(XtCP)ContractCB);
    FinishThisMenu(mbar,mpane,"Hierarchy ");
  }
  if(type==WIN_TYPE_TABLE) {
    mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
    MakeMenuItem(0,mpane,"Show  value(s).",             (XtCP)OneLnPerRowCB);
    MakeMenuItem(1,mpane,"Write value(s) to .txt file.",(XtCP)OneLnPerRowCB);
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
  if(type==WIN_TYPE_TABLE||type==WIN_TYPE_PRIMARY) {
    mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
    if(type==WIN_TYPE_PRIMARY) {
      MakeMenuItem(NOTUSED,mpane,"Skip averages (faster)",(XtCP)SkipCB);
      MakeMenuItem(NOTUSED,mpane,"Calc averages (slower)",(XtCP)DoCB);
    }
    if(type==WIN_TYPE_TABLE) {
      MakeMenuItem(NOTUSED,mpane,"Do not truncate strings",(XtCP)NotTruncCB);
      MakeMenuItem(NOTUSED,mpane,"Truncate strings",(XtCP)TruncCB);
      MakeMenuItem(NOTUSED,mpane,"Format of numerical output",(XtCP)SigFigCB);
      MakeMenuItem(NOTUSED,mpane,"Bottom part longer (may be iterated)",
        (XtCP)BotLongerCB);
    }
    FinishThisMenu(mbar,mpane,"Preferences ");
  }
  mpane=XmCreatePulldownMenu(mbar,"a2",args,0);
  if(type==WIN_TYPE_PRIMARY) {
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
int LineNumber(myBool *inTriangle,int whWin,Position pos) {
  int fromLeft=0,rv,ii,len; char *text;
  myBool reachedTriangle=FALSE;
  if(whWin<0||whWin>=gNWin) Err(103);
  text=gWin[whWin]->textClickPart;
  len=strlen(text); if(pos>len) Err(107);
  rv=0;
  for(ii=pos-1;ii>=0;ii--) if(text[ii]=='\n') rv++;
  for(ii=0;ii<pos;ii++) {
    if(text[ii]=='\n') { reachedTriangle=FALSE; fromLeft=0; continue; }
    if(text[ii]!=' ') {
      reachedTriangle=TRUE;
    }
    if(reachedTriangle) fromLeft++;
  }
  if(fromLeft>3) *inTriangle=FALSE; else *inTriangle=TRUE;
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
void DelFromHiliteList(int whWin,int lineNum) {
  int jj,ii;
  for(ii=gWin[whWin]->nhlLst-1;ii>=0;ii--) {
    if(gWin[whWin]->hlLst[ii]==lineNum) break;
  }
  if(ii<0) return;
  for(jj=ii;jj<gWin[whWin]->nhlLst;jj++) {
    gWin[whWin]->hlLst[jj]=gWin[whWin]->hlLst[jj+1];
  }
  ii=gWin[whWin]->nhlLst; gWin[whWin]->nhlLst=ii-1;
}
void AddToHiliteList(int whWin,int lineNum) {
  int ii;
  for(ii=gWin[whWin]->nhlLst-1;ii>=0;ii--) {
    if(gWin[whWin]->hlLst[ii]==lineNum) {
      return;
      /*------- so user can click twice on same line in primary window
      PP"I am being asked to add line# %d to my highlight list for\n",lineNum);
      PP"window %d, but it is already on the list.\n",whWin);
      Err(772);
      -----------------------------------------------*/
    }
  }
  ii=gWin[whWin]->nhlLst;
  if(ii>=MAX_LINES_CLICK_PART) Err(773);
  gWin[whWin]->hlLst[ii]=lineNum; gWin[whWin]->nhlLst=ii+1;
}
void SetHilite(int control,int whWin,int lineNum) {
  /* What the user sees is set in Loop A, what program sees is
  ** gWin[]->isHilited[].  Up to programmer to keep these two equal. */
  char buf[WIDE*MCOL+EXT+1]; /* see comments in x.h for meaning of MCOL etc */
  char tmp[111],format[22];
  char cp[43],*otext; int num=0,len,ii,ln; myBool doReturn=FALSE;
  int hlLstIx,lnfhl; /* line number from hilite list */
  XmTextPosition left,right;
  Widget txtWid;
  txtWid=gWin[whWin]->txtWidClick;
  otext=gWin[whWin]->textClickPart;
  len=strlen(otext); left=0; right=len-1; ln=0;
  for(ii=0;ii<len;ii++) {
    if(otext[ii]=='\n') { right=ii; if(ln==lineNum) break; left=ii+1; ln++; }
  }
  if(control==HILITE_TURN_ON||!gWin[whWin]->isHilited[lineNum]) {
    AddToHiliteList(whWin,lineNum);
    gWin[whWin]->isHilited[lineNum]=TRUE;
    XmTextSetHighlight(txtWid,left,right,XmHIGHLIGHT_SELECTED);
  } else {
    DelFromHiliteList(whWin,lineNum);
    gWin[whWin]->isHilited[lineNum]=FALSE; /*BBB is isHilited still needed? */
    XmTextSetHighlight(txtWid,left,right,XmHIGHLIGHT_NORMAL);
  }
  XmUpdateDisplay(txtWid);
  if(gWin[whWin]->win_type==WIN_TYPE_TABLE) {
    sprintf(format,"%%%ds",WIDE); strcpy(buf,"  What RowNum");
    for(hlLstIx=0;hlLstIx<gWin[whWin]->nhlLst;hlLstIx++) {
      lnfhl=gWin[whWin]->hlLst[hlLstIx];
      if(!gWin[whWin]->isHilited[lnfhl]) Err(666);
      if(num>=gWin[whWin]->ncol) IncrementByOneCol(whWin);
      if(num++>=MCOL) { Complain(); doReturn=TRUE; break; }
      strcpy(cp,ColName(lnfhl,whWin)); Abbr(cp); sprintf(tmp,format,cp);
      if(strlen(tmp)+strlen(buf)>WIDE*MCOL+EXT) {
        PP"Table browser fatal error 712p.\n");
        PP"This is error 91P. %d + %d > %d, MCOL = %d.\n",
        strlen(tmp),strlen(buf),WIDE*MCOL+EXT,MCOL); gDone=7; return;
      } strcat(buf,tmp);
    } /* loop over hlLstIx */
    if(doReturn) return; gWin[whWin]->textOutput[0]='\0';
    XmTextSetString(gWin[whWin]->txtWidWrite,gWin[whWin]->textOutput);
    XmTextSetString(gWin[whWin]->txtWidWriteH,buf);
  }
}
void ResetTheTriangles(int wds) {
  XmTextPosition pos;
  char junk[100]; int len,ii,nLines;
  if(wds>=0) {
    if(!ToggleCase(wds)) {
      Say(
      "You can't expand/contract that.\nIt's the lowest hierarchical level.");
      return;
    }
  }
  else if(wds==-10) { for(ii=0;ii<gNDs;ii++) ExpandCase(ii); }
  else if(wds==-15) { for(ii=0;ii<gNDs;ii++) ExpandExceptCase(ii); }
  else if(wds==-20) { for(ii=0;ii<gNDs;ii++) ContractCase(ii); }
  else Err(123);
  PrimaryList(junk,0,&nLines,gWin[0]->tlm,MAX_LINES_CLICK_PART,
              gWin[0]->textClickPart,TEXT_SIZE_PART_2);
  gWin[0]->nlcpwtto=nLines;
  XmTextDisableRedisplay(gWin[0]->txtWidClick);
  pos=XmTextGetTopCharacter(gWin[0]->txtWidClick);
  XmTextSetString(gWin[0]->txtWidClick,gWin[0]->textClickPart);
  XmTextSetTopCharacter(gWin[0]->txtWidClick,pos);
  XmTextEnableRedisplay(gWin[0]->txtWidClick);
}
XtCP ContractCB(Widget w,caddr_t cld,caddr_t cad) {
  ResetTheTriangles(-20);
}
XtCP ExpandExceptCB(Widget w,caddr_t cld,caddr_t cad) {
  ResetTheTriangles(-15);
}
XtCP ExpandCB(Widget w,caddr_t cld,caddr_t cad) {
  ResetTheTriangles(-10);
}
XtCP TextCB(Widget w,caddr_t cld,caddr_t cad) { /* user click in text window */
  int whWin,tlm; myBool inTri;
  /* tlm is either an index for gDs
  or it is an arg (cast to size_t) for dsColumnName (column number). */
  XmAnyCallbackStruct *xx; Position pos;
  XButtonEvent *bev; int lineNumber;
  whWin=(int)cld;
  if(whWin<0||whWin>=gNWin) Err(104);
  xx=(XmAnyCallbackStruct*)cad;
  bev=(XButtonEvent*)(xx->event); /* Vol1 p512 */
  pos=Pos(whWin,(Position)(bev->x),(Position)(bev->y));
  lineNumber=LineNumber(&inTri,whWin,pos);
  if(lineNumber<0||lineNumber>=gWin[whWin]->nlcpwtto) {
    PP"lineNumber=%d, whWin=%d, gWin[whWin]->nlcpwtto=%d.\n",
    lineNumber,whWin,gWin[whWin]->nlcpwtto);
    Say("You clicked too low."); return 0;
  }
  tlm=gWin[whWin]->tlm[lineNumber];
  switch(gWin[whWin]->win_type) {
    case WIN_TYPE_D_TREE: /* does not work yet */
      Err(192); /* June 28 1995 see err(552) */
      MakeWindow(tlm,WIN_TYPE_PRIMARY);
      SetHilite(HILITE_TURN_ON,whWin,lineNumber); /*BBB un-hilite when closed*/
      break;
    case WIN_TYPE_PRIMARY:
      if(tlm<0) { Say(gMess0); return NULL; }
      if(inTri) { ResetTheTriangles(tlm); return NULL; }
      if(!ATable(tlm)) { Say(gBlurb7); return; }
      if(!TableHasMoreThanZeroCols(tlm)) return;
      if(!TableHasMoreThanZeroRows(tlm)) return;
      SetHilite(HILITE_TURN_ON,whWin,lineNumber); /*BBB un-hilite when closed*/
      MakeWindow(tlm,WIN_TYPE_TABLE);
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
  if(rv<0||rv>=gWin[whWin]->nRow) { rv=0; gLast=-1; }
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
    if(type==WIN_TYPE_PRIMARY) strcpy(name,"Dataset Browser");
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
  gWin[gNWin]->width=COL;
  gWin[gNWin]->whichRadio=0;
  gWin[gNWin]->ncol=MCOL_INIT;
  switch(type) {
    case WIN_TYPE_D_TREE: /* make primary window (for choosing a dataset) */
      Err(552); /* June 28 1995 */
      gWin[gNWin]->rowSel=ROW_SEL_NOT_USED;
      SetToPrimaryInfo(gWin[gNWin]->textTop,TEXT_SIZE_PART_1);
      DatasetList(&nLines,gWin[gNWin]->tlm,MAX_LINES_CLICK_PART,
              gWin[gNWin]->textClickPart,TEXT_SIZE_PART_2);
      gWin[gNWin]->txtWidTop=TxtWid(ver,gWin[gNWin]->textTop,0,5,48);
      gWin[gNWin]->txtWidClick=TxtWid(ver,gWin[gNWin]->textClickPart,1,8,46);
      gWin[gNWin]->txtWidWrite=NULL;
      break;
    case WIN_TYPE_PRIMARY: /* we are making a window for choosing a table */
      if(++gNumDatasetWindows>1) Err(553); /*6-28-95*/
      if(wh_gDs!=0) Err(554); /*6-28-95*/
      gWin[gNWin]->rowSel=ROW_SEL_NOT_USED;
      SetToDatasetInfo(wh_gDs,gWin[gNWin]->textTop,TEXT_SIZE_PART_1); /*BBB*/
      PrimaryList(header,wh_gDs,&nLines,gWin[gNWin]->tlm,MAX_LINES_CLICK_PART,
              gWin[gNWin]->textClickPart,TEXT_SIZE_PART_2);
      /* gWin[gNWin]->txtWidTop=TxtWid(ver,gWin[gNWin]->textTop,0,5,T41+2); */
      /* June 28 1995 gWin[gNWin]->txtWidClickH=TxtWid(ver,header,0,1,T41+2);*/
      gWin[gNWin]->txtWidClick=TxtWid(ver,gWin[gNWin]->textClickPart,1,22,T41);
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
      if(gWin[gNWin]->textOutput==NULL) {
        PP"Table browser:  no more dynamic memory.\n"); Err(118);
      }
      strcpy(gWin[gNWin]->textOutput,"");
      hor=Row(ver);
      gWin[gNWin]->txtWidTop=TxtWid(hor,gWin[gNWin]->textTop,0,7,30);
      MakeRowSelectionWidget(hor);
      gWin[gNWin]->txtWidClickH=TxtWid(ver,header,0,1,COL);
      gWin[gNWin]->txtWidClick=TxtWid(ver,gWin[gNWin]->textClickPart,1,6,COL);
      gWin[gNWin]->txtWidWriteH=TxtWid(ver,"",0,1,5); /* 47->5 */
      gWin[gNWin]->txtWidWrite=TxtWid(ver,gWin[gNWin]->textOutput,2,RW,COL);
      gWin[gNWin]->nRowsWritePart=RW; /* num Rows in Write part init */
      break;
    case WIN_TYPE_GRAPHICS:
      gWin[gNWin]->rowSel=ROW_SEL_NOT_USED;
      MakeDrawingArea(ver,GRAPHWIDTH,GRAPHHITE); gNGraphicsUp++;
      break;
    default: Err(101);
  }
  gWin[gNWin]->nlcpwtto=nLines;
  gWin[gNWin]->shell=ls;
  gNWin++; /*BBB chk init */
  if(type!=WIN_TYPE_PRIMARY) XtPopup(ls,XtGrabNone);
  if(type==WIN_TYPE_GRAPHICS) Clear();
}
void GetRidOfWindows(void) {
  XtUnrealizeWidget(gAppShell);
  XtDestroyWidget(gAppShell);
}
void DoXStuff(void) {
  ARGS
  XEvent event; static myBool haveInited=FALSE;
  XButtonEvent *bev;
  Widget j1; caddr_t j2,j3; int zero=0;
  DoOnce2();
#ifdef STANDALONE
  PP"Doing X Toolkit init.\n"); XtToolkitInitialize();
#else
  PP"Skipping X Toolkit init.\n");
#endif
  gBlurb2=FALSE; gBlurb1=FALSE;
  PP"XtCreateApplicationContext()\n");
  if(!haveInited) { gAppCon=XtCreateApplicationContext(); }
  PP"XtOpenDisplay()\n");
  if(!haveInited) {
    gDisplay=XtOpenDisplay(gAppCon,NULL,"Dataset Catalog Browser",
      "1hhh",(XrmOptionDescRec*)NULL,0,&zero,NULL);
  }
  if(!gDisplay) {
    PP"STAR Table Browser is an X-windows program.\n");
    PP"I can't open a display.\n");
    PP"Have you set your DISPLAY env var?\n"); gDone=7; return;
  }
  nn=0;
  PP"XtAppCreateShell()\n");
  gAppShell=XtAppCreateShell("Dataset Catalog Browser",
  "starbrowser",applicationShellWidgetClass,gDisplay,args,nn);
  PP"MakeWindow()\n");
  MakeWindow(0,WIN_TYPE_PRIMARY);
  PrepareProgressPopup(); PrepareCutsPopup();
  PP"XtRealizeWidget()\n");
  haveInited=TRUE;
  XtRealizeWidget(gAppShell);
  PP"main loop\n");
  for(;;) {
    XtAppNextEvent(gAppCon,&event);
    if(event.type==ButtonPress) {
      bev=(XButtonEvent*)&event;
      if(bev->button==2&&gLastWhWin>=0) {
        if(gWin[gLastWhWin]->whichRadio==3) {
          OneLnPerRowCB((Widget)2,(caddr_t)5,(caddr_t)14);
        } else {
          Say("This only works when we are doing\n\"Next 10\".");
        }
      }
    }
    XtDispatchEvent(&event);    /* Vol1 p509 */
    if(gDone) break;
  }
}
