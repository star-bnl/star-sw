/* This file is the result of modifications applied to a
 * file written by the people mentioned immediately
 * below. HW 5/23/95 */
/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */
#ifndef WIN32
/***********************************************************  TYPEDEFS  **/

/**********************************************************  INCLUDES  **/
#include <stdio.h>
#include <string.h>
#include <Xm/DrawingA.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#define PP printf(
static Window gGraphicsWindow;
Widget gDrawWrapper,gDrawingArea;
Pixmap gPM;
GC gGraphicsContext;
Widget Row(Widget parent);
unsigned int gWidth,gHeight;

void ExposeCB(Widget w,caddr_t cld,caddr_t cad) {
  /*---------------------------------
  XmDrawingAreaCallbackStruct *cbs=(XmDrawingAreaCallbackStruct*)cad;
  XCopyArea(cbs->event->xexpose.display,gPM,cbs->window,gGraphicsContext,
        0,0,gWidth,gHeight,0,0);
  -----------------------------------------------------*/
  XCopyArea(XtDisplay(gDrawingArea),gPM,XtWindow(gDrawingArea),
      gGraphicsContext,0,0,gWidth,gHeight,0,0);
}
void SetColor(XtPointer cld) { /*makeproto*/
  String color = (String) cld; Display *dpy = XtDisplay(gDrawingArea);
  Colormap cmap = DefaultColormapOfScreen(XtScreen(gDrawingArea));
  XColor col,unused;
  if(!XAllocNamedColor(dpy,cmap,color,&col,&unused)) {
    char buf[62]; sprintf(buf,"Can't alloc %s",color); XtWarning(buf); return;
  }
  XSetForeground(dpy,gGraphicsContext,col.pixel);
}
void dsu_Clear(void) {
  /* Don't call this (or XtWindow) before realization of gDrawingArea) */
  XClearWindow(XtDisplay(gDrawingArea),XtWindow(gDrawingArea));
  SetColor("white");
  XFillRectangle(XtDisplay(gDrawingArea),gPM,gGraphicsContext,0,0,
  gWidth,gHeight);
}
void MakeDrawingArea(Widget parent,Dimension width,Dimension height) {
  Arg args[40]; int nn;
  XGCValues gcv;
  gDrawWrapper=Row(parent);
  nn=0;
  XtSetArg(args[nn],XmNwidth,width); nn++;
  XtSetArg(args[nn],XmNheight,height); nn++;

  XtSetArg(args[nn],XmNtopAttachment,XmATTACH_FORM); nn++;
  XtSetArg(args[nn],XmNtopOffset,10); nn++;
  XtSetArg(args[nn],XmNleftAttachment,XmATTACH_FORM); nn++;
  XtSetArg(args[nn],XmNleftOffset,10); nn++;

  gDrawingArea=XmCreateDrawingArea(gDrawWrapper,"da",args,nn);
  XtAddCallback (gDrawingArea, XmNexposeCallback,
     (XtCallbackProc)ExposeCB, NULL);
  gGraphicsWindow=XtWindow(gDrawingArea);
  gcv.foreground = BlackPixelOfScreen (XtScreen (gDrawingArea));
  gGraphicsContext = XCreateGC (XtDisplay (gDrawingArea),
      RootWindowOfScreen (XtScreen (gDrawingArea)), GCForeground, &gcv);
  XtManageChild(gDrawingArea);
  gWidth=width; gHeight=height;
  gPM=XCreatePixmap(XtDisplay(gDrawingArea),
        RootWindowOfScreen(XtScreen(gDrawingArea)),gWidth,gHeight,
        DefaultDepthOfScreen(XtScreen(gDrawingArea)));
}
void DrawStringXOnly(int xx,int yy,char *cc) {
  Position x,y; int len;
  x=xx; y=yy; len=strlen(cc);
  SetColor("black");
  /* XDrawString(XtDisplay(gDrawingArea),XtWindow(gDrawingArea),
      gGraphicsContext,x,y,cc,len); */
  XDrawString(XtDisplay(gDrawingArea), gPM,gGraphicsContext,
      x,y,cc,len);
}
void DrawDotXOnly(int nbin,int wide,int hite,int size,int max,int xx,int yy) {
  unsigned int sizex,sizey;
  SetColor("black");
  sizey=0.5+(1.0*hite *size)/(1.0*nbin*max);
  sizex=sizey;  /* sizex=0.5+(1.0*wide*size)/(1.0*nbin*max); */
  XFillArc(XtDisplay(gDrawingArea),gPM,gGraphicsContext,
                (int)(xx-sizex/2),(int)(yy-sizey/2),sizex,sizey,0,23040);
}
void DrawLineXOnly(int xx1,int yy1,int xx2,int yy2) {
  Position x1,y1; Position x2,y2;
  x2=xx2; y2=yy2; x1=xx1; y1=yy1;
  SetColor("black");
  XDrawLine(XtDisplay(gDrawingArea), gPM,gGraphicsContext,
                x1,y1,x2,y2);
  /* XDrawLine(XtDisplay(gDrawingArea), 
     XtWindow(gDrawingArea),gGraphicsContext,x1,y1,x2,y2); */
}
/* XClearWindow (event->xany.display, XtWindow (XtParent (widget))); */

#endif
