// @(#)root/win32gdk:$Name:  $:$Id: TGQtGL.h,v 1.4 2013/08/30 15:59:48 perev Exp $
// Author: Valeriy Onuchin  05/08/04

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TGQtGL
#define ROOT_TGQtGL


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TGQtGL                                                            //
//                                                                      //
// The TGQtGL is win32gdk implementation of TVirtualGLImp class.     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TVirtualGL
#include "TVirtualGL.h"
#endif
#ifndef ROOT_TVirtualViewer3D
#include "TVirtualViewer3D.h"
#endif

#if 0
class TGQtGL : public TVirtualGLImp {

public:
   TGQtGL();
   ~TGQtGL();

   Window_t CreateGLWindow(Window_t wind);
   ULong_t  CreateContext(Window_t wind);
   void     DeleteContext(ULong_t ctx);
   void     MakeCurrent(Window_t wind, ULong_t ctx);
   void     SwapBuffers(Window_t wind);

   ClassDef(TGQtGL,0);
};

class TGQtGLManager : public TGLManager {
private:
	class TGQtGLImpl;
	TGQtGLImpl *fPimpl;

public:
	TGQtGLManager();
	~TGQtGLManager();

   //All public functions are TGLManager's final-overriders

   //index returned can be used as a result of gVirtualX->InitWindow
   Int_t    InitGLWindow(Window_t winID);
   //winInd is the index, returned by InitGLWindow
   Int_t    CreateGLContext(Int_t winInd);

   //[            Off-screen rendering part
   //create DIB section to read GL buffer into it, 
   //ctxInd is the index, returned by CreateGLContext
   Bool_t   AttachOffScreenDevice(Int_t ctxInd, Int_t x, Int_t y, UInt_t w, UInt_t h);
   Bool_t   ResizeOffScreenDevice(Int_t devInd, Int_t x, Int_t y, UInt_t w, UInt_t h);
   //analog of gVirtualX->SelectWindow(fPixmapID) => gVirtualGL->SelectOffScreenDevice(fPixmapID)
   void     SelectOffScreenDevice(Int_t devInd);
   //Index of DIB, valid for gVirtualX
   Int_t    GetVirtualXInd(Int_t devInd);
   //copy DIB into window directly/by pad
   void     MarkForDirectCopy(Int_t devInd, Bool_t);
   //Off-screen device holds sizes for glViewport
   void     ExtractViewport(Int_t devInd, Int_t *vp);
   //Read GL buffer into DIB
   void     ReadGLBuffer(Int_t devInd);
   //]            

   //Make the gl context current
   Bool_t   MakeCurrent(Int_t devInd);
   //Swap buffers or "blits" DIB
   void     Flush(Int_t ctxInd);
   //Generic function for gl context and off-screen device deletion
   void     DeleteGLContext(Int_t devInd);

   //functions to switch between threads in win32
   //used by viewer
   void     DrawViewer(TVirtualViewer3D *vv);
   TObject* Select(TVirtualViewer3D *vv, Int_t x, Int_t y);
   void     PaintSingleObject(TVirtualGLPainter *);
   void     PrintViewer(TVirtualViewer3D *vv);

private:
   struct TGLContext;
	Bool_t   CreateDIB(TGLContext &ctx)const;

   TGQtGLManager(const TGQtGLManager &);
   TGQtGLManager &operator = (const TGQtGLManager &);

	ClassDef(TGQtGLManager, 0)
};

#endif
#endif
