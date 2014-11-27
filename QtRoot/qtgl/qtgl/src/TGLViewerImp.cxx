// @(#)root/g3d:$Name:  $:$Id: TGLViewerImp.cxx,v 1.9 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      23/05/97

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TGLViewerImp                                                         //
//                                                                      //
// TGLViewerImp is abstract OpenGL viewer. Actual implementations are   //
// TRootGLViewer and TWin32GLViewerImp. The TGLViewer is used by the    //
// TPadOpenGLView.                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "RVersion.h"
#if ROOT_VERSION_CODE >= ROOT_VERSION(4,01,00)

//  This class was relocated from ROOT 4.00.08 g3d package

#include "TQGLViewerImp.h"
#include "TGuiFactory.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TEnv.h"
#include "TQPadOpenGLView.h"
#include "TQVirtualGL.h"

//______________________________________________________________________________
QString TGLViewerImp::GetSnapShotFileName() 
{
      QString fullShapShotFileName;

      // Create the default SnapShot file name and type if any
      const char *fileDir = gSystem->Getenv("SnapShotDirectory");
      if (!(fileDir  && fileDir[0]) && ( gEnv ) ) {
           fileDir  = gEnv->GetValue("Gui.SnapShotDirectory",(const char *)0);
      }
      if (fileDir  && fileDir[0]) {
             fullShapShotFileName = fileDir;
             fullShapShotFileName += "/"; 
      }

      fileDir = gSystem->Getenv("SnapShotFile");
      if (!(fileDir  && fileDir[0]) && ( gEnv ) ) {
           fileDir  = gEnv->GetValue("Gui.SnapShotFile",(const char *)0);
      }
      printf("\n\n -- %s -- \n",fileDir ); 
      if (fileDir  && fileDir[0])    fullShapShotFileName += fileDir;

  return fullShapShotFileName;
}

//______________________________________________________________________________
void TQtSlotProxy::Disconnect()
{
   if (fMaster){  /* fMaster->fViewer = 0; fMaster->Disconnect();  */ } 
}
//______________________________________________________________________________
void TQtSlotProxy::DestroyMaster()
{ 
   // emit the Qt signal on behalf of non-QObject interface
   Disconnect();
   TGLViewerImp *master = fMaster; 
   fMaster = 0; 
   emit Destroyed(master);
   delete master;
}

//______________________________________________________________________________
void  TQtSlotProxy::EmitObjectSelected(TObject *o, const QPoint&p)
{
   // emit the Qt signal on behalf of non-QObject interface
   emit ObjectSelected(o,p);
}

ClassImp(TGLViewerImp)

TGLViewerImp *gGLViewerImp = 0;

//______________________________________________________________________________
void TGLViewerImp::MakeCurrent(TGLViewerImp *viewer)
{ 
   // static: set the current viewer
   gGLViewerImp = viewer; 
}
//______________________________________________________________________________
TGLViewerImp *TGLViewerImp::CurrentViewer() 
{ 
   // static: get the current viewer
   return gGLViewerImp; 
}

//______________________________________________________________________________
TGLViewerImp::TGLViewerImp(): fProxy(this)
{
    fDrawList = 0;
    fGLView   = 0;
    fPaint    = kFALSE;
}

//______________________________________________________________________________
TGLViewerImp::TGLViewerImp(TPadOpenGLView *, const char *, UInt_t, UInt_t) : fProxy(this)
{
    fDrawList = 0;
    fGLView   = 0;
    fPaint    = kFALSE;
}

//______________________________________________________________________________
TGLViewerImp::TGLViewerImp(TPadOpenGLView *, const char *, Int_t, Int_t, UInt_t, UInt_t)
             : fProxy(this)
{
    fDrawList = 0;
    fGLView   = 0;
    fPaint    = kFALSE;
}

//______________________________________________________________________________
TGLViewerImp::~TGLViewerImp()
{
   // break view / viewer relationship
  if (CurrentViewer() == this) {
     MakeCurrent(0);
  }
  if (fGLView) {
      TPadOpenGLView *view = fGLView;
      fGLView = 0;
      view->Disconnect();
      delete view;
   }
   fPaint    = kFALSE;
   // Delete the browser.

//   gROOT->GetListOfGLViewers()->Remove(this);
}

//______________________________________________________________________________
void TGLViewerImp::DeleteView()
{ 
   if(fGLView) { 
      // some protection  to avoid a cross deleting 
      // of the  "view" and "viewer"
      TPadOpenGLView *view = fGLView;
      fGLView = 0;
      delete view; 
   } 
}

//______________________________________________________________________________
void TGLViewerImp::HandleInput(EEventType button, Int_t x, Int_t y)
{
    if (!fGLView) return;

    switch (button) {
    case kButton1Down:
    case kButton1Up:
    case kButton1Motion:
    case kKeyPress :
        fGLView->ExecuteEvent(button,x,y);
        break;
    default:
        break;
    }
}

//______________________________________________________________________________
void TGLViewerImp::Paint(Option_t *)
{
 //   if (fPaint)
    {
        MakeCurrent();
        if (fGLView) fGLView->Paint();
        //*-* Run extra GL list if any
        UInt_t list = 0;
        if ((list=GetDrawList()))
            gQVirtualGL->RunGLList(list);
        gQVirtualGL->FlushGL();
        SwapBuffers();
//        SetStatusText("Done",1);
 //       fPaint = kFALSE;
    }
}
#endif
