// @(#)root/x3d:$Name:  $:$Id: TQtViewerX3D.h,v 1.3 2013/08/30 16:00:29 perev Exp $
// Author: Valeri Fine   25/10/02

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/
#ifndef ROOT_TQtViewerX3D
#define ROOT_TQtViewerX3D

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtViewerX3D                                                         //
//                                                                      //
// QT-based implementation of the X3D viewer                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "qmainwindow.h"
#endif

#include "TString.h"

class TVirtualPad;
class QMouseEvent;

#ifdef __CINT__
// MOC_SKIP_BEGIN
class TQtViewerX3D {
// MOC_SKIP_END
#else
class TQtViewerX3D  : public QMainWindow {
  Q_OBJECT
#endif
// friend class TQtX3DWidget;

private:
   TVirtualPad    *fPad;                // pad that should be displayed in X3D
   TString         fOption;             // option string to be passed to X3D
   TString         fSaveFile;           // the file name to save the pixmap to
   TString         fSaveType;           // the image format type name

   void     CreateViewer(const char *name);
   void     InitX3DWindow();
   void     MakeMenu();
protected:
   static Bool_t fgActive;    // TQtViewerX3D is a singleton
public:
   TQtViewerX3D();
   TQtViewerX3D(TVirtualPad *pad, Option_t *option, const char *title="X3D Viewer",
              UInt_t width = 800, UInt_t height = 600);
   TQtViewerX3D(TVirtualPad *pad, Option_t *option, const char *title,
              Int_t x, Int_t y, UInt_t width, UInt_t height);
   virtual ~TQtViewerX3D();

   Int_t    ExecCommand(int px, int py, char cmd);
   void     GetPosition(Float_t &longitude, Float_t &latitude, Float_t &psi);
   void     Iconify() { }
   void     Update();

//   Bool_t   ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
#ifndef __CINT__
  public slots:
     void Disconnect();
     void NewViewer();
     void PrintCB();
     void CopyCB();
     void CopyFrameCB();
     void SaveCB();
     void SaveAsCB();
     void AboutCB();
     void HelpCB();
     void SetPadView(float longitude_rad, float latitude_rad, float psi);
  signals:
    void executeCommand(int px, int py, char cmd);
#endif

// MOC_SKIP_BEGIN
   ClassDef(TQtViewerX3D,0);  //C++ interface to the X3D viewer
// MOC_SKIP_END
};
#endif

