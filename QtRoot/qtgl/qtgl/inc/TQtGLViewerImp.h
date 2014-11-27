// @(#)root/g3d:$Name:  $:$Id: TQtGLViewerImp.h,v 1.19 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      23/05/97

/****************************************************************************
** $Id: TQtGLViewerImp.h,v 1.19 2013/08/30 16:00:17 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifndef ROOT_TQtGLViewerImp
#define ROOT_TQtGLViewerImp

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtGLViewerImp                                                       //
//                                                                      //
// Second ABC TQtGLViewerImp specifies Window system independent openGL //
// interface. This class uses the GL includes and isn't passed to CINT  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <cassert>
#include "RVersion.h"
#if ROOT_VERSION_CODE >= 262400
// ROOT_VERSION(4,01,00)
#  include "TQGLViewerImp.h"
#else
#  include "TGLViewerImp.h"
#endif

#include "TString.h"
#include "Gtypes.h"

#ifndef __CINT__
#  include <QtGlobal>
#  include <QMainWindow>
#  include <QLabel>
#else
   class QMainWindow;
   class QString;
#endif
   
#include <vector>
   
class TVirtualPad;
class QGLWidget;
class TColor;
class TQtGLViewerWidget;
class TContextMenu;
class QAction;

#ifndef __CINT__
  class TQtGLViewerImp :public QMainWindow, public TGLViewerImp {
Q_OBJECT
#else 
//MOC_SKIP_BEGIN
  class TQtGLViewerImp : public TGLViewerImp {
//MOC_SKIP_END
#endif
private:
   TQtGLViewerImp(const TQtGLViewerImp&);
   void operator=(const TQtGLViewerImp&);
protected:
   TString         fSaveFile;           // the file name to save the pixmap to
   TString         fSaveType;           // the image format type name
   Int_t           fMaxSnapFileCounter; // The max number of the difffrent "snapshot files" (The length of the cyclic buffer)
   static Int_t    gfDefaultMaxSnapFileCounter; // the default max number of the different "snapshot files" (The length of the cyclic bugger)
   QGLWidget      *fGLWidget;           // QT GL widget to render the view
   TVirtualPad    *fPad;                // For forward compatibility with the new viewer
   TContextMenu   *fContextMenu;        // ROOT Context menu for the 3D widget
#ifndef __CINT__
//MOC_SKIP_BEGIN
   std::vector<QLabel*> fStatusBar;
//MOC_SKIP_END
#endif
   TQtGLViewerImp *fSelectedView;        // extra viewer to show the selected object only
   Bool_t          fSelectedViewActive;  // the flag to activate the "Selected view"
   Bool_t          fSelectionViewer;     // Flag to create the slave viewer with no own layout
   Bool_t          fSelectionHighlight;  // Flag to highlight the selection object in place
   Bool_t          fShowSelectionGlobal; // Show the selected object in the global coordinate
   Bool_t          fWantRootContextMenu; // Create "ROOT Context menu" for the seelcted ROOT objects
   QAction        *fSnapShotAction;      // QAction to toglle the snap shot file saving
   
protected:
   void CopyFile(const QString &fileName2Copy,Int_t counter);
   void CreateViewer(const char *name="qglviewer");
   void CreateViewer(QGLWidget *share, const char *name="qglviewershared");
   void MakeMenu();
   void SaveHtml(Int_t counter);
   void SaveHtml(QString &fileName, Int_t counter);
   void CreateSelectionViewer();
   static int CreateSnapShotCounter();

public:
   enum {kStatusPopIn, kStatusNoBorders, kStatusOwn, kStatusPopOut};
   TQtGLViewerImp();
   TQtGLViewerImp(TQtGLViewerImp&);
   TQtGLViewerImp(TPadOpenGLView *padview, const char *title="OpenGL Viewer", UInt_t width=400, UInt_t height=300);
   TQtGLViewerImp(TPadOpenGLView *padview, const char *title, Int_t x, Int_t y,UInt_t width, UInt_t height);
   TQtGLViewerImp(TVirtualPad *pad, const char *title="OpenGL Viewer", UInt_t width=400, UInt_t height=300);
   TQtGLViewerImp(TVirtualPad *pad, const char *title, Int_t x, Int_t y,UInt_t width, UInt_t height);

   virtual ~TQtGLViewerImp();
   virtual void   Clear(const char *opt=0);
   virtual void   CreateStatusBar(Int_t nparts=1);
   virtual void   CreateStatusBar(Int_t *parts, Int_t nparts=1);
   virtual void   DeleteContext() { }
   virtual void   MakeCurrent();
   virtual void   Paint(Option_t *opt="");

   virtual void   SetStatusText(const char *text, Int_t partidx=0,Int_t stype=-1);
   virtual void   ShowStatusBar(Bool_t show = kTRUE);

   virtual void   SwapBuffers() { };

   virtual UInt_t GetDrawList() { return fDrawList; }
   virtual void   SetDrawList(UInt_t list) { fDrawList = list; }

   virtual void   Iconify() { hide(); };
   virtual void   Show() { show(); };
   virtual void   Update();
   
   virtual ULong_t GetViewerID() const;   
   virtual void    SetPadSynchronize(Bool_t on=kTRUE);
   // static void     SetDefaultFileCounter(int counter);
   virtual void    DisconnectPad();
        QGLWidget *GLWidget() const { return fGLWidget;}
   virtual TVirtualPad *GetPad();
//  Qt proxy   
   virtual void SetUpdatesEnabled( bool);
   virtual bool GetUpdatesEnabled() const;
   virtual bool IsFullScreen( )     const;
   virtual bool ObjectPickEnabled() const;
   virtual Option_t   *GetDrawOption() const;


#ifndef __CINT__
  public slots:
     virtual void ActivateSelectorWidgetCB(bool);
     virtual void ActivateSelectionHighlighCB(bool);
     virtual void ActivateSelectionGlobalCB(bool);
     virtual void DisconnectSelectorWidgetCB();
     virtual void EnableObjectPick(bool enable=true);
     virtual void AddGLList(unsigned int list, EObject3DType type=kSolid);
     virtual void AddRootChild(ULong_t, EObject3DType ){ assert(0); /* this to be called for Coin3D only */ };
     virtual void RemoveGLList(unsigned int list);
     virtual void NewViewer();
     virtual void PrintCB();
     virtual void CopyCB();
     virtual void CopyFrameCB();
     virtual void ReadInputFile(const char *fileName);
     virtual void SaveCB();
     virtual void SaveAsCB();
     virtual void SelectEventCB(bool on);
     virtual void SelectDetectorCB(bool on);
     virtual void SetBackgroundColor(Color_t color);
     virtual void SetBackgroundColor(const TColor *color);
     virtual void SetDrawOption(Option_t *option="");
     virtual void ShowObjectInfo(TObject *, const QPoint&);
     virtual void SnapShotSaveCB(bool);
     virtual void SaveSnapShot(bool);
     virtual void Save(const QString &filename,const QString  &type="png");
     virtual void Save(const char *filename="", const char *type="png");
     virtual void Print(const char *filename="", const char *type="wrl");
     virtual void Print(const QString &filename,const QString  &type="wrl");
     virtual void ShowFrameAxisCB(bool);
     virtual void ShowLightsCB(bool);
     virtual void SynchTPadCB(bool);
     virtual void SetFullScreenView(bool);
     virtual void SetFooter(const char *);
     virtual void SetRotationAxisAngle(const float  x, const float  y, const float  z, const   float a);
     virtual void SetSnapFileCounter(int counter);
     virtual void SetFooter(QString &text);
     virtual void ViewAll();
     virtual void WantRootContextMenuCB(bool on);
     virtual void AboutCB();
     virtual void HelpCB();
#endif
//   ClassDef(TQtGLViewerImp,0)  //ROOT OpenGL viewer implementation
};

// $log$
#endif
