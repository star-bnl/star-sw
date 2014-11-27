// @(#)root/g3d:$Name:  $:$Id: TQtCoinViewerImp.h,v 1.27 2013/08/30 16:00:15 perev Exp $
// Author: Valery Fine      23/05/97

/****************************************************************************
** $Id: TQtCoinViewerImp.h,v 1.27 2013/08/30 16:00:15 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifndef ROOT_TQtCoinViewerImp
#define ROOT_TQtCoinViewerImp

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtCoinViewerImp                                                     //
//                                                                      //
// Second ABC TQtCoinViewerImp specifies Window system independent openGL //
// interface. This class uses the GL includes and isn't passed to CINT  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "RVersion.h"
#if ROOT_VERSION_CODE >= 262400
// ROOT_VERSION(4,01,00)
#  include "TQGLViewerImp.h"
#else
#  include "TGLViewerImp.h"
#endif

#include "TString.h"

#ifndef QT_VERSION
#define QT_VERSION 0x30307
#endif

#ifndef __CINT__
#  include <qglobal.h>
#  if QT_VERSION < 0x40000
#    include <qmainwindow.h>
#    include <qptrvector.h>
#  else /* QT_VERSION */
#    include <QMainWindow>
#    include <q3ptrvector.h>
#  endif /* QT_VERSION */
#  include <qlabel.h>
#else
   class QMainWindow;
   class QString;
#endif

//====================
class SoSeparator;
class SoNode;
class SoMaterial;
class SoCallback;
class SoQtViewer;
class SoPerspectiveCamera;
class SoCamera;
class SoSelection;
class SmAxisDisplayKit;
class SoClipPlaneManip;
class SmAxisKit;
class SoFieldSensor;
class SoClipPlane;

//#include <qintdict.h>
//class TQtRootAction;
//====================   
   
#include <vector>

class QAction;
class TVirtualPad;
class SoQtExaminerViewer;
class TObject3DView;
class TContextMenu;
class SoGLRenderAction;
class TQtCoinWidget;

class TQtCoinViewerImp :public QMainWindow, public TGLViewerImp {
  Q_OBJECT	  
private:
   TQtCoinWidget         *fCoinWidget;
   
   TQtCoinViewerImp(const TQtCoinViewerImp&);
   void operator=(const TQtCoinViewerImp&)  {}
protected:
   //static Int_t    gfDefaultMaxSnapFileCounter; // the default max number of the different "snapshot files" (The length of the cyclic bugger)
   //QGLWidget      *fGLWidget;           // QT GL widget to render the view


#ifndef __CINT__
#if QT_VERSION < 0x40000
   QPtrVector<QLabel> fStatusBar;
#else
//MOC_SKIP_BEGIN
   Q3PtrVector<QLabel> fStatusBar;
//MOC_SKIP_END
#endif
#endif

   //TQtCoinViewerImp *fSelectedView;        // extra viewer to show the selected object only
   //Bool_t          fSelectedViewActive;  // the flag to activate the "Selected view"
   //Bool_t          fSelectionViewer;     // Flag to create the slave viewer with no own layout
   //Bool_t          fSelectionHighlight;  // Flag to highlight the selection object in place
   //Bool_t          fShowSelectionGlobal; // Show the selected object in the global coordinate
   QAction        *fSnapShotAction;        // QAction to toggle the snap shot file saving
   QAction        *fShowFrameAxisAction;   // QAction to toggle the the 3D axes on /off 
     
   
protected:
   void CopyFile(const QString &fileName2Copy,Int_t counter);
   void CreateViewer(const char *name="qcoinviewer");
   // void CreateViewer(QGLWidget *share, const char *name="qglviewershared"){;}
   void MakeMenu();
   //void SaveHtml(Int_t counter);
   //void SaveHtml(QString &fileName, Int_t counter);
   //void CreateSelectionViewer();
   //static int CreateSnapShotCounter();

   //TQtCoinViewerImp(TQtCoinViewerImp &);
   SoGLRenderAction &BoxHighlightAction();
   SoGLRenderAction &LineHighlightAction();
   void SetCliPlaneMan(bool on=kTRUE);
public:
   enum {kStatusPopIn, kStatusNoBorders, kStatusOwn, kStatusPopOut};
   //TQtCoinViewerImp();
   //TQtCoinViewerImp(TPadOpenGLView *padview, const char *title="OpenGL Viewer", UInt_t width=400, UInt_t height=300);

   //TQtCoinViewerImp(TPadOpenGLView *padview, const char *title, Int_t x, Int_t y,UInt_t width, UInt_t height);
   TQtCoinViewerImp(TVirtualPad *pad, const char *title="Coin Viewer", UInt_t width=400, UInt_t height=300);
   //TQtCoinViewerImp(TVirtualPad *pad, const char *title, Int_t x, Int_t y,UInt_t width, UInt_t height);

   virtual ~TQtCoinViewerImp();
   void AddRootChild(ULong_t id, EObject3DType type=kSolid);
   virtual void   Clear(const char *opt=0);
   virtual void   CreateStatusBar(Int_t /* nparts=1 */ ){;}
   virtual void   CreateStatusBar(Int_t *parts, Int_t nparts=1);
   virtual TContextMenu &ContextMenu(); 
   //virtual void   DeleteContext() { }
   //virtual void   MakeCurrent();
   //virtual void   Paint(Option_t *opt="");

   //virtual void   SetStatusText(const char *text, Int_t partidx=0,Int_t stype=-1);
   //virtual void   ShowStatusBar(Bool_t show = kTRUE);

   //virtual void   SwapBuffers() { };
   SoCamera *GetCamera() const; 
   SmAxisDisplayKit *GetAxis() const;
   std::vector<int> GetMyGLList1() const ;
   std::vector<int> GetMyGLList2() const ;
   std::vector<int> GetMyGLList3() const ;
   //virtual void   SetDrawList(UInt_t list) { fDrawList = list; }

   //virtual void   Iconify() { hide(); };
   //virtual void   Show() { show(); };
   virtual void   Update();
   
   virtual ULong_t GetViewerID() const;   
   //virtual void    SetPadSynchronize(Bool_t on=kTRUE);
   // static void    SetDefaultFileCounter(int counter);
   virtual bool  GetUpdatesEnabled() const;
   virtual void    DisconnectPad();
   //QGLWidget *GLWidget() const { return fGLWidget;}
   virtual TVirtualPad *GetPad();
   void CreateViewer(const int /* id */ ){;}
   void SetBoxSelection();
   void SetLineSelection();
   virtual bool ObjectPickEnabled()   const;
   TObject     *GetSelected()         const;
   SoQtViewer  *GetCoinViewer()       const;
   Bool_t       WantRootContextMenu() const;
   Bool_t       WasPicked(void *p)    const;
   bool         IsFullScreen( )       const;
// Off screen rendering   
   virtual Bool_t IsOffScreen()     const;
   virtual void SetOffScreen(Bool_t offscreen=kTRUE);
   virtual Option_t   *GetDrawOption() const;
  
#ifndef __CINT__
  public slots:
     //virtual void ActivateSelectorWidgetCB(bool);
     //virtual void ActivateSelectionHighlighCB(bool);
     //virtual void ActivateSelectionGlobalCB(bool);
     //virtual void DisconnectSelectorWidgetCB();
     virtual void EnableObjectPick(bool enable=true);
     virtual void AddGLList(unsigned int list, EObject3DType type=kSolid);
     virtual void RemoveGLList(unsigned int list);
     virtual void FrameAxisActionCB(bool);
      //virtual void NewViewer();
     virtual void PrintCB();
     virtual void CopyCB();
     virtual void CopyFrameCB();
     virtual void SaveCB();
     virtual void OpenCB();
     virtual void ClearCB();
     virtual void ReadInputFile(const char *fileName);
     virtual void SaveAsCB();
     //virtual void SelectEventCB(bool on);
     //virtual void SelectDetectorCB(bool on);
     virtual void SetBackgroundColor(Color_t color);
     //virtual void SetBackgroundColor(const TColor *color);
     virtual void SetDrawOption(Option_t *option="");
     virtual void ShowObjectInfo(TObject *, const QPoint&);
     virtual void SnapShotSaveCB(bool);
     virtual void SaveSnapShot(bool);
     virtual void Save(const QString &filename,const QString  &type="png");
     virtual void Save(const char *filename="", const char *type="png");
     virtual void Print(const char *filename="", const char *type="wrl");
     virtual void Print(const QString &filename,const QString  &type="wrl");
     virtual void SmallAxesActionCB(bool on);
     virtual void ShowFrameAxisCB(bool);
     virtual void ShowLightsCB(bool);
     virtual void SynchTPadCB(bool);
     virtual void SetRotationAxisAngle(const float  x, const float  y, const float  z, const float a);
     virtual void SetSnapFileCounter(int counter);
     virtual void SetUpdatesEnabled(bool);
     virtual void SetAxisPositionCB(QAction *, int axIndex=0);
     virtual void SetAxisPositionXCB(QAction *);
     virtual void SetAxisPositionYCB(QAction *);
     virtual void SetAxisPositionZCB(QAction *);
     virtual void SetFooter(const char *);
     virtual void SetFooter(QString &text);
     virtual void SetFullScreenView(bool);
     virtual void ViewAll();
     virtual void WantRootContextMenuCB(bool on);
     virtual void WantClipFileNodeMenuCB(bool);
     virtual void AboutCB();
     virtual void HelpCB();
  signals:
       void ObjectSelected(TObject *, const QPoint&);
#endif

//   ClassDef(TQtCoinViewerImp,0)  //ROOT OpenGL viewer implementation
};

// $log$
#endif
