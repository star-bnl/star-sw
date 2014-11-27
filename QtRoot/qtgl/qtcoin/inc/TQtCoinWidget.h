// @(#)root/g3d:$Name:  $:$Id: TQtCoinWidget.h,v 1.40 2013/08/30 16:00:15 perev Exp $
// Author: Valery Fine      23/05/97

/****************************************************************************
** $Id: TQtCoinWidget.h,v 1.40 2013/08/30 16:00:15 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifndef ROOT_TQtCoinWidget
#define ROOT_TQtCoinWidget

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtCoinWidget                                                        //
//                                                                      //
// class TQtCoinWidget specifies Window system independent openGL       //
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

#ifndef __CINT__
#  include <qglobal.h>
#  if QT_VERSION < 0x40000
#    include <qframe.h>
#    include <qptrvector.h>
#  else /* QT_VERSION */
#    include <QFrame>
#    include <q3ptrvector.h>
#  endif /* QT_VERSION */
#  include <qlabel.h>
#else
   class QFrame;
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
class SmAxisKit;
class SoClipPlaneManip;
class TCoinAxisSeparator;
class SoFieldSensor;
class SoClipPlane;
class SoCallback;
class SoPath;
 
class QTabWidget;

//#include <qintdict.h>
//class TQtRootAction;
//====================   
   
#include <vector>

class QAction;
class QWidget;
class TVirtualPad;
class SoQtExaminerViewer;
class TObject3DView;
class TContextMenu;
class SoGLRenderAction;
class TSimageMovie;
class QCheckBox;
class SoEventCallback;
class SbVec3f;
class SoOffscreenRenderer;

#ifdef __CINT__
#  define COINWIDGETFLAGSTYPE  UInt_t
#else
#  if QT_VERSION < 0x40000
#    define COINWIDGETFLAGSTYPE  Qt::WindowFlags
#  else
#    define COINWIDGETFLAGSTYPE  Qt::WindowFlags
#  endif
#endif     

#if QT_VERSION < 0x40000
  class TQtCoinWidget :public QFrame, public TGLViewerImp {
#else /* QT_VERSION */
//MOC_SKIP_BEGIN
  class TQtCoinWidget :public QFrame, public TGLViewerImp {
//MOC_SKIP_END
#endif /* QT_VERSION */
Q_OBJECT	  
private:
	
   SoQtViewer             *fInventorViewer;
   SoSeparator            *fRootNode;
   SoSeparator            *fShapeNode;
   SoSeparator            *fWiredShapeNode;
   SoSeparator            *fClippingShapeNode;
   SoSeparator            *fSolidShapeNode;
   SoSeparator            *fRawShapeNode;
   SoSeparator            *fFileNode;
   SoSelection            *fSelNode;
   SoSeparator            *fAnnotation;
   SoNode                 *fFooterText;
   SoCamera               *fCamera;
   SmAxisDisplayKit       *fAxes;
   std::vector<int>        flist[3];
   TCoinAxisSeparator     *fXAxis;
   SoFieldSensor          *fCameraSensor;
   void                   *fPickedObject;
   float                   fPivotClipPoint[3];
   bool                    fEnableObjectPick;

   TQtCoinWidget(const TQtCoinWidget&);
   void operator=(const TQtCoinWidget&)  {}
protected:
   QString         fSaveFile;            // the file name to save the pixmap to
   QString         fSaveType;            // the image format type name
   QString         fSaveFileMoviePattern;// the file name to save the pixmap to, frame by frame
   Int_t           fMaxSnapFileCounter;  // The max number of the difffrent "snapshot files" (The length of the cyclic buffer)
   Int_t           fSnapshotCounter;     // Current value of the snapshot counter
   static Int_t    gfDefaultMaxSnapFileCounter; // the default max number of the different "snapshot files" (The length of the cyclic bugger)
   //QGLWidget      *fGLWidget;           // QT GL widget to render the view
   TVirtualPad    *fPad;                // For forward compatibility with the new viewer
   TContextMenu   *fContextMenu;        // ROOT Context menu for the 3D widget
   static Bool_t   fgCoinInitialized;
   TObject        *fSelectedObject;     // The last selected TObject

#ifndef __CINT__
#if QT_VERSION < 0x40000
   QPtrVector<QLabel> fStatusBar;
#else
//MOC_SKIP_BEGIN
   Q3PtrVector<QLabel> fStatusBar;
//MOC_SKIP_END
#endif
#endif

   //TQtCoinWidget *fSelectedView;        // extra viewer to show the selected object only
   //Bool_t          fSelectedViewActive;  // the flag to activate the "Selected view"
   //Bool_t          fSelectionViewer;     // Flag to create the slave viewer with no own layout
   //Bool_t          fSelectionHighlight;  // Flag to highlight the selection object in place
   //Bool_t          fShowSelectionGlobal; // Show the selected object in the global coordinate
   Bool_t          fWantRootContextMenu; // Create "ROOT Context menu" for the seelcted ROOT objects
   QAction        *fSnapShotAction;      // QAction to toglle the snap shot file saving
   SoGLRenderAction *fBoxHighlightAction;
   SoGLRenderAction *fLineHighlightAction;
   Bool_t            fWantClipPlane;       //
   SoClipPlaneManip *fClipPlaneMan; 
   SoClipPlane      *fClipPlane; 
   SoClipPlane      *fSlicePlane; 
   QTabWidget       *fHelpWidget;
   Bool_t            fRecord;
   SoCallback       *fMovie;
   TSimageMovie     *fMPegMovie;
   QCheckBox        *fClipPlaneState;
   SoPath           *fClipPlanePath;
   SoEventCallback  *fKeyboardHandler;
   Bool_t            fOffScreenBatch;    // The offscreen (batch) mode
   SoOffscreenRenderer *fOffScreenRender; 
   Bool_t            fAddBackground;
   unsigned int      fClipMask;// The mask to indicate which kind of shape can be clipped
   QString           fViewerDrawOption; // the comma seoparated lis of the draw options

protected:
   friend class TQtCoinViewerImp;
   void CreateViewer(const QString &title);
   void CreateViewer(const char *name="qcoinviewer");
   // void CreateViewer(QGLWidget *share, const char *name="qglviewershared"){;}
   virtual void EmitImageSaved(const QString &fileName,const QString &fileType, int frameCounter);
   //void CreateSelectionViewer();
   static int CreateSnapShotCounter();

   //TQtCoinWidget(TQtCoinWidget &);
   SoGLRenderAction &BoxHighlightAction();
   SoGLRenderAction &LineHighlightAction();
   QTabWidget* HelpWidget()   { return fHelpWidget; }
            void SetClipPlane(SoClipPlane *plane, int planeDirection);
            void SetActiveClipPlane(int planeDirection);
   virtual  void SetPad(TVirtualPad *pad);  
   virtual  bool OffScreenRender();
public:
   enum {kStatusPopIn, kStatusNoBorders, kStatusOwn, kStatusPopOut};
   TQtCoinWidget(QWidget *parent=0, COINWIDGETFLAGSTYPE f=0); 
   TQtCoinWidget(TVirtualPad *pad, const char *title="Coin Viewer", UInt_t width=400, UInt_t height=300);

   virtual ~TQtCoinWidget();
   void AddRootChild(ULong_t id, EObject3DType type=kSolid);
   virtual void   Clear(const char *opt=0);
   virtual void   CreateStatusBar(Int_t /*nparts=1*/ ){;}
   virtual void   CreateStatusBar(Int_t *parts, Int_t nparts=1);
   virtual TContextMenu &ContextMenu(); 
   //virtual void   DeleteContext() { }
   //virtual void   MakeCurrent();
   //virtual void   Paint(Option_t *opt="");

   //virtual void   SetStatusText(const char *text, Int_t partidx=0,Int_t stype=-1);
   //virtual void   ShowStatusBar(Bool_t show = kTRUE);

   //virtual void   SwapBuffers() { };
   SoCamera *GetCamera() const;
   SoCamera &Camera() const;
   SmAxisDisplayKit *GetAxis() const { return fAxes;}
   std::vector<int> GetMyGLList1() { return flist[0]; }
   std::vector<int> GetMyGLList2() { return flist[1]; }
   std::vector<int> GetMyGLList3() { return flist[2]; }
   //virtual void   SetDrawList(UInt_t list) { fDrawList = list; }

   //virtual void   Iconify() { hide(); };
   //virtual void   Show() { show(); };
   
   virtual ULong_t GetViewerID() const;   
   //virtual void    SetPadSynchronize(Bool_t on=kTRUE);
   // static void    SetDefaultFileCounter(int counter);
   virtual bool GetUpdatesEnabled() const;
   virtual void    DisconnectPad();
   //QGLWidget *GLWidget() const { return fGLWidget;}
   virtual TVirtualPad *GetPad();
   void CreateViewer(const int){;}
   bool ObjectPickEnabled() const { return  fEnableObjectPick; }

   void EmitSelectSignal(TObject *view);
   void EmitNodeSelectSignal(SoNode *node);
   Bool_t Recording()  const { return fRecord;}
   void SetBoxSelection();
   void SetLineSelection();
   SoSelection *GetSelectingNode()    const { return fSelNode;            }
   TObject     *GetSelected()         const { return fSelectedObject;     }
   SoQtViewer  *GetCoinViewer()       const { return fInventorViewer;     }
   Bool_t       IsOffScreen()         const { return fOffScreenBatch;     }
   Bool_t       WantRootContextMenu() const { return fWantRootContextMenu;}
   Bool_t       WasPicked(void *p) { 
      Bool_t res = (p != fPickedObject); if (res) fPickedObject = p; 
      return res; 
   }
   bool         IsFullScreen( )       const;
   const QString &SaveType()          const { return fSaveType; }
   const QString &SaveFile()          const { return fSaveFile; }
   const QString &SaveFilePattern()   const { return fSaveFileMoviePattern; }
   SoEventCallback *KeyboardHandler() const { return fKeyboardHandler;      }
   static void RotateCamera(SoCamera * cam,const SbVec3f & aroundaxis,const float delta);
   unsigned int ClipMask() const { return fClipMask; }
   virtual Option_t   *GetDrawOption() const;
   virtual unsigned  char GetLocation(int axIndex = 0);
   virtual TCoinAxisSeparator    *Axes();
#ifndef __CINT__
  public slots:
     //virtual void ActivateSelectorWidgetCB(bool);
     //virtual void ActivateSelectionHighlighCB(bool);
     //virtual void ActivateSelectionGlobalCB(bool);
     //virtual void DisconnectSelectorWidgetCB();
     virtual void AddGLList(unsigned int list, EObject3DType type=kSolid);
     virtual void EnableObjectPick(bool enable=true)  { fEnableObjectPick = enable; }
     virtual void RemoveGLList(unsigned int list);
     virtual void FrameAxisActionCB(bool);
      //virtual void NewViewer();
     virtual void PrintCB();
     virtual void ClipPlaneModeCB(int mode);
     virtual void CopyCB();
     virtual void CopyFrameCB();
     virtual void ReadInputFile(const char *fileName);
     virtual void ReadInputFile(const QString &fileName);
     virtual void RotateCamera(int axis,float angle);
     virtual void RotateCamera(int axis,bool clockWise=true);
     virtual void Save(const QString &fileName,const QString &type="png");
     virtual void SetClipPlaneMan(bool on=kTRUE,float x=1.0f, float y=0.0f, float z=0.0f);
     virtual void SetOffScreen(Bool_t offscreen=kTRUE);
     virtual void SetUpdatesEnabled(bool enable);
     virtual void SetFileName(const QString &fileName);
     virtual void SetFileType(const QString &fileType);
     virtual void SetClipMask(unsigned int mask);
     virtual void SetDrawOption(Option_t *option="");
     virtual void ClearCB();
     virtual void SaveAsCB();
     //virtual void SelectEventCB(bool on);
     //virtual void SelectDetectorCB(bool on);
     virtual void SetBackgroundColor(Color_t color);
     //virtual void SetBackgroundColor(const TColor *color);
     virtual void ShowObjectInfo(TObject *, const QPoint&);
     virtual void SnapShotSaveCB(bool);
     virtual void SetSnapshotCounter(int counter=2);
     virtual void SaveMpegShot(bool);
     virtual void SaveSnapShot(bool on=TRUE);
     virtual void Save(const char *filename="", const char *type="png");
     virtual void Print(const char *filename="", const char *type="wrl");
     virtual void Print(const QString &filename,const QString  &type="wrl");     
     virtual void SmallAxesActionCB(bool on);
     virtual void ShowFrameAxisCB(bool);
     virtual void ShowLightsCB(bool);
     virtual void SynchTPadCB(bool);
     virtual void StartRecordingCB(bool on=kTRUE);
     virtual void StopRecordingCB(bool on=kTRUE);
     virtual void SetRotationAxisAngle(const float  x, const float  y, const float  z, const float a);
     virtual void SetSnapFileCounter(int counter);
     virtual void Update();
     virtual void ViewPlaneX() const;
     virtual void ViewPlaneY() const;
     virtual void ViewPlaneZ() const;
     virtual void SetClipPlaneXCB();
     virtual void SetClipPlaneYCB();
     virtual void SetClipPlaneZCB();
     virtual void SetSlicePlaneCB();
     virtual void ViewAll();
     virtual void SetFooter(const char *text);
     virtual void SetFooter(QString &text);
     virtual void SetFullScreenView(bool);
     virtual void WantRootContextMenuCB(bool on);
     virtual void AboutCB();
     virtual void HelpCB();
     virtual void SetLocation(unsigned  char location, int axIndex = 0);
  signals:
       void ObjectSelected(TObject *, const QPoint&);
       void NodeSelected(ULong_t, const QPoint&);
       void NextFrameReady(bool on=TRUE);
       void ImageSaved(const QString &fileName,const QString &fileType, int frameCounter);
#endif

//   ClassDef(TQtCoinWidget,0)  //ROOT OpenGL viewer implementation
};

// $log$
#endif
