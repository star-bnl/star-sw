// @(#)root/g3d:$Name:  $:$Id: TQGLViewerImp.h,v 1.22 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      12/03/2005

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/


#ifndef ROOT_TQGLViewerImp
#define ROOT_TQGLViewerImp
#include "RVersion.h"
//#if ROOT_VERSION_CODE >= ROOT_VERSION(4,01,00)
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TGLViewerImp                                                         //
//                                                                      //
// Second ABC TGLViewerImp specifies Window system independent openGL   //
// interface. This class uses the GL includes and isn't passed to CINT  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_Rtypes
# include "Rtypes.h"
#endif

#ifndef ROOT_Gtypes
# include "Gtypes.h"
#endif

#ifndef ROOT_Buttons
# include "Buttons.h"
#endif

class TGLViewerImp;
class QPoint;

#ifndef __CINT__
#include <qobject.h>
#include <qstring.h>
class TQtSlotProxy : public QObject {
   Q_OBJECT
private:
   TQtSlotProxy(const TQtSlotProxy&); 
   void operator=(const TQtSlotProxy&);   
protected:
   friend class TGLViewerImp;
   TGLViewerImp *fMaster;
   TQtSlotProxy (TGLViewerImp *master=0) {fMaster = master;}
   virtual ~TQtSlotProxy() {}
   void EmitObjectSelected(TObject *, const QPoint&);
   void EmitHandleSelected(ULong_t , const QPoint&);
   
protected slots:
   void Disconnect();
   void DestroyMaster();
   
signals:
   void Destroyed( TGLViewerImp *);
   void ObjectSelected(TObject *, const QPoint&);
   void HandleSelected(ULong_t, const QPoint&);
   void ImageSaved(QString fileName,QString fileType, int frameCounter);
};

#else
class TQtSlotProxy;
class QString;
#endif

class TPadOpenGLView;
class TGLViewerImp  {

private:
   TQtSlotProxy fProxy;
   TGLViewerImp(const TGLViewerImp&);
   void operator=(const TGLViewerImp&);
protected:
   friend class TPadOpenGLView;
   UInt_t               fDrawList;      // GL list used to redraw the contents
   TPadOpenGLView      *fGLView;        // Pointer to Pad GL View object
   Bool_t               fPaint;         // Allows "Refresh" OpenGL window
   virtual void Disconnect(){ fGLView = 0;} // to be called from TPadOpenGLView dtor
public:
   enum {kStatusPopIn,  kStatusNoBorders, kStatusOwn, kStatusPopOut};
   enum  EObject3DType { kWired, kSolid, kSelecting, kSelected, kRaw };
   TGLViewerImp();
   TGLViewerImp(TPadOpenGLView *padview, const char *title="OpenGL Viewer", UInt_t width=400, UInt_t height=300);
   TGLViewerImp(TPadOpenGLView *padview, const char *title, Int_t x, Int_t y,UInt_t width, UInt_t height);
   static QString GetSnapShotFileName();
   static void MakeCurrent(TGLViewerImp *viewer);
   static TGLViewerImp *CurrentViewer();
   virtual ~TGLViewerImp();

   virtual void   CreateContext() { }
   virtual void   CreateStatusBar(Int_t nparts=1);
   virtual void   CreateStatusBar(Int_t *parts, Int_t nparts=1);
   virtual void   DeleteContext() { }
   virtual void   DeleteView();
   virtual void   HandleInput(EEventType button, Int_t x, Int_t y);
   TPadOpenGLView *GetGLView();
   virtual void   MakeCurrent() { MakeCurrent(this); }
   virtual void   Paint(Option_t *opt="");

   virtual void   SetStatusText(const char *text, Int_t partidx=0,Int_t stype=0);
   virtual void   ShowStatusBar(Bool_t show = kTRUE);

   virtual void   SwapBuffers() { };

   virtual UInt_t GetDrawList() { return fDrawList; }
   virtual void   SetDrawList(UInt_t list) { fDrawList = list; }

   virtual void   Iconify() { };
   virtual void   Show() { };
   virtual void   Update() { fPaint = kTRUE; }
   virtual ULong_t GetViewerID() const                 = 0;
   virtual void   Clear(const char *opt=0)             = 0;
   
   virtual void   SetDrawOption(Option_t *option="")   = 0;
   virtual Option_t   *GetDrawOption() const           = 0;

// New methods for Qt
   virtual void AddGLList(unsigned int list, EObject3DType type=kSolid)= 0;
   virtual void AddRootChild(ULong_t id, EObject3DType type=kSolid)    = 0;
   virtual void DisconnectPad()                         = 0;
   virtual void EnableObjectPick(bool enable=true)      = 0;
   virtual bool ObjectPickEnabled()   const             = 0;
   virtual void ReadInputFile(const char *fileName)     = 0;
   virtual void RemoveGLList(unsigned int list)         = 0;
   virtual void SetBackgroundColor(Color_t color)       = 0;
   virtual void SaveSnapShot(bool)                      = 0;
   virtual void Save(const char *filename="", const char *type="png") = 0;
   virtual void Print(const char *filename="", const char *type="wrl") = 0;
   virtual void ShowFrameAxisCB(bool)                   = 0;
   virtual void ShowLightsCB(bool)                      = 0;
   virtual void SynchTPadCB(bool)                       = 0;
   virtual void SetFullScreenView(bool)                 = 0;
   virtual void SetFooter(const char *)                 = 0;
   virtual void SetRotationAxisAngle(const float  x, const float  y, const float  z, const   float a)=0;
   virtual void SetSnapFileCounter(int counter)         = 0;
   virtual void ViewAll()                               = 0;
   
// Method to be forwarded to the QObjects:
   virtual void SetUpdatesEnabled( bool)                = 0;
   virtual bool GetUpdatesEnabled() const               = 0;
   virtual bool IsFullScreen()      const               = 0;
// Off screen rendering   
   virtual Bool_t IsOffScreen()     const { return kFALSE;} 
   virtual void SetOffScreen(Bool_t offscreen=kTRUE) {if (offscreen){;} }

//Methods to provide Qt Signal/Slot communication
   TQtSlotProxy &Signals()             { return fProxy;}
   const TQtSlotProxy &Signals() const { return fProxy; /* ((TQtSlotProxy *)this)->Signal(); */ }
   TQtSlotProxy &Slots()               { return Signals();}
   const TQtSlotProxy &Slots() const   { return Signals();}

   ClassDef(TGLViewerImp,0)  //ROOT OpenGL viewer implementation
};

R__EXTERN TGLViewerImp *gGLViewerImp;

inline void TGLViewerImp::CreateStatusBar(Int_t) { }
inline void TGLViewerImp::CreateStatusBar(Int_t *, Int_t) { }
inline TPadOpenGLView *TGLViewerImp::GetGLView() { return fGLView;}
inline void TGLViewerImp::SetStatusText(const char *, Int_t, Int_t) { }
inline void TGLViewerImp::ShowStatusBar(Bool_t) { }

// $log$
//#else
//#  include "TGLViewerImp.h"
// #endif
#endif

