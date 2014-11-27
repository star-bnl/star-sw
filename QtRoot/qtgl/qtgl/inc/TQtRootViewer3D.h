// @(#)root/base:$Name:  $:$Id: TQtRootViewer3D.h
// Author: Valeri Fine 05/10/2004

/*************************************************************************
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtRootViewer3D
#define ROOT_TQtRootViewer3D

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtRootViewer3D                                                     //
//                                                                      //
// Abstract 3D shapes viewer. The concrete implementations are:         //
//                                                                      //
// TViewerX3D   : X3d viewer                                            //
// TViewerOpenGL: OpenGL viewer                                         //
// TViewerPad3D : visualise the 3D scene in the current Pad             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

#include "RVersion.h"
#include "TObject3DView.h"
#include "TVirtualViewer3D.h"

#ifndef __CINT__
#include <qobject.h>
class TQtRootViewer3D;
class QPixmap;

class SlotDisconnect : public QObject {
   Q_OBJECT
protected: 
   friend class TQtRootViewer3D;
   friend class TQtRootCoinViewer3D;
   TQtRootViewer3D *fMaster;
   SlotDisconnect(TQtRootViewer3D *master) {fMaster = master;}
   virtual ~SlotDisconnect() {}
protected slots:
   void CleanPrimitives();
   void Disconnect();
   void DestroyMaster();
   
   void UpdateView(QPixmap *);
signals:
   void Destroyed(TVirtualViewer3D *);
};

#else
class SlotDisconnect;
#endif

class TGLViewerImp;

class TQtRootViewer3D : public TVirtualViewer3D
{
protected:
   friend class     SlotDisconnect;
   TObject3DViewFactoryABC  *fView3DFactory;	   
   TVirtualPad      *fPad;
   TObject3DView     fListOfPrimitives;
   TGLViewerImp     *fViewer;
   SlotDisconnect   *fDisconnectSlot;
   Int_t             fDepth;              // the current scene depth
   Bool_t            fBuildingScena;      // Flag to mark we are building the view
   
   virtual  void    Disconnect();
   virtual  void    Viewer();
   virtual  void    ClearPrimitives();
   virtual  void    MakeViewerNil();
public:
    TQtRootViewer3D(TVirtualPad*pad=0);
    virtual ~TQtRootViewer3D() ;
   
   // Viewers must always handle master (absolute) positions - and
   // buffer producers must be able to supply them. Some viewers may 
   // prefer local frame & translation - and producers can optionally
   // supply them
    virtual Bool_t PreferLocalFrame() const {return kTRUE; }

       // Viewers can implement their own loop over pad's primitive list.
    virtual Bool_t CanLoopOnPrimitives() const;
    // When they can, TPad::Paint() and TPad::PaintModified() simply 
    // call the following function:
    virtual void   PadPaint(TVirtualPad*);
    virtual void   ObjectPaint(TObject*, Option_t* = "");
    virtual void   Print(const Option_t *opt="wgl") const;
    virtual void   PrintObjects(); 
   // Addition/removal of objects must occur between Begin/EndUpdate calls
   virtual void   BeginScene();          // called by TPad::Paint | PaintModified
   virtual void   BeginScene(TVirtualPad *pad); 
   virtual Bool_t BuildingScene() const; // called by TPad::Paint | PaintModified
   virtual void   EndScene();            // called by TPad::Paint | PaintModified

   virtual void   SetUpdatesEnabled(bool on=true);
   virtual void   SetDrawOption(Option_t *option="");
   virtual Option_t   *GetDrawOption() const;

   // Simple object addition - buffer represents a unique single positioned object
   virtual Int_t  AddObject(const TBuffer3D & buffer, Bool_t * addChildren = 0);

   // Complex object addition - for adding placed objects which have common template 
   // shapes. In this case buffer describes template shape (aside from kCore). 
   virtual Int_t  AddObject(UInt_t placedID, const TBuffer3D & buffer, Bool_t * addChildren = 0);
   virtual Int_t  AddObject(TObject *, Option_t* drawOption = 0, Bool_t * addChildren = 0);
   virtual Int_t  AddObjectFirst(TObject *, Option_t* drawOption = 0, Bool_t * addChildren = 0);
   virtual Int_t  AddRawObject(ULong_t placedID, UInt_t optMask);
   virtual void   CloseScene();            // called by EndScene
   
   virtual  void    Clear(Option_t *opt="");
 

#if  ROOT_VERSION_CODE < 327937
      virtual void   OpenComposite(const TBuffer3D & buffer, Bool_t * addChildren = 0);
#else
      virtual Bool_t OpenComposite(const TBuffer3D & buffer, Bool_t * addChildren = 0);
#endif
   virtual void   CloseComposite() {}
   virtual void   AddCompositeOp(UInt_t /*operation*/) {}
   virtual void   SetDepth(Int_t depth) { fDepth = depth; }
   virtual Int_t  Depth() const         { return fDepth;  }
   virtual const TGLViewerImp *GetViewerImp() const;
   virtual TGLViewerImp *GetViewerImp();
   inline SlotDisconnect *GetSlotDisconnect() const { return fDisconnectSlot;}
   virtual void   UpdateView();
   virtual void   DisconnectPad();
   virtual void   SetFooter(const char *title="");

   ClassDef(TQtRootViewer3D,0) // Abstract interface to 3D viewers
};

#endif
