// @(#)root/ged:$Name:  $:$Id: TQtGedAttFrame.h,v 1.3 2013/08/30 15:59:52 perev Exp $
// Author: Valeri Fine 25/06/04
// based on the code by Marek Biskup, Ilka  Antcheva 28/07/03

/****************************************************************************
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * based on the code by Marek Biskup, Ilka  Antcheva 28/07/03            *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtGedAttFrame
#define ROOT_TQtGedAttFrame

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  Interfaces:       TQtGedAttInterfaceB  TQtGedFactoryI               //
//  Implmentations:  TQtGedAttInterface   TQtGedFactory                 //
//                                                                      //
//  TQtGedAttFrame, TQtGedAttNameFrame, TQtGedAttFillFrame,             //
//  TQtGedAttLineFrame, TQtGedAttTextFrame, TQtGedAttMarkerFrame        //
//                                                                      //
//  Frames with object attributes, just like on TAttCanvases.           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TObject.h"
#include "GuiTypes.h"
#include "Gtypes.h"

#ifndef __CINT__
// #include <qtoolbar.h> 
#  include <QApplication>
#  include <QCursor> 
#    define QTDOCKCLASSTYPE QDockWidget

#    include <QDockWidget> 
#    include <QMainWindow>
#    include <QObject>
#    include <QList>

#  include <QComboBox> 

#  include <QLayout> 
#  include <QLabel> 
#  include <QToolTip>

#else /* CINT */
// typedef  TObject QToolBar;
   class QObjectList;
   typedef  TObject QDockWindow;  
   typedef  TObject QDockWidget;  
#  define QTDOCKCLASSTYPE TObject
#  define  slots 
#endif

// ROOT  classes forward declarations:

class TVirtualPad;
class TCanvas;
class TVirtualPadEditor;

// TQtGed package classes forward declarations

class TQtColorSelect;
class TQtPatternSelect;
class TQtGedPatternSelect;
class TQtGedMarkerSelect;

//  Qt classes forward declarations:

class QWidget;
class QMainWindow;
class QLabel;
class QString;
  
class QObject;
class TQtGedAttInterfaceB;

//_________________________________________________________________________________________________________
class TQtFrameUpdate : public QObject {
#ifndef __CINT__
   Q_OBJECT
#endif
  private:
     TQtGedAttInterfaceB *fFrame;
     TQtFrameUpdate (){}
     TQtFrameUpdate (const TQtFrameUpdate&);
     void operator=(const TQtFrameUpdate&);
  public:
     TQtFrameUpdate(TQtGedAttInterfaceB *frame) : fFrame(frame) {}
  public slots:
     void Disconnect();
     void Reconnect();
     void Update();
};

//_________________________________________________________________________________________________________
class TQtGedAttInterfaceB : public TObject  {

friend class TQtGedEditor;
friend class TQtFrameUpdate;

private:
   TQtGedAttInterfaceB (const TQtGedAttInterfaceB&);
   void operator=(const TQtGedAttInterfaceB&);

protected:
   Int_t         fId;          // This interface index
   TCanvas      *fCanvas;      // selected canvas, if exists
   TVirtualPad  *fPad;         // selected pad, if exists
   Bool_t        fActive;      // indicate whether the interface is active
   TObject      *fObject;      // TObject cast of the model
   bool          fLockUpdate;  // temporary lock the update
   bool          fInitialized; // flag whether the ediotr was initialized
   bool          fCanvasConnected; // status of the ROOT signal / slot connection
   TQtFrameUpdate fUpdateSlot; // object to hold the view update slot
   
   virtual void AddControl2List(const QObject *)  = 0;
   virtual void BlockChildSignals(bool block=TRUE)= 0;
   virtual void BuildView(QWidget  *editorPanel)  = 0;
   virtual void ChangeView()                      = 0;
   virtual void Show()                            = 0;
   virtual void Hide()                            = 0;
   virtual void SetUpdatesEnabled(Bool_t)         = 0;
   virtual TObject *SetModel(TObject *obj)        = 0;
   virtual const char *ModelName() const          = 0;
   virtual void ConnectSignals2Slots()            = 0;
   virtual void CompleteInit()                    = 0;
   const TObject *Model()  const   { return fObject;}

   virtual void Connect(TCanvas *c=0);
   void ConnectToCanvas(TCanvas* c,const char *className, TObject *thisPointer, Bool_t connect=kTRUE);
   bool ConnectView(const QObject *sender,const char *signal, const QObject *receiver, const char *slot);
   virtual void Disconnect() { ConnectToCanvas(fCanvas,Class_Name(),(TObject *)this,kFALSE);}
   virtual void Reconnect()  { ConnectToCanvas(fCanvas,Class_Name(),(TObject *)this);       }
   void DoNotUpdate();
   virtual Option_t *GetDrawOption() const;
           void SetCanvas(TCanvas  *canvas);
   virtual void SetDrawOption(Option_t *option);
           void SetInitialized();
public:
   static Int_t EditorDefaultWidth() { return 170; }
   TQtGedAttInterfaceB(Int_t id=0,
      Int_t   = TQtGedAttInterfaceB::EditorDefaultWidth() /* width */ , Int_t  = 70 /*height*/,
                UInt_t  = 0,  /* option = kChildFrame, */
                Pixel_t = 0   /* back = GetDefaultFrameBackground()*/ )
     : TObject(),      fId(id),   fCanvas(0),        fPad(0)
     , fActive(kFALSE),fObject(0),fLockUpdate(FALSE),fInitialized(FALSE)
     , fCanvasConnected(FALSE), fUpdateSlot(this){}
    virtual ~TQtGedAttInterfaceB() {  }

   static void ResetPadOption(TVirtualPad *pad, TObject *obj, Option_t *opt);
   
   TCanvas *Canvas() const { return fCanvas;  }
   virtual QTDOCKCLASSTYPE *Dock()    const     = 0;
   virtual unsigned long Handle() const { return  (unsigned long )Dock(); }
   virtual void  ConnectToCanvas(TCanvas *c) { ConnectToCanvas(c,Class_Name(), this);} 
   virtual Int_t GetId()         const  { return fId;}
   virtual const char *GetTitle() const     = 0;
   virtual Bool_t IsActive()  const     { return fActive;      }
           Bool_t IsInitialized()  const;
           Bool_t IsCanvasConnected()  const;
   virtual void SetActive(Bool_t active = true){ fActive = active;    }
           void SetModel(TVirtualPad *pad, TObject *obj, Int_t event);

   virtual void Update();
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGedAttInterfaceB, 0); //attribute Qt tool bar
//MOC_SKIP_END
#endif
};
//_________________________________________________________________________________________________________
inline void TQtFrameUpdate::Disconnect()
{ 
   // diconnect frame from the TCanvas to update safely 
   QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
   fFrame->Disconnect();
}
//_________________________________________________________________________________________________________
inline void TQtFrameUpdate::Update()
{
   // Update the parent class
   fFrame->Update();
}
//_________________________________________________________________________________________________________
inline void TQtFrameUpdate::Reconnect()
{ 
   // reconnect frame to the TCanvas after update
   fFrame->Reconnect();
   QApplication::restoreOverrideCursor();
}

//_________________________________________________________________________________________________________
inline void TQtGedAttInterfaceB::DoNotUpdate()
{
   // Temporary lock the next Update() method
   fLockUpdate=TRUE;
}
//_________________________________________________________________________________________________________
inline Bool_t TQtGedAttInterfaceB::IsInitialized()  const 
{ return fInitialized;                    }
//_________________________________________________________________________________________________________
inline Bool_t TQtGedAttInterfaceB::IsCanvasConnected()  const
{ return fCanvasConnected;                }
//_________________________________________________________________________________________________________
inline void TQtGedAttInterfaceB::SetCanvas(TCanvas  *canvas)
{ fCanvas = canvas;                                        }

//_________________________________________________________________________________________________________
inline void TQtGedAttInterfaceB::SetInitialized()
{ fInitialized = TRUE;                   }

//_________________________________________________________________________________________________________
//
// class TQtGedFactoryI
// The base class for the instantiation of the primitive ROOT object editors
//_________________________________________________________________________________________________________
class TQtGedFactoryI : public TObject {

  friend class TQtGedEditor;
  protected:
    virtual TQtGedAttInterfaceB * Create(QMainWindow *mainWidget, TCanvas *canvas) const = 0;
    virtual TQtGedAttInterfaceB * Create(TCanvas *canvas, QWidget *parent=0 ) const      = 0;

  public:
     TQtGedFactoryI();
     virtual ~TQtGedFactoryI();
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGedFactoryI, 0); // The GEd primitive edito factory
//MOC_SKIP_END
#endif
};

//_________________________________________________________________________________________________________
template<class T>
class TQtGedFactory : public TQtGedFactoryI {
   protected:
      virtual TQtGedAttInterfaceB * Create(QMainWindow *mainWidget, TCanvas *canvas) const ;
      virtual TQtGedAttInterfaceB * Create(TCanvas *canvas, QWidget *parent=0 ) const;
   public:
      TQtGedFactory();
      virtual ~TQtGedFactory();
};

//_________________________________________________________________________________________________________
 class TQtGedAttInterface : public QTDOCKCLASSTYPE, public TQtGedAttInterfaceB  {

 private: 
   TQtGedAttInterface(const TQtGedAttInterface&);
   void operator=(const TQtGedAttInterface&);
 protected:
       
   QWidget     *fPanelBox;
   QObjectList *fControlList;
   virtual  void MakeTitle(const char *c,QWidget *parent=0);
   virtual  void AddControl2List(const QObject *obj);
   void     BlockChildSignals(bool block=TRUE);
   void     Constructor(const QString &label,  TCanvas *canvas
                     , Int_t id, Int_t width, Int_t height
                     , UInt_t options, Pixel_t back);
   virtual void Show()  { show(); }
   virtual void Hide()  { hide(); }
   virtual QTDOCKCLASSTYPE *Dock() const { return (QTDOCKCLASSTYPE *)this; }
   virtual void SetUpdatesEnabled(Bool_t on) { setUpdatesEnabled(on);  }
   virtual void CompleteInit();
public:
   TQtGedAttInterface(QMainWindow *mainWidget, const QString &label, TCanvas *canvas, Int_t id,
                Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   TQtGedAttInterface(const QString &label, QWidget *parent, TCanvas *canvas, Int_t id,
                Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );

   virtual ~TQtGedAttInterface() ;

   virtual void SetActive(Bool_t active = true);
   virtual const char *GetTitle() const;

// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
//   ClassDef(TQtGedAttFrame, 0); //attribute Qt tool bar
//MOC_SKIP_END
#endif
};

//______________________________________________________________________________
//
//               TQtGedAttFrame
//______________________________________________________________________________
template<class T>
class TQtGedAttFrame : public TQtGedAttInterface {
protected:
   T            *fModel;       // selected object, if exists 

   virtual TObject *SetModel(TObject *obj);
   virtual const char *ModelName() const;
   void operator=(const TQtGedAttFrame &);

public:
   TQtGedAttFrame(QMainWindow *mainWidget, const QString &label, TCanvas *canvas, Int_t id,
                Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ )
                :TQtGedAttInterface(mainWidget, label, canvas, id,
                width, height,option,back) ,fModel(0){}

   TQtGedAttFrame(const QString &label, QWidget *parent, TCanvas *canvas, Int_t id,
                Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ )
                : TQtGedAttInterface(label,parent,canvas, id,
                width, height,option,back), fModel(0){}
   virtual ~TQtGedAttFrame() {  }
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
//   ClassDef(TQtGedAttInterface, 0); //attribute Qt tool bar
//MOC_SKIP_END
#endif
};

//_________________________________________________________________________________________________________
template <class T>
TObject *TQtGedAttFrame<T>::SetModel(TObject *obj)
{
    fPad   = 0;
    if (obj && (obj != fObject)) 
       {
       fModel = dynamic_cast<T *>(obj);
       if (!fModel) {
          fObject = 0;
          SetActive(kFALSE);
       } else {
          fObject = obj;
       }
    }

    return fObject;
}

//_________________________________________________________________________________________________________
template <class T>
const char *TQtGedAttFrame<T>::ModelName() const
{ 
   // Return the model class name
   return  fModel ? fModel->Class_Name() : 0;
}

//______________________________________________________________________________
//
//    TQtEditorFactory - factory to create the GED primitive editors
//______________________________________________________________________________
template<class V>
TQtGedFactory<V>::TQtGedFactory() : TQtGedFactoryI(){ }
//______________________________________________________________________________
template<class V>
TQtGedFactory<V>::~TQtGedFactory() { }

//______________________________________________________________________________
template<class V>
TQtGedAttInterfaceB *TQtGedFactory<V>::Create(QMainWindow *mainWidget, TCanvas *canvas) const
{
   // Create the Ged editor
    TQtGedAttInterfaceB *v = new V(mainWidget, canvas);
    return v;
}

//______________________________________________________________________________
template<class V>
TQtGedAttInterfaceB *TQtGedFactory<V>::Create(TCanvas *canvas, QWidget *parent) const
{
   // Create the Ged editor
   TQtGedAttInterfaceB *v =  new V(canvas, parent);
   return v;
}


#endif
