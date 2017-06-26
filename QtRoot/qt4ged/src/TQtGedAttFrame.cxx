// @(#)root/ged:$Name:  $:$Id: TQtGedAttFrame.cxx,v 1.3 2013/08/30 15:59:53 perev Exp $
// Author: Valeri Fine   22/07/06

/****************************************************************************
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TGedEditor, a ROOT GUI toolkit.               *
 * Author: Marek Biskup, Ilka Antcheva   22/07/03                        *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtGedAttFrame, TQtGedAttNameFrame, TQtGedAttFillFrame,             //
//  TQtGedAttLineFrame, TQtGedAttTextFrame, TQtGedAttMarkerFrame        //
//                                                                      //
//  Frames with object attributes, just like on TAttCanvases.           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <assert.h>
#include "TQtGedAttFrame.h"
#include "TColor.h"
//#include "TCint.h"
#include "TCanvas.h"
#include "TVirtualMutex.h"
#include "TVirtualPad.h"
#include "TAttMarker.h"
#include "TAttLine.h"
#include "TAttText.h"
#include "TAttFill.h"
#include "TAxis.h"
#include "TQtGedEditor.h"
#include "TGQt.h"
#include "TQtPatternSelect.h"
#include "TQtColorSelect.h"

#include <qlabel.h> 
#if QT_VERSION >= 0x40000
//Added by qt3to4:
#    include <QDebug>
#endif /* QT_VERSION */

//#include "Api.h"
#include "TPaveLabel.h"
#include <snprintf.h>
#include "TList.h"
// ClassImp(TQtGedAttInterface)
// ClassImp(TQtGedAttFrame)

//_________________________________________________________________________________________________________
bool TQtGedAttInterfaceB::ConnectView(const QObject *sender,const char *signal, const QObject *receiver, const char *slot)
{
    // decoratopr for the original
    // QObject::connect(const QObject *sender,const char *signal, const QObject *receiver, const char *slot)
    // method
     AddControl2List(sender);
     QObject::connect(sender,signal, &fUpdateSlot, SLOT(Disconnect()));
     bool  res = QObject::connect(sender,signal, receiver, slot);
     QObject::connect(sender,signal, &fUpdateSlot, SLOT(Update()));
     QObject::connect(sender,signal, &fUpdateSlot, SLOT(Reconnect()));
     return res;
}

//______________________________________________________________________________
void TQtGedAttInterfaceB::Connect(TCanvas *c)
{  
   // Connect(TCanvas *c) - connect the editor frame to ROOT TCanvas
   //   c  = 0 - Connect the frame to existing fCanvas 
   //     !=0 - Dicsonncet and reconnect it to the new one
   if (c && ( c != fCanvas)) {
      // reconnect the frame
      Disconnect();
      fCanvas = c;
   }
   ConnectToCanvas(fCanvas,Class_Name(),(TObject *)this);
}
//______________________________________________________________________________
Option_t *TQtGedAttInterfaceB::GetDrawOption() const
{
   // Get draw options of the selected object.

   if (!(fPad && fObject) ) return "";

   TListIter next(fPad->GetListOfPrimitives());
   TObject *obj;
   while ((obj = next())) {
      if (obj == fObject) return next.GetOption();
   }
   return "";
}
//______________________________________________________________________________
void TQtGedAttInterfaceB::SetDrawOption(Option_t *option)
{
   // Set drawing option for object. This option only affects
   // the drawing style and is stored in the option field of the
   // TObjOptLink supporting a TPad's primitive list (TList).

   if (fPad && fObject) {

      TListIter next(fPad->GetListOfPrimitives());
      delete fPad->FindObject("Tframe");
      TObject *obj = 0;
      while ((obj = next()) && (obj != fObject) ) {} 
      if (obj) {
         next.SetOption(option);
         Update();
      }
   }
}

//______________________________________________________________________________
void TQtGedAttInterfaceB::ResetPadOption(TVirtualPad *pad, TObject *object, Option_t *opt)
{
   //
   // Workaround of the ROOT design flaw
   // Needed by TQtArrowEditor
   // It is a hack of the TObject::SetDrawOption method
   //
   if (pad) {
     TListIter next(pad->GetListOfPrimitives());
     TObject *obj;
     while ((obj = next()))
        if (obj == object) {
           next.SetOption(opt);
           break;
       }
   }
}
//______________________________________________________________________________
void TQtGedAttInterfaceB::SetModel(TVirtualPad *pad, TObject *obj, Int_t /* event  */)
{
   // Pick up the proper editor
   SetUpdatesEnabled(FALSE);
   BlockChildSignals(TRUE);
   if ( SetModel(obj) ){

      fPad = pad;

      ChangeView();
      // Release the  the view for the non-zero model
      BlockChildSignals(FALSE);
      SetUpdatesEnabled(TRUE);
      SetActive();
   }
}

//______________________________________________________________________________
void TQtGedAttInterfaceB::Update()
{
   // Update the current pad when an attribute is changed via GUI.
   if (fPad && !fLockUpdate) {
      fPad->Modified();
      fPad->Update();
      fLockUpdate = FALSE;
   }
}

//______________________________________________________________________________
void TQtGedAttInterfaceB::ConnectToCanvas(TCanvas* c,const char *className, TObject *thisPointer, Bool_t connect)
{
   // Connect/Disconnect the GUI attribute frames to the selected object in the canvas.
   if ( fCanvasConnected != connect) {
      if (c) {
         if (connect) {
            assert(TQObject::Connect(c, "Selected(TVirtualPad*,TObject*,Int_t)", className,
               thisPointer, "SetModel(TVirtualPad*,TObject*,Int_t)"));
            fCanvasConnected = TRUE;
         } else {
            assert(TQObject::Disconnect(c, "Selected(TVirtualPad*,TObject*,Int_t)",
               thisPointer, "SetModel(TVirtualPad*,TObject*,Int_t)"));
            fCanvasConnected = FALSE;
         }
      }
   }
}

//______________________________________________________________________________
//
//               TQtGedAttInterface
//______________________________________________________________________________
TQtGedAttInterface::TQtGedAttInterface(QMainWindow  *mainWidget, const QString &label, TCanvas *canvas,
                           Int_t id, Int_t width,
                           Int_t height, UInt_t options, Pixel_t back)
#if QT_VERSION < 0x40000
                          : QTDOCKCLASSTYPE(QDockWindow::InDock,mainWidget)
#else /* QT_VERSION */
                          : QTDOCKCLASSTYPE(mainWidget)
#endif /* QT_VERSION */
   , TQtGedAttInterfaceB(id,width,height, options,back),fPanelBox(0),fControlList(0)
{
   // Constructor of the base GUI attribute frame.
   Constructor(label, canvas, id, width, height, options, back);
   
#if QT_VERSION >= 0x40000
   if (mainWidget) mainWidget->addDockWidget(Qt::LeftDockWidgetArea,this);
#endif   
}

//______________________________________________________________________________
TQtGedAttInterface::TQtGedAttInterface(const QString &label, QWidget  *parent, TCanvas *canvas,
                               Int_t id, Int_t width,
                           Int_t height, UInt_t options, Pixel_t back)
                          : QTDOCKCLASSTYPE(parent)
   ,  TQtGedAttInterfaceB(id,width,height, options,back),fPanelBox(0),fControlList(0)
{
   // Constructor of the base GUI attribute frame.
   Constructor(label, canvas, id, width, height, options, back);
}
//______________________________________________________________________________
TQtGedAttInterface::~TQtGedAttInterface() 
{
   delete fControlList; fControlList = 0;
}
//______________________________________________________________________________
void TQtGedAttInterface::Constructor(const QString &label,  TCanvas *canvas,
                             Int_t /* id */, Int_t width,
                             Int_t height, UInt_t /* options */, Pixel_t /* back*/ )
{
   // Constructor of the base GUI attribute frame.
   SetCanvas(canvas);
#if QT_VERSION < 0x40000
   setFixedExtentWidth(width);
   setFixedExtentHeight(height);
#else
   resize(width, height);
#endif      
   if (!label.isNull()) { 
        QString tooltip;
        tooltip = QString("%1 class editor").arg(label);
#if QT_VERSION < 0x40000
        setCaption(label);
        QToolTip::add(this,tooltip);
#else
        setWindowTitle(label);
        setToolTip(tooltip);
#endif                
   }
   fPanelBox = new QWidget(this);
   QHBoxLayout *panelLayout =  new QHBoxLayout(fPanelBox);
#if QT_VERSION >= 0x40000
   setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea );
   setWidget(fPanelBox);
#endif /* QT_VERSION */
   fPanelBox->setMaximumSize(QSize(width,32767));
   panelLayout->setSpacing(2);
   panelLayout->setMargin(2);
//   connect(this,SIGNAL(placeChanged( QDockWindow::Place)), this, SLOT(Update(QDockWindow::Place)));
}

//______________________________________________________________________________
void TQtGedAttInterface::AddControl2List(const QObject *obj)
{
   // Add the widget to list of the blocked widget
   if (!fControlList) fControlList = new QObjectList();
   fControlList->append((QObject *)obj);
}
//______________________________________________________________________________
void TQtGedAttInterface::BlockChildSignals(bool block){
   // Block child signals to allow the editor to change the model without "side" recursion
   if (fControlList) {
#if QT_VERSION < 0x40000
      QObjectListIt it( *fControlList ); // iterate over the children
      QObject *obj;

      while ( (obj = it.current()) != 0 ) {
         // for each found object...
          ++it; obj->blockSignals(block);
      }
#else
      for (int i = 0; i < fControlList->size(); ++i) {
         fControlList->at(i)->blockSignals(block); 
      }  
#endif            
   }
}
//______________________________________________________________________________
void TQtGedAttInterface::CompleteInit(){
   // The last step to complete the object editor instantiation
   SetInitialized();
   BuildView(fPanelBox);
   ConnectSignals2Slots();
#if QT_VERSION < 0x40000
   fPanelBox->show();
#endif   
} 

//______________________________________________________________________________
void TQtGedAttInterface::MakeTitle(const char *p,QWidget * parent)
{   
   // Create attribute frame title.
   if (p && p[0]) {
      //     setLabel(p);
      QString tooltip;
      tooltip = QString(tr("%1 class editor panel")).arg(p);
#if QT_VERSION < 0x40000
      QWidget *thisWidget = this;      
      QToolTip::add(this,tooltip);      
#else
      QWidget *thisWidget = widget();
      setToolTip(tooltip);
      setWindowTitle(tr(p));
#endif
      
      QWidget *boxWidget = 0;
      QHBoxLayout *box = 0;
      if (!parent) {
         boxWidget     = new QWidget(thisWidget);
#if QT_VERSION < 0x40000
         boxLayout()->addWidget(boxWidget);
#endif         
      } else {
         boxWidget     = new QWidget(parent);
      }
      box = new QHBoxLayout(boxWidget);      
      new QLabel(p,boxWidget);box->setSpacing(0);
      QFrame *line1 = new QFrame( boxWidget);
      line1->setFrameShape ( QFrame::HLine  );
      line1->setFrameShadow( QFrame::Sunken );   
      boxWidget->show();
      // Add the second "tool box"
   }
}

//______________________________________________________________________________
void TQtGedAttInterface::SetActive(Bool_t active)
{
   // Set active GUI attribute frames related to the selected object.
   
   TQtGedAttInterfaceB::SetActive(active);
   if (active)
      show();
   else 
      hide();
}

//______________________________________________________________________________
const char *TQtGedAttInterface::GetTitle() const
{   
   return caption();
}

//______________________________________________________________________________
//
//    TQtEditorFactory - factory to create the GED primitive editors
//______________________________________________________________________________
TQtGedFactoryI::TQtGedFactoryI() : TObject()
{
   // Create and register the Ged primitive editor factory
   TQtGedEditor::Register(this);
}
//______________________________________________________________________________
TQtGedFactoryI::~TQtGedFactoryI() {
      TQtGedEditor::UnRegister(this);
}
