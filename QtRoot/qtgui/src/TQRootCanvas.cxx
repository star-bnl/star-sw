// @(#)root/qtgsi:$Name:  $:$Id: TQRootCanvas.cxx,v 1.5 2013/08/30 16:00:23 perev Exp $
// Author: Denis Bertini, M. Al-Turany  01/11/2000

/*************************************************************************
 * Copyright (C) 1995-2006, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#include "Riostream.h"
#include "TROOT.h"
#include "qevent.h"
#include "qdialog.h"
#include "qpushbutton.h"
#include "qlabel.h"
#include "qpainter.h"
//Added by qt3to4:
#include <QDragEnterEvent>
#include <QDropEvent>

#include "TQGsiRootCanvas.h"
#include "TCanvas.h"

///////////////////////////////////////////////////////////////////////
//
// TQRootCanvas
//
// This class replicates the interface of the TQRootCanvas class from qtgsi 
// package  developed by 
// @authors Denis Bertini <d.bertini@gsi.de>
//	   M. AL-Turany  <m.al-turany@gsi.de>
//version 2.0
//
// to allow the QtGSI application works with Qt-layer ON.
//
// It eliminates the need for the TContextMenuImp hack of the original GSI code
// The context menu implementation is defined by ".rootrc: file and 
// provided by ROOT GUI TGuiFactory factory
//
//////////////////////////////////////////////////////////////////////
ClassImp(TQRootCanvas)

//______________________________________________________________________________
TQRootCanvas::TQRootCanvas( QWidget *parent, const char *name, TCanvas *c )
#if QT_VERSION < 0x40000
  : TQtWidget( parent, name ,WRepaintNoErase | WResizeNoErase, (c ? false : true) ),
#else /* QT_VERSION */
  : TQtWidget( parent, name ,Qt::WNoAutoErase | Qt::WResizeNoErase, (c ? false : true) ),
#endif /* QT_VERSION */
        fNeedResize(kTRUE)
{
   // set defaults
   setUpdatesEnabled( kTRUE );
   setMouseTracking(kTRUE);

   //  setBackgroundMode( NoBackground );
#if QT_VERSION < 0x40000
   setFocusPolicy( TabFocus );
#else /* QT_VERSION */
   setFocusPolicy( Qt::TabFocus );
#endif /* QT_VERSION */
   setCursor( Qt::crossCursor );

   // add the Qt::WinId to TGX11 interface
   // fWid=gVirtualX->AddWindow((ULong_t)winId(),100,30);
   if (c) SetCanvas(c);
   // create the context menu
   // fContextMenu = new TQCanvasMenu( parent, fCanvas );

   // test here all the events sent to the QWidget
   // has a parent widget then install filter
   if ( parent ) {
//      parent->installEventFilter( this );
      fParent = parent;
   }
   else
      fParent=0;

   // drag and drop support  (M. Al-Turany)
   setAcceptDrops(kTRUE);
}

//______________________________________________________________________________
TQRootCanvas::TQRootCanvas( QWidget *parent, QWidget* tabWin, const char *name, TCanvas *c )
#if QT_VERSION < 0x40000
: TQtWidget( tabWin, name ,WRepaintNoErase | WResizeNoErase, (c ? false : true) ),
#else /* QT_VERSION */
: TQtWidget( tabWin, name ,Qt::WNoAutoErase | Qt::WResizeNoErase, (c ? false : true) ),
#endif /* QT_VERSION */
    fNeedResize(kTRUE)
{
   // set defaults
   setUpdatesEnabled( kTRUE );
   setMouseTracking(kTRUE);

#if QT_VERSION < 0x40000
   setFocusPolicy( TabFocus );
#else /* QT_VERSION */
   setFocusPolicy( Qt::TabFocus );
#endif /* QT_VERSION */
   setCursor( Qt::crossCursor );

   // add the Qt::WinId to TGX11 interface
   // fWid=gVirtualX->AddWindow((ULong_t)winId(),100,30);
   if (c) SetCanvas(c);
   // create the context menu
   //fContextMenu = new TQCanvasMenu( parent, tabWin, fCanvas );

   // test here all the events sent to the QWidget
   // has a parent widget then install filter
   if ( parent ) {
      fParent = parent;
   }
   else
      fParent=0;


   // drag and drop support  (M. Al-Turany)
   setAcceptDrops(true);
}

////////////////////////////////////// drag and drop support

//______________________________________________________________________________
void TQRootCanvas::dragEnterEvent( QDragEnterEvent *e )
{
   // Entering a drag event.
   if ( e->mimeData()->hasText() ) {
      QString str = e->mimeData()->text();
      if(gROOT->FindObject(str.toStdString().c_str()))
         e->acceptProposedAction();
      else
         qWarning("object %s  not found by ROOT", str.toStdString().c_str());
   }
}

//______________________________________________________________________________
void TQRootCanvas::dropEvent( QDropEvent *Event )
{
   // Start a drop, for now only histogram objects can be drawn by droping.

   if ( Event->mimeData()->hasText() ) {
      QString str = Event->mimeData()->text();
      TObject *dragedObject = gROOT->FindObject(str.toStdString().c_str());
      QPoint Pos = Event->pos();
      TObject *object=0;
      TPad *pad = fCanvas->Pick(Pos.x(), Pos.y(), object);
      if (dragedObject!=0) {
         if (dragedObject->InheritsFrom("TH1")) {
            pad->cd();
            dragedObject->Draw();
            pad->Update();
         }
      }
      else
         qWarning("object %s  not found by ROOT", str.toStdString().c_str());
   }
}

/////////////////////////////////////End Drag and drop Support (Mohammad Al-Turany)

//______________________________________________________________________________
void TQRootCanvas::Browse(TBrowser *b)
{
   // Just a wrapper.

   fCanvas->Browse(b);
}

//______________________________________________________________________________
void TQRootCanvas::Clear(Option_t *option)
{
   // Just a wrapper.

   fCanvas->Clear(option);
}

//______________________________________________________________________________
void TQRootCanvas::Close(Option_t *option)
{
   // Just a wrapper.

   fCanvas->Close(option);
}

//______________________________________________________________________________
void TQRootCanvas::Draw(Option_t *option)
{
   // Just a wrapper.

   fCanvas->Draw(option);
}

//______________________________________________________________________________
TObject *TQRootCanvas::DrawClone(Option_t *option)
{
   // Just a wrapper.

   return  fCanvas->DrawClone(option);
}

//______________________________________________________________________________
TObject *TQRootCanvas::DrawClonePad()
{
   // Just a wrapper.

   return  fCanvas->DrawClonePad();
}

//______________________________________________________________________________
void TQRootCanvas::EditorBar()
{
   // Just a wrapper.

   fCanvas->EditorBar();
}

//______________________________________________________________________________
void TQRootCanvas::EnterLeave(TPad *prevSelPad, TObject *prevSelObj)
{
   // just a wrapper
   fCanvas->EnterLeave(prevSelPad, prevSelObj);
}

//______________________________________________________________________________
void TQRootCanvas::FeedbackMode(Bool_t set)
{
   // just a wrapper
   fCanvas->FeedbackMode(set);
}

//______________________________________________________________________________
void TQRootCanvas::Flush()
{
   // just a wrapper
   fCanvas->Flush();
}

//______________________________________________________________________________
void TQRootCanvas::UseCurrentStyle()
{
   // just a wrapper
   fCanvas->UseCurrentStyle();
}

//______________________________________________________________________________
void TQRootCanvas::ForceUpdate()
{
   // just a wrapper
   fCanvas->ForceUpdate() ;
}

//______________________________________________________________________________
const char *TQRootCanvas::GetDISPLAY()
{
   // just a wrapper
   return fCanvas->GetDISPLAY() ;
}

//______________________________________________________________________________
TContextMenu *TQRootCanvas::GetContextMenu()
{
   // just a wrapper
   return  fCanvas->GetContextMenu() ;
}

//______________________________________________________________________________
Int_t TQRootCanvas::GetDoubleBuffer()
{
   // just a wrapper
   return fCanvas->GetDoubleBuffer();
}

//______________________________________________________________________________
Color_t TQRootCanvas::GetHighLightColor()
{
   // just a wrapper
   return fCanvas->GetHighLightColor() ;
}

//______________________________________________________________________________
TVirtualPad *TQRootCanvas::GetPadSave()
{
   // just a wrapper
   return fCanvas->GetPadSave();
}

//______________________________________________________________________________
Option_t *TQRootCanvas::GetSelectedOpt()
{
   // just a wrapper
   return fCanvas->GetSelectedOpt();
}

//______________________________________________________________________________
Bool_t TQRootCanvas::GetShowEventStatus()
{
   // just a wrapper
   return fCanvas->GetShowEventStatus() ;
}

//______________________________________________________________________________
Bool_t TQRootCanvas::GetAutoExec()
{
   // just a wrapper
   return fCanvas->GetAutoExec();
}

//______________________________________________________________________________
Size_t TQRootCanvas::GetXsizeUser()
{
   // just a wrapper
   return fCanvas->GetXsizeUser();
}

//______________________________________________________________________________
Size_t TQRootCanvas::GetYsizeUser()
{
   // just a wrapper
   return fCanvas->GetYsizeUser();
}

//______________________________________________________________________________
Size_t TQRootCanvas::GetXsizeReal()
{
   // just a wrapper
   return fCanvas->GetXsizeReal();
}

//______________________________________________________________________________
Size_t TQRootCanvas::GetYsizeReal()
{
   // just a wrapper
   return fCanvas->GetYsizeReal();
}

//______________________________________________________________________________
Int_t TQRootCanvas::GetCanvasID()
{
   // just a wrapper
   return fCanvas->GetCanvasID();
}

//______________________________________________________________________________
Int_t TQRootCanvas::GetWindowTopX()
{
   // just a wrapper
   return fCanvas->GetWindowTopX();
}

//______________________________________________________________________________
Int_t TQRootCanvas::GetWindowTopY()
{
   // just a wrapper
   return fCanvas->GetWindowTopY();
}

//______________________________________________________________________________
UInt_t TQRootCanvas::GetWindowWidth()
{
   // just a wrapper
   return fCanvas->GetWindowWidth() ;
}

//______________________________________________________________________________
UInt_t TQRootCanvas::GetWindowHeight()
{
   // just a wrapper
   return fCanvas->GetWindowHeight();
}

//______________________________________________________________________________
UInt_t TQRootCanvas::GetWw()
{
   // just a wrapper
   return fCanvas->GetWw();
}

//______________________________________________________________________________
UInt_t TQRootCanvas::GetWh()
{
   // just a wrapper
   return fCanvas->GetWh() ;
}

//______________________________________________________________________________
void TQRootCanvas::GetCanvasPar(Int_t &wtopx, Int_t &wtopy, UInt_t &ww, UInt_t &wh)
{
   // just a wrapper
   fCanvas->GetCanvasPar(wtopx, wtopy, ww, wh);
}

//______________________________________________________________________________
void TQRootCanvas::HandleInput(EEventType button, Int_t x, Int_t y)
{
   // just a wrapper
   fCanvas->HandleInput(button, x, y);
}

//______________________________________________________________________________
Bool_t TQRootCanvas::HasMenuBar()
{
   // just a wrapper
   return fCanvas->HasMenuBar() ;
}

//______________________________________________________________________________
void TQRootCanvas::Iconify()
{
   // just a wrapper
   fCanvas->Iconify();
}

//______________________________________________________________________________
Bool_t TQRootCanvas::IsBatch()
{
   // just a wrapper
   return fCanvas->IsBatch() ;
}

//______________________________________________________________________________
Bool_t TQRootCanvas::IsRetained()
{
   // just a wrapper
   return fCanvas->IsRetained();
}

//______________________________________________________________________________
void TQRootCanvas::ls(Option_t *option)
{
   // just a wrapper
   fCanvas->ls(option);
}

//______________________________________________________________________________
void TQRootCanvas::MoveOpaque(Int_t set)
{
   // just a wrapper
   fCanvas->MoveOpaque(set);
}

//______________________________________________________________________________
Bool_t TQRootCanvas::OpaqueMoving()
{
   // just a wrapper
   return fCanvas->OpaqueMoving();
}

//______________________________________________________________________________
Bool_t TQRootCanvas::OpaqueResizing()
{
   // just a wrapper
   return fCanvas->OpaqueResizing();
}

//______________________________________________________________________________
void TQRootCanvas::Paint(Option_t *option)
{
   // just a wrapper
   fCanvas->Paint(option);
}

//______________________________________________________________________________
TPad *TQRootCanvas::Pick(Int_t px, Int_t py, TObjLink *&pickobj)
{
   // just a wrapper
   return fCanvas->Pick(px, py, pickobj);
}

//______________________________________________________________________________
TPad *TQRootCanvas::Pick(Int_t px, Int_t py, TObject *prevSelObj)
{
   // just a wrapper
   return fCanvas->Pick(px, py, prevSelObj);
}

//______________________________________________________________________________
void TQRootCanvas::Resize(Option_t *option)
{
   // just a wrapper
   fCanvas->Resize(option);
}

//______________________________________________________________________________
void TQRootCanvas::ResizeOpaque(Int_t set)
{
   // just a wrapper
   fCanvas->ResizeOpaque(set);
}

//______________________________________________________________________________
void TQRootCanvas::SaveSource(const char *filename, Option_t *option)
{
   // just a wrapper
   fCanvas->SaveSource(filename, option);
}

//______________________________________________________________________________
void TQRootCanvas::SetCursor(ECursor cursor)
{
   // just a wrapper
   fCanvas->SetCursor(cursor);
}

//______________________________________________________________________________
void TQRootCanvas::SetDoubleBuffer(Int_t mode)
{
   // just a wrapper
   fCanvas->SetDoubleBuffer(mode);
}

//______________________________________________________________________________
void TQRootCanvas::SetWindowPosition(Int_t x, Int_t y)
{
   // just a wrapper
   fCanvas->SetWindowPosition(x, y) ;
}

//______________________________________________________________________________
void TQRootCanvas::SetWindowSize(UInt_t ww, UInt_t wh)
{
   // just a wrapper
   fCanvas->SetWindowSize(ww,wh) ;
}

//______________________________________________________________________________
void TQRootCanvas::SetCanvasSize(UInt_t ww, UInt_t wh)
{
   // just a wrapper
   fCanvas->SetCanvasSize(ww, wh);
}

//______________________________________________________________________________
void TQRootCanvas::SetHighLightColor(Color_t col)
{
   // just a wrapper
   fCanvas->SetHighLightColor(col);
}

//______________________________________________________________________________
void TQRootCanvas::SetSelected(TObject *obj)
{
   // just a wrapper
   fCanvas->SetSelected(obj);
}

//______________________________________________________________________________
void TQRootCanvas::SetSelectedPad(TPad *pad)
{
   // just a wrapper
   fCanvas->SetSelectedPad(pad);
}

//______________________________________________________________________________
void TQRootCanvas::Show()
{
   // just a wrapper
   fCanvas->Show() ;
}

//______________________________________________________________________________
void TQRootCanvas::Size(Float_t xsizeuser, Float_t ysizeuser)
{
   // just a wrapper
   fCanvas->Size(xsizeuser, ysizeuser);
}

//______________________________________________________________________________
void TQRootCanvas::SetBatch(Bool_t batch)
{
   // just a wrapper
   fCanvas->SetBatch(batch);
}

//______________________________________________________________________________
void TQRootCanvas::SetRetained(Bool_t retained)
{
  // just a wrapper
   fCanvas->SetRetained(retained);
}

//______________________________________________________________________________
void TQRootCanvas::SetTitle(const char *title)
{
   // just a wrapper
   fCanvas->SetTitle(title);
}

//______________________________________________________________________________
void TQRootCanvas::ToggleEventStatus()
{
   // just a wrapper
   fCanvas->ToggleEventStatus();
}

//______________________________________________________________________________
void TQRootCanvas::ToggleAutoExec()
{
   // just a wrapper
   fCanvas->ToggleAutoExec();
}

//______________________________________________________________________________
void TQRootCanvas::Update()
{
   // just a wrapper
   fCanvas->Update();
}

//______________________________________________________________________________
TQRootCanvas::~TQRootCanvas()
{
   // dtor
}



