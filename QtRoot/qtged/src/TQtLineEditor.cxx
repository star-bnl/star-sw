// @(#)root/ged:$Name:  $:$Id: TQtLineEditor.cxx,v 1.4 2013/08/30 16:00:10 perev Exp $
// Author: Valeri Fine 13/06/06

/*************************************************************************
 * This source is based on TLineEditor, a ROOT GUI toolkit.              *
 * Author: Ilka  Antcheva 24/04/06
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtLineEditor                                                       //
//                                                                      //
//  Implements GUI for editing line attributes: shape, size, angle.     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//Begin_Html
/*
<img src="gif/TQtLineEditor.gif">
*/
//End_Html


#include "TQtLineEditor.h"
#include "TQtFloatSpinBox.h"

#include <qtooltip.h>
#include <qlabel.h>
#include <qvbox.h> 
#include <qcheckbox.h> 
#include <qvgroupbox.h> 
#include <qhbuttongroup.h> 
#include <qvbuttongroup.h>

ClassImp(TQtLineEditor)

//______________________________________________________________________________
TQtLineEditor::TQtLineEditor(QMainWindow *p, TCanvas *canvas,  Int_t id, Int_t width,
                           Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TLine>(p, "Line", canvas, id, width, height, options, back)
{
   
}

//______________________________________________________________________________
TQtLineEditor::TQtLineEditor( TCanvas *canvas,QWidget  *p, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TLine>("Line", p, canvas, id, width, height, options, back)
{
   
}
//______________________________________________________________________________
void TQtLineEditor::BuildView(QWidget  *editorPanel)
{
  // Constructor of line GUI.
  const int space =  3; //  the space between the label and spinbox
  fStartPointX=0; fEndPointX=0; fStartPointY=0;  fEndPointY  =0;
  
  QVBox *vframe  = new QVBox(editorPanel);
  QHBox *hbox = new QHBox(vframe);
  
  QLabel *label = new QLabel("Points:",hbox); label->setAlignment(Qt::AlignHCenter);
  
  hbox = new QHBox(vframe); hbox->setSpacing(space);
  label = new QLabel("Start X:",hbox); label->setAlignment(Qt::AlignRight); 
  fStartPointX = new TQtFloatSpinBox(0.0, -1000000.0, 1000000.0, 2, hbox);
  QToolTip::add(fStartPointX,"Set start point X coordinate of Line.");
  
  hbox = new QHBox(vframe); hbox->setSpacing(space);
  label = new QLabel("Y:",hbox); label->setAlignment(Qt::AlignRight);
  fStartPointY = new TQtFloatSpinBox(0.0, -1000000.0, 1000000.0, 2, hbox);
  QToolTip::add(fStartPointY,"Set start point Y coordinate of Line.");
  
  hbox = new QHBox(vframe); hbox->setSpacing(space);
  label = new QLabel("End X:",hbox);label->setAlignment(Qt::AlignRight);
  fEndPointX = new TQtFloatSpinBox(0.25, -1000000.0, 1000000.0, 2, hbox);
  QToolTip::add(fEndPointX,"Set end point X coordinate of Line.");
  
  hbox = new QHBox(vframe); hbox->setSpacing(space);
  label = new QLabel("Y:",hbox);label->setAlignment(Qt::AlignRight);
  fEndPointY = new TQtFloatSpinBox(0.25, -1000000.0, 1000000.0, 2, hbox);
  QToolTip::add(fEndPointY,"Set end point Y coordinate of Line.");
  
#if ROOT_VERSION_CODE > ROOT_VERSION(5,11,3) 
  fOrientation  = new QVButtonGroup(vframe); fOrientation->setExclusive(TRUE);
  fVertical = new QCheckBox("Vertical",fOrientation);
  QToolTip::add(fVertical,   "Set vertical");
  fVertical->setTristate();
  
  fHorizontal = new QCheckBox("Horizontal",fOrientation);
  QToolTip::add(fHorizontal,"Set horizontal");
  fHorizontal->setTristate();
#else
   fOrientation  = 0; fHorizontal = 0;
#endif   
}

//______________________________________________________________________________
TQtLineEditor::~TQtLineEditor()
{
   // Destructor of line editor.
}

//______________________________________________________________________________
void TQtLineEditor::ConnectSignals2Slots()
{
   // Connect signals to slots.

   ConnectView(fStartPointX,SIGNAL(ValueChanged(double)),this, SLOT(DoStartPointX(double))     );
   ConnectView(fStartPointY,SIGNAL(ValueChanged(double)),this, SLOT(DoStartPointY(double))     );
   ConnectView(fEndPointX,  SIGNAL(ValueChanged(double)),this, SLOT(DoEndPointX(double))       );
   ConnectView(fEndPointY,  SIGNAL(ValueChanged(double)),this, SLOT(DoEndPointY(double))       );
#if ROOT_VERSION_CODE > ROOT_VERSION(5,11,3) 
   ConnectView(fOrientation,SIGNAL(clicked(int))     ,this, SLOT(DoLineOrientation(int)));
#endif   
}

//______________________________________________________________________________
void TQtLineEditor::ChangeView()
{
   // Pick up the used line attributes.
   //  View                    Model

   fStartPointX->SetValue  (fModel->GetX1() );
   fEndPointX  ->SetValue  (fModel->GetX2() );
   fStartPointY->SetValue  (fModel->GetY1() );
   fEndPointY  ->SetValue  (fModel->GetY2() );
#if ROOT_VERSION_CODE > ROOT_VERSION(5,11,3) 
   fHorizontal ->setChecked(fModel->IsHorizontal());
#endif   
}

//______________________________________________________________________________
void TQtLineEditor::DoStartPointX(double value)
{
   // Slot connected to the line start point.

   fModel->SetX1(value);
   // fModel->Paint(fModel->GetDrawOption());
}
//______________________________________________________________________________
void TQtLineEditor::DoStartPointY(double value)
{
   // Slot connected to the line start point.
   fModel->SetY1(value);
}

//______________________________________________________________________________
void TQtLineEditor::DoEndPointX(double value)
{
   // Slot connected to the line EndPoint X.

   fModel->SetX2(value);
}

//______________________________________________________________________________
void TQtLineEditor::DoEndPointY(double value)
{
   // Slot connected to the line EndPoint Y.
   fModel->SetY2(value);
}

//______________________________________________________________________________                                                                                
void TQtLineEditor::DoLineOrientation(int)
{
   // Slot so set the line orientation

#if ROOT_VERSION_CODE > ROOT_VERSION(5,11,3) 
   fModel->SetVertical  (fVertical  ->state() == QButton::On);
   fModel->SetHorizontal(fHorizontal->state() == QButton::On);
#endif   
}


//______________________________________________________________________________                                                                                
//
// Register the GED factory interfaces:
//______________________________________________________________________________                                                                                
static TQtGedFactory<TQtLineEditor>   gQtLineEditor;
