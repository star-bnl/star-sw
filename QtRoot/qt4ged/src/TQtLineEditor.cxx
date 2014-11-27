// @(#)root/ged:$Name:  $:$Id: TQtLineEditor.cxx,v 1.5 2013/08/30 15:59:53 perev Exp $
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
#include <qcheckbox.h> 

#include <QGroupBox>
#include <QButtonGroup>
#include <QVBoxLayout>
#include <QHBoxLayout>

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
  
  delete editorPanel->layout();
  QVBoxLayout *vframeLayout  = new QVBoxLayout(editorPanel);
  QHBoxLayout *hbox    = new QHBoxLayout();
  vframeLayout->addLayout(hbox);
  QLabel *label = new QLabel("Points:",editorPanel);
  hbox->addWidget(label);

  label->setAlignment(Qt::AlignHCenter);
  
  
  hbox = new  QHBoxLayout; hbox->setSpacing(space);
  vframeLayout->addLayout(hbox);
  label = new QLabel("Start X:",editorPanel); label->setAlignment(Qt::AlignRight); 
  hbox->addWidget(label);  
  fStartPointX = new TQtFloatSpinBox(0.0, -1000000.0, 1000000.0, 2,editorPanel);
  hbox->addWidget(fStartPointX); 
  fStartPointX->setToolTip( tr("Set start point X coordinate of Line."));

  hbox = new QHBoxLayout; hbox->setSpacing(space);
  vframeLayout->addLayout(hbox);
  label = new QLabel("Y:",editorPanel); label->setAlignment(Qt::AlignRight);
  hbox->addWidget(label);  
  fStartPointY = new TQtFloatSpinBox(0.0, -1000000.0, 1000000.0, 2,editorPanel);
  hbox->addWidget(fStartPointY); 
  fStartPointY->setToolTip(tr("Set start point Y coordinate of Line."));

  hbox = new QHBoxLayout; hbox->setSpacing(space);
  vframeLayout->addLayout(hbox);
  label = new QLabel("End X:",editorPanel);label->setAlignment(Qt::AlignRight);
  hbox->addWidget(label);  
  fEndPointX = new TQtFloatSpinBox(0.25, -1000000.0, 1000000.0, 2,editorPanel);
  fEndPointX->setToolTip(tr("Set end point X coordinate of Line."));

  hbox = new QHBoxLayout; hbox->setSpacing(space);
  vframeLayout->addLayout(hbox);
  label = new QLabel("Y:",editorPanel);label->setAlignment(Qt::AlignRight);
  hbox->addWidget(label);  
  fEndPointY = new TQtFloatSpinBox(0.25, -1000000.0, 1000000.0, 2,editorPanel);
  fEndPointY->setToolTip(tr("Set end point Y coordinate of Line."));
  
#if ROOT_VERSION_CODE > ROOT_VERSION(5,11,3) 
  QGroupBox   *group  = new QGroupBox(editorPanel);
  QVBoxLayout *layout = new QVBoxLayout(group);
  fOrientation        = new QButtonGroup(editorPanel); fOrientation->setExclusive(TRUE);
  vframeLayout->addWidget(group);
  
  fVertical = new QCheckBox(tr("Vertical"),group);
  fVertical->setToolTip(tr("Set vertical"));
  fVertical->setTristate();
  layout->addWidget(fVertical);
  fOrientation->addButton(fVertical);
  
  fHorizontal = new QCheckBox(tr("Horizontal"),group);
  fHorizontal->setToolTip(tr("Set horizontal"));
  fHorizontal->setTristate();
  layout->addWidget(fHorizontal);
  fOrientation->addButton(fHorizontal);
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
   ConnectView(fOrientation,SIGNAL(buttonClicked(int))     ,this, SLOT(DoLineOrientation(int)));
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
   fModel->SetVertical  (fVertical  ->checkState() == Qt::Checked);
   fModel->SetHorizontal(fHorizontal->checkState() == Qt::Checked);
#endif   
}


//______________________________________________________________________________                                                                                
//
// Register the GED factory interfaces:
//______________________________________________________________________________                                                                                
static TQtGedFactory<TQtLineEditor>   gQtLineEditor;
