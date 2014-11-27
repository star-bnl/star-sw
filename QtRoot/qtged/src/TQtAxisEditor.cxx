// @(#)root/ged:$Name:  $:$Id: TQtAxisEditor.cxx,v 1.4 2013/08/30 16:00:10 perev Exp $
// Author: Ilka Antcheva   11/05/04

/****************************************************************************
** $Id: TQtAxisEditor.cxx,v 1.4 2013/08/30 16:00:10 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TAxisEditor, a ROOT GUI toolkit.               *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 * Author: Ilka Antcheva                                                 *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtAxisEditor                                                         //
//                                                                      //
//  Implements GUI for axis attributes.                                 //
//     axis color                                                       //
//     ticks parameters: length, setting on both axis sides,            //
//     logarithmic or linear scale along the selected axis,             //
//     primary, secondary and tertiary axis divisions,                  //
//     setting more logarithmic labels,                                 //
//     optimizing labels' position if available                         //
//     axis title - a title can be added via the text entry field       //
//     and can be set to be centered or rotated via the corresponding   //
//     check buttons, the title color, offset, font can be set easily   //
//     axis labels - their color, size, offset can be set similarly,    //
//     in addition there is a check box for no exponent choice,         //
//     and another one for setting the same decimal part for all labels //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//Begin_Html
/*
<img src="gif/TAxisEditor.gif">
*/
//End_Html

#ifndef ROOT_TAxis
#include "TAxis.h"
#endif
#include "TQtAxisEditor.h"
#include "TGQt.h"

#include "TQtColorSelect.h"
#include "TQtFloatSpinBox.h"
#include "TQtStyleComboBox.h"
#include <qlineedit.h>
#include <qtooltip.h>
#include <qlabel.h>
#include <qvbox.h> 
#include <qcheckbox.h> 
#include <qbuttongroup.h> 
#include <qtextcodec.h> 
#include <qvgroupbox.h> 
#include <qhgroupbox.h> 
#include <qgrid.h> 

#include "TColor.h"
#include "TVirtualPad.h"
#include "TStyle.h"

ClassImp(TQtAxisEditor)


//______________________________________________________________________________
TQtAxisEditor::TQtAxisEditor(QMainWindow *p, TCanvas *canvas, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TAxis>(p, "Axis", canvas, id, width, height, options, back)
{
   // TQtAxisEditor class ctor
}
//______________________________________________________________________________
TQtAxisEditor::TQtAxisEditor( TCanvas *canvas,QWidget  *p, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TAxis>("Axis", p, canvas, id, width, height, options, back)
{
   // TQtAxisEditor class ctor
}

//______________________________________________________________________________
void TQtAxisEditor::BuildView(QWidget  *editorPanel) {
   const int space = 0;
   const int margin = 4;
   // Constructor of axis attribute GUI.
   QVBox *vframe  = new QVBox(editorPanel);
//--- 
   QHBox *hbox = new QHBox(vframe);hbox->setSpacing(space);
   hbox->setMargin(margin);
   fAxisColor   = new TQtColorSelect  (hbox, 0);

   QLabel *label = new QLabel("Ticks:",hbox); label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
   fTickLength = new TQtFloatSpinBox(0.03, -1.0, 1.0, 2, hbox);
   QToolTip::add(fTickLength,"Set ticks' length");
   //fTickLength->setPrefix("Ticks: ");
//---
   QButtonGroup *f2 = new   QButtonGroup (2, Qt::Horizontal, vframe);
   f2->setFlat(true); f2->setInsideMargin(6); 
   f2->setMargin(margin+2);
   fTicksBoth = new QCheckBox("+-",f2); 
   QToolTip::add(fTicksBoth,"Draw ticks on both axis sides");

   fOptimize = new QCheckBox("Optimize", f2);
   fOptimize->setChecked(true);
   QToolTip::add(fOptimize,"Optimize the number of axis divisions");

   fLogAxis = new QCheckBox("Log",f2);
   QToolTip::add(fLogAxis,"Draw logarithmic scale");

   fMoreLog = new QCheckBox("MoreLog", f2);
   QToolTip::add(fMoreLog,"Draw more logarithmic labels");
//---

   hbox = new QHBox(vframe); // hbox->setSpacing(1);
   hbox->setMargin(margin); hbox->setSpacing(2); 

   fDiv3 = new QSpinBox(0, 99, 1, hbox); fDiv3->setValue(10);
   QToolTip::add(fDiv3, "Tertiary axis divisions");

   fDiv2 = new QSpinBox( 0, 99,1,hbox); fDiv3->setValue(5);
   QToolTip::add(fDiv2,"Secondary axis divisions");

   fDiv1 = new QSpinBox( 0, 99,1,hbox); fDiv3->setValue(0);
   QToolTip::add(fDiv1, "Primary axis divisions");

   fTicksFlag = 1;
// --  title 
   // MakeTitle("Title",vframe);
   QGroupBox *vgroup = new QVGroupBox("Title",vframe);
//---    
   vgroup->setMargin(3);
   // vgroup->setInsideMargin(4);
   fTitlePrec = 2;
   fTitle = new QLineEdit(vgroup);
   QToolTip::add(fTitle,"Enter the axis title string");
//---  5  
   hbox = new QHBox(vgroup); hbox->setSpacing(space);
   fTitleColor = new TQtColorSelect(hbox, 0);

   label = new QLabel("Size:",hbox);label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

   fTitleSize = new TQtFloatSpinBox(0.05, 0.0, 1.0, 2, hbox);
   QToolTip::add(fTitleSize,"Set title size");

  fTitleFont  = new TQtFontComboBox(vgroup);
//---    7
   hbox = new QHBox(vgroup);
//   f2 = new   QButtonGroup (2, Qt::Horizontal, vgroup);
//   f2->setFlat(true); f2->setInsideMargin(0); 

   fCentered = new QCheckBox("Centered",hbox);
   QToolTip::add(fCentered,"Center axis title");

   fRotated = new QCheckBox("Rotated", hbox); fRotated->setChecked(true);
   QToolTip::add(fRotated,"Rotate axis title by 180 degrees");

   hbox = new QHBox(vgroup); hbox->setSpacing(space);
   label = new QLabel("Offset:",hbox); label->setAlignment(Qt::AlignRight | Qt::AlignVCenter );
//---    7
//   hbox = new QHBox(vgroup);

   fTitleOffset = new TQtFloatSpinBox(1.00, 0.1, 10., 1, hbox);
   QToolTip::add(fTitleOffset,"Set title offset");

   // MakeTitle("Labels",vframe);
   vgroup = new QVGroupBox("Labels",vframe);
   vgroup->setMargin(margin);
   vgroup->setInsideMargin(4);
   fLabelPrec = 2;
   hbox = new QHBox(vgroup); hbox->setSpacing(space);
   fLabelColor = new TQtColorSelect(hbox, 0);

   label = new QLabel("Size:",hbox); label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
   fLabelSize = new TQtFloatSpinBox(0.05,0.,1.,2,hbox);
   QToolTip::add(fLabelSize,"Set labels' size");

   hbox = new QHBox(vgroup);
   fNoExponent = new QCheckBox("NoExp", hbox);
   QToolTip::add(fNoExponent,"Labels drawn without exponent notation");
   fLabelOffset = new TQtFloatSpinBox( 0.005, -1.,1.,3, hbox);
   QToolTip::add(fLabelOffset,"Set labels' offset");
   
   fLabelFont = new TQtFontComboBox(vgroup);

   fDecimal = new QCheckBox("Decimal labels' part", vgroup);
   QToolTip::add(fDecimal,"Draw the decimal part of labels");
}

//______________________________________________________________________________
TQtAxisEditor::~TQtAxisEditor()
{  
   // Destructor of axis editor. 
}

//______________________________________________________________________________
void TQtAxisEditor::ConnectSignals2Slots()
{
   // Connect signals to slots.
   //                   Model                                  View
   ConnectView(fAxisColor  ,SIGNAL(ColorSelected(Pixel_t)), this, SLOT(DoAxisColor(Pixel_t)));
   ConnectView(fTickLength ,SIGNAL(valueChanged(int)),      this, SLOT(DoTickLength(int))   );
   ConnectView(fTicksBoth  ,SIGNAL(toggled(bool)),          this, SLOT(DoTicks(bool))       );
   ConnectView(fOptimize   ,SIGNAL(toggled(bool)),          this, SLOT(DoDivisions(bool))   );
   ConnectView(fLogAxis    ,SIGNAL(toggled(bool)),          this, SLOT(DoLogAxis(bool))     );
   ConnectView(fMoreLog    ,SIGNAL(toggled(bool)),          this, SLOT(DoMoreLog(bool))     );
   ConnectView(fDiv3       ,SIGNAL(valueChanged(int)),      this, SLOT(DoDivisions(int))    );
   ConnectView(fDiv2       ,SIGNAL(valueChanged(int)),      this, SLOT(DoDivisions(int))    );
   ConnectView(fDiv1       ,SIGNAL(valueChanged(int)),      this, SLOT(DoDivisions(int))    );
   ConnectView(fTitle      ,SIGNAL(textChanged(const QString &)),this, SLOT(DoTitle(const QString &)));;
   ConnectView(fTitleColor ,SIGNAL(ColorSelected(Pixel_t)), this, SLOT(DoTitleColor(Pixel_t)));
   ConnectView(fTitleSize  ,SIGNAL(valueChanged(int)),      this, SLOT(DoTitleSize(int))    );
   ConnectView(fTitleFont  ,SIGNAL(activated(int)),         this, SLOT(DoTitleFont(int))    );
   ConnectView(fCentered   ,SIGNAL(toggled(bool)),          this, SLOT(DoTitleCentered(bool)));
   ConnectView(fRotated    ,SIGNAL(toggled(bool)),          this, SLOT(DoTitleRotated(bool)));
   ConnectView(fTitleOffset,SIGNAL(valueChanged(int)),      this, SLOT(DoTitleOffset(int))  );
   ConnectView(fLabelColor ,SIGNAL(ColorSelected(Pixel_t)), this, SLOT(DoLabelColor(Pixel_t)));
   ConnectView(fLabelSize  ,SIGNAL(valueChanged(int)),      this, SLOT(DoLabelSize(int))    );
   ConnectView(fNoExponent ,SIGNAL(toggled(bool)),          this, SLOT(DoNoExponent(bool))  );
   ConnectView(fDecimal    ,SIGNAL(toggled(bool)),          this, SLOT(DoDecimal(bool))     );
   ConnectView(fLabelOffset,SIGNAL(valueChanged(int)),      this, SLOT(DoLabelOffset(int))  );
   ConnectView(fLabelFont  ,SIGNAL(activated(int)),         this, SLOT(DoLabelFont(int))    );
}

//______________________________________________________________________________
void TQtAxisEditor::ChangeView()
{
   // Pick up the used values of axis attributes.
   fAxisColor ->SetColor(fModel->GetAxisColor());

   fTickLength->SetValue(fModel->GetTickLength());

   Int_t div = fModel->GetNdivisions();
   fDiv1->setValue(div % 100);
   fDiv2->setValue((div/100) % 100);
   fDiv3->setValue((div/10000) % 100);

   fLogAxis->setChecked (
       (!strcmp(fModel->GetName(),"xaxis") && fPad->GetLogx()) ||
       (!strcmp(fModel->GetName(),"yaxis") && fPad->GetLogy()) ||
       (!strcmp(fModel->GetName(),"zaxis") && fPad->GetLogz())
   );


   if (fLogAxis->isChecked()) {
      fMoreLog->setEnabled(true);
      fMoreLog->setChecked(fModel->GetMoreLogLabels());
   } else {
      fMoreLog->setEnabled(false);
   }

   const char *both = fModel->GetTicks();
   fTicksBoth->setChecked (!strcmp(both,"+-"));
   if (!strcmp(both,"-")) fTicksFlag = -1;
   if (!strcmp(both,"+")) fTicksFlag =  1;

   fTitle->setText(gQt->GetTextDecoder()->toUnicode (fModel->GetTitle()));
   fTitleColor ->SetColor(fModel->GetTitleColor());
   fTitleSize  ->SetValue(fModel->GetTitleSize() );

   Style_t font = fModel->GetTitleFont();
   fTitleFont->SetCurrentItem((int)font / 10);
   fTitlePrec = (Int_t)(font % 10);

   fTitleOffset->SetValue  (fModel->GetTitleOffset());
   fCentered   ->setChecked(fModel->GetCenterTitle());

   fRotated    ->setChecked(fModel->GetRotateTitle());

   fLabelColor ->SetColor  (fModel->GetLabelColor() );
   fLabelSize  ->SetValue  (fModel->GetLabelSize()  );

   font = fModel->GetLabelFont();
   fLabelFont->SetCurrentItem((int)font / 10);
   fLabelPrec = (Int_t)(font % 10);

   fLabelOffset->SetValue  (fModel->GetLabelOffset());
   fNoExponent ->setChecked(fModel->GetNoExponent() );
   fDecimal    ->setChecked(fModel->GetDecimals()   );
}

//______________________________________________________________________________
void TQtAxisEditor::DoAxisColor(Pixel_t color)
{
   // Slot connected to the axis color.
   fModel->SetAxisColor(TColor::GetColor(color));
}
//______________________________________________________________________________
void TQtAxisEditor::DoTickLength(int)
{
   // Slot connected to the tick length settings.

   fModel->SetTickLength(fTickLength->Value());
   
   if (fTickLength->Value() < 0) fTicksFlag = -1;
   else fTicksFlag = 1;
}

//______________________________________________________________________________
void TQtAxisEditor::DoTicks(bool )
{
   // Slot connected to the ticks draw settings.

   if (fTicksBoth->isChecked()) {
      fModel->SetTicks("+-");
   } else {
      if (fTicksFlag == -1) fModel->SetTicks("-");
         else fModel->SetTicks("");
   }
}

//______________________________________________________________________________
void TQtAxisEditor::DoDivisions(bool)
{  DoDivisions();  }
//______________________________________________________________________________
void TQtAxisEditor::DoDivisions(int)
{  DoDivisions();  } 
//______________________________________________________________________________
void TQtAxisEditor::DoDivisions()
{
   // Slot connected to the number of divisions.

   // the number of divisions are used 3 number entry widgets
   Int_t div = fDiv1->value() + fDiv2->value()  * 100 
             + fDiv3->value() * 10000;
   fModel->SetNdivisions(div, fOptimize->isChecked());
}

//______________________________________________________________________________
void TQtAxisEditor::DoLogAxis(bool on)
{
   // Slot for Log scale setting.

   if (on) {

      if (!strcmp(fModel->GetName(),"xaxis")) fPad->SetLogx(1);
      if (!strcmp(fModel->GetName(),"yaxis")) fPad->SetLogy(1);
      if (!strcmp(fModel->GetName(),"zaxis")) fPad->SetLogz(1);

      fMoreLog->setChecked(fModel->GetMoreLogLabels());
      fOptimize->setEnabled(false);

   } else {
      if (!strcmp(fModel->GetName(),"xaxis")) fPad->SetLogx(0);
      if (!strcmp(fModel->GetName(),"yaxis")) fPad->SetLogy(0);
      if (!strcmp(fModel->GetName(),"zaxis")) fPad->SetLogz(0);
      fMoreLog->setEnabled(false);
      fOptimize->setChecked(true);
   }
}

//______________________________________________________________________________
void TQtAxisEditor::DoMoreLog(bool on)
{
   // Slot connected to more Log labels flag

   fModel->SetMoreLogLabels(on);
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitle(const QString &text)
{
   // Slot connected to the axis color.

#if QT_VERSION < 0x40000
   QCString r = gQt->GetTextDecoder()->fromUnicode(text);
#else /* QT_VERSION */
   Q3CString r = gQt->GetTextDecoder()->fromUnicode(text);
#endif /* QT_VERSION */
   fModel->SetTitle((const char*)r);
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleColor(Pixel_t color)
{
   // Slot connected to the title color.

   fModel->SetTitleColor(TColor::GetColor(color));
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleSize(int)
{
   // Slot connected to the title font size.

   Float_t size = fTitleSize->Value();
   fModel->SetTitleSize(size);
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleFont(Int_t font)
{
   // Slot connected to the title font.

   Int_t f = (font+1) * 10 + fTitlePrec;
   fModel->SetTitleFont(f);
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleOffset(int)
{
   // Slot connected to the title offset.

   Float_t offset = fTitleOffset->Value();
   fModel->SetTitleOffset(offset);
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleCentered(bool on)
{
   // Slot connected to centered title option.

   fModel->CenterTitle(on);
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleRotated(bool on)
{
   // Slot connected to the title rotation.

   fModel->RotateTitle(on);
}

//______________________________________________________________________________
void TQtAxisEditor::DoLabelColor(Pixel_t color)
{
   // Slot connected to the label color.

   fModel->SetLabelColor(TColor::GetColor(color));
}

//______________________________________________________________________________
void TQtAxisEditor::DoLabelSize(int)
{
   // Slot connected to the label size.

   Float_t size = fLabelSize->Value();
   fModel->SetLabelSize(size);
}

//______________________________________________________________________________
void TQtAxisEditor::DoLabelFont(int font)
{
   // Slot connected to the label font.

   Int_t f = (font+1) * 10 + fLabelPrec;
   fModel->SetLabelFont(f);
}

//______________________________________________________________________________
void TQtAxisEditor::DoLabelOffset(int)
{
   // Slot connected to the label offset.

   Float_t offset = fLabelOffset->Value();
   fModel->SetLabelOffset(offset);
}

//______________________________________________________________________________
void TQtAxisEditor::DoNoExponent(bool on )
{
   // Slot connected to the labels' exponent flag.

   fModel->SetNoExponent(on);
}

//______________________________________________________________________________
void TQtAxisEditor::DoDecimal(bool on)
{
   // Slot connected to the decimal part setting.

   fModel->SetDecimals(on);
   gStyle->SetStripDecimals(!on);
}

// Regiaster the GED factory interfaces:
static TQtGedFactory<TQtAxisEditor>   gQtAxisEditor;
