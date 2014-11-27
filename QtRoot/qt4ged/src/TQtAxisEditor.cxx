// @(#)root/ged:$Name:  $:$Id: TQtAxisEditor.cxx,v 1.3 2013/08/30 15:59:53 perev Exp $
// Author: Ilka Antcheva   11/05/04

/****************************************************************************
** $Id: TQtAxisEditor.cxx,v 1.3 2013/08/30 15:59:53 perev Exp $
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
#include "TQtColorSelectButton.h"
#include "TQtFloatSpinBox.h"
#include "TQtStyleComboBox.h"
#include <qlineedit.h>
#include <qtooltip.h>
#include <qlabel.h>
#if QT_VERSION < 0x40000
#  include <qvgroupbox.h> 
#  include <qhgroupbox.h> 
#  include <qgrid.h> 
#  include <qvbox.h> 
#  include <qbuttongroup.h> 
#include <qcheckbox.h> 
#else /* QT_VERSION */
#  include <QVBoxLayout>
#  include <QHBoxLayout>
#  include <QGroupBox>
//Added by qt3to4:
#  include <Q3CString>
#  include <QCheckBox> 
#endif /* QT_VERSION */
#include <qtextcodec.h> 

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
   const int space = 2;
   const int margin = 1;
   // Constructor of axis attribute GUI.
   delete editorPanel->layout();
   QVBoxLayout *vframe =   new QVBoxLayout(editorPanel);
   vframe->setMargin(margin);  vframe->setSpacing(space);
   QWidget *hbox = new QWidget(this);
   vframe->addWidget(hbox);
   
   QHBoxLayout *hboxLayout  = new QHBoxLayout(hbox);
   hboxLayout->setSpacing(space);
   hboxLayout->setMargin(margin);
//--- 

  fAxisColor   = new TQtColorSelect(hbox, 0);
  hboxLayout->addWidget(fAxisColor->GetColorSelectButton());
 
   QLabel *label = 0;
   hboxLayout->addWidget(
           label = new QLabel("Ticks:",hbox)); 
   label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
   hboxLayout->addWidget(
            fTickLength = new TQtFloatSpinBox(0.03, -1.0, 1.0, 2, hbox));
#if QT_VERSION < 0x40000
   QToolTip::add(fTickLength,"Set ticks' length");
#else   
   fTickLength->setToolTip(tr("Set ticks' length"));
#endif   
   //fTickLength->setPrefix("Ticks: ");
//---
#if QT_VERSION < 0x40000
   QButtonGroup *f2 = new   QButtonGroup (2, Qt::Horizontal, this);
   vframe->addWidget(f2);
   f2->setFlat(true); f2->setInsideMargin(0); 
   
   fTicksBoth = new QCheckBox("+-",f2);
   QToolTip::add(fTicksBoth,"Draw ticks on both axis sides");

   fOptimize = new QCheckBox("Optimize", f2);
   fOptimize->setChecked(true);
   QToolTip::add(fOptimize,"Optimize the number of axis divisions");

   fLogAxis = new QCheckBox("Log",f2);
   QToolTip::add(fLogAxis,"Draw logarithmic scale");

   fMoreLog = new QCheckBox("MoreLog", f2);
   QToolTip::add(fMoreLog,"Draw more logarithmic labels");
#else /* QT_VERSION */   
   QWidget *f2 = new  QWidget(editorPanel);
   vframe->addWidget(f2);
   
   QGridLayout *f2Layout = new QGridLayout(f2);
   f2Layout->setSpacing(0); 
   f2Layout->setMargin(2); 
   
   fTicksBoth = new QCheckBox("+-",f2);
   f2Layout->addWidget(fTicksBoth,0,0);
   fTicksBoth->setToolTip(tr("Draw ticks on both axis sides"));

   fOptimize = new QCheckBox("Optimize",f2);
   f2Layout->addWidget(fOptimize,0,1);
   fOptimize->setChecked(true);
   fOptimize->setToolTip(tr("Optimize the number of axis divisions"));

   fLogAxis = new QCheckBox("Log",f2);
   f2Layout->addWidget(fLogAxis,1,0);
   fLogAxis->setToolTip(tr("Draw logarithmic scale"));

   fMoreLog = new QCheckBox("MoreLog", f2);
   f2Layout->addWidget(fMoreLog,1,1);
   fMoreLog->setToolTip(tr("Draw more logarithmic labels"));
#endif /* QT_VERSION */

//---
#if QT_VERSION < 0x40000
   hbox = new QHBox(this); // hbox->setSpacing(1);
   vframe->addWidget(hbox);
   
   fDiv3 = new QSpinBox(0, 99, 1, hbox); fDiv3->setValue(10);
   QToolTip::add(fDiv3, "Tertiary axis divisions");

   fDiv2 = new QSpinBox( 0, 99,1,hbox); fDiv3->setValue(5);
   QToolTip::add(fDiv2,"Secondary axis divisions");

   fDiv1 = new QSpinBox( 0, 99,1,hbox); fDiv3->setValue(0);
   QToolTip::add(fDiv1, "Primary axis divisions");
#else /* QT_VERSION */
   hbox = new QWidget(editorPanel);
   vframe->addWidget(hbox);
   hboxLayout  = new QHBoxLayout(hbox);
   hboxLayout->setMargin(2); hboxLayout->setSpacing(2);
   
   hboxLayout->addWidget(
         fDiv3 = new QSpinBox(0, 99, 1, hbox));
   fDiv3->setValue(10);
   fDiv3->setToolTip(tr("Tertiary axis divisions"));

   hboxLayout->addWidget(
         fDiv2 = new QSpinBox( 0, 99,1,hbox));
   fDiv3->setValue(5);
   fDiv2->setToolTip(tr("Secondary axis divisions"));

   hboxLayout->addWidget(
         fDiv1 = new QSpinBox( 0, 99,1,hbox));
   fDiv3->setValue(0);
   fDiv1->setToolTip(tr("Primary axis divisions"));   
#endif /* QT_VERSION */


   fTicksFlag = 1;
// --  title 
   // MakeTitle("Title",vframe);
#if QT_VERSION < 0x40000
   QGroupBox *vgroup = new QVGroupBox("Title",this);
#else /* QT_VERSION */
   QGroupBox *vgroup = new QGroupBox("Title",this);
   QVBoxLayout *vgroupLayout = new QVBoxLayout(vgroup);
   vgroupLayout->setMargin(1);
   vgroupLayout->setSpacing(1);
#endif /* QT_VERSION */
   vframe->addWidget(vgroup);

//---    
   fTitlePrec = 2;
   fTitle = new QLineEdit(vgroup);
#if QT_VERSION < 0x40000
   QToolTip::add(fTitle,"Enter the axis title string");
#else /* QT_VERSION */
   vgroupLayout->addWidget(fTitle);
   fTitle->setToolTip(tr("Enter the axis title string"));
#endif   
   
//---  5  
   
#if QT_VERSION < 0x40000
   hbox = new QHBox(vgroup); ((QHBox *)hbox)->setSpacing(space);
#else /* QT_VERSION */
    vgroupLayout->addWidget(
          hbox       = new QWidget(vgroup));    
    hboxLayout = new QHBoxLayout(hbox);
    hboxLayout->setSpacing(space);
#endif /* QT_VERSION */
   fTitleColor = new TQtColorSelect(hbox, 0);

   label = new QLabel("Size:",hbox);
   label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

   fTitleSize = new TQtFloatSpinBox(0.05, 0.0, 1.0, 2, hbox);
#if QT_VERSION < 0x40000
   QToolTip::add(fTitleSize,"Set title size");
#else   
   hboxLayout->addWidget(fTitleColor->GetColorSelectButton());
   hboxLayout->addWidget(label);
   hboxLayout->addWidget(fTitleSize);
   
   fTitleSize->setToolTip(tr("Set title size"));
#endif   

  fTitleFont  = new TQtFontComboBox(vgroup);
//---    7
#if QT_VERSION < 0x40000
   hbox = new QHBox(vgroup);
#else /* QT_VERSION */
    vgroupLayout->addWidget(fTitleFont);
    vgroupLayout->addWidget(
          hbox       = new QWidget(vgroup));    
    hboxLayout = new QHBoxLayout(hbox);
#endif /* QT_VERSION */
//   f2 = new   QButtonGroup (2, Qt::Horizontal, vgroup);
//   f2->setFlat(true); f2->setInsideMargin(0); 

   fCentered = new QCheckBox("Centered",hbox);
   fRotated = new QCheckBox("Rotated", hbox); fRotated->setChecked(true);
   
#if QT_VERSION < 0x40000
   QToolTip::add(fCentered,"Center axis title");
   QToolTip::add(fRotated,"Rotate axis title by 180 degrees");
#else
   hboxLayout->addWidget(fCentered);
   hboxLayout->addWidget(fRotated);
   
   fCentered->setToolTip(tr("Center axis title"));
   fRotated->setToolTip(tr("Rotate axis title by 180 degrees"));
#endif      

#if QT_VERSION < 0x40000
   hbox = new QHBox(vgroup); ((QHBox *)hbox)->setSpacing(space);
#else /* QT_VERSION */
   vgroupLayout->addWidget(
         hbox       = new QWidget(vgroup));    
    hboxLayout = new QHBoxLayout(hbox);
    hboxLayout->setSpacing(space);
#endif /* QT_VERSION */
  hboxLayout->addWidget(
       label = new QLabel("Offset:",hbox)); 
   label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
//---    7
//   hbox = new QHBox(vgroup);

   fTitleOffset = new TQtFloatSpinBox(1.00, 0.1, 10., 1, hbox);
#if QT_VERSION < 0x40000
   QToolTip::add(fTitleOffset,"Set title offset");
#else
   hboxLayout->addWidget(fTitleOffset);
   fTitleOffset->setToolTip(tr("Set title offset"));
#endif      

   // MakeTitle("Labels",vframe);
#if QT_VERSION < 0x40000
   vgroup = new QVGroupBox("Labels",this);
#else /* QT_VERSION */
   vframe->addWidget(
         vgroup = new QGroupBox("Labels",vgroup));
   QVBoxLayout *vbox = new QVBoxLayout(vgroup);
   vbox->setMargin(margin); vbox->setSpacing(2);

#endif /* QT_VERSION */
   fLabelPrec = 2;
#if QT_VERSION < 0x40000
   hbox = new QHBox(vgroup); ((QHBox *)hbox)->setSpacing(space);
#else /* QT_VERSION */
   vbox->addWidget(
         hbox       = new QWidget(vgroup));    
    hboxLayout = new QHBoxLayout(hbox);
    hboxLayout->setSpacing(space);
    hboxLayout->setMargin(margin);
#endif /* QT_VERSION */
   fLabelColor = new TQtColorSelect(hbox, 0);

   label = new QLabel("Size:",hbox); 
   label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);                             
   fLabelSize = new TQtFloatSpinBox(0.05,0.,1.,2,hbox);
#if QT_VERSION < 0x40000
   QToolTip::add(fLabelSize,"Set labels' size");
#else
   hboxLayout->addWidget(fLabelColor->GetColorSelectButton());
   hboxLayout->addWidget(label);
   hboxLayout->addWidget(fLabelSize);
   
   fLabelSize->setToolTip(tr("Set labels' size"));
#endif   

#if QT_VERSION < 0x40000
   hbox = new QHBox(vgroup);
#else /* QT_VERSION */
    vbox->addWidget(
          hbox       = new QWidget(vgroup));    
    hboxLayout = new QHBoxLayout(hbox);
    hboxLayout->setSpacing(space);
    hboxLayout->setMargin(0);
#endif /* QT_VERSION */
   fNoExponent = new QCheckBox("NoExp", hbox);
   fLabelOffset = new TQtFloatSpinBox( 0.005, -1.,1.,3, hbox);
   
   fLabelFont = new TQtFontComboBox(vgroup);

   fDecimal = new QCheckBox("Decimal labels' part", vgroup);
#if QT_VERSION < 0x40000
   QToolTip::add(fDecimal,"Draw the decimal part of labels");
   QToolTip::add(fNoExponent,"Labels drawn without exponent notation");
   QToolTip::add(fLabelOffset,"Set labels' offset");
#else
   hboxLayout->addWidget(fNoExponent);
   hboxLayout->addWidget(fLabelOffset);   
   vbox->addWidget(fLabelFont);
   vbox->addWidget(fDecimal);

      fDecimal ->setToolTip(tr("Draw the decimal part of labels"));
   fNoExponent ->setToolTip(tr("Labels drawn without exponent notation"));
   fLabelOffset->setToolTip(tr("Set labels' offset"));
#endif      
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
   ConnectView(fTicksBoth  ,SIGNAL(toggled(bool)),          this, SLOT(DoTicks(bool))       );
   ConnectView(fOptimize   ,SIGNAL(toggled(bool)),          this, SLOT(DoDivisions(bool))   );
   ConnectView(fLogAxis    ,SIGNAL(toggled(bool)),          this, SLOT(DoLogAxis(bool))     );
   ConnectView(fMoreLog    ,SIGNAL(toggled(bool)),          this, SLOT(DoMoreLog(bool))     );
   ConnectView(fDiv3       ,SIGNAL(valueChanged(int)),      this, SLOT(DoDivisions(int))    );
   ConnectView(fDiv2       ,SIGNAL(valueChanged(int)),      this, SLOT(DoDivisions(int))    );
   ConnectView(fDiv1       ,SIGNAL(valueChanged(int)),      this, SLOT(DoDivisions(int))    );
   ConnectView(fTitle      ,SIGNAL(textChanged(const QString &)),this, SLOT(DoTitle(const QString &)));;
   ConnectView(fTitleColor ,SIGNAL(ColorSelected(Pixel_t)), this, SLOT(DoTitleColor(Pixel_t)));
   ConnectView(fTitleFont  ,SIGNAL(activated(int)),         this, SLOT(DoTitleFont(int))    );
   ConnectView(fCentered   ,SIGNAL(toggled(bool)),          this, SLOT(DoTitleCentered(bool)));
   ConnectView(fRotated    ,SIGNAL(toggled(bool)),          this, SLOT(DoTitleRotated(bool)));
   ConnectView(fLabelColor ,SIGNAL(ColorSelected(Pixel_t)), this, SLOT(DoLabelColor(Pixel_t)));
   ConnectView(fNoExponent ,SIGNAL(toggled(bool)),          this, SLOT(DoNoExponent(bool))  );
   ConnectView(fDecimal    ,SIGNAL(toggled(bool)),          this, SLOT(DoDecimal(bool))     );
   ConnectView(fLabelFont  ,SIGNAL(activated(int)),         this, SLOT(DoLabelFont(int))    );

#if QT_VERSION < 0x40000
   ConnectView(fTickLength ,SIGNAL(valueChanged(int)),      this, SLOT(DoTickLength(int))   );
   ConnectView(fTitleSize  ,SIGNAL(valueChanged(int)),      this, SLOT(DoTitleSize(int))    );
   ConnectView(fTitleOffset,SIGNAL(valueChanged(int)),      this, SLOT(DoTitleOffset(int))  );
   ConnectView(fLabelSize  ,SIGNAL(valueChanged(int)),      this, SLOT(DoLabelSize(int))    );
   ConnectView(fLabelOffset,SIGNAL(valueChanged(int)),      this, SLOT(DoLabelOffset(int))  );
#else
   ConnectView(fTickLength ,SIGNAL(valueChanged(double)),      this, SLOT(DoTickLength(double))   );
   ConnectView(fTitleSize  ,SIGNAL(valueChanged(double)),      this, SLOT(DoTitleSize(double))    );
   ConnectView(fTitleOffset,SIGNAL(valueChanged(double)),      this, SLOT(DoTitleOffset(double))  );
   ConnectView(fLabelSize  ,SIGNAL(valueChanged(double)),      this, SLOT(DoLabelSize(double))    );
   ConnectView(fLabelOffset,SIGNAL(valueChanged(double)),      this, SLOT(DoLabelOffset(double))  );
#endif      

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
void TQtAxisEditor::DoTickLength(double )
{
   // Slot connected to the tick length settings.

   fModel->SetTickLength(fTickLength->Value());
   
   if (fTickLength->Value() < 0) fTicksFlag = -1;
   else fTicksFlag = 1;
}

//______________________________________________________________________________
void TQtAxisEditor::DoTickLength(int v)
{
   // Slot connected to the tick length settings.
   DoTickLength(double(v));
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
void TQtAxisEditor::DoTitleSize(int v)
{
   // Slot connected to the title font size.
   DoTitleSize(double(v));
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleSize(double)
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
void TQtAxisEditor::DoTitleOffset(int v)
{
   // Slot connected to the title offset.
   DoTitleOffset(double(v));
}

//______________________________________________________________________________
void TQtAxisEditor::DoTitleOffset(double)
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
void TQtAxisEditor::DoLabelSize(int v)
{
   // Slot connected to the label size.

   DoLabelSize(double(v));
}

//______________________________________________________________________________
void TQtAxisEditor::DoLabelSize(double)
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
void TQtAxisEditor::DoLabelOffset(double)
{
   // Slot connected to the label offset.

   Float_t offset = fLabelOffset->Value();
   fModel->SetLabelOffset(offset);
}

//______________________________________________________________________________
void TQtAxisEditor::DoLabelOffset(int v)
{
   // Slot connected to the label offset.

   DoLabelOffset(double(v));   
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
