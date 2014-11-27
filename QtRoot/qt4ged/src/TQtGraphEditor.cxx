// @(#)root/ged:$Name:  $:$Id: TQtGraphEditor.cxx,v 1.3 2013/08/30 15:59:53 perev Exp $
// Author: Valeri Fine 16/08/04

/****************************************************************************
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TGraphEditor , a ROOT GUI toolkit.            *
 * Author: Valeri  16/08/04                                              *
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/


//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtGraphEditor                                                        //
//                                                                      //
//  Implements GUI for graph attributes.                                //
//                                                                      //
//  Title': set the title of the graph                                  //
//  Change the Shape of the graph:                                      //
//     'No Line'     = " ": just draw unconnected points                //
//     'Simple Line' = "L":simple poly line between every point is drawn//
//     'Smooth Line' = "C":smooth curve is drawn                        //
//     'Bar Chart'   = "B": A bar chart is drawn at each point          //
//     'Fill Area'   = "F": A fill area is drawn                        //
//  Check box: 'Marker On/Off' Set Marker visible/invisible             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//Begin_Html
/*
<img src="gif/TQtGraphEditor.gif">
*/
//End_Html

#include "TQtGraphEditor.h"
#include "TQtStyleComboBox.h"
#include "TGQt.h"

#include "TColor.h"
#include "TVirtualPad.h"
#include "TStyle.h"

#include <qlabel.h>
#if QT_VERSION < 0x40000
#  include <qvbox.h> 
#  include <qvgroupbox.h> 
#  include <qhbuttongroup.h> 
#  include <qvbuttongroup.h>
#else /* QT_VERSION */
#  include <Q3CString>
#  include <QVBoxLayout>
#  include <QHBoxLayout>
#  include <QButtonGroup>
#  include <QGroupBox>
#endif /* QT_VERSION */
#include <qcheckbox.h> 

#include <qtooltip.h>
#include <qlineedit.h>
#include <qradiobutton.h> 
#include <qtextcodec.h>

ClassImp(TQtGraphEditor)

enum EGraphWid {
   kShape = 0,
   kMARKER_ONOFF,
   kGRAPH_TITLE,
   kGRAPH_LINE_WIDTH,
   kGRAPH_LINE_SIDE
};
   
enum {
   kSHAPE_NOLINE,
   kSHAPE_SMOOTH,
   kSHAPE_SIMPLE,
   kSHAPE_BAR,
   kSHAPE_FILL
};


//______________________________________________________________________________
TQtGraphEditor::TQtGraphEditor(QMainWindow *p, TCanvas *canvas, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TGraph>(p, QString::null, canvas, id, width, height, options, back)
{ 
}
//______________________________________________________________________________
TQtGraphEditor::TQtGraphEditor( TCanvas *canvas,QWidget  *p, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TGraph>("Graph", p, canvas, id, width, height, options, back)
{ 
}
//______________________________________________________________________________
void TQtGraphEditor::BuildView(QWidget  *editorPanel) {

   // Constructor of graph editor.

   delete editorPanel->layout();
   QVBoxLayout *vframe= new QVBoxLayout(editorPanel);
   
//    MakeTitle("Title",vframe);
   
   // TextEntry to change the title
   fTitlePrec = 2;
   fTitle = new QLineEdit(editorPanel);
   vframe->addWidget(fTitle);
#if QT_VERSION < 0x40000
   QToolTip::add(fTitle,"Enter the graph title string");

    // Radio Buttons to change the draw options of the graph
   fgr = new  QVButtonGroup ("Shape", vframe);
   fgr->setRadioButtonExclusive(TRUE);fgr->setInsideSpacing(0);
#else /* QT_VERSION */
   fTitle->setToolTip(tr("Enter the graph title string"));
   
   QGroupBox   *group  = new QGroupBox(editorPanel);
   vframe->addWidget(group);
   QVBoxLayout *vbox = new QVBoxLayout(group);
   fgr = new  QButtonGroup (group);
   fgr->setExclusive(TRUE);// fgr->setInsideSpacing(0);
#endif /* QT_VERSION */
   const char *shapes[] = {
      //    "Label"       "ToolTip"
        "no line",    "The points are not connected by a line"
      , "Smooth Line","Draw a smooth graph curve"
      , "Simple Line","Draw a simple poly-line between the graph points"
      , "Bar Chart",  "Draw a bar chart at each graph point"
      , "Fill area",  "A fill area is drawn"
      , 0,0
   };
   const char **nextShape = shapes;
   int id = kSHAPE_NOLINE;
   while (*nextShape) {
#if QT_VERSION < 0x40000
      QRadioButton *shape = new QRadioButton(*nextShape++,fgr);
      QToolTip::add(shape,*nextShape++);
#else      
      QRadioButton *shape = new QRadioButton(*nextShape++,group);
      vbox->addWidget(shape);
      fgr->addButton(shape,id++);
      shape->setToolTip(tr(*nextShape++));
#endif            
   }

   // CheckBox to activate/deactivate the drawing of the Marker
   fMarkerOnOff = new QCheckBox("Show Marker",editorPanel);
   vframe->addWidget(fMarkerOnOff);
#if QT_VERSION < 0x40000
   QToolTip::add(fMarkerOnOff,"Make Marker visible/invisible");
#else
   fMarkerOnOff->setToolTip(tr("Make Marker visible/invisible"));
#endif      

   // Exclusion zone parameters
 //   MakeTitle("Exclusion Zone",vframe);
#if QT_VERSION < 0x40000
   QHBox *hbox = new QHBox(vframe);
   fExSide = new QCheckBox("+-",hbox);
   QToolTip::add(fExSide,"Zone is drawing side");

   fWidthCombo = new TQtLineWidthComboBox (hbox);
   //fWidthCombo->Resize(91, 20);
#else /* QT_VERSION */
   QWidget *hbox = new QWidget(editorPanel);
   vframe->addWidget(hbox);
   QHBoxLayout *hboxLayout = new QHBoxLayout(hbox);
   
   fExSide = new QCheckBox("+-",hbox);
   fExSide->setToolTip(tr("Zone is drawing side"));
   hboxLayout->addWidget(fExSide);

   fWidthCombo = new TQtLineWidthComboBox (hbox);
   hboxLayout->addWidget(fWidthCombo);
#endif /* QT_VERSION */
   
  

}

//______________________________________________________________________________
TQtGraphEditor::~TQtGraphEditor()
{
   // Destructor of graph editor.
}

//______________________________________________________________________________
void TQtGraphEditor::ConnectSignals2Slots()
{
   // Connect signals to slots to change the "Model"

   ConnectView(fTitle,      SIGNAL(textChanged(const QString &)),this, SLOT(DoTitle(const QString &)) );
#if QT_VERSION < 0x40000
   ConnectView(fgr,         SIGNAL(clicked(int))                ,this, SLOT(DoShape(int))             );
#else   
   ConnectView(fgr,         SIGNAL(buttonClicked(int))          ,this, SLOT(DoShape(int))             );
#endif   
   ConnectView(fMarkerOnOff,SIGNAL(toggled(bool))               ,this, SLOT(DoMarkerOnOff(bool))      );
   ConnectView(fWidthCombo, SIGNAL(activated(int))              ,this, SLOT(DoGraphLineWidth(int))    );
   ConnectView(fExSide,     SIGNAL(toggled(bool))               ,this, SLOT(DoGraphLineWidth(bool))   );
}
//______________________________________________________________________________
void TQtGraphEditor::ChangeView()
{
   // Pick up the used values of graph attributes.
   
   // set the Title TextEntry
   const char *text = fModel->GetTitle();
   fTitle->setText(gQt->GetTextDecoder()->toUnicode (text) );

   TString opt = GetDrawOption();
   opt.ToUpper();
   Int_t i=0;
   Bool_t make=kFALSE;
   // Remove characters which appear twice in the draw option
   TString dum  = opt;
   Int_t l = opt.Length()-1;
   while (i < l) {
      dum.Remove(dum.First(opt[i]),1);
      if (dum.Contains(opt[i])){
         opt.Remove(opt.First(opt[i]),1);
         l--;
         i--;
         make=kTRUE;
      }
      i++;
   }
   // initialise the RadioButton group which shows the drawoption
#if QT_VERSION < 0x40000
   if (opt.Contains("C")) {
      fgr->setButton(kSHAPE_SMOOTH);
      fDrawShape='C';
   } else if (opt.Contains("L")) {
      fgr->setButton(kSHAPE_SIMPLE);
      fDrawShape='L';
   } else if (opt.Contains("B")){
      fgr->setButton(kSHAPE_BAR);
      fDrawShape='B';
   } else if (opt.Contains("F")){
      fgr->setButton(kSHAPE_FILL);
      fDrawShape='F';
   } else {
      fgr->setButton(kSHAPE_NOLINE);
      fDrawShape=' ';
   }
#else
   QAbstractButton *button2Check = 0;
   if (opt.Contains("C")) {
      button2Check = fgr->button(kSHAPE_SMOOTH);
      fDrawShape='C';
   } else if (opt.Contains("L")) {
      button2Check = fgr->button(kSHAPE_SIMPLE);
      fDrawShape='L';
   } else if (opt.Contains("B")){
      button2Check = fgr->button(kSHAPE_BAR);
      fDrawShape='B';
   } else if (opt.Contains("F")){
      button2Check = fgr->button(kSHAPE_FILL);
      fDrawShape='F';
   } else {
      button2Check = fgr->button(kSHAPE_NOLINE);
      fDrawShape=' ';
   }
   if (button2Check) button2Check->setChecked(true);
#endif      
   if (make) SetDrawOption(opt);
   // if the draw option is A, P, AP the P option cannot be removed,
   // we deactivate the CheckBox
   // also initialising the MarkerOnOff checkbutton (== P option)
   if (opt=="A" || opt=="AP" || opt=="PA" || opt == "P") {
      if (!opt.Contains("P"))
         opt +="P";
      fMarkerOnOff->setEnabled(FALSE);
   } else {
      fMarkerOnOff->setEnabled(TRUE);
      if (opt.Contains("P")) 
        fMarkerOnOff->setChecked(TRUE);
      else 
        fMarkerOnOff->setChecked(FALSE);
   }

   // Exclusion zone parameters
   if (fModel->GetLineWidth()<0) fExSide->setChecked(TRUE); //kFALSE);
   else fExSide->setChecked(FALSE); //  kFALSE);
   fWidthCombo ->SetCurrentItem((int)TMath::Abs(Int_t(fModel->GetLineWidth()/100))); //, kFALSE);
}

//______________________________________________________________________________
void TQtGraphEditor::DoTitle(const QString &text)
{
   // Slot for setting the graph title.
#if QT_VERSION < 0x40000
   QCString r = gQt->GetTextDecoder()->fromUnicode(text);
#else /* QT_VERSION */
   Q3CString r = gQt->GetTextDecoder()->fromUnicode(text);
#endif /* QT_VERSION */

   fModel->SetTitle((const char *)r );
}

//______________________________________________________________________________
void TQtGraphEditor::DoShape(int s)
{
   // Slot connected to the draw options.

   TString opt;
   if (fModel->InheritsFrom("TGraphErrors"))
      opt = fModel->GetDrawOption();
   else
      opt = GetDrawOption();

   opt.ToUpper();
    
   switch (s) {

      // change draw option to No Line:
      case kSHAPE_NOLINE: 
          if (opt.Contains(fDrawShape))
               opt.Remove(opt.First(fDrawShape),1);
          fDrawShape = ' ';
          fMarkerOnOff->setEnabled(FALSE);
          break;

     // change draw option to Smooth Line (C)
     case kSHAPE_SMOOTH: 
        if (fDrawShape == ' ')
             opt +="C";
        else if (opt.Contains(fDrawShape))
           opt.Replace(opt.First(fDrawShape),1,'C');
        fDrawShape = 'C';
        break;

     // change draw option to Simple Line (L)
     case kSHAPE_SIMPLE:
        if (fDrawShape == ' ')
            opt +="L";
        else if (opt.Contains(fDrawShape))
            opt.Replace(opt.First(fDrawShape),1,'L');
        fDrawShape='L';
        break;

     // change draw option to Bar Chart (B)
     case kSHAPE_BAR: 
        if (fDrawShape == ' ')
            opt +="B";
        else if (opt.Contains(fDrawShape))
            opt.Replace(opt.First(fDrawShape),1,'B');
        fDrawShape='B';
        break;

     // change draw option to Fill Area (F)
     case kSHAPE_FILL: 
        if (fDrawShape == ' ')
           opt +="F";
        else if (opt.Contains(fDrawShape))
           opt.Replace(opt.First(fDrawShape),1,'F');
        fDrawShape='F';
        break;
   }

   // set/reset the Marker CheckBox
   if (opt.Contains("P"))
      fMarkerOnOff->setChecked(TRUE);
   else
      fMarkerOnOff->setChecked(FALSE);
   if (opt=="A" || opt=="AP" || opt=="PA" || opt == "P") {
      if (!opt.Contains("P"))
         opt +="P";
      fMarkerOnOff->setEnabled(FALSE);
   } else {
      fMarkerOnOff->setEnabled(TRUE);
   }


   // set/reset the exclusion zone CheckBox
   if (opt.Contains("L") || opt.Contains("C")) {
      if (fModel->GetLineWidth()<0) fExSide->setChecked(TRUE); // FALSE
      else fExSide->setChecked(FALSE); // , kFALSE);
      fWidthCombo->setEnabled(kTRUE);
      fExSide    ->setEnabled(TRUE);
   } else {
      fExSide    ->setEnabled(FALSE);
      fWidthCombo->setEnabled(FALSE);
   }
   SetDrawOption(opt);
}

//______________________________________________________________________________
void TQtGraphEditor::DoMarkerOnOff(bool on)
{
   // Slot for setting markers as visible/invisible.

   TString t = GetDrawOption();
   t.ToUpper();

   // showing the marker:
   if (on) {
      if  (!t.Contains("P")) t+="P";
#if QT_VERSION < 0x40000
            fgr->find(kSHAPE_NOLINE)->setEnabled(TRUE);
#else            
            fgr->button(kSHAPE_NOLINE)->setEnabled(TRUE);
#endif            
   } else {
      // remove the marker option P
      while (t.Contains("P")) t.Remove(t.First("P"),1);
#if QT_VERSION < 0x40000
      fgr->find(kSHAPE_NOLINE)->setEnabled(FALSE);
#else      
      fgr->button(kSHAPE_NOLINE)->setEnabled(FALSE);
#endif      
   }
   SetDrawOption(t); 
}
//______________________________________________________________________________
void TQtGraphEditor::DoGraphLineWidth(bool)
{
   // Slot connected to the graph line exclusion check box.
   DoGraphLineWidth(fWidthCombo->currentItem());
}
//______________________________________________________________________________
void TQtGraphEditor::DoGraphLineWidth(int width)
{
   // Slot connected to the graph line width.

   // width;
   Int_t lineWidth = TMath::Abs(fModel->GetLineWidth()%100);
   Int_t side = 1;
   if (fExSide->isChecked() ) side = -1;
   fModel->SetLineWidth(side*(100*width+lineWidth));
}

static TQtGedFactory<TQtGraphEditor>   gQtGraphEditor;
