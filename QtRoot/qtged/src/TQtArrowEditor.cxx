// Author: Eric Dumonteil   26/01/2006

/****************************************************************************
** $Id: TQtArrowEditor.cxx,v 1.3 2013/08/30 16:00:10 perev Exp $
**
** Copyright (C) 2006 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TArrowEditor, a ROOT GUI toolkit.             *
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtArrowEditor                                                      //
//                                                                      //
//  Implements GUI for editing arrow attributes: shape, size, angle.    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//Begin_Html
/*
<img src="gif/TArrowEditor.gif">
*/
//End_Html


#include "TQtArrowEditor.h"
#include "TGQt.h"
#include "TQtColorSelect.h"
#include "TQtFloatSpinBox.h"
#include <qlineedit.h>
#include <qtooltip.h>
#include <qlabel.h>
#include <qvbox.h> 
#include <qcheckbox.h> 
#include <qcombobox.h> 
#include <qbuttongroup.h> 
#include <qtextcodec.h> 
#include <qvgroupbox.h> 
#include <qhgroupbox.h> 
#include <qgrid.h> 

#include "TColor.h"
#include "TVirtualPad.h"
#include "TStyle.h"

#include "TCanvas.h"

class TQtArrowStyleSelect : public QComboBox {
protected:
   static const char *fgListOfOptions[];
   static const char *fgListOfVisuals[];
protected:
   void Ctor(){
      insertStrList(fgListOfVisuals);
      setCurrentItem(2);
   }
public:
   TQtArrowStyleSelect(QWidget * parent=0, const char *name=0) : QComboBox(parent,name){ Ctor(); } 
   TQtArrowStyleSelect(bool rw, QWidget *parent=0,const char *name=0): QComboBox(rw,parent,name) { Ctor(); }
   const char *GetCurrentShapeEntry() const {
      return fgListOfOptions[currentItem()];
   }   
};

ClassImp(TQtArrowEditor)

const char *TQtArrowStyleSelect::fgListOfOptions[] =
{
   "|>"   ,
   "<|"   ,
   ">"    ,
   "<"    ,
   "->-"  ,
   "-<-"  ,
   "-|>-" ,
   "-<|-" ,
   "<>"   ,
   "<|>"  ,
   0
};

const char *TQtArrowStyleSelect::fgListOfVisuals[] = 
{
   " -------|>",
   " <|-------",
   " -------->",
   " <--------",
   " ---->----",
   " ----<----",
   " ----|>---",
   " ---<|----",
   " <------>" ,
   " <|-----|>",
   0
};


//______________________________________________________________________________
TQtArrowEditor::TQtArrowEditor(QMainWindow *p, TCanvas *canvas, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TArrow>(p, "Arrow", canvas, id, width, height, options, back)
{
   
}
//______________________________________________________________________________
TQtArrowEditor::TQtArrowEditor( TCanvas *canvas,QWidget  *p, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TArrow>("Arrow", p, canvas, id, width, height, options, back)
{
   
}

//______________________________________________________________________________
void TQtArrowEditor::BuildView(QWidget  *editorPanel) 
{
   // Create GUI control

  fOptionCombo     =0; fAngleEntry    =0; fSizeEntry       =0;
  
  QVBox *vframe  = new QVBox(editorPanel);
  QHBox *hbox = new QHBox(vframe);
  
  new QLabel("Shape:",hbox);
  fOptionCombo = new TQtArrowStyleSelect(FALSE, hbox);
  
  hbox = new QHBox(vframe);vframe->setSpacing(10);
  new QLabel("Angle:",hbox);
  fAngleEntry = new TQtFloatSpinBox(60, 0, 180, 2, hbox);
  QToolTip::add(fAngleEntry,"Set the arrow opening angle in degrees.");
  
  hbox = new QHBox(vframe);vframe->setSpacing(2);
  new QLabel("Size:",hbox); 
  fSizeEntry = new TQtFloatSpinBox(0.05, 0.01, 0.3, 2, hbox);
  QToolTip::add(fSizeEntry,"Set the size of arrow");
//   SetActive(kTRUE);
}

//______________________________________________________________________________
TQtArrowEditor::~TQtArrowEditor() {  }

//______________________________________________________________________________
void TQtArrowEditor::ConnectSignals2Slots()
{
   // Connect signals to slots.

   ConnectView(fAngleEntry,  SIGNAL(valueChanged(int)),this, SLOT(DoAngle(int))   );
   ConnectView(fSizeEntry,   SIGNAL(valueChanged(int)),this, SLOT(DoSize(int))    );
   ConnectView(fOptionCombo, SIGNAL(activated(int )),  this, SLOT(DoOption(int))  );
}

//______________________________________________________________________________
void TQtArrowEditor::ChangeView()
{

   // Pick up the used values of arrow attributes.
   //   View                        Model
   fAngleEntry      ->SetValue(fModel->GetAngle());
   fSizeEntry       ->SetValue(fModel->GetArrowSize());
}

//______________________________________________________________________________
void TQtArrowEditor::DoAngle(int )
{
   // Slot connected to the arrow opening angle setting.

   fModel->SetAngle(fAngleEntry->Value());
}
//______________________________________________________________________________
void TQtArrowEditor::DoOption(int )
{
   // Slot connected to the arrow shape setting.

   const char* opt=fOptionCombo->GetCurrentShapeEntry();
   // to set the option one has to call TWO methods (God knows why)
   fModel->SetOption(opt);   
   // We cannot just call: fModel->SetDrawOption(opt);
   // We need our workaround to set things properly   
   ResetPadOption(fPad,fModel,opt);
}


//______________________________________________________________________________
void TQtArrowEditor::DoSize(int )
{
   // Slot connected to the arrow size.
   fModel->SetArrowSize(fSizeEntry->Value());
}

//______________________________________________________________________________
//
// Register the GED factory interfaces:
//______________________________________________________________________________
static TQtGedFactory<TQtArrowEditor>   gQtArrowEditor;
