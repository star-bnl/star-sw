// Author: Eric Dumonteil   26/01/2006

/****************************************************************************
** $Id: TQtArrowEditor.cxx,v 1.3 2013/08/30 15:59:53 perev Exp $
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
#if QT_VERSION < 0x40000
#  include <qvbox.h> 
#  include <qbuttongroup.h> 
#  include <qvgroupbox.h> 
#  include <qhgroupbox.h> 
#  include <qgrid.h> 
#else /* QT_VERSION */
#  include <QVBoxLayout>
#  include <QHBoxLayout>
#  include <QGroupBox>
#endif /* QT_VERSION */
#include <qcheckbox.h> 
#include <qcombobox.h> 
#include <qtextcodec.h> 

#include "TColor.h"
#include "TVirtualPad.h"
#include "TStyle.h"

#include "TCanvas.h"

class TQtArrowStyleSelect : public QComboBox {
protected:
   static const char *fgListOfOptions[];
   static const char *fgListOfVisuals[];
protected:
#if QT_VERSION >= 0x40000    
   void insertStrList(const char*itemList[])
   {
       int i = 0;
       while (const char *item = *itemList++) {
          insertItem(i++,QString(item));
       }
   }
#endif   
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
  
  delete editorPanel->layout();
  QVBoxLayout *vframe = new QVBoxLayout(editorPanel);
   
  QWidget *hbox = new QWidget(editorPanel);
  vframe->addWidget(hbox);
  QHBoxLayout *hboxLayout  = new QHBoxLayout(hbox);
  
  hboxLayout->addWidget(
        new QLabel("Shape:",hbox));
  hboxLayout->addWidget(
        fOptionCombo = new TQtArrowStyleSelect(FALSE, hbox));
  
  vframe->addWidget(
        hbox        = new QWidget(this));
  hboxLayout  = new QHBoxLayout(hbox);
  vframe->setSpacing(10);
  
  hboxLayout->addWidget(
        new QLabel("Angle:",hbox));
  hboxLayout->addWidget(
        fAngleEntry = new TQtFloatSpinBox(60, 0, 180, 2, hbox));
#if QT_VERSION < 0x40000
  QToolTip::add(fAngleEntry,"Set the arrow opening angle in degrees.");
#else  
  fAngleEntry->setToolTip(tr("Set the arrow opening angle in degrees."));
#endif  
  
   vframe->addWidget(
         hbox = new QWidget(this));
   hboxLayout = new QHBoxLayout(hbox);
   vframe->setSpacing(2);

  hboxLayout->addWidget(
        new QLabel("Size:",hbox)); 
  hboxLayout->addWidget(
        fSizeEntry = new TQtFloatSpinBox(0.05, 0.01, 0.3, 2, hbox));
#if QT_VERSION < 0x40000
  QToolTip::add(fSizeEntry,"Set the size of arrow");
#else  
  fSizeEntry->setToolTip(tr("Set the size of arrow"));
#endif  
//   SetActive(kTRUE);
}

//______________________________________________________________________________
TQtArrowEditor::~TQtArrowEditor() {  }

//______________________________________________________________________________
void TQtArrowEditor::ConnectSignals2Slots()
{
   // Connect signals to slots.

#if QT_VERSION < 0x40000
   ConnectView(fAngleEntry,  SIGNAL(valueChanged(int)),this, SLOT(DoAngle(int))   );
   ConnectView(fSizeEntry,   SIGNAL(valueChanged(int)),this, SLOT(DoSize(int))    );
#else
   ConnectView(fAngleEntry,  SIGNAL(valueChanged(double)),this, SLOT(DoAngle(double)));
   ConnectView(fSizeEntry,   SIGNAL(valueChanged(double)),this, SLOT(DoSize(double) ));
#endif      
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
void TQtArrowEditor::DoAngle(int v)
{
   // Slot connected to the arrow opening angle setting.

   DoAngle(double (v));
}
//______________________________________________________________________________
void TQtArrowEditor::DoAngle(double )
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
void TQtArrowEditor::DoSize(int v)
{
   // Slot connected to the arrow size.
   DoSize(double(v));
}

//______________________________________________________________________________
void TQtArrowEditor::DoSize(double )
{
   // Slot connected to the arrow size.
   fModel->SetArrowSize(fSizeEntry->Value());
}

//______________________________________________________________________________
//
// Register the GED factory interfaces:
//______________________________________________________________________________
static TQtGedFactory<TQtArrowEditor>   gQtArrowEditor;
