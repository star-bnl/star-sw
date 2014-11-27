// @(#)root/ged:$Name:  $:$Id: TQtMarkerEditor.cxx,v 1.4 2013/08/30 16:00:10 perev Exp $
// Author: Valeri Fine  11/07/06

/****************************************************************************
** Copyright (C) 2006 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TAttMarkerEditor, a ROOT GUI toolkit.         *
 * Author: Ilka  Antcheva 11/05/04                                       *
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtMarkerEditor                                                    //
//                                                                      //
//  Implements GUI for editing marker attributes.                       //
//            color, style and size                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//Begin_Html
/*
<img src="gif/TQtMarkerEditor.gif">
*/
//End_Html


#include "TQtMarkerEditor.h"
#include "TQtColorSelect.h"
#include "TQtColorSelectButton.h"
#include "TQtMarkerSelect.h"
#include "TQtFloatSpinBox.h"
#include "TColor.h"
#include "TVirtualPad.h"
#include "snprintf.h"

#include <qhbox.h> 
#include <qvbox.h> 
#include <qmainwindow.h>
#include <qtooltip.h>

ClassImp(TQtMarkerEditor)

//______________________________________________________________________________
TQtMarkerEditor::TQtMarkerEditor(QMainWindow *p, TCanvas *canvas,  Int_t id, Int_t width,
                           Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TAttMarker>(p, "Marker", canvas, id, width, height, options, back)
{
   
}

//______________________________________________________________________________
TQtMarkerEditor::TQtMarkerEditor( TCanvas *canvas,QWidget  *p, Int_t id, Int_t width,
                         Int_t height, UInt_t options, Pixel_t back)
   : TQtGedAttFrame<TAttMarker>("Marker", p, canvas, id, width, height, options, back)
{
   
}

//______________________________________________________________________________
void TQtMarkerEditor::BuildView(QWidget  *editorPanel)
{

   // Constructor of marker attributes GUI.

   // MakeTitle("Marker");
   QHBox *hbox = new QHBox(editorPanel);
   hbox->setSpacing(3);
   fColorSelect = new TQtColorSelect(hbox, 0);
   fStyleSelect = new TQtMarkerSelect(hbox);
   fMarkerSize  = new TQtFloatSpinBox(0.2, 0.2, 5.0, 1, hbox);
   QToolTip::add(fMarkerSize ,"Set marker size");
}

//______________________________________________________________________________
TQtMarkerEditor::~TQtMarkerEditor()
{
   // Destructor of marker editor.
}

//______________________________________________________________________________
void TQtMarkerEditor::ConnectSignals2Slots()
{
   // Connect signals to slots.

   ConnectView(fColorSelect, SIGNAL(ColorSelected(Pixel_t)),this, SLOT(DoMarkerColor(Pixel_t)) );
   ConnectView(fStyleSelect, SIGNAL(StyleSelected(Style_t)),this, SLOT(DoMarkerStyle(Style_t)) );
   ConnectView(fMarkerSize,  SIGNAL(valueChanged(int)),     this, SLOT(DoMarkerSize(int) )     );

//   fMarkerType->Connect("MarkerSelected(Style_t)", "TQtMarkerEditor", this, "DoMarkerStyle(Style_t)");
//   fMarkerSize->Connect("ValueSet(Long_t)", "TQtMarkerEditor", this, "DoMarkerSize()");
//   (fMarkerSize->GetNumberEntry())->Connect("ReturnPressed()", "TQtMarkerEditor", this, "DoMarkerSize()");
}

//______________________________________________________________________________
void TQtMarkerEditor::ChangeView()
{
   // Pick up the values of used marker attributes.

   //  View                    Model

   Style_t marker = fModel->GetMarkerStyle();
   if (marker==1 || marker==6 || marker==7) {
      fMarkerSize->SetValue (1.);
      fMarkerSize->setEnabled(false);
   } else {
      Float_t s = fModel->GetMarkerSize();
      fMarkerSize->setEnabled(true);
      fMarkerSize->SetValue(s);
   }
   fColorSelect->SetColor(fModel->GetMarkerColor());
   fStyleSelect->SetStyle(marker);

}


//______________________________________________________________________________
void TQtMarkerEditor::DoMarkerColor(Pixel_t color)
{
   // Slot connected to the marker color.

   fModel->SetMarkerColor(TColor::GetColor(color));
}

//______________________________________________________________________________
void TQtMarkerEditor::DoMarkerStyle(Style_t marker)
{
   // Slot connected to the marker type.

   if (marker==1 || marker==6 || marker==7) {
      fMarkerSize->SetValue(1.);
      fMarkerSize->setEnabled(false);
   } else {
      fMarkerSize->setEnabled(true);
   }

   fModel->SetMarkerStyle(marker);
}

//______________________________________________________________________________
void TQtMarkerEditor::DoMarkerSize(int)
{
   // Slot connected to the marker size.

   fModel->SetMarkerSize((float)fMarkerSize->Value());
}

// Register the GED factory interfaces:
static TQtGedFactory<TQtMarkerEditor>   gQtMarkerEditor;
