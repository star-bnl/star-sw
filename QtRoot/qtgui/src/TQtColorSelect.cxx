// @(#)root/gui:$Name:  $:$Id: TQtColorSelect.cxx,v 1.6 2013/08/30 16:00:23 perev Exp $
// Author: Valeri Fine  21/05/2004
/****************************************************************************
** $Id: TQtColorSelect.cxx,v 1.6 2013/08/30 16:00:23 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
// The TQtColorSelectButton widget is like a checkbutton but instead of the    //
// check mark there is color area with a little down arrow. When        //
// clicked on the arrow the TQtColorPopup pops up.                       //
//                                                                      //
// Selecting a color in this widget will generate the event:            //
// kC_COLORSEL, kCOL_SELCHANGED, widget id, pixel.                      //
// and the signal:                                                      //
// ColorSelected(ULong_t pixel)                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtColorSelect.h"
#include "TGQt.h"
#include "TQtColorSelectButton.h"
#include "TQtEvent.h"
#include "TQtApplication.h"
#include <QApplication>

ClassImp(TQtColorSelect)
//______________________________________________________________________________
TQtColorSelect::TQtColorSelect(QWidget *p,  UInt_t pixel, Int_t id )
               : QObject(), fColorSelector(0) 
{
  Constructor(p, pixel, id);
}

//______________________________________________________________________________
void TQtColorSelect::Constructor(QWidget *p, UInt_t pixel, Int_t id)
{
   fColorSelector = new TQtColorSelectButton(p, pixel, id, this);
   fColorSelector->show();
}

//______________________________________________________________________________
TQtColorSelect::~TQtColorSelect() {
   if (fColorSelector) {
      fColorSelector->hide();
      delete fColorSelector;
      fColorSelector = 0;
   }
}

//______________________________________________________________________________
void TQtColorSelect::ColorEmit(Pixel_t pixel) 
 { 
   emit ColorSelected(pixel);
   Emit("ColorSelected(Pixel_t)", pixel); 
 } //*SIGNAL*
//______________________________________________________________________________
Pixel_t TQtColorSelect::GetColor() const      
{ return  (fColorSelector) ? fColorSelector->GetColor().pixel() : 0; }
//______________________________________________________________________________
void    TQtColorSelect::SetColor(Color_t color)
{if (fColorSelector) fColorSelector->SetColor(gQt->ColorIndex(gQt->UpdateColor(color))); }
//______________________________________________________________________________
void TQtColorSelect::SavePrimitive(std::ofstream & out, Option_t *opt)
{
   // To make code forward backward compatible with the different ROOT versions
   SavePrimitive(*(std::ostream *)&out,opt);
}
//______________________________________________________________________________
void TQtColorSelect::SavePrimitive(std::ostream & out, Option_t *)
{
    // Save a color select widget as a C++ statement(s) on output stream out

   char quote = '"';
   //   ULong_t color = GetColor();
   const char *colorname = fColorSelector->GetColor().name();
   //const char *colorname = TColor::PixelAsHexString(color);
   // gClient->GetColorByName(colorname, color);

   out << std::endl << "   // color select widget" << std::endl;
   out << "   ULong_t ColPar;" << std::endl;
   out << "   gClient->GetColorByName(" << quote << colorname << quote
       << ", ColPar);" << std::endl;

   out <<"   TQtColorSelect*";
//   out << GetName() << " = new TQtColorSelect(" << fParent->GetName()
//       << ", ColPar, " << WidgetId() << ");" << std::endl;

   //if (!IsEnabled()) {
   //   out << "   " << GetName() << "->Disable();" << std::endl;
   // }
   out << std::endl;
}
