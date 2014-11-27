// @(#)root/gui:$Name:  $:$Id: TQtPatternSelect.cxx,v 1.6 2013/08/30 16:00:24 perev Exp $
// Author: Valeri Fine  21/05/2004
/****************************************************************************
** $Id: TQtPatternSelect.cxx,v 1.6 2013/08/30 16:00:24 perev Exp $
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
// The TQtPatternSelectButton widget is like a checkbutton but instead of the    //
// check mark there is color area with a little down arrow. When        //
// clicked on the arrow the TQtPatternPopup pops up.                       //
//                                                                      //
// Selecting a color in this widget will generate the event:            //
// kC_COLORSEL, kCOL_SELCHANGED, widget id, pixel.                      //
// and the signal:                                                      //
// ColorSelected(ULong_t pixel)                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtPatternSelect.h"
#include "TGQt.h"
#include "TQtPatternSelectButton.h"
#include "TQtEvent.h"
#include "TQtApplication.h"
#include <QApplication>
//Added by qt3to4:
#include <QEvent>

ClassImp(TQtPatternSelect)
//______________________________________________________________________________
TQtPatternSelect::TQtPatternSelect(QWidget *p,  Style_t pattern, Int_t id )
               : QObject(), fPatternSelector(0) 
{
   ConstructorThread(p, pattern, id);
}
//______________________________________________________________________________
void   TQtPatternSelect::ConstructorThread(QWidget *p,  Style_t pattern, Int_t id)
{
   if (!TQtApplication::IsThisGuiThread()) 
   { //---- Define a message ---
      class ev : public TQtEvent {
      public:
         QWidget *fP;  UInt_t fPattern; Int_t fId; TQtPatternSelect *fObj;
         ev(TQtPatternSelect *obj, QWidget *p,   UInt_t pattern,  Int_t id)
            :  TQtEvent(0), fP(p), fPattern(pattern), fId(id),fObj(obj) {;}
            void ExecuteCB()  {         
               fObj->Constructor(fP,fPattern,fId);
            }
      };
      //---- Send the message ---
      ev *e = new ev(this,p,pattern,id);
      TGQt::PostQtEvent(this,e);
   } else {
      //----- Execute the method directly 
      Constructor(p,pattern,id);
   }  
}

//______________________________________________________________________________
void TQtPatternSelect::Constructor(QWidget *p, Style_t pattern, Int_t id)
{
   fPatternSelector = new TQtPatternSelectButton(p, pattern, id, this);//
//   fPatternSelector->show();
}

//______________________________________________________________________________
TQtPatternSelect::~TQtPatternSelect() {
   if (fPatternSelector) {
      fPatternSelector->hide();
      delete fPatternSelector;
      fPatternSelector = 0;
   }
}
//______________________________________________________________________________
void TQtPatternSelect::EmitRootSignal(ULong_t style)
{ //*SIGNAL*
    emit PatternSelected(style);
    Emit("PatternSelected(Style_t)", style); 
} 

//______________________________________________________________________________
Style_t TQtPatternSelect::GetPattern() const      
{ return  (fPatternSelector) ? fPatternSelector->GetStyle() : 0; }
//______________________________________________________________________________
void    TQtPatternSelect::SetPattern(Style_t style)
{if (fPatternSelector) fPatternSelector->SetStyle(style); }

//______________________________________________________________________________
void TQtPatternSelect::SavePrimitive(std::ofstream & out , Option_t *opt)
{
   // To make code forward backward compatible with the different ROOT versions
   SavePrimitive(*(std::ostream *)&out, opt);
}
//______________________________________________________________________________
void TQtPatternSelect::SavePrimitive(std::ostream & /*out*/ , Option_t *)
{
    // Save a color select widget as a C++ statement(s) on output stream out

//   char quote = '"';
//   //   ULong_t color = GetColor();
//   const char *colorname = fColorSelector->GetColor().name();
//   //const char *colorname = TColor::PixelAsHexString(color);
//   // gClient->GetColorByName(colorname, color);
//
//   out << endl << "   // color select widget" << endl;
//   out << "   ULong_t ColPar;" << endl;
//   out << "   gClient->GetColorByName(" << quote << colorname << quote
//       << ", ColPar);" << endl;
//
//   out <<"   TQtPatternSelect*";
////   out << GetName() << " = new TQtPatternSelect(" << fParent->GetName()
////       << ", ColPar, " << WidgetId() << ");" << endl;
//
//   //if (!IsEnabled()) {
//   //   out << "   " << GetName() << "->Disable();" << endl;
//   // }
//   out << endl;
}

//______________________________________________________________________________
bool TQtPatternSelect::event(QEvent *e)
{
  if (e) {
    if (e->type() < QEvent::User) return FALSE;
    ((TQtEvent *)e)->ExecuteCB();
  }
  return true;
}

