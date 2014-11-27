// @(#)root/graf:$Name:  $:$Id: TQtPixmapBox.cxx,v 1.5 2013/08/30 16:00:25 perev Exp $
// Author: Valeri Fine   03/09/2006

/****************************************************************************
**
** Copyright (C) 2006 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include <stdlib.h>

#include "Riostream.h"
#include "TROOT.h"
#include "TQtPixmapBox.h"
#include "TVirtualPad.h"
#include "TVirtualX.h"
#include "TClass.h"
#include "TMath.h"
#include "TGQt.h"

#include <QPixmap>
#include <QRect>
#include <QPainter>
#include <QToolTip> 

ClassImp(TQtPixmapBox)

//______________________________________________________________________________
//
// A pixmap box is painted with the QPixmap object:
//
//Begin_Html
/*
<img src="png/PixmapBoxExample.png">
*/
//End_Html
//

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(): TBox(), fPixmap(0)
{
//*-*-*-*-*-*-*-*-*-*-*Box default constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  =======================
//  wow !!! TBox default ctor doesn't assign any value for its datamember
// this job for him
  SetX1(0);   SetX2(1);  SetY1(0);  SetY2(1);
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(const QPixmap &src)
     : TBox(), fPixmap(0)
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
// Create object from the QPixmap
// It can be used to create the object from pixmap file
   SetX1(0);   SetX2(1);  SetY1(0);  SetY2(1);
   SetPixmap(src);
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(Pixmap_t src)
     : TBox(), fPixmap(0) 
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
// Create object from the ROOT Gui pixmap created with TVirtualX::CreatePixmap
   SetX1(0);   SetX2(1);  SetY1(0);  SetY2(1);
   SetPixmap(src);
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(Int_t src)
     : TBox(), fPixmap(0) 
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
// Create object from the ROOT TPad pixmap created with TVirtualX::OpenPixmap
   SetX1(0);   SetX2(1);  SetY1(0);  SetY2(1);
   SetPixmap(src);
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(Double_t x1, Double_t y1, Double_t x2, Double_t y2)
     : TBox(x1,y1,x2,y2), fPixmap(0)
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(const QPixmap &src, Double_t x1, Double_t y1, Double_t x2, Double_t y2)
     : TBox(x1,y1,x2,y2), fPixmap(0)
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
// Create object from the QPixmap
// It can be used to create the object from pixmap file
   SetPixmap(src);
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(Pixmap_t src, Double_t x1, Double_t y1, Double_t x2, Double_t y2)
     : TBox(x1,y1,x2,y2), fPixmap(0) 
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
// Create object from the ROOT Gui pixmap created with TVirtualX::CreatePixmap
   SetPixmap(src);
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(Int_t src, Double_t x1, Double_t y1, Double_t x2, Double_t y2)
     : TBox(x1,y1,x2,y2), fPixmap(0) 
{
//*-*-*-*-*-*-*-*-*-*-*Box standard constructor-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
// Create object from the ROOT TPad pixmap created with TVirtualX::OpenPixmap
   SetPixmap(src);
}

//______________________________________________________________________________
TQtPixmapBox::~TQtPixmapBox()
{
//*-*-*-*-*-*-*-*-*-*-*Box destructor*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ==============
   DeletePixmap();
}

//______________________________________________________________________________
TQtPixmapBox::TQtPixmapBox(const TQtPixmapBox &box) : TBox(box)
{
  if (box.fPixmap) {
     SetPixmap(*box.fPixmap);
  } else  {
      DeletePixmap();
  }
}

//______________________________________________________________________________
void TQtPixmapBox::Copy(TObject &obj) const
{
//*-*-*-*-*-*-*-*-*-*-*Copy a Box*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ==========

   TBox::Copy(((TBox&)obj));
   if (fPixmap)  {
      ((TQtPixmapBox&)obj).SetPixmap(*fPixmap);
   } else {
      ((TQtPixmapBox&)obj).DeletePixmap();
   }
}
//______________________________________________________________________________
void TQtPixmapBox::DeletePixmap()
{
   // remove the internal pixmap
   QPixmap *p = fPixmap; fPixmap = 0;
   delete p;
}

//______________________________________________________________________________
void TQtPixmapBox::Draw(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*Draw this box with its current attributes*-*-*-*-*-*-*
//*-*                  =========================================

   AppendPad(option);

}

//______________________________________________________________________________
#if ROOT_VERSION_CODE < 331523 
void 
#else
TBox *      
#endif      
TQtPixmapBox::DrawBox(Double_t x1, Double_t y1,Double_t x2, Double_t  y2)
{
//*-*-*-*-*-*-*-*-*-*-*Draw this box with new coordinates*-*-*-*-*-*-*-*-*-*
//*-*                  ==================================
   TQtPixmapBox *newbox = new TQtPixmapBox(Pixmap(),x1,y1,x2,y2);
   TBox::Copy(*newbox);
   newbox->SetBit(kCanDelete);
   newbox->AppendPad();
#if ROOT_VERSION_CODE >= 331523 
   return newbox;
#endif   
}

//______________________________________________________________________________
void TQtPixmapBox::ls(Option_t *opt) const
{
//*-*-*-*-*-*-*-*-*-*-*-*List this box with its attributes*-*-*-*-*-*-*-*-*
//*-*                    =================================
   TBox::ls(opt);
}

//______________________________________________________________________________
void TQtPixmapBox::Paint(Option_t *)
{
//*-*-*-*-*-*-*-*-*-*-*Paint this box with its current attributes*-*-*-*-*-*-*
//*-*                  ==========================================
   PaintBox(gPad->XtoPad(fX1),gPad->YtoPad(fY1),gPad->XtoPad(fX2),gPad->YtoPad(fY2));
}

//______________________________________________________________________________
void TQtPixmapBox::PaintBox(Double_t x1, Double_t y1, Double_t x2, Double_t y2, Option_t *)
{
//*-*-*-*-*-*-*-*-*-*-*Draw this box with new coordinates*-*-*-*-*-*-*-*-*-*
//*-*                  ==================================
 
   if (fPixmap) {
      TAttLine::Modify();  //Change line attributes only if necessary
      TAttFill::Modify();  //Change fill area attributes only if necessary
      Int_t px1 = gPad->XtoPixel(x1);
      Int_t px2 = gPad->XtoPixel(x2);
      Int_t py1 = gPad->YtoPixel(y1);
      Int_t py2 = gPad->YtoPixel(y2);
      //box width must be at least one pixel
      if (TMath::Abs(px2-px1) < 1) px2 = px1+1;
      if (TMath::Abs(py1-py2) < 1) py1 = py2+1;

      QRect rect(0,0, TMath::Abs(px2-px1) , TMath::Abs(py1-py2) );
      if (Pixmap().size() != rect.size()) {
        //   resize
        QPixmap p(rect.size());
        QPainter pnt(&p);
        pnt.drawPixmap(rect,Pixmap());
        //   emit AboutToPaint(p,Pixmap().size()); 
        //   Give them the chance to change the fPixmap size and its appearance           
        SetPixmap(p);
      } 
      gQt->CopyPixmap(Pixmap(), px1, py2);
      gPad->Modified();gPad->Update();
   }
}
//______________________________________________________________________________
const QPixmap &TQtPixmapBox::Pixmap() const
{
  return ((TQtPixmapBox*)this)->Pixmap();
}  
//______________________________________________________________________________
QPixmap &TQtPixmapBox::Pixmap()
{
  // QPixmap *p = dynamic_cast<QPixmap *>(TGQt::iwid(fPixmap));
  return *fPixmap;
}

//______________________________________________________________________________
void TQtPixmapBox::Print(Option_t *opt) const
{
//*-*-*-*-*-*-*-*-*-*-*Dump this box with its attributes*-*-*-*-*-*-*-*-*-*
//*-*                  =================================
   TBox::Print(opt);
}

//______________________________________________________________________________
void
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,11,0)
    TQtPixmapBox::SavePrimitive(std::ostream &out, Option_t *)
#else
    TQtPixmapBox::SavePrimitive(std::ofstream &out, Option_t *)
#endif
{
    // Save primitive as a C++ statement(s) on output stream out

   if (gROOT->ClassSaved(TQtPixmapBox::Class())) {
       out<<"   ";
   } else {
       out<<"   TQtPixmapBox *";
   }
   out<<"pixmapbox = new TQtPixmapBox("<<fX1<<","<<fY1<<","<<fX2<<","<<fY2<<");"<<std::endl;

   SaveFillAttributes(out,"pixmapbox",0,1001);
   SaveLineAttributes(out,"pixmapbox",1,1,1);

   out<<"   pixmapbox->Draw();"<<std::endl;
}
//______________________________________________________________________________
void  TQtPixmapBox::SetPixmap(const QPixmap &src)
{
   // Provide the QPixmap to draw the TBox interior
   if (!src.isNull()) {
      if (!fPixmap) 
         fPixmap = new QPixmap();
      QPixmap &pix =  Pixmap();   
      pix = src;
   }
}
//______________________________________________________________________________
void  TQtPixmapBox::SetPixmap(Pixmap_t src)
{
  // Provide the QPixmap to draw the TBox interior
  QPixmap *p = dynamic_cast<QPixmap *>(TGQt::iwid(src));
  if (p) SetPixmap(*p);
}
//______________________________________________________________________________
void  TQtPixmapBox::SetPixmap(Int_t src)
{
  // Provide the QPixmap to draw the TBox interior
  QPixmap *p = dynamic_cast<QPixmap *>(TGQt::wid(src));
  if (p) SetPixmap(*p);
}

//______________________________________________________________________________
void TQtPixmapBox::SetToolTipText(const char *text, Long_t /* delayms */)
{
   // Set tool tip text associated with this box. The delay is in
   // milliseconds (minimum 250). To remove tool tip call method with
   // text = 0.

   if (!gPad) {
      Warning("SetToolTipText", "a canvas must exist before setting the tool tip text");
      return;
   }
   if (text && strlen(text)) {
//      QToolTip::add ( QWidget * widget, const QRect & rect,text);
  }  else {
//      QToolTip::remove ( QWidget * widget, const QRect & rect) ;
  }

}

