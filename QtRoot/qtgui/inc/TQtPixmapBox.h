// @(#)root/graf:$Name:  $:$Id: TQtPixmapBox.h,v 1.4 2013/08/30 16:00:22 perev Exp $
// Author: Valeri Fine   02/09/06

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

#ifndef ROOT_TQtPixmapBox
#define ROOT_TQtPixmapBox


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtPixmapBox                                                         //
//                                                                      //
// TBox subclass that uses the QPixmap object to paint its interior.    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TBox
#include "TBox.h"
#endif

#include "GuiTypes.h"

class QPixmap;

class TQtPixmapBox : public TBox {

private:
   QPixmap  *fPixmap;         //!The QPixmap object to paint the box interior
   
protected:
   void DeletePixmap();     // remove the internal pixmap

public:
   TQtPixmapBox();
   TQtPixmapBox(Double_t x1, Double_t y1,Double_t x2, Double_t  y2);
   TQtPixmapBox(const QPixmap &src);
   TQtPixmapBox(Pixmap_t src);
   TQtPixmapBox(Int_t src);
   TQtPixmapBox(const QPixmap &src, Double_t x1, Double_t y1,Double_t x2, Double_t  y2);
   TQtPixmapBox(Pixmap_t src, Double_t x1, Double_t y1,Double_t x2, Double_t  y2);
   TQtPixmapBox(Int_t src, Double_t x1, Double_t y1,Double_t x2, Double_t  y2);
   TQtPixmapBox(const TQtPixmapBox &box);
   virtual ~TQtPixmapBox();
           void  Copy(TObject &box) const;
   virtual void  Draw(Option_t *option="");
#if ROOT_VERSION_CODE < 331523 
   virtual void  DrawBox(Double_t x1, Double_t y1, Double_t x2, Double_t  y2);
#else
   virtual TBox *DrawBox(Double_t x1, Double_t y1, Double_t x2, Double_t  y2);
#endif      
   virtual void  ls(Option_t *option="") const;
   virtual void  Paint(Option_t *option="");
   virtual void  PaintBox(Double_t x1, Double_t y1, Double_t x2, Double_t y2, Option_t *option="");
   virtual const QPixmap &Pixmap() const;
   virtual       QPixmap &Pixmap();
   virtual void  Print(Option_t *option="") const;
#ifndef __CINT__
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,11,0)
   virtual void  SavePrimitive(std::ostream &out, Option_t *option = "");
#else
   virtual void  SavePrimitive(std::ofstream &out, Option_t *option);
#endif
#endif
   virtual void  SetPixmap(const QPixmap &src);
   virtual void  SetPixmap(Pixmap_t src);
   virtual void  SetPixmap(Int_t src);
   virtual void  SetToolTipText(const char *text, Long_t delayms = 1000);

   ClassDef(TQtPixmapBox,0)  //TQtPixmapBox class
};

#endif

