// @(#)root/qt:$Name:  $:$Id: TQtBrush.h,v 1.5 2013/08/30 15:59:49 perev Exp $
// Author: Valeri Fine   21/01/2002
/****************************************************************************
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtBrush
#define ROOT_TQtBrush

#ifndef __CINT__
#  include <QBrush>
#  include <QColor>
#  include <QPixmap>
#else
   class  QColor;
   class  QBrush;
   class  QPixmap;
#endif

#include "Rtypes.h"
#include "Gtypes.h"

class TAttFill;
class TPoint;

   //
   // TQtBrush creates the QBrush Qt object based on the ROOT "fill" attributes 
   //
class TQtBrush : public QBrush
{
protected:
   QColor fBackground;
   int fStyle;
   int fFasi;
   int fAlpha; // transparency
   void SetColorOwn();

public:
   TQtBrush();
   TQtBrush(const TQtBrush &src):QBrush(src)
      ,fBackground(src.fBackground)
      ,fStyle(src.fStyle)
      ,fFasi(src.fFasi)
      ,fAlpha(src.fFasi)
   { }
   TQtBrush(const TAttFill &rootFillAttributes);
   virtual ~TQtBrush();
   TQtBrush &operator=(const TAttFill &rootFillAttributes);
   void  SetFillAttributes(const TAttFill &rootFillAttributes);
   Bool_t IsTransparent() const;
   void SetStyle(int newStyle=1000){ if (newStyle < 0) fStyle = fFasi = -1;
                                     else  SetStyle(newStyle/1000,newStyle%1000); 
                                   };
   void SetStyle(int style, int fasi);
   void SetColor(const QColor &qtcolor);
   void SetColor(Color_t cindex);
   const QColor &GetColor() const { return fBackground;}
   int   GetStyle()         const { return 1000*fStyle + fFasi; }
   ClassDef(TQtBrush,0); // create QBrush object based on the ROOT "fill" attributes 
};

inline Bool_t TQtBrush::IsTransparent() const
{ return fStyle >= 4000 && fStyle <= 4100 ? kTRUE : kFALSE; }

#endif
