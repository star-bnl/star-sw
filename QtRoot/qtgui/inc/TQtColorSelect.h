// @(#)root/gui:$Name:  $:$Id: TQtColorSelect.h,v 1.6 2013/08/30 16:00:20 perev Exp $
// Author: Valeri Fine  21/05/2004

#ifndef ROOT_TQtColorSelect
#define ROOT_TQtColorSelect

/****************************************************************************
** $Id: TQtColorSelect.h,v 1.6 2013/08/30 16:00:20 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// The TQtColorSelect widget is like a checkbutton but instead of the    //
// check mark there is color area with a little down arrow. When        //
// clicked on the arrow the TQtColorPopup pops up.                       //
//                                                                      //
// Selecting a color in this widget will generate the event:            //
// kC_COLORSEL, kCOL_SELCHANGED, widget id, pixel.                      //
// and the signal:                                                      //
// ColorSelected(Pixel_t pixel)                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "GuiTypes.h"
#include "Gtypes.h"
#include "Riostream.h"
#include "TClass.h"
#ifndef ROOT_TQObject
#  include "TQObject.h"
#endif


#ifndef __CINT__
#include <QObject>
#endif

class QWidget;
class TQtColorSelectButton;
class QEvent;

//----------------------------------------------------------------------
class TColorEmit {
public:
   TColorEmit(){}
   virtual ~TColorEmit(){}
   virtual  void ColorEmit(Pixel_t ) { }
};

//----------------------------------------------------------------------

class TQtColorSelect : 
#ifndef __CINT__
   public QObject, 
#endif
   public TObject, public TQObject, public TColorEmit {
#ifndef __CINT__
Q_OBJECT
#endif
private:
   TQtColorSelectButton *fColorSelector;
   TQtColorSelect(const TQtColorSelect&);
   TQtColorSelect &operator=(const TQtColorSelect&);

public:
   TQtColorSelect(QWidget *p=0, UInt_t pixel=0, Int_t id=-1);
   virtual ~TQtColorSelect();
   virtual void ColorEmit(Pixel_t pixel);  // *SIGNAL*
   virtual void Constructor(QWidget *p=0, UInt_t pixel=0, Int_t id=-1);
   Pixel_t GetColor() const;
   void    SetColor(Color_t color);
   TQtColorSelectButton *GetColorSelectButton() const { return fColorSelector;}
   virtual void SavePrimitive(std::ofstream &out, Option_t *);
   virtual void SavePrimitive(std::ostream &out, Option_t *);

#ifndef __CINT__
signals:
  void  ColorSelected(Pixel_t);
#endif      
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtColorSelect,0)  // Color selection checkbutton
//MOC_SKIP_END
#endif
};

#endif
