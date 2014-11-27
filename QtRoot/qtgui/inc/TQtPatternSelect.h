// @(#)root/gui:$Name:  $:$Id: TQtPatternSelect.h,v 1.7 2013/08/30 16:00:22 perev Exp $
// Author: Valeri Fine  21/05/2004

#ifndef ROOT_TQtPatternSelect
#define ROOT_TQtPatternSelect

/****************************************************************************
** $Id: TQtPatternSelect.h,v 1.7 2013/08/30 16:00:22 perev Exp $
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
// The TQtPatternSelect widget is like a checkbutton but instead of the //
// check mark there is color area with a little down arrow. When        //
// clicked on the arrow the TQtPatternPopup pops up.                    //
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
#include "TObject.h"
#ifndef ROOT_TQObject
#include "TQObject.h"
#endif

#ifndef __CINT__
#  include <QObject>
#  include <QEvent>
#endif

class QWidget;
class TQtPatternSelectButton;
class QEvent;

//----------------------------------------------------------------------
class TEmitRootSignal {
public:
   TEmitRootSignal(){}
   virtual ~TEmitRootSignal(){}
   virtual  void EmitRootSignal(ULong_t ) { }
};

//----------------------------------------------------------------------

class TQtPatternSelect : 
#ifndef __CINT__
   public QObject, 
#endif
   public TObject, public TQObject, public TEmitRootSignal {
#ifndef __CINT__
Q_OBJECT
#endif
private:
   TQtPatternSelectButton *fPatternSelector;
   TQtPatternSelect(const TQtPatternSelect&);
   TQtPatternSelect &operator=(const TQtPatternSelect&);
protected:
   friend class ev;
   virtual void ConstructorThread(QWidget *p=0, Style_t pixel=0, Int_t id=-1);

public:
   TQtPatternSelect(QWidget *p=0, Style_t pattern=0, Int_t id=-1);
   virtual ~TQtPatternSelect();
   virtual void EmitRootSignal(ULong_t style); // *SIGNAL*
   TQtPatternSelectButton *GetPatternSelectButton() const { return fPatternSelector;}
   virtual void Constructor(QWidget *p=0, Style_t pattern=0, Int_t id=-1);
   void         SetPattern(Style_t pattern);
   Style_t      GetPattern() const;
   
   virtual void SavePrimitive(std::ofstream &out, Option_t *);
   virtual void SavePrimitive(std::ostream &out, Option_t *);

protected:
   bool event(QEvent *e);
signals:
   void PatternSelected(Style_t);
      // 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtPatternSelect,0)  // Color selection checkbutton
//MOC_SKIP_END
#endif
};

#endif
