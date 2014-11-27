// @(#)root/ged:$Name:  $:$Id: TQtLineEditor.h,v 1.3 2013/08/30 16:00:10 perev Exp $
// Author: Valeri Fine 13/06/06

/****************************************************************************
** Copyright (C) 2006 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TLineEditor, a ROOT GUI toolkit.              *
 * Author: Ilka  Antcheva 24/04/06
 * Copyright (C) 1995-2006, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtLineEditor
#define ROOT_TQtLineEditor

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtLineEditor                                                       //
//                                                                      //
//  Implements GUI for editing line attributes, start/end points.       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtGedAttFrame.h"
#include "TLine.h"

class TQtFloatSpinBox;
class QCheckBox;
class QButtonGroup;

class TQtLineEditor : public TQtGedAttFrame<TLine> {
#ifndef __CINT__
   Q_OBJECT
#endif

protected:
   TQtFloatSpinBox  *fStartPointX;  //start point x coordinate
   TQtFloatSpinBox  *fStartPointY;  //start point y coordinate
   TQtFloatSpinBox  *fEndPointX;    //end point x coordinate
   TQtFloatSpinBox  *fEndPointY;    //end point y coordinate
   QButtonGroup     *fOrientation;  //set the line orientation
   QCheckBox        *fVertical;     //set the line vertical
   QCheckBox        *fHorizontal;   //set the line horizontal
   virtual void BuildView(QWidget  *editorPanel);
   virtual void   ConnectSignals2Slots();
   virtual void   ChangeView();

public:
   TQtLineEditor(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, //kChildFrame,
               Pixel_t back = 0);   //GetDefaultFrameBackground());
   TQtLineEditor(TCanvas *canvas, QWidget *parent=0,  Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, //kChildFrame,
               Pixel_t back = 0);   //GetDefaultFrameBackground());
   virtual ~TQtLineEditor();

public slots:
   virtual void   DoStartPointX(double);
   virtual void   DoEndPointX(double);
   virtual void   DoStartPointY(double);
   virtual void   DoEndPointY(double);
   virtual void   DoLineOrientation(int);

#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtLineEditor,0)  // GUI for editing Line attributes
//MOC_SKIP_END
#endif  
};

#endif
