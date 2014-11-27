// @(#)root/ged:$Name:  $:$Id: TQtMarkerEditor.h,v 1.5 2013/08/30 16:00:10 perev Exp $
// Author: Valeri Fine 11/06/06

/****************************************************************************
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on ROOT_TMarkerEditor, a ROOT GUI toolkit.       *
 * Author: Ilka  Antcheva 11/05/04                                       *
 *                                                                       *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtMarkerEditor
#define ROOT_TQtMarkerEditor

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtMarkerEditor                                                    //
//                                                                      //
//  Implements GUI for editing marker attributes.                       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtGedAttFrame.h"
#include "TAttMarker.h"

class TQtFloatSpinBox;
class TQtColorSelect;
class TQtMarkerSelect;
// class QedMarkerSelect;
class QFontTypeComboBox;
// class TQtMarker;

#ifdef __CINT__
#define slots
#endif

class TQtMarkerEditor : public TQtGedAttFrame<TAttMarker> {
#ifndef __CINT__
   Q_OBJECT
#endif

protected:
   TQtFloatSpinBox     *fMarkerSize;      // marker size combo box
   TQtColorSelect      *fColorSelect;     // marker color
   TQtMarkerSelect     *fStyleSelect;     // marker style

   virtual void  BuildView(QWidget  *editorPanel);
   virtual void  ConnectSignals2Slots();
   virtual void  ChangeView();

public:
   TQtMarkerEditor(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, //kChildFrame,
               Pixel_t back = 0);   //GetDefaultFrameBackground());
   TQtMarkerEditor(TCanvas *canvas, QWidget *parent=0,  Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, //kChildFrame,
               Pixel_t back = 0);   //GetDefaultFrameBackground());
   virtual ~TQtMarkerEditor();

public slots:
   virtual void     DoMarkerColor(Pixel_t color);
   virtual void     DoMarkerSize(int);
   virtual void     DoMarkerStyle(Style_t style);

#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtMarkerEditor,0)  // GUI for editing marker attributes
//MOC_SKIP_END
#endif  
};

#endif
