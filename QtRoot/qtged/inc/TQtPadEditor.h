// @(#)root/ged:$Name:  TQtPadEditor.h
// Author: Valeri Fine 24/06/04

/****************************************************************************
** $Id: TQtPadEditor.h,v 1.3 2013/08/30 16:00:10 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TPadEditor, a ROOT GUI toolkit.               *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 * Author: Ilka Antcheva                                                 *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtPadEditor
#define ROOT_TQtPadEditor

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtPadEditor                                                        //
//                                                                      //
//  Editor of pad/canvas objects.                                       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TQtGedAttFrame
#include "TQtGedAttFrame.h"
#endif
#include "TPad.h"

class QCheckBox;
class QRadioButton;
class TQtLineWidthComboBox;


class TQtPadEditor : public TQtGedAttFrame<TPad>{
#ifndef __CINT__
   Q_OBJECT
#endif
protected:

   QCheckBox       *fEditable;         // set pad editable
   QCheckBox       *fCrosshair;        // set crosshair   
   QCheckBox       *fFixedAR;          // set fixed aspect ratio
   QCheckBox       *fGridX;            // set grid on X
   QCheckBox       *fGridY;            // set grid on Y
   QCheckBox       *fLogX;             // set log scale on X
   QCheckBox       *fLogY;             // set log scale on Y
   QCheckBox       *fLogZ;             // set log scale on Z
   QCheckBox       *fTickX;            // set ticks on X
   QCheckBox       *fTickY;            // set ticks on Y
   QRadioButton         *fBmode;         // set sinken pad border mode
   QRadioButton         *fBmode0;        // set no pad border
   QRadioButton         *fBmode1;        // set raised pad border mode
   TQtLineWidthComboBox *fBsize;         // set pad border size
//   QCompositeFrame    *f7;             // container frame;  

           virtual void BuildView(QWidget  *editorPanel);
   virtual void ConnectSignals2Slots();

public:
   TQtPadEditor(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
              Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
              UInt_t options = 0, // kChildFrame,
              Pixel_t back = 0);
   TQtPadEditor(TCanvas *canvas, QWidget *parent=0, Int_t id=0,
              Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
              UInt_t options = 0, // kChildFrame,
              Pixel_t back = 0);

   virtual ~TQtPadEditor();

   virtual void   ChangeView();
public slots:
   virtual void   DoEditable(bool on);
   virtual void   DoCrosshair(bool on);
   virtual void   DoFixedAspectRatio(bool on);
   virtual void   DoGridX(bool on);
   virtual void   DoGridY(bool on);
   virtual void   DoLogX(bool on);
   virtual void   DoLogY(bool on);
   virtual void   DoLogZ(bool on);
   virtual void   DoTickX(bool on);
   virtual void   DoTickY(bool on);
   virtual void   DoBorderMode();
   virtual void   DoBorderSize(Int_t size);
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtPadEditor,0)  //editor of TPad objects
//MOC_SKIP_END
#endif
};

#endif
