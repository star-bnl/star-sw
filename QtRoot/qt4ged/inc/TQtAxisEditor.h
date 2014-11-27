// @(#)root/ged:$Name:  $:$Id: TQtAxisEditor.h,v 1.3 2013/08/30 15:59:52 perev Exp $
// Author: Valeri Fine 10/07/2004

/****************************************************************************
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TAxisEditor, a ROOT GUI toolkit.               *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 * Author: Ilka Antcheva                                                 *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtAxisEditor
#define ROOT_TQtAxisEditor

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtAxisEditor                                                         //
//                                                                      //
//  Implements GUI for axis attributes.                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TQtGedAttFrame
#include "TQtGedAttFrame.h"
#endif
#include "TAxis.h"

class QCheckBox;
class QRadioButton;
class QLineEdit;
class QSpinBox;

class TQtColorSelect;
class TQtFloatSpinBox;
class TQtFontComboBox;


class TQtAxisEditor : public TQtGedAttFrame<TAxis>{
#ifndef __CINT__
   Q_OBJECT
#endif
protected:
   TQtColorSelect     *fAxisColor;    // color selection widget
   QCheckBox          *fLogAxis;      // logarithmic check box    
   TQtFloatSpinBox    *fTickLength;   // tick length number entry
   QSpinBox           *fDiv1;         // primary axis division number entry
   QSpinBox           *fDiv2;         // secondary axis division number entry
   QSpinBox           *fDiv3;         // tertiary axis division number entry
   QCheckBox          *fOptimize;     // tick optimization check box
   QCheckBox          *fTicksBoth;    // check box setting ticks on both axis sides
   QCheckBox          *fMoreLog;      // more logarithmic labels check box
   Int_t              fTicksFlag;     // positive/negative ticks' flag
   QLineEdit          *fTitle;        // axis title input field
   TQtColorSelect     *fTitleColor;   // color selection widget
   TQtFontComboBox    *fTitleFont;    // title font combo box
   Int_t               fTitlePrec;    // font precision level
   TQtFloatSpinBox    *fTitleSize;    // title size number entry
   TQtFloatSpinBox    *fTitleOffset;  // title offset number entry
   QCheckBox          *fCentered;     // check button for centered title
   QCheckBox          *fRotated;      // check button for rotated title
   TQtColorSelect     *fLabelColor;   // color selection widget
   TQtFontComboBox    *fLabelFont;    // label font combo box
   Int_t               fLabelPrec;    // font precision level
   TQtFloatSpinBox    *fLabelSize;    // label size number entry
   TQtFloatSpinBox    *fLabelOffset;  // label offset number entry
   QCheckBox          *fNoExponent;   // check box for No exponent choice
   QCheckBox          *fDecimal;      // decimal part check box    

           virtual void BuildView(QWidget  *editorPanel);
   virtual void ConnectSignals2Slots();
   virtual void ChangeView();

public:
   TQtAxisEditor(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, // kChildFrame,
               Pixel_t back = 0);
   TQtAxisEditor(TCanvas *canvas, QWidget *parent=0, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, // kChildFrame,
               Pixel_t back = 0);
   virtual ~TQtAxisEditor();

public slots:
   // slots related to axis attributes
   virtual void   DoTickLength(int);
   virtual void   DoTickLength(double);
   virtual void   DoAxisColor(Pixel_t color);
   virtual void   DoTicks(bool);
   virtual void   DoDivisions(bool);
   virtual void   DoDivisions(int);
   virtual void   DoDivisions();
   virtual void   DoLogAxis(bool);
   virtual void   DoMoreLog(bool);
   // slots related to axis title attributes
   virtual void   DoTitleColor(Pixel_t color);
   virtual void   DoTitle(const QString &text);
   virtual void   DoTitleSize(int);
   virtual void   DoTitleSize(double);
   virtual void   DoTitleFont(int font);
   virtual void   DoTitleOffset(int);
   virtual void   DoTitleOffset(double);
   virtual void   DoTitleCentered(bool);
   virtual void   DoTitleRotated(bool);
   // slots related to axis labels attributes
   virtual void   DoLabelColor(Pixel_t color);
   virtual void   DoLabelSize(int);
   virtual void   DoLabelSize(double);
   virtual void   DoLabelFont(int font);
   virtual void   DoLabelOffset(int);
   virtual void   DoLabelOffset(double);
   virtual void   DoNoExponent(bool);
   virtual void   DoDecimal(bool);

// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtAxisEditor,0)  // axis editor
//MOC_SKIP_END
#endif
};

#endif
