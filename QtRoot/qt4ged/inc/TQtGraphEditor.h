// @(#)root/ged:$Name:  $:$Id: TQtGraphEditor.h,v 1.3 2013/08/30 15:59:53 perev Exp $
// Author: Valeri Fine 16/07/06

/*************************************************************************
 * This source is based on TGraphEditor , a ROOT GUI toolkit.            *
 * Author: Carsten Hof 28/07/04                                          *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtGraphEditor
#define ROOT_TQtGraphEditor

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtGraphEditor                                                      //
//                                                                      //
//  Editor for changing Graph attributes.                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtGedAttFrame.h"
#include "TGraph.h"

class QLabel;
class QLineEdit;
class QCheckBox;
class QRadioButton;
class TQtLineWidthComboBox;
class QButtonGroup;

class TQtGraphEditor : public TQtGedAttFrame<TGraph> {
#ifndef __CINT__
   Q_OBJECT
#endif

protected:
   char               fDrawShape;   // Shape of the Graph (simple, smooth, bar)
   QLineEdit         *fTitle;       // Contains the title of the graph
   Int_t               fTitlePrec;   // font precision level
   QButtonGroup      *fgr;          // Group the Radiobuttons:
   QCheckBox          *fMarkerOnOff; // set Marker visible/unvisible
   TQtLineWidthComboBox  *fWidthCombo;  // Exclusion zone width 
   QCheckBox          *fExSide;      // set the exclusion zone side

   virtual void  BuildView(QWidget  *editorPanel);
   virtual void  ConnectSignals2Slots();
   virtual void  ChangeView();

public:
   TQtGraphEditor(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, //kChildFrame,
               Pixel_t back = 0);   //GetDefaultFrameBackground());
   TQtGraphEditor(TCanvas *canvas, QWidget *parent=0,  Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, //kChildFrame,
               Pixel_t back = 0);   //GetDefaultFrameBackground());
   virtual ~TQtGraphEditor();

public slots:
  // slots related to graph attributes
   virtual void DoShape(int);
   virtual void DoMarkerOnOff(bool on);
   virtual void DoTitle(const QString &);
   virtual void DoGraphLineWidth(int);
   virtual void DoGraphLineWidth(bool);


#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGraphEditor,0)        // graph editor
//MOC_SKIP_END
#endif
};
#endif

