// @(#)root/ged:$Name:  $:$Id: TQtGedFrames.h,v 1.3 2013/08/30 16:00:10 perev Exp $
// Author: Valeri Fine 25/06/04

/****************************************************************************
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on ROOT_TMarkerEditor, a ROOT GUI toolkit.       *
 * based on the code by Marek Biskup, Ilka  Antcheva 28/07/03            *
 *                                                                       *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/


#ifndef ROOT_TQtGedFrames
#define ROOT_TQtGedFrames

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtGedAttFrame, TQtGedAttNameFrame, TQtGedAttFillFrame,             //
//  TQtGedAttLineFrame, TQtGedAttTextFrame, TQtGedAttMarkerFrame        //
//                                                                      //
//  Frames with object attributes, just like on TAttCanvases.           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TQtGedAttFrame.h"

// TQtGed package classes forward declarations

class TQtColorSelect;
class TQtPatternSelect;
class TQtGedPatternSelect;
class TQtGedMarkerSelect;
class TQtLineStyleComboBox;
class TQtLineWidthComboBox;
class TQtFontComboBox;
class TQtFloatSpinBox;
//  Qt classes forward declarations:

class QWidget;
class QMainWindow;
class QLabel;
class QString;
class QComboBox;
class QHBox;
class QObjectList ;
class QObject;
#include "TAttFill.h"
#include "TAttText.h"
#include "TAttLine.h"
#include "TAttMarker.h"


class TQtGedAttNameFrame : public TQtGedAttFrame<TObject> {

protected:
   QLabel        *fLabel;      //label of attribute frame
   
              virtual void BuildView(QWidget  *editorPanel);

public:
   TQtGedAttNameFrame(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
                    Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   TQtGedAttNameFrame(
                      TCanvas *canvas, QWidget *parent=0,Int_t id=0,
                      Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                      UInt_t option= 0, /* = kChildFrame, */
                      Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   virtual ~TQtGedAttNameFrame() {}

   virtual void  ConnectSignals2Slots();
   virtual void  ChangeView();
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGedAttNameFrame,0)  //name attribute farme
//MOC_SKIP_END
#endif
};


class TQtGedAttFillFrame : public TQtGedAttFrame<TAttFill> {
#ifndef __CINT__
   Q_OBJECT
#endif
protected:
   TQtColorSelect        *fColorSelect;
   TQtPatternSelect      *fPatternSelect;

   virtual void BuildView(QWidget  *editorPanel);

public:
   TQtGedAttFillFrame(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
                    Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   TQtGedAttFillFrame(
                      TCanvas *canvas, QWidget *parent=0,Int_t id=0,
                      Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                      UInt_t option= 0, /* = kChildFrame, */
                      Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );

   virtual ~TQtGedAttFillFrame() {  }

   virtual void  ConnectSignals2Slots();
   virtual void   ChangeView();

public slots:
   void SetColor(Pixel_t);
   void SetPattern(Style_t);

// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGedAttFillFrame,0)  //fill attribute frame
//MOC_SKIP_END
#endif
};


class TQtGedAttLineFrame : public TQtGedAttFrame<TAttLine> {
#ifndef __CINT__
   Q_OBJECT
#endif

protected:
    TQtLineStyleComboBox  *fStyleCombo;       // line style combo box
    TQtLineWidthComboBox  *fWidthCombo;       // line width combo box
    TQtColorSelect        *fColorSelect;      // color selection widget

   virtual void BuildView(QWidget  *editorPanel);

public:
   TQtGedAttLineFrame(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
                    Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   TQtGedAttLineFrame(
                      TCanvas *canvas, QWidget *parent=0,Int_t id=0,
                      Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                      UInt_t option= 0, /* = kChildFrame, */
                      Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   virtual ~TQtGedAttLineFrame() { }

   virtual void   ConnectSignals2Slots();
   virtual void   ChangeView();

public slots:
   void SetColor(Pixel_t);
   void SetSize(int size);
   void SetStyle(int style);

// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGedAttLineFrame,0)  // line attribute frame
//MOC_SKIP_END
#endif
};


class TQtGedAttTextFrame : public TQtGedAttFrame<TAttText>{
#ifndef __CINT__
   Q_OBJECT
#endif

protected:

   TQtFontComboBox  *fTypeCombo;       // font style combo box
   QComboBox        *fSizeCombo;       // font size combo box
   QComboBox        *fAlignCombo;      // font aligh combo box
   TQtColorSelect   *fColorSelect;     // color selection widget

   static QComboBox *BuildFontSizeComboBox(QWidget *parent, Int_t id=0);
   static QComboBox *BuildTextAlignComboBox(QWidget *parent, Int_t id=0);

   virtual void BuildView(QWidget  *editorPanel);

public:
   TQtGedAttTextFrame(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
                    Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                UInt_t option= 0, /* = kChildFrame, */
                Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   TQtGedAttTextFrame(
                      TCanvas *canvas, QWidget *parent=0,Int_t id=0,
                      Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 70,
                      UInt_t option= 0, /* = kChildFrame, */
                      Pixel_t back = 0  /* = GetDefaultFrameBackground()*/ );
   virtual ~TQtGedAttTextFrame() {}

   virtual void  ConnectSignals2Slots();
   virtual void  ChangeView();

public slots:
   void SetColor(Pixel_t);
   void SetSize(int size);
   void SetStyle(int style);
   void SetAlign(int align);

// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtGedAttTextFrame,0)  //text attribute frame
//MOC_SKIP_END
#endif
};

#endif
