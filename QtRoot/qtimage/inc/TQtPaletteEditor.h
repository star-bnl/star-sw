// @(#)root/asimage:$Name:  $:$Id: TQtPaletteEditor.h,v 1.3 2013/08/30 16:00:27 perev Exp $
// Author: Reiner Rohlfs 24/03/2002

/*************************************************************************
 * Copyright (C) 1995-2002, Rene Brun, Fons Rademakers and Reiner Rohlfs *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtPaletteEditor
#define ROOT_TQtPaletteEditor


//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtPaletteEditor                                                    //
//                                                                      //
//  This is a GUI window to edit a color palette.                       //
//  It is called by a context menu item of TImage.                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TAttImage.h"
#include "TLine.h"

#ifndef __CINT__
# include <qmainwindow.h> 
#else
 typedef void  QWidget;
#endif 

class TH1D;
class TQtWidget;
class QPushButton;
class QCheckBox;
class QComboBox;
class QRadioButton;
class TAttImage;
class TVirtualPad;


class TQtPaletteEditor : 
#ifndef __CINT__
   public QMainWindow,
#endif
   public TPaletteEditor
{
#ifndef __CINT__
  Q_OBJECT
#endif
private:
  TQtPaletteEditor& operator=(const TQtPaletteEditor&); // AXEL: intentionally not implemented
  TQtPaletteEditor(const TQtPaletteEditor&); // AXEL: intentionally not implemented
public: 
   TQtPaletteEditor(TAttImage *attImage, UInt_t w, UInt_t h);
   virtual ~TQtPaletteEditor();
   static TQtPaletteEditor  *Create(TAttImage *attImage, UInt_t w, UInt_t h);
// 
protected:
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   class PaintPalette : public TObject {
   protected :
      TImagePalette  **fPalette;
      TAttImage       *fAttImage;
   public:
      PaintPalette(TImagePalette **palette, TAttImage *attImage)
         { fPalette = palette; fAttImage = attImage; }
      void Paint(Option_t *option);
   };

   class LimitLine : public TLine {
   private:
      TQtPaletteEditor  *fGui;
   protected:
      virtual void ExecuteEvent(Int_t event, Int_t px, Int_t py);
   public:
      LimitLine(Coord_t x, Coord_t y1, Coord_t y2, TQtPaletteEditor *gui);
      void Paint(Option_t *option);
   };
//MOC_SKIP_END
#endif
   Double_t              fMinValue;           // min value of image
   Double_t              fMaxValue;           // max value of image

   TH1D                 *fHisto;              // hitogram of image pixels

   TQtWidget            *fPaletteCanvas;      // canvas to draw the current palette
   TQtWidget            *fHistCanvas;         // canvas to draw the histogram
   TList                *fPaletteList;        // list of palettes for undo and redo
   TImagePalette        *fPalette;            // current palette
   TVirtualPad          *fImagePad;
   PaintPalette         *fPaintPalette;
   LimitLine            *fLimitLine[2];

   QPushButton          *fUnDoButton;
   QPushButton          *fReDoButton;

   QCheckBox            *fAutoUpdate;
   QCheckBox            *fStepButton;
   QRadioButton         *fRamps[3];
   Int_t                 fRampFactor;

   QComboBox             *fComboBox;

   void  InsertNewPalette(TImagePalette *newPalette);
   void  UpdateScreen(Bool_t histoUpdate);

public:
   void   UpdateRange();
   void   CloseWindow();
   void   SetRamp(int ramp){SetRampCB(ramp);}

protected:
   virtual QWidget   *CreateButtonFrame(QWidget *controlFrame);
   virtual QWidget   *CreateMenuToolBox();
   virtual TQtWidget *CreateHistogramArea(QWidget *parent,TAttImage *attImage);

#ifndef  __CINT__
public slots:
   void NewPaletteCB(int id);
   void ApplyCB();
   void OkCB();
   void CancelCB();
   void SetRampCB(int id);
   void SaveCB();
   void StepCB();
   void OpenCB();
   void LogPaletteCB();
   void ExpPaletteCB();
   void InvertPaletteCB();

   void LinPaletteCB();
   void UndoCB();
   void RedoCB();

#endif
// 
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
    ClassDef(TQtPaletteEditor,0)  // GUI to edit a color palette
//MOC_SKIP_END
#endif
};

#endif
