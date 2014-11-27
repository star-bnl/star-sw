// @(#)root/asimage:$Name:  $:$Id: TQtPaletteEditor.cxx,v 1.5 2013/08/30 16:00:27 perev Exp $
// Author: Reiner Rohlfs   24/03/2002

/*************************************************************************
 * Copyright (C) 1995-2002, Rene Brun, Fons Rademakers and Reiner Rohlfs *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtPaletteEditor                                                    //
//                                                                      //
//  This is a GUI window to edit a color palette.                       //
//  It is called by a pull down menu item of TQtImage.                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtImage.h"
#include "TQtWidget.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TQtPaletteEditor.h"
#if QT_VERSION < 0x40000
#  include <qtoolbar.h> 
#  include <qcombobox.h> 
#  include <qradiobutton.h> 
#  include <qcheckbox.h> 
#  include <qpushbutton.h> 
#  include <qlayout.h>
#  include <qtooltip.h> 
#  include <qcheckbox.h>
#  include <qvbox.h> 
#  include <qhbox.h> 
#  include <qdockwindow.h> 
#  include <qbuttongroup.h>
#  include <qimage.h> 
#  include <qframe.h>
#else
#  include <QImage>
#  include <QToolBar>
#  include <QBoxLayout>
#  include <QFrame>
#endif

//// #include "QXYLayout.h"
//#include "qbutton.h"
//#include "omboBox.h"
//#include "QFileDialog.h"
#include "TDirectory.h"
#include "TFile.h"
#include "TLine.h"
#include "TMath.h"
static const char *gFileTypes[] = {
   "ROOT palette file",  "*.pal.root",
   "ASCII palette file", "*.pal.txt",
   0,                    0
};

static UShort_t gRedRainbow[12] = {
   0x0000, 0x7000, 0x0000, 0x0000, 0x0000,
   0xffff, 0xffff, 0x7000, 0x8000, 0xffff
};
static UShort_t gGreenRainbow[12] = {
   0x0000, 0x0000, 0x0000, 0xffff, 0xffff,
   0xffff, 0x0000, 0x0000, 0x8000, 0xffff
};
static UShort_t gBlueRainbow[12] = {
   0x0000, 0x7000, 0xffff, 0xffff, 0x0000,
   0x0000, 0x0000, 0x0000, 0xa000, 0xffff
};


ClassImp(TQtPaletteEditor)
//______________________________________________________________________________
TQtPaletteEditor::TQtPaletteEditor(TAttImage *attImage, UInt_t w, UInt_t h)
    : TPaletteEditor(attImage, w, h)
{
   // Palette editor constructor.
   // The palette editor aloows the editing of the color palette of the image.

//   SetLayoutManager(new QXYLayout(this));
//   QGridLayout::QGridLayout ( QWidget * parent, int nRows = 1, int nCols = 1, int margin = 0, int space = -1, const char * name = 0 ) 
   fHisto        = 0;
   fLimitLine[0] = 0;
   fLimitLine[1] = 0;
   fRampFactor   = 0;
   fImagePad     = gPad;

   fPaletteList = new TList;
   fPaletteList->SetOwner();

   fPalette = new TImagePalette(attImage->GetPalette());
   fPaletteList->Add(fPalette);

   CreateMenuToolBox();

   QWidget *centralWidget = new QFrame(this);
   QVBoxLayout *boxLayout = new QVBoxLayout(centralWidget);

   setCentralWidget(centralWidget);

      QWidget *controlFrame = new QWidget(centralWidget);
      boxLayout->addWidget(controlFrame);
      QVBoxLayout *vboxLayout = new QVBoxLayout (controlFrame);
      // the histogram of the data
      fHistCanvas = CreateHistogramArea(controlFrame,attImage);
      // button area
      CreateButtonFrame(controlFrame);


      // the palette
      fPaletteCanvas = new TQtWidget(centralWidget, "palette");
      fPaletteCanvas->resize( 300, 50);
   
   fPaintPalette = new PaintPalette(&fPalette, attImage);
   fPaintPalette->Draw();

#if QT_VERSION < 0x40000
   setCaption ( "Palette Editor" );
   setIconText( "Palette Editor" ); 
#else
   setWindowTitle   ( "Palette Editor" );
   setWindowIconText( "Palette Editor" ); 
#endif
   resize (w+300, h+2*50);
   UpdateScreen(kFALSE);
   show();
}
//______________________________________________________________________________
TQtPaletteEditor  *TQtPaletteEditor::Create(TAttImage *attImage, UInt_t w, UInt_t h)
{ return new TQtPaletteEditor(attImage, w, h); }

//______________________________________________________________________________
TQtPaletteEditor::~TQtPaletteEditor()
{
   // Palette editor destructor. Deletes all frames and their layout hints.

   delete fHisto;
   delete fPaintPalette;
   delete fLimitLine[0];
   delete fLimitLine[1];
   delete fPaletteList;
}

//______________________________________________________________________________
void TQtPaletteEditor::CloseWindow()
{
   // Close editor.

   TPaletteEditor::CloseWindow();
#if QT_VERSION >= 0x40000
   if (!testAttribute(Qt::WA_DeleteOnClose)) setAttribute(Qt::WA_DeleteOnClose);
   close();
#else
   close(true);
#endif
}
//______________________________________________________________________________
QWidget *TQtPaletteEditor::CreateMenuToolBox()
{
   QToolBar *menuTool = new QToolBar(this);
#if QT_VERSION < 0x40000
   addDockWindow(menuTool);
   moveDockWindow(menuTool,Qt::DockRight);

  //  QDockWindow *menuTool = new QDockWindow(QDockWindow::InDock,this);
   // QVBox *buttonBox = new QVBox(menuTool);
   QPushButton *button;

   button = new QPushButton("&Apply",menuTool);
   QToolTip::add(button,"Apply the palette to the image");
   connect(button,SIGNAL(clicked()),this,SLOT(ApplyCB()));

   button = new QPushButton( "&Ok",menuTool);
   QToolTip::add(button,"Same as Apply and Cancel button");
   connect(button,SIGNAL(clicked()),this,SLOT(OkCB()));

   button = new QPushButton( "&Cancel",menuTool);
   QToolTip::add(button,"Close this window");
   connect(button,SIGNAL(clicked()),this,SLOT(CancelCB()));

   menuTool->addSeparator();

   button = new QPushButton("&Save",menuTool);
   QToolTip::add(button,"Save the palette in a ROOT or an ASCII file");
   connect(button,SIGNAL(clicked()),this,SLOT(SaveCB()));

   button = new QPushButton("O&pen",menuTool);
   QToolTip::add(button,"Read a palette from a ROOT or an ASCII file");
   connect(button,SIGNAL(clicked()),this,SLOT(OpenCB()));

   menuTool->addSeparator();

   button = new QPushButton("&New", menuTool);
   QToolTip::add(button,"Create a new palette (not yet implemented)");
   // connect(button,SIGNAL(clicked()),this,SLOT(NewCB()));
   button->setEnabled(false);

   button = new QPushButton("&Edit",menuTool);
   QToolTip::add(button,"Edit a palette (not yet implemented)");
   // connect(button,SIGNAL(clicked()),this,SLOT(EditCB()));
   button->setEnabled(false);
#endif
   return menuTool;
}
//______________________________________________________________________________
QWidget *TQtPaletteEditor::CreateButtonFrame(QWidget *controlFrame)
{

   QWidget *buttonFrame = new QWidget(controlFrame);
#if QT_VERSION < 0x40000
   QGridLayout *grid    = new QGridLayout(buttonFrame, 10, 2); // 8 rows, 2 columns grid layout

   // buttons

   fAutoUpdate = new QCheckBox("Auto Update",buttonFrame);
   QToolTip::add(fAutoUpdate,"Automatic update of the image (without Apply button)");
   grid->addWidget(fAutoUpdate,0,0);

   fUnDoButton = new QPushButton("&Undo", buttonFrame);
   connect(fUnDoButton,SIGNAL(clicked()),this,SLOT(UndoCB()));
   QToolTip::add(fUnDoButton,"Undo the last modification (repeatable)");
   grid->addWidget(fUnDoButton,1,0);
   fUnDoButton->setEnabled(false);


   fReDoButton = new QPushButton("&Redo", buttonFrame);
   QToolTip::add(fReDoButton,"Undo the last undo operation (repeatable)");
   connect(fReDoButton,SIGNAL(clicked()),this,SLOT(RedoCB()));
   grid->addWidget(fReDoButton,1,1);
   fReDoButton->setEnabled(false);

   QPushButton *button = new QPushButton("&Log", buttonFrame);
   QToolTip::add(button,"Apply a log operation to the anchor points of the palette");
   connect(button,SIGNAL(clicked()),this,SLOT(LogPaletteCB()));
   grid->addWidget(button,7,0);

   button = new QPushButton( "E&xp", buttonFrame);
   QToolTip::add(button,"Apply a exp operation to the anchor points of the palette");
   connect(button,SIGNAL(clicked()),this,SLOT(ExpPaletteCB()));
   grid->addWidget(button,8,0);

   button = new QPushButton("L&in", buttonFrame);
   QToolTip::add(button,"Make the distance of all anchor points constant");
   connect(button,SIGNAL(clicked()),this,SLOT(LinPaletteCB()));
   grid->addWidget(button,9,0);

   button = new QPushButton( "In&vert", buttonFrame);
   QToolTip::add(button,"Invert the order of the colors");
   connect(button,SIGNAL(clicked()),this,SLOT(InvertPaletteCB()));
   grid->addWidget(button,8,1);

   fStepButton = new QCheckBox( "Step", buttonFrame);
   QToolTip::add(fStepButton,"Apply a step function to the palette");
   connect(button,SIGNAL(clicked()),this,SLOT(StepCB()));
   grid->addWidget(fStepButton,9,1);

   // ramp: 1, 2 or 4
   QButtonGroup *rampFrame = new QButtonGroup(2,Qt::Vertical, "Ramps", buttonFrame);
   grid->addMultiCellWidget(rampFrame,4,5,0,1);
   connect(rampFrame,SIGNAL(clicked(int)),this,SLOT(SetRampCB(int)));

     fRamps[0] = new QRadioButton("1", rampFrame);
     QToolTip::add(fRamps[0],"Repeat the palette once");
     //connect(fRamps[0],SIGNAL(toggled(bool)),this,SLOT(SetRampCB(bool)));
     // fRamps[0]->setChecked(true);
     
     fRamps[1] = new QRadioButton("2",rampFrame);
     QToolTip::add(fRamps[1],"Repeat the palette twice");
     connect(fRamps[1],SIGNAL(toggled(bool)),this,SLOT(SetRampCB(int)));

     fRamps[2] = new QRadioButton("4", rampFrame);
     QToolTip::add(fRamps[2],"Repeat the palette four times");
     //connect(fRamps[2],SIGNAL(toggled(bool)),this,SLOT(SetRampCB(bool)));
     //SetRamp(1);

     rampFrame->setButton(0);

   // the combobox of different palettes
   fComboBox = new QComboBox(buttonFrame);
   connect(fComboBox, SIGNAL(activated (int)),this, SLOT(NewPaletteCB(int)));
   grid->addMultiCellWidget(fComboBox,2,2,0,1);

   fComboBox->insertItem("Rainbow", 0);
   fComboBox->insertItem("Grey", 1);
   fComboBox->insertItem("Hot",  2);
   fComboBox->insertItem("Cold", 3);
   fComboBox->insertItem("Bowlerhat", 4);
   fComboBox->insertItem("", 5);
#else
   fprintf(stderr," The method TQtPaletteEditor::CreateButtonFrame has not been implemented yet for Qt4\n");
#endif
   return buttonFrame;
}

//______________________________________________________________________________
TQtWidget *TQtPaletteEditor::CreateHistogramArea(QWidget *parent,TAttImage *attImage)
{
   // the histogram of the data
   fHistCanvas = new TQtWidget(parent,"data hist");
   fHistCanvas->resize(300,50);

   const QImage *image = ((TQtImage*)attImage)->GetImage();
   if (image) {
#if 0
      if (image->alt.vector) {
         Int_t pixel;
         Double_t *data = image->alt.vector;
         Int_t numPixel = image->width * image->height;
         Int_t numBins = numPixel / 20;
         numBins = (numBins < 10) ? 10 : (numBins > 200) ? 200 : numBins;

         // get min and max value of image
         fMinValue = fMaxValue = *image->alt.vector;
         for (pixel = 1; pixel < numPixel; pixel++) {
            if (fMinValue > *(data + pixel)) fMinValue = *(data + pixel);
            if (fMaxValue < *(data + pixel)) fMaxValue = *(data + pixel);
         }

         fHisto = new TH1D("Statistics", "Pixel histogram of unzoomed image    ",
            numBins, fMinValue, fMaxValue);
         for (pixel = 0; pixel < numPixel; pixel++)
            fHisto->Fill(*(data + pixel));

         fHisto->Draw("HIST");
         fHisto->GetXaxis()->SetLabelFont(63);
         fHisto->GetXaxis()->SetLabelSize(10);
         fHisto->GetYaxis()->SetLabelFont(63);
         fHisto->GetYaxis()->SetLabelSize(10);

         fLimitLine[0] = new LimitLine(fMinValue + fPalette->fPoints[1] * (fMaxValue - fMinValue),
            0, fHisto->GetMaximum(), this);
         fLimitLine[0]->Draw();
         fLimitLine[1] = new LimitLine(fMinValue + fPalette->fPoints[fPalette->fNumPoints - 2] *
            (fMaxValue - fMinValue), 0, fHisto->GetMaximum(), this);
         fLimitLine[1]->Draw();
      }
#endif
   }
   return fHistCanvas;
}

//______________________________________________________________________________
//
//slots: 
//______________________________________________________________________________
void TQtPaletteEditor::ApplyCB() {
   fAttImage->SetPalette(fPalette);
   fImagePad->Modified();
   fImagePad->Update();
}
//______________________________________________________________________________
void TQtPaletteEditor::OkCB() {
   fAttImage->SetPalette(fPalette);
   fImagePad->Modified();
   fImagePad->Update();
   CloseWindow();
}
//______________________________________________________________________________
void TQtPaletteEditor::CancelCB() {
   CloseWindow();
}
//______________________________________________________________________________
void TQtPaletteEditor::UndoCB() {
   fPalette = (TImagePalette*)(fPaletteList->Before(fPalette));
#if QT_VERSION < 0x40000
   if (fPalette && fAutoUpdate->isChecked() ) {
      fAttImage->SetPalette(fPalette);
      fImagePad->Modified();
      fImagePad->Update();
   }
#else
      fprintf(stderr," The method TQtPaletteEditor::UndoCB has not been implemented yet for Qt4\n");
#endif 
   UpdateScreen(kTRUE);
}
//______________________________________________________________________________
void TQtPaletteEditor::RedoCB() {
   fPalette = (TImagePalette*)(fPaletteList->After(fPalette));
#if QT_VERSION < 0x40000
   if (fPalette && fAutoUpdate->isChecked() ) {
      fAttImage->SetPalette(fPalette);
      fImagePad->Modified();
      fImagePad->Update();
   }
#else
      fprintf(stderr," The method TQtPaletteEditor::RedoCB has not been implemented yet for Qt4\n");
#endif 

   UpdateScreen(kTRUE);
}

//______________________________________________________________________________
void TQtPaletteEditor::InsertNewPalette(TImagePalette *newPalette)
{
   // The newPalette is inserted in the list of palettes (fPaletteList) and
   // fPalette is set to the newPalette. Protected method,

   // first remove all palettes in the list which are behind the
   // current palette
   TObject *obj;
   while ((obj = fPaletteList->After(fPalette)) != 0)
      delete fPaletteList->Remove(obj);

   // add new palette and make it to the current palette
   fPaletteList->Add(newPalette);
   fPalette = newPalette;
#if QT_VERSION < 0x40000
   // update the image
   if (fAutoUpdate->isChecked ()) {
      fAttImage->SetPalette(fPalette);
      fImagePad->Modified();
      fImagePad->Update();
   }
#else
      fprintf(stderr," The method TQtPaletteEditor::InsertNewPalette has not been implemented yet for Qt4\n");
#endif 

}

//______________________________________________________________________________
void TQtPaletteEditor::SaveCB()
{
   // Saves the current palette either into a ROOT file or in an ASCII file.
   // It is called by the Save - button. Protected method.
#if 0
   QFileInfo fi;
   fi.fFileTypes = gFileTypes;

   new QFileDialog(gClient->GetRoot(), this, kFDSave, &fi);
   if (fi.fFilename == 0)
      return;

   if (strcmp(".pal.txt", fi.fFilename + strlen(fi.fFilename) - 8) == 0) {
      // write into an ASCII file
      FILE *fl = fopen(fi.fFilename, "w");
      if (!fl) return;
      fprintf(fl, "%u\n", fPalette->fNumPoints);
      for (Int_t pt = 0; pt < Int_t(fPalette->fNumPoints); pt++)
         fprintf(fl, "%10.9f %04hx %04hx %04hx %04hx\n",
                 fPalette->fPoints[pt],
                 fPalette->fColorRed[pt],
                 fPalette->fColorGreen[pt],
                 fPalette->fColorBlue[pt],
                 fPalette->fColorAlpha[pt] );
      fclose(fl);
   } else {
      // write into a ROOT file
      char fn[512];
      if (strcmp(".pal.root", fi.fFilename + strlen(fi.fFilename) - 9) != 0)
         sprintf(fn, "%s%s", fi.fFilename, ".pal.root");
      else
         strcpy(fn, fi.fFilename);
      TDirectory *dirsav = gDirectory;
      TFile *fsave = new TFile(fn, "RECREATE");
      if (!fsave->IsOpen()) {
         delete fsave;
         return;
      }

      fPalette->Write();
      fsave->Close();
      delete fsave;
      if (dirsav)
         dirsav->cd();
   }
#endif
}

//______________________________________________________________________________
void TQtPaletteEditor::OpenCB()
{
   // Opens either a ROOT file or an ASCII file and reads a palette.
   // It is called by the Open - button. Protected method.
#if 0
   QFileInfo fi;
   fi.fFileTypes = gFileTypes;

   new QFileDialog(gClient->GetRoot(), this, kFDOpen, &fi);
   if (fi.fFilename == 0)
      return;

   TImagePalette *newPalette;

   if (strcmp(".pal.txt", fi.fFilename + strlen(fi.fFilename) - 8) == 0) {
      FILE *fl = fopen(fi.fFilename, "r");
      if (!fl) return;
      UInt_t numPoints;
      fscanf(fl, "%u\n", &numPoints);
      newPalette = new TImagePalette(numPoints);
      for (Int_t pt = 0; pt < Int_t(numPoints); pt++)
         fscanf(fl, "%lf %hx %hx %hx %hx\n",
                newPalette->fPoints + pt,
                newPalette->fColorRed + pt,
                newPalette->fColorGreen + pt,
                newPalette->fColorBlue + pt,
                newPalette->fColorAlpha + pt );
      fclose(fl);
   } else {
      // read from a ROOT file
      char fn[512];
      if (strcmp(".pal.root", fi.fFilename + strlen(fi.fFilename) - 9) != 0)
         sprintf(fn, "%s%s", fi.fFilename, ".pal.root");
      else
         strcpy(fn, fi.fFilename);
      TDirectory *dirsav = gDirectory;

      TFile *fsave = new TFile(fn, "READ");
      if (!fsave->IsOpen()) {
         delete fsave;
         return;
      }

      newPalette = (TImagePalette*)fsave->Get("TImagePalette");
      delete fsave;
      if (dirsav) dirsav->cd();
      if (!newPalette)
         return;
   }

   InsertNewPalette(newPalette);
   UpdateScreen(kTRUE);

   fComboBox->Select(5);  // empty entry
#endif
}

//______________________________________________________________________________
void TQtPaletteEditor::UpdateScreen(Bool_t histoUpdate)
{
   // All widgeds of the screen are updated with the current palette.
   // Protected method.

   // update the color palette
   fPaletteCanvas->GetCanvas()->Modified();
   fPaletteCanvas->GetCanvas()->Update();

   if (histoUpdate) {
      // update the limit lines
      Double_t xPos = fMinValue + fPalette->fPoints[1] * (fMaxValue - fMinValue);
      fLimitLine[0]->SetX1(xPos);
      fLimitLine[0]->SetX2(xPos);

      xPos = fMinValue + fPalette->fPoints[fPalette->fNumPoints - 2] * (fMaxValue - fMinValue);
      fLimitLine[1]->SetX1(xPos);
      fLimitLine[1]->SetX2(xPos);

      fHistCanvas->GetCanvas()->Modified();
      fHistCanvas->GetCanvas()->Update();
   }

#if QT_VERSION < 0x40000
   // update undo / redo button
   fUnDoButton->setEnabled(fPalette != fPaletteList->First());
   fReDoButton->setEnabled(fPalette != fPaletteList->Last());

   // test if it is a step palette
   bool step = true;

   Int_t pt;
   for (pt = 2; pt < Int_t(fPalette->fNumPoints - 2); pt += 2)
      if (TMath::Abs(fPalette->fPoints[pt] - fPalette->fPoints[pt + 1])  > 0.0001 ||
          fPalette->fColorRed[pt]   != fPalette->fColorRed[pt-1]   ||
          fPalette->fColorGreen[pt] != fPalette->fColorGreen[pt-1] ||
          fPalette->fColorBlue[pt]  != fPalette->fColorBlue[pt-1])
         step = false;
   fStepButton->setChecked(step);

   // find the ramp factor
   fRampFactor = 4;
   Int_t off = (fPalette->fNumPoints - 2) / 4;
   for (pt = 0; pt < Int_t(fPalette->fNumPoints - 2) / 4 * 3; pt++)
      if (fPalette->fColorRed[pt + 1]   != fPalette->fColorRed[pt + 1 + off]   ||
          fPalette->fColorGreen[pt + 1] != fPalette->fColorGreen[pt + 1 + off] ||
          fPalette->fColorBlue[pt + 1]  != fPalette->fColorBlue[pt + 1 + off]  ||
          fPalette->fColorAlpha[pt + 1] != fPalette->fColorAlpha[pt + 1 + off]) {
         fRampFactor = 2;
         break;
      }

   off = (fPalette->fNumPoints - 2) / 2;
   for (pt = 0; pt < Int_t(fPalette->fNumPoints - 2) / 2; pt++)
      if (fPalette->fColorRed[pt + 1]   != fPalette->fColorRed[pt + 1 + off]   ||
          fPalette->fColorGreen[pt + 1] != fPalette->fColorGreen[pt + 1 + off] ||
          fPalette->fColorBlue[pt + 1]  != fPalette->fColorBlue[pt + 1 + off]  ||
          fPalette->fColorAlpha[pt + 1] != fPalette->fColorAlpha[pt + 1 + off]) {
         fRampFactor = 1;
         break;
      }

#else
      fprintf(stderr," The method TQtPaletteEditor::UpdateScreen has not been implemented yet for Qt4\n");
#endif 
   //fRamps[0]->setChecked(fRampFactor == 1);
   //fRamps[1]->setChecked(fRampFactor == 2);
   //fRamps[2]->setChecked(fRampFactor == 4);
}

//______________________________________________________________________________
void TQtPaletteEditor::LogPaletteCB()
{
   // The anchor points are rescaled by a log operation.
   // It is called by the log - button. Protected method.

   TImagePalette *newPalette = new TImagePalette(*fPalette);

   Double_t delta = fPalette->fPoints[fPalette->fNumPoints-2] - fPalette->fPoints[1];

   for (Int_t pt = 2; pt < Int_t(fPalette->fNumPoints - 2); pt++)
      newPalette->fPoints[pt] = fPalette->fPoints[1] +
         TMath::Log(fPalette->fPoints[pt] - fPalette->fPoints[1] + 1) /
         TMath::Log(delta + 1) * delta;

   InsertNewPalette(newPalette);
   UpdateScreen(kFALSE);
}

//______________________________________________________________________________
void TQtPaletteEditor::ExpPaletteCB()
{
   // The anchor points are rescaled by a exp operation.
   // It is called by the exp - button. Protected method.

   TImagePalette *newPalette = new TImagePalette(*fPalette);

   Double_t delta = fPalette->fPoints[fPalette->fNumPoints-2] - fPalette->fPoints[1];

   for (Int_t pt = 2; pt < Int_t(fPalette->fNumPoints - 2); pt++)
      newPalette->fPoints[pt] = fPalette->fPoints[1] +
         TMath::Exp((fPalette->fPoints[pt] - fPalette->fPoints[1]) *
         TMath::Log(delta + 1) / delta) - 1;

   InsertNewPalette(newPalette);
   UpdateScreen(kFALSE);
}

//______________________________________________________________________________
void TQtPaletteEditor::LinPaletteCB()
{
   // The anchor points are rescaled to be linar.
   // It is called by the lin - button. Protected method.

   TImagePalette *newPalette = new TImagePalette(*fPalette);

   Double_t delta = fPalette->fPoints[fPalette->fNumPoints-2] - fPalette->fPoints[1];
#if QT_VERSION < 0x40000
   if (! fStepButton->isChecked ()) {
      for (Int_t pt = 2; pt < Int_t(fPalette->fNumPoints - 2); pt++)
         newPalette->fPoints[pt] = fPalette->fPoints[1] +
            delta * (pt - 1) / (fPalette->fNumPoints - 3);
   } else {
      for (Int_t pt = 0; pt < Int_t(fPalette->fNumPoints - 4); pt += 2) {
         newPalette->fPoints[pt + 3] = fPalette->fPoints[1] + delta * (pt + 2) /
                                       (fPalette->fNumPoints - 2) ;
         newPalette->fPoints[pt + 2] = newPalette->fPoints[pt + 3];
      }
   }

   InsertNewPalette(newPalette);
#else
      fprintf(stderr," The method TQtPaletteEditor::LinPaletteCB has not been implemented yet for Qt4\n");
#endif 
  UpdateScreen(kFALSE);
}

//______________________________________________________________________________
void TQtPaletteEditor::InvertPaletteCB()
{
   // The palette is inverted.
   // It is called by the invert - button. Protected method.

   TImagePalette *newPalette = new TImagePalette(*fPalette);

   Int_t pt;
   for (pt = 0; pt < Int_t(fPalette->fNumPoints); pt++)  {
      newPalette->fColorRed[pt]   = fPalette->fColorRed[fPalette->fNumPoints - 1 - pt];
      newPalette->fColorGreen[pt] = fPalette->fColorGreen[fPalette->fNumPoints - 1 - pt];
      newPalette->fColorBlue[pt]  = fPalette->fColorBlue[fPalette->fNumPoints - 1 - pt];
      newPalette->fColorAlpha[pt] = fPalette->fColorAlpha[fPalette->fNumPoints - 1 - pt];
   }

   for (pt = 2; pt < Int_t(fPalette->fNumPoints - 2); pt++)
      newPalette->fPoints[pt] = fPalette->fPoints[1] +
         fPalette->fPoints[fPalette->fNumPoints - 2] -
         fPalette->fPoints[fPalette->fNumPoints - 1 - pt];

   InsertNewPalette(newPalette);
   UpdateScreen(kFALSE);
}

//______________________________________________________________________________
void TQtPaletteEditor::NewPaletteCB(int id)
{
   // A new palette is created, depending on the id.
   // It is called by the combo box. Protected method.

   if (id == 5) // empty entry
      return;

   TImagePalette *newPalette;

   Double_t delta = fPalette->fPoints[fPalette->fNumPoints-2] - fPalette->fPoints[1];
   UInt_t   numPt;

   numPt = id == 0 ? 12 : 13;
   newPalette = new TImagePalette(numPt);
   Int_t pt;
   for (pt = 1; pt < Int_t(numPt - 1); pt++) {
      newPalette->fPoints[pt] = fPalette->fPoints[1] + (pt - 1) * delta / (numPt - 3);
      newPalette->fColorAlpha[pt] = 0xffff;
   }

   switch (id) {
      case 0:  // rainbow
         memcpy(newPalette->fColorRed + 1,   gRedRainbow,   12 * sizeof(UShort_t));
         memcpy(newPalette->fColorGreen + 1, gGreenRainbow, 12 * sizeof(UShort_t));
         memcpy(newPalette->fColorBlue + 1,  gBlueRainbow,  12 * sizeof(UShort_t));
         break;

      case 1:  // gray
         for (pt = 1; pt < Int_t(numPt - 1); pt++) {
            newPalette->fColorRed[pt]   = 0xffff * (pt - 1) / (numPt - 3);
            newPalette->fColorGreen[pt] = 0xffff * (pt - 1) / (numPt - 3);
            newPalette->fColorBlue[pt]  = 0xffff * (pt - 1) / (numPt - 3);
         }
         break;

      case 2:  // hot (red)
         for (pt = 1; pt < Int_t(numPt - 1) / 2; pt++) {
            newPalette->fColorRed[pt]   = 0xffff * (pt - 1) / ((numPt - 3) / 2);
            newPalette->fColorGreen[pt] = 0;
            newPalette->fColorBlue[pt]  = 0;
         }

         for (; pt < Int_t(numPt - 1); pt++) {
            newPalette->fColorRed[pt]   = 0xffff;
            newPalette->fColorGreen[pt] = 0xffff * (pt - (numPt - 1) / 2) / ((numPt - 3) / 2);
            newPalette->fColorBlue[pt]  = 0xffff * (pt - (numPt - 1) / 2) / ((numPt - 3) / 2);
         }
         break;

      case 3:  // cold (blue)
         for (pt = 1; pt < Int_t(numPt - 1) / 2; pt++) {
            newPalette->fColorRed[pt]   = 0;
            newPalette->fColorGreen[pt] = 0;
            newPalette->fColorBlue[pt]  = 0xffff * (pt - 1) / ((numPt - 3) / 2);
         }

         for (; pt < Int_t(numPt - 1); pt++) {
            newPalette->fColorRed[pt]   = 0xffff * (pt - (numPt - 1) / 2) / ((numPt - 3) / 2);
            newPalette->fColorGreen[pt] = 0xffff * (pt - (numPt - 1) / 2) / ((numPt - 3) / 2);
            newPalette->fColorBlue[pt]  = 0xffff;
         }
         break;

      case 4:  // bolwerhat
         for (pt = 1; pt < Int_t(numPt + 1) / 2; pt++) {
            newPalette->fColorRed[pt]   = newPalette->fColorRed[numPt - pt - 1]
                                        = 0xffff * (pt - 1) / ((numPt - 3) / 2);
            newPalette->fColorGreen[pt] = newPalette->fColorGreen[numPt - pt - 1]
                                        = 0xffff * (pt - 1) / ((numPt - 3) / 2);
            newPalette->fColorBlue[pt]  = newPalette->fColorBlue[numPt - pt - 1]
                                        = 0xffff * (pt - 1) / ((numPt - 3) / 2);
         }
         break;
   }

   newPalette->fPoints[0]     = 0;
   newPalette->fColorRed[0]   = newPalette->fColorRed[1];
   newPalette->fColorGreen[0] = newPalette->fColorGreen[1];
   newPalette->fColorBlue[0]  = newPalette->fColorBlue[1];
   newPalette->fColorAlpha[0] = newPalette->fColorAlpha[1];

   newPalette->fPoints[newPalette->fNumPoints-1]     = 1.0;
   newPalette->fColorRed[newPalette->fNumPoints-1]   = newPalette->fColorRed[newPalette->fNumPoints-2];
   newPalette->fColorGreen[newPalette->fNumPoints-1] = newPalette->fColorGreen[newPalette->fNumPoints-2];
   newPalette->fColorBlue[newPalette->fNumPoints-1]  = newPalette->fColorBlue[newPalette->fNumPoints-2];
   newPalette->fColorAlpha[newPalette->fNumPoints-1] = newPalette->fColorAlpha[newPalette->fNumPoints-2];

   InsertNewPalette(newPalette);
   UpdateScreen(kFALSE);
}

//______________________________________________________________________________
void TQtPaletteEditor::StepCB()
{
   // Create a step palette. This is called by the step - check button.
   // Protected method.

#if QT_VERSION < 0x40000
   TImagePalette *newPalette;
   QCheckBox &button = *(QCheckBox *)sender();

   if (button.isChecked ()) {
      // change colors in steps
      newPalette = new TImagePalette(fPalette->fNumPoints * 2 - 2);
      Double_t fkt = (Double_t)(fPalette->fNumPoints - 3) / (fPalette->fNumPoints - 2);
      for (Int_t pt = 1; pt < Int_t(fPalette->fNumPoints - 1); pt++) {
         newPalette->fPoints[pt * 2 - 1] = fPalette->fPoints[1] + (fPalette->fPoints[pt] - fPalette->fPoints[1]) * fkt;
         newPalette->fPoints[pt * 2] = fPalette->fPoints[1] + (fPalette->fPoints[pt + 1] - fPalette->fPoints[1]) * fkt;
         newPalette->fColorRed[pt * 2 - 1]   = newPalette->fColorRed[pt * 2]   = fPalette->fColorRed[pt];
         newPalette->fColorGreen[pt * 2 - 1] = newPalette->fColorGreen[pt * 2] = fPalette->fColorGreen[pt];
         newPalette->fColorBlue[pt * 2 - 1]  = newPalette->fColorBlue[pt * 2]  = fPalette->fColorBlue[pt];
         newPalette->fColorAlpha[pt * 2 - 1] = newPalette->fColorAlpha[pt * 2] = fPalette->fColorAlpha[pt];
      }
   } else {
      // continuous change of colors
      newPalette = new TImagePalette(fPalette->fNumPoints / 2 + 1);
      Double_t fkt = (Double_t) (fPalette->fPoints[fPalette->fNumPoints - 2] - fPalette->fPoints[1]) /
                                (fPalette->fPoints[fPalette->fNumPoints - 3] - fPalette->fPoints[1]);
      for (Int_t pt = 1; pt < Int_t(newPalette->fNumPoints - 1); pt++) {
         newPalette->fPoints[pt] = fPalette->fPoints[pt * 2 -1] * fkt;
         newPalette->fColorRed[pt]   = fPalette->fColorRed[pt * 2 - 1];
         newPalette->fColorGreen[pt] = fPalette->fColorGreen[pt * 2 - 1];
         newPalette->fColorBlue[pt]  = fPalette->fColorBlue[pt * 2 - 1];
         newPalette->fColorAlpha[pt] = fPalette->fColorAlpha[pt * 2 - 1];
      }
   }

   newPalette->fPoints[0]     = fPalette->fPoints[0];
   newPalette->fColorRed[0]   = fPalette->fColorRed[0];
   newPalette->fColorGreen[0] = fPalette->fColorGreen[0];
   newPalette->fColorBlue[0]  = fPalette->fColorBlue[0];
   newPalette->fColorAlpha[0] = fPalette->fColorAlpha[0];

   newPalette->fPoints[newPalette->fNumPoints-2]     = fPalette->fPoints[fPalette->fNumPoints-2];
   newPalette->fPoints[newPalette->fNumPoints-1]     = fPalette->fPoints[fPalette->fNumPoints-1];
   newPalette->fColorRed[newPalette->fNumPoints-1]   = fPalette->fColorRed[fPalette->fNumPoints-1];
   newPalette->fColorGreen[newPalette->fNumPoints-1] = fPalette->fColorGreen[fPalette->fNumPoints-1];
   newPalette->fColorBlue[newPalette->fNumPoints-1]  = fPalette->fColorBlue[fPalette->fNumPoints-1];
   newPalette->fColorAlpha[newPalette->fNumPoints-1] = fPalette->fColorAlpha[fPalette->fNumPoints-1];

   InsertNewPalette(newPalette);
#else
      fprintf(stderr," The method TQtPaletteEditor::StepCB has not been implemented yet for Qt4\n");
#endif 
   UpdateScreen(kFALSE);
}
#if 0
//______________________________________________________________________________
void TQtPaletteEditor::SetRampCB(bool checked)
{
   if (!checked) return;
   bool ok;
   QRadioButton *thisButton = (QRadioButton*)sender();
   int ramp = thisButton->text().toInt(&ok);
   printf("SetRamp %d \n", ramp);
   assert(ok);
   SetRamp(ramp);
   UpdateScreen(kFALSE);
}
#endif
//______________________________________________________________________________
void TQtPaletteEditor::SetRampCB(int ramp)
{
   // The palette is repeated up to 4 times.
   // This is called by one of the ramp radio buttons. Protected method.
   if (ramp == fRampFactor)
      return;
   printf("SetRamp %d \n", ramp);
   Int_t ptPerRamp = (fPalette->fNumPoints - 2) / fRampFactor;
   TImagePalette *newPalette = new TImagePalette(ptPerRamp * ramp + 2);

   Double_t delta = fPalette->fPoints[fPalette->fNumPoints-2] - fPalette->fPoints[1];
   for (Int_t rp = 0; rp < ramp; rp++) {
      for (Int_t pt = 0; pt < ptPerRamp; pt++) {
         newPalette->fPoints[1 + pt + rp * ptPerRamp] = fPalette->fPoints[1] +
              delta / ramp * rp +
              (fPalette->fPoints[1+pt] - fPalette->fPoints[1]) * fRampFactor / ramp;
         newPalette->fColorRed  [1 + pt + rp * ptPerRamp] = fPalette->fColorRed  [1 + pt];
         newPalette->fColorGreen[1 + pt + rp * ptPerRamp] = fPalette->fColorGreen[1 + pt];
         newPalette->fColorBlue [1 + pt + rp * ptPerRamp] = fPalette->fColorBlue [1 + pt];
         newPalette->fColorAlpha[1 + pt + rp * ptPerRamp] = fPalette->fColorAlpha[1 + pt];
      }
   }

   newPalette->fPoints[0]     = fPalette->fPoints[0];
   newPalette->fColorRed[0]   = fPalette->fColorRed[0];
   newPalette->fColorGreen[0] = fPalette->fColorGreen[0];
   newPalette->fColorBlue[0]  = fPalette->fColorBlue[0];
   newPalette->fColorAlpha[0] = fPalette->fColorAlpha[0];

   newPalette->fPoints[newPalette->fNumPoints-2]     = fPalette->fPoints[fPalette->fNumPoints-2];
   newPalette->fPoints[newPalette->fNumPoints-1]     = fPalette->fPoints[fPalette->fNumPoints-1];
   newPalette->fColorRed[newPalette->fNumPoints-1]   = fPalette->fColorRed[fPalette->fNumPoints-1];
   newPalette->fColorGreen[newPalette->fNumPoints-1] = fPalette->fColorGreen[fPalette->fNumPoints-1];
   newPalette->fColorBlue[newPalette->fNumPoints-1]  = fPalette->fColorBlue[fPalette->fNumPoints-1];
   newPalette->fColorAlpha[newPalette->fNumPoints-1] = fPalette->fColorAlpha[fPalette->fNumPoints-1];

   InsertNewPalette(newPalette);
}

//______________________________________________________________________________
void TQtPaletteEditor::UpdateRange()
{
   // Updates the range of the palette.
   // This is called after the blue limit lines were moved to define
   // a new range.

   if (fMaxValue == fMinValue)
      return;

   TImagePalette *newPalette = new TImagePalette(*fPalette);

   Double_t l0 = fLimitLine[0]->GetX1();
   Double_t l1 = fLimitLine[1]->GetX1();
   l0 = (l0 < fMinValue) ? fMinValue : ((l0 > fMaxValue) ?  fMaxValue : l0);
   l1 = (l1 < fMinValue) ? fMinValue : ((l1 > fMaxValue) ?  fMaxValue : l1);
   if (l0 > l1) {
      Double_t tmp = l0;
      l0 = l1;
      l1 = tmp;
   }

   Double_t oldDelta = fPalette->fPoints[fPalette->fNumPoints - 2] - fPalette->fPoints[1];
   Double_t newDelta = (l1 - l0) / (fMaxValue - fMinValue);
   Double_t newOff = (l0 - fMinValue) / (fMaxValue - fMinValue);

   if (newDelta < 0.001 || oldDelta < 0.001)
      return;

   for (Int_t pt = 1; pt < Int_t(fPalette->fNumPoints - 1); pt++)
      newPalette->fPoints[pt] = newOff +
            (fPalette->fPoints[pt] - fPalette->fPoints[1]) * newDelta / oldDelta;

   InsertNewPalette(newPalette);
   UpdateScreen(kFALSE);
}


//______________________________________________________________________________
void TQtPaletteEditor::PaintPalette::Paint(Option_t *)
{
   // Actually paint the paletter.

   // get geometry of pad
   Int_t to_w = TMath::Abs(gPad->XtoPixel(gPad->GetX2()) -
                           gPad->XtoPixel(gPad->GetX1()));
   Int_t to_h = TMath::Abs(gPad->YtoPixel(gPad->GetY2()) -
                           gPad->YtoPixel(gPad->GetY1()));
   if (to_w * to_h) {
#if 0
   ASGradient grad;

   grad.npoints = (*fPalette)->fNumPoints - 2;
   grad.type = GRADIENT_Left2Right;
   grad.color = new ARGB32[grad.npoints];
   grad.offset = new double[grad.npoints];
   for (Int_t pt = 0; pt < grad.npoints; pt++) {
      grad.offset[pt] = ((*fPalette)->fPoints[pt + 1] - (*fPalette)->fPoints[1]) /
                        ((*fPalette)->fPoints[(*fPalette)->fNumPoints - 2] - (*fPalette)->fPoints[1]);
      grad.color[pt] = (((ARGB32)((*fPalette)->fColorBlue[pt + 1]   & 0xff00)) >>  8) |
                        (((ARGB32)((*fPalette)->fColorGreen[pt + 1] & 0xff00))      ) |
                        (((ARGB32)((*fPalette)->fColorRed[pt + 1]   & 0xff00)) <<  8) |
                        (((ARGB32)((*fPalette)->fColorAlpha[pt + 1] & 0xff00)) << 16);
   }

   QImage * grad_im = make_gradient((ASVisual*)TQtImage::GetVisual(), &grad , to_w, to_h,
                                     SCL_DO_COLOR, ASA_ASImage, 0,
                                     fAttImage->GetImageQuality());
   delete [] grad.color;
   delete [] grad.offset;

   Display *dpy = (Display*)gVirtualX->GetDisplay();
   Pixmap pxmap = asimage2pixmap((ASVisual*)TQtImage::GetVisual(), DefaultRootWindow(dpy),
                                 grad_im, 0, kTRUE);
   Int_t wid = gVirtualX->AddWindow(pxmap, to_w, to_h);
   gPad->cd();
   gVirtualX->CopyPixmap(wid, 0, 0);
   gVirtualX->RemoveWindow(wid);
   gVirtualX->DeletePixmap(pxmap);
   destroy_asimage(&grad_im);
#endif 
}
}


//______________________________________________________________________________
TQtPaletteEditor::LimitLine::LimitLine(Coord_t x, Coord_t y1, Coord_t y2,
                                       TQtPaletteEditor *gui)
   : TLine(x, y1, x, y2)
{
   // The blue limit line in the pixel value histogram.

   fGui = gui;
   SetLineColor(4);
   SetLineWidth(2);
}

//______________________________________________________________________________
void TQtPaletteEditor::LimitLine::Paint(Option_t *option)
{
   // Paint the limit lines.

   fY1 = gPad->GetUymin();
   fY2 = gPad->GetUymax();

   TLine::Paint(option);
}

//______________________________________________________________________________
void TQtPaletteEditor::LimitLine::ExecuteEvent(Int_t event,
                                               Int_t px, Int_t /*py*/)
{
   static Int_t oldX;

   switch(event) {
      case kMouseMotion:
         gPad->SetCursor(kMove);
         break;

      case kButton1Down:
         gVirtualX->SetLineColor(-1);
         TAttLine::Modify();  //Change line attributes only if necessary
         oldX = gPad->XtoAbsPixel(fX1);
         break;

      case kButton1Motion:
         gVirtualX->DrawLine(oldX, gPad->YtoPixel(fY1), oldX, gPad->YtoPixel(fY2));
         oldX = px;
         gVirtualX->DrawLine(oldX, gPad->YtoPixel(fY1), oldX, gPad->YtoPixel(fY2));
         break;

      case kButton1Up:
         gVirtualX->SetLineColor(-1);
         TAttLine::Modify();  //Change line attributes only if necessary
         fX1 = fX2 = gPad->AbsPixeltoX(oldX);
         fGui->UpdateRange();
         gPad->Modified(kTRUE);
         gPad->Update();
         break;

      default:
         break;
   }
}
