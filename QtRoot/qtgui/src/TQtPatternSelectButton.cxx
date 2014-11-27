// @(#)root/gui:$Name:  $:$Id: TQtPatternSelectButton.cxx,v 1.9 2013/08/30 16:00:25 perev Exp $
// Author: Bertrand Bellenot + Fons Rademakers   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/
/**************************************************************************

    This file is part of xclass.
    Copyright (C) 2000, 2001, Hector Peraza.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

**************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtPatternFrame, TQt16ColorSelector, TQtPatternPopup and TQtPatternSelectButton.     //
//                                                                      //
// The TQtPatternFrame is a small frame with border showing a specific  //
// color.                                                               //
//                                                                      //
// The TQt16BrushSelector is a composite frame with 16 TQtPatternFrames.//
//                                                                      //
// The TQtPatternPopup is a popup containing a TQt16BrushSelector and a //
// "More..." button which popups up a TQtBrushDialog allowing custom    //
// color selection.                                                     //
//                                                                      //
// The TQtPatternSelectButton widget is like a checkbutton but instead of the //
// check mark there is color area with a little down arrow. When        //
// clicked on the arrow the TQtPatternPopup pops up.                    //
//                                                                      //
// Selecting a color in this widget will generate the event:            //
// kC_COLORSEL, kCOL_SELCHANGED, widget id, pixel.                      //
// and the signal:                                                      //
// BrushSelected(ULong_t pixel)                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

// #include "TGClient.h"
#include "TGQt.h"
#include "TQtPatternSelectButton.h"
#include "TQtPatternSelect.h"
#include "TEmbeddedPad.h"
#include "TColor.h"

#include <QColorDialog> 
#include <QToolTip> 
#include <QColor> 
#include <QStyleOptionButton>
#include <QPixmap>
#include <QBitmap>
#include <QHBoxLayout>
#include <QToolButton>
#include <QMenu>
#include <QTimer>
#include <QMouseEvent>
#include <QApplication> 

#include <QStyle>
#include <QVariant>
#include <QLayout>
#include <QSpinBox>
//ClassImp(TQtPatternFrame)
//ClassImp(TQt16ColorSelector)
//ClassImp(TQtPatternPopup)
//ClassImp(TQtPatternSelectButton)

static int boxSize = 8;
static int wSize = 3*boxSize ;
static int hSize = 2*boxSize ;

TQtPatternPopup *TQtPatternPopup::fgBrushPopup = 0;
QPalette *TQtPatternFrame::fgPalette = 0;

 //________________________________________________________________________________
QPalette &TQtPatternFrame::palette()
{
   // Create a singleton pallete to control the buttons
   if (!fgPalette) {
      fgPalette = new QPalette;
   }
   return *fgPalette;
}

 //________________________________________________________________________________
TQtPatternFrame::TQtPatternFrame(QWidget *p, Style_t pattern, Int_t n):QToolButton(p)
,fActive(n)
{
   fBrush.SetStyle(pattern);
   fBrush.SetColor(palette().color(QPalette::Text));
   languageChange();
   SetIcon();
}
 //________________________________________________________________________________
TQtPatternFrame::TQtPatternFrame(QWidget *p, TQtBrush &pattern, Int_t n): QToolButton(p)
,fActive(n),fBrush(pattern)
{ 
   if (fActive == -1) {
      QSizePolicy szp = sizePolicy();
      szp.setHorizontalPolicy ( QSizePolicy::Expanding );
      setSizePolicy(szp);
   }
   languageChange();
   SetIcon();
}
//________________________________________________________________________________
void TQtPatternFrame::SetBrush(TQtBrush &newBrush)
{ 
   fBrush = newBrush;
      QSize iSize = fActive == -1 ? size() - QSize(0,3) :sizeHint() - QSize(4,4);
      setIconSize(iSize);
      if (fBrush.GetStyle()) {
         QPixmap px(iSize);
         QPainter p(&px);
         p.setBackgroundMode(Qt::OpaqueMode);
         QPalette pal;
         p.setBackground(palette().color(QPalette::Button));
         p.setBrush(fBrush);
         p.drawRect(contentsRect());
         p.end();
         setIcon(px);
      } else {
         // create the spinbox 
          QSpinBox *alphaBox = new QSpinBox(this);
          alphaBox->setRange(0,100);
          alphaBox->setSingleStep(5);
          alphaBox->setValue(0);
          connect(alphaBox,SIGNAL(editingFinished()),this,SLOT(SetBrushAlpha()));
      }
}
//________________________________________________________________________________
void TQtPatternFrame::SetBrushAlpha()
{
   // Set the alpha value for the translucent box
   QSpinBox *sp = (QSpinBox *)sender();
   int alpha = 0;
   if (sp) {
      alpha = sp->value();
      int oldStyle = fBrush.GetStyle();
      // convert alpha to style
      int newStyle = 4000 + alpha;
      if (newStyle != oldStyle) {
         if (alpha) {
            setToolTip(QString(tr("Style %1 - translucent")).arg(newStyle));
            fBrush.SetStyle(newStyle);
         } else {
            setToolTip(tr("Style 0 - trasparent"));
            fBrush.SetStyle(0);
         }
         emit clicked();
      }
   }
}
//________________________________________________________________________________
void TQtPatternFrame::paintEvent(QPaintEvent *e) 
{
   QToolButton::paintEvent(e);
}
//________________________________________________________________________________
void TQtPatternFrame::SetIcon()
{   
   SetBrush(fBrush);
   setToolTip(fBrushTipLabel + QString::number(fBrush.GetStyle()) );
}
//________________________________________________________________________________
QSize TQtPatternFrame::sizeHint () const 
{
 //   return fActive == -1 ? QToolButton::sizeHint () : QSize(wSize, hSize);
   return fActive == -1 ? size() : QSize(wSize, hSize);
}

//______________________________________________________________________________
void TQtPatternFrame::languageChange()
{
   fBrushTipLabel = tr("Fill Style:  ");
}

//________________________________________________________________________________
void TQtPatternFrame::mouseReleaseEvent(QMouseEvent *event)
{
   return QToolButton::mouseReleaseEvent(event);
}

//________________________________________________________________________________
TQtPatternPopup::TQtPatternPopup(QWidget *p, TQtBrush &color,const char *name, bool modal, Qt::WindowFlags f) :
   QDialog(p,f)
 ,fCurrentBrush(color)
{ 
   delete layout();
   QGridLayout *gridLayout = new QGridLayout(this);
   setLayout(gridLayout);
   gridLayout->setMargin(0);

   QFrame *group = new QFrame(this);
   gridLayout->addWidget(group,0,0);
   group->setFrameShape(QFrame::Panel);
   setName(name);
   setModal(modal);
   
   gridLayout = new QGridLayout(group);
   gridLayout->setMargin(2);  
   gridLayout->setSpacing(1); 
   
   int i = 0;
   TQtPatternFrame *fr = new TQtPatternFrame(group,ULong_t(0), i++); 
   gridLayout->addWidget(fr,0,0);
   connect(fr,SIGNAL(clicked()),this,SLOT(SetActiveSlot()));
   fr->setToolTip(tr(" Style: 0 - transparent" ));
   
   fr = new TQtPatternFrame(group,ULong_t(1000), i++); 
   gridLayout->addWidget(fr,0,1);
   connect(fr,SIGNAL(clicked()),this,SLOT(SetActiveSlot()));
   fr->setToolTip(tr(" Style: 1000 - solid"    ));
   
   fr = new TQtPatternFrame(group,ULong_t(2000), i++); 
   gridLayout->addWidget(fr,0,2);
   connect(fr,SIGNAL(clicked()),this,SLOT(SetActiveSlot()));
   fr->setToolTip(tr(" Style: 2000 - pattern"  ));
   for (int k=1;k < 10;k++) {
      for (int j=0;j <= 2;j++) {    
        fr = new TQtPatternFrame(group,ULong_t(3000+i-3), i++); 
        gridLayout->addWidget(fr,k,j);
        connect(fr,SIGNAL(clicked()),this,SLOT(SetActiveSlot()));
      }
   }
  resize(2  + 3*(wSize+1) +1, 2 + 10*(hSize+1) + 2 );
  updateGeometry();
}
//________________________________________________________________________________
QSize TQtPatternPopup::sizeHint () const
{
   return QSize(2  + 3*(wSize+1) +1, 2 + 10*(hSize+1) + 2);
}
//________________________________________________________________________________
TQtPatternPopup *TQtPatternPopup::Create(QWidget *p, TQtBrush &color,const char *name, bool modal, Qt::WindowFlags f) 
{ 
  // Create the singletone object
  if (!fgBrushPopup) 
     fgBrushPopup =  new TQtPatternPopup(p, color,name, modal, f);
  return fgBrushPopup;
}
      
//________________________________________________________________________________
TQtPatternPopup::~TQtPatternPopup()
{ }

//________________________________________________________________________________
void TQtPatternPopup::SetActiveSlot()
{
   TQtPatternFrame *patternFrame = (TQtPatternFrame*)sender();
   BrushSelected(patternFrame->GetBrush());
}

//________________________________________________________________________________
void TQtPatternPopup::BrushSelected(const TQtBrush &pattern )
{   
   fCurrentBrush = pattern;
   accept();
}
//______________________________________________________________________________
void TQtPatternPopup::languageChange()
{
    //setCaption( tr( "Select Brush" ) );
    //fPushButton->setText( tr( "pushButton39" ) );
    //QToolTip::add( fPushButton, tr( "Current Brush" ) );
    //QWhatsThis::add( fPushButton, tr( "Your current attribute fill color" ) );
}

//________________________________________________________________________________
TQtPatternSelectButton::TQtPatternSelectButton(QWidget *p) : QFrame(p),  fFakeMenu(0)
   ,fBrushEmitter(0)
{
   fBrush.SetStyle();
   CreateWidget();
}
//________________________________________________________________________________
TQtPatternSelectButton::TQtPatternSelectButton(QWidget *p, const char *name, Qt::WindowFlags f)
    : QFrame(p,f)
    , fFakeMenu(0), fBrushEmitter(0)
{
   if (name && name[0]) setName(name);
   fBrush.SetStyle();
   CreateWidget();
}

//________________________________________________________________________________
TQtPatternSelectButton::TQtPatternSelectButton( QWidget *p, UInt_t pattern, Int_t /*id*/,TEmitRootSignal *emitter) 
    : QFrame(p)
   , fFakeMenu(0), fBrushEmitter(emitter)
{
   setName("BrushSelectButton");
   setWindowFlags (Qt::WindowStaysOnTopHint);
   fBrush.SetStyle(pattern);
   CreateWidget();
}

//________________________________________________________________________________
TQtPatternSelectButton::TQtPatternSelectButton( QWidget *p, TQtBrush &pattern, Int_t /*id*/,TEmitRootSignal *emitter) 
    : QFrame(p)
    , fFakeMenu(0), fBrush(pattern),fBrushEmitter(emitter)
{
   setName("BrushSelectButton");
   CreateWidget();
}
//________________________________________________________________________________
void TQtPatternSelectButton::CreateWidget() 
{
    setLineWidth(1);
    fBrush.SetColor("white");
    setFrameStyle(QFrame::NoFrame);
    setContentsMargins(0,0,0,0);
    QHBoxLayout *layout = new QHBoxLayout(this, 0, 0, "layoutPatternSelect"); 
    layout->setSpacing(0);
    layout->setMargin(0);

    //  Brush Button TQtPatternFrame
    fPushButton = new TQtPatternFrame( this, fBrush );
      layout->addWidget(fPushButton);
      connect(fPushButton,SIGNAL(clicked()),this, SLOT(PopupDialog()));
    setSizePolicy(QSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed,
                                 QSizePolicy::ComboBox));
    fPushButton->setPopupMode( QToolButton::MenuButtonPopup); 
    if (!fFakeMenu) {
       // Add fake menu to force the "real" Dialog popup
       fFakeMenu = new QMenu(this);
       fPushButton->setMenu(fFakeMenu);
       connect(fFakeMenu,SIGNAL(aboutToShow()), this, SLOT(PopupDialog()));
    }
    languageChange();
}
//________________________________________________________________________________
TQtPatternSelectButton::~TQtPatternSelectButton()
{ }

//________________________________________________________________________________
void TQtPatternSelectButton::PopupDialog()
{
   TQtPatternPopup *popup = TQtPatternPopup::Create(0,fBrush);
   QPoint global = 
      fPushButton->mapToGlobal(QPoint(0,0));
   popup->move(global.x()+fPushButton->frameSize().width()-10
              ,global.y()+fPushButton->frameSize().height());
   popup->resize(2  + 3*(wSize+1) +1, 2 + 10*(hSize+1) + 2);
   if ( popup->exec() == QDialog::Accepted ){
      // to emit the ROOT signal
      SetBrush(popup->Brush());
      if (fBrushEmitter) fBrushEmitter->EmitRootSignal(fBrush.GetStyle());
      emit brushSelected(fBrush);
   }
   if (fFakeMenu && (sender() == fFakeMenu) ) QTimer::singleShot(0,fFakeMenu, SLOT(close()) );

}
//________________________________________________________________________________
void TQtPatternSelectButton::SetBrush(UInt_t style) 
{
   // Set the Qt brush style from the ROOT pattern style
   if (UInt_t(fBrush.GetStyle()) != style) {
      fBrush.SetStyle(style);
      fPushButton->SetBrush(fBrush);
   }
}

//________________________________________________________________________________
void TQtPatternSelectButton::SetBrush(const TQtBrush &pattern)
{
   // Set the Qt brush style from the input pattern brush
   fBrush = pattern;
   fPushButton->SetBrush(fBrush);
}
//________________________________________________________________________________
void TQtPatternSelectButton::SetBrush(const QColor &color)
{
   // Set the brush color
   fBrush.SetColor(color);
   fPushButton->SetBrush(fBrush);
}

//______________________________________________________________________________
void TQtPatternSelectButton::languageChange()
{
   setCaption( tr( "Select Pattern" ) );
   if (fPushButton) {
      QToolTip::add( fPushButton, tr( "Current Pattern" ) );
      fPushButton->setWhatsThis(tr( "Your current attribute fill pattern" ) );
   }
}
