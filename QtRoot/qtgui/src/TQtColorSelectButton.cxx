// @(#)root/gui:$Name:  $:$Id: TQtColorSelectButton.cxx,v 1.10 2013/08/30 16:00:23 perev Exp $
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
// TQtColorFrame, TQt16ColorSelector, TQtColorPopup and TQtColorSelectButton.     //
//                                                                      //
// The TQtColorFrame is a small frame with border showing a specific     //
// color.                                                               //
//                                                                      //
// The TQt16ColorSelector is a composite frame with 16 TQtColorFrames.  //
//                                                                      //
// The TQtColorPopup is a popup containing a TQt16ColorSelector and a   //
// "More..." button which popups up a QColorDialog allowing custom      //
// color selection.                                                     //
//                                                                      //
// The TQtColorSelectButton widget is like a checkbutton but instead of the //
// check mark there is color area with a little down arrow. When        //
// clicked on the arrow the TQtColorPopup pops up.                       //
//                                                                      //
// Selecting a color in this widget will generate the event:            //
// kC_COLORSEL, kCOL_SELCHANGED, widget id, pixel.                      //
// and the signal:                                                      //
// ColorSelected(ULong_t pixel)                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

// #include "TGClient.h"
#include "TGQt.h"
#include "TQtColorSelectButton.h"
#include "TQtColorSelect.h"
#include "TColor.h"

#include <QColorDialog> 
#include <QToolTip> 
#include <QColor> 
#include <QApplication> 

#include <QStyle>
#include <QVariant>
#include <QLayout>
#include <QWhatsThis>
#include <QToolButton>
#include <QFrame>
#include <QHBoxLayout>
#include <QStyleOptionButton>
#include <QButtonGroup>
#include <QDebug>
#include <QMenu>
#include <QTimer>

//ClassImp(TQtColorFrame)
//ClassImp(TQt16ColorSelector)
//ClassImp(TQtColorPopup)
//ClassImp(TQtColorSelectButton)

static int boxSize = 22;

TQtColorPopup *TQtColorPopup::fgColorPopup = 0;

//________________________________________________________________________________
TQtColorFrame::TQtColorFrame(QWidget *p, const QColor &color, Int_t n): QToolButton(p),fActive(n),fColor()
{
   SetColor(color);
   if (fActive == -1) {
      QSizePolicy szp = sizePolicy();
      szp.setHorizontalPolicy ( QSizePolicy::Expanding );
      setSizePolicy(szp);
   }
   setToolTip(fColorTipLabel + fColor.name());
   languageChange();
  //  resize(10,10);
}
//________________________________________________________________________________
QSize TQtColorFrame::sizeHint () const 
{
   return fActive != -1 ? QSize(2*boxSize,2*boxSize): size();
}
//________________________________________________________________________________
void TQtColorFrame::drawButtonLabel(QPainter *paint)
{
     if(paint) {}
     //QStyleOptionButton button;
     //QRect buttonDraw = QApplication::style()->subElementRect(QStyle::SE_PushButtonContents,&button);
}
//______________________________________________________________________________
void TQtColorFrame::languageChange()
{
   fColorTipLabel = tr("Color ");
}
//________________________________________________________________________________
void TQtColorFrame::SetColor(const QColor &color)
{
   if (color != fColor) {
      fColor = color;
      QSize iSize = (fActive == -1) ? size() - QSize(0,3) : QSize(boxSize-6,boxSize-6);
      setIconSize(iSize);
      QPixmap px(iSize);
      px.fill(fColor);
      setIcon(px);
   }
}

//________________________________________________________________________________
TQt16ColorSelector::TQt16ColorSelector( QWidget *p,const char *name) :
   QFrame (p),fActive(-1)
{
   if (!name)
      setName("ColorSelect16");
   else 
      setName(name);
   // setGeometry(QRect(0,0,200,200));
   
   QWidget *group          = this;
   QGridLayout *gridLayout = new QGridLayout(group);
   QButtonGroup *buttonGroup = new QButtonGroup(group);
   gridLayout->setMargin(2);  
   gridLayout->setSpacing(1); 
   group->setGeometry(QRect(0,0,2 + 4*(boxSize+1) +1, 2 + 4*(boxSize+1) +1));

   languageChange();
   
   Color_t  defColors[] = {  0, 1 , 2 , 3 
                           , 4, 5 , 6 , 7
                           , 8, 9 ,30 ,38
                           , 41,42,50 ,51 
   };
   int i =0;
//   int nDefColors = sizeof(defColors)/sizeof(Color_t);
   for (int k=0;k < 4;k++) {
      for (int j=0;j < 4;j++,i++) {
         fCe[i] = new TQtColorFrame(group,gQt->ColorIndex(gQt->UpdateColor(defColors[i])), i);
#if QT_VERSION >= 0x40000
         gridLayout->addWidget(fCe[i],j,k);
         buttonGroup->addButton(fCe[i],i);
#endif     
      }
  }
  connect(buttonGroup,SIGNAL(buttonClicked(int)),this,SLOT(SetActiveSlot(int)));
 }
//________________________________________________________________________________
TQt16ColorSelector::~TQt16ColorSelector()
{ }
//________________________________________________________________________________
void TQt16ColorSelector::SetActiveSlot()
{
   TQtColorFrame *colorFrame = (TQtColorFrame*)sender();
   colorFrame->GetColor();
   emit ColorChanged(colorFrame->GetColor());
}
//________________________________________________________________________________
void TQt16ColorSelector::SetActiveSlot(Int_t newat)
{ 
   if (fActive != newat ) {
      fActive = newat;
      emit ColorChanged(GetActiveColor());
   }
}
//________________________________________________________________________________
void TQt16ColorSelector::SetActive(Int_t newat)
{
   if ( fActive != newat)  {
      SetActiveSlot(newat);
      // setButton(newat);
   }
}
//________________________________________________________________________________
const QColor  &TQt16ColorSelector::GetActiveColor() const
{
   const TQtColorFrame &activeButton = *fCe[GetActive()];
   return activeButton.GetColor();
}
//______________________________________________________________________________
void TQt16ColorSelector::languageChange()
{
    //setCaption( tr( "Select Color" ) );
    //fPushButton->setText( tr( "pushButton39" ) );
    //QToolTip::add( fPushButton, tr( "Current Color" ) );
    //QWhatsThis::add( fPushButton, tr( "Your current attribute fill color" ) );
}
//________________________________________________________________________________
TQtColorPopup::TQtColorPopup(QWidget *p, QColor &color,const char *name, bool modal, Qt::WindowFlags f) :
   QDialog(p,f)
   ,fCurrentColor(color)
{ 
   QFrame *inter = new QFrame(this);
   inter->setFrameShape(QFrame::Panel);
   QVBoxLayout *vLayout = new QVBoxLayout(inter);
   setName(name);
   setModal(modal);
   inter->setGeometry( QRect( 2, 2, 2 + 4*(boxSize+1) +1 ,2 + 5*(boxSize+1) + boxSize/2 + 4 ) );
   TQt16ColorSelector *cs = new TQt16ColorSelector(inter);
   
   // The horizontal divider 
   QFrame *line1 = new QFrame( inter, "lineH" );
   line1->setFrameShape ( QFrame::HLine  );
   line1->setFrameShadow( QFrame::Sunken );

   QPushButton *other     = new QPushButton("Other...",inter);
   other->setAutoDefault(true); 
   
   vLayout->addWidget(cs);
   vLayout->addWidget(line1);
   vLayout->addWidget(other);
   
   vLayout->setMargin(0);
   vLayout->setSpacing(0);
   other->setToolTip(tr("Popups up Color Selector"));
   connect(cs,SIGNAL(ColorChanged(const QColor &)), this, SLOT(ColorSelected(const QColor &)));
   connect(other,SIGNAL(clicked()), this, SLOT(ColorSelectDialog()));
   adjustSize();
}
//________________________________________________________________________________
TQtColorPopup *TQtColorPopup::Create(QWidget *p, QColor &color,const char *name, bool modal, Qt::WindowFlags f) 
{ 
  // Create the singletone object
  if (!fgColorPopup) 
     fgColorPopup =  new TQtColorPopup(p, color,name, modal, f);
  return fgColorPopup;
}
      
//________________________________________________________________________________
TQtColorPopup::~TQtColorPopup()
{ }

//________________________________________________________________________________
void TQtColorPopup::ColorSelectDialog()
{
   ColorSelected(QColorDialog::getColor(fCurrentColor, this));
}
//________________________________________________________________________________
void TQtColorPopup::ColorSelected(const QColor &color )
{   
   if (fCurrentColor != color) {
      fCurrentColor = color;
   }
   accept();
}
//______________________________________________________________________________
void TQtColorPopup::languageChange()
{
    //setCaption( tr( "Select Color" ) );
    //fPushButton->setText( tr( "pushButton39" ) );
    //QToolTip::add( fPushButton, tr( "Current Color" ) );
    //QWhatsThis::add( fPushButton, tr( "Your current attribute fill color" ) );
}
//________________________________________________________________________________
TQtColorSelectButton::TQtColorSelectButton( QWidget *p,  const char *name,Qt::WindowFlags f)
    : QFrame(p,f)
      ,fColor("grey"),fColorEmitter(0), fFakeMenu(0)
{
   if (name) setName(name);
   CreateWidget();
}

//________________________________________________________________________________
TQtColorSelectButton::TQtColorSelectButton( QWidget *p) : QFrame(p)
      ,fColor("grey"),fColorEmitter(0), fFakeMenu(0)
{
   CreateWidget();
}

//________________________________________________________________________________
TQtColorSelectButton::TQtColorSelectButton( QWidget *p, UInt_t pixel, Int_t /*id*/,TColorEmit *emitter) 
    : QFrame(p)
      ,fColor(gQt->QtColor(pixel)),fColorEmitter(emitter), fFakeMenu(0)
{
   setName("ColorSelectButton");
   setWindowFlags (Qt::WindowStaysOnTopHint);
   CreateWidget();
}
//________________________________________________________________________________
TQtColorSelectButton::TQtColorSelectButton( QWidget *p, QColor &color, Int_t /*id*/,TColorEmit *emitter) 
    : QFrame(p),fColor(color),fColorEmitter(emitter), fFakeMenu(0)
{
   setName("ColorSelectButton");
   CreateWidget();
}
//________________________________________________________________________________
void TQtColorSelectButton::CreateWidget() 
{
    setSizePolicy(QSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed,
                                 QSizePolicy::ComboBox));
    setLineWidth(1);
    //setFrameShape( QFrame::StyledPanel );
    setFrameShadow( QFrame::Raised );
    //setFrameShadow(QFrame::Plain);
    
       
    //  Color Button
    QHBoxLayout *layout = new QHBoxLayout( this, 0, 0, "layoutColorButton"); 
    fPushButton = new  TQtColorFrame(this, fColor);
    ((TQtColorFrame *)fPushButton)->setPopupMode( QToolButton::MenuButtonPopup); 
    if (!fFakeMenu) {
       // Add fake menu to force the "real" Dialog popup
       fFakeMenu = new QMenu(this);
       ((TQtColorFrame *)fPushButton)->setMenu(fFakeMenu);
       connect(fFakeMenu,SIGNAL(aboutToShow()), this, SLOT(PopupDialog()));
    }

    layout->addWidget(fPushButton);
    connect(fPushButton,SIGNAL(clicked()),this, SLOT(PopupDialog()));
    // The arrow button
//      fArrowButton = new QToolButton(this);
//      fArrowButton->setArrowType(Qt::DownArrow);
      
    languageChange();
}
//________________________________________________________________________________
TQtColorSelectButton::~TQtColorSelectButton()
{ }

//________________________________________________________________________________
void TQtColorSelectButton::PopupDialog()
{
   TQtColorPopup *popup = TQtColorPopup::Create(0,fColor);
   QPoint global = 
      fPushButton->mapToGlobal(QPoint(0,0));
   popup->move(global.x()+fPushButton->frameSize().width()-10
              ,global.y()+fPushButton->frameSize().height());
   if ( popup->exec() == QDialog::Accepted ){
      // to emit the ROOT signal
      SetColor(popup->Color());
      ColorSelected();
      if (fColorEmitter) fColorEmitter->ColorEmit(fColor.pixel());
      emit colorSelected(fColor);
   }
   if (fFakeMenu && (sender() == fFakeMenu) ) QTimer::singleShot(0,fFakeMenu, SLOT(close()) );
}
//________________________________________________________________________________
void TQtColorSelectButton::SetColor(const QColor &color)
{
   // Set color.
   if (fColor != color) {
      fColor = color;
      ((TQtColorFrame *)fPushButton)->SetColor(color);
      //QPalette palette;
      //palette.setColor(fPushButton->backgroundRole(), color);
      //palette.setColor(fPushButton->foregroundRole(), color);
      //fPushButton->setPalette(palette);
      update();
   }
}

//______________________________________________________________________________
void TQtColorSelectButton::languageChange()
{
    setCaption( tr( "Select Color" ) );
    if (fPushButton) {
       fPushButton->setText( tr( "pushButton39" ) );
       fPushButton->setToolTip(tr( "Current Color" ) );
       fPushButton->setWhatsThis( tr( "Your current attribute fill color" ) );
    }
}
