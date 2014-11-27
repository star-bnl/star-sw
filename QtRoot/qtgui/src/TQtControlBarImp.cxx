// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtControlBarImp.cxx,v 1.5 2013/08/30 16:00:23 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TQtControlBarImp.h"
#include "TControlBar.h"
#include "TClass.h"
#include "TControlBarButton.h"

#include <QApplication>

#include <QPushButton>
#include <QToolTip>
#include <QLayout>

#include <Q3VBoxLayout>
#include <Q3HBoxLayout>
//______________________________________________________________________________
TQtControlBarItem::TQtControlBarItem( TControlBarButton *b,QPushButton *i)
 :fButton(b),fButtonImp(i)
{
  // Find TMethod("Action)
  TClass *buttonClass = b->Class();
  fActionMethod = buttonClass->GetMethodAny("Action");
}

//______________________________________________________________________________
void TQtControlBarItem::Exec(bool on)
{
  if (on) { 
    fExecute.Execute(fButton,fActionMethod);
    fButtonImp->toggle(); 
  } 
}
//______________________________________________________________________________
TQtControlBarImp::TQtControlBarImp(TControlBar *c, const char *title):
 QObject(0,title),TControlBarImp(c,title),fWidget(0),fClicked(0)
 {
    fItems.setAutoDelete(true);
    fXpos = -999;       //Initial x position
    fYpos = -999;       //Initial y position
 }
//______________________________________________________________________________
TQtControlBarImp::TQtControlBarImp(TControlBar *c, const char *title,Int_t x, Int_t y):
 QObject(0,title),TControlBarImp(c,title,x,y),fWidget(0),fClicked(0)
{
    fItems.setAutoDelete(true);
    fXpos = x;       //Initial x position
    fYpos = y;       //Initial y position
}
//______________________________________________________________________________
TQtControlBarImp::~TQtControlBarImp() {
  if (fWidget) {
    fWidget->disconnect(this);
    delete fWidget; 
    fWidget = 0;
  }
}
//______________________________________________________________________________
void TQtControlBarImp::Create()
{ }
//______________________________________________________________________________
void TQtControlBarImp::Disconnect()
{   
   fWidget = 0; delete this;     
}
//______________________________________________________________________________
void TQtControlBarImp::Hide(){ if (fWidget) fWidget->hide(); }
//______________________________________________________________________________
void TQtControlBarImp::Clicked(bool)
{
   TQtControlBarItem *item = (TQtControlBarItem*)sender();
   fClicked = item->GetClicked();  
}

//______________________________________________________________________________
TControlBarButton *TQtControlBarImp::GetClicked()
{
   TControlBarButton *lastButton = fClicked;
   return lastButton;
}
//______________________________________________________________________________
void TQtControlBarImp::Show() 
{ 
  if (!fWidget) {
    TControlBar *bar = GetControlBar();
    TList *buttons = bar ? bar->GetListOfButtons() : 0;
    if (bar && buttons) {
      fWidget = new QWidget(0,"controlbar",Qt::WDestructiveClose);
      fWidget->setCaption(name());
      QObject::connect(fWidget,SIGNAL(destroyed()),this,SLOT(Disconnect()));
      if (fXpos > 0 && fYpos > 0) fWidget->move(fXpos,fYpos);
      Bool_t vertical = bar->GetOrientation() == TControlBar::kVertical;
      Q3BoxLayout *layout = 0;
      if (vertical) {layout = new Q3VBoxLayout( fWidget );}
      else          {layout = new Q3HBoxLayout( fWidget );}
      layout->setAutoAdd( true );

      TIter nextButton(buttons);
      TControlBarButton *button = 0;
      while (( button = (TControlBarButton *)nextButton()) ) {
        QPushButton *b = new QPushButton(button->GetName(),fWidget);
        b->setToggleButton(true);
        QToolTip::add ( b, button->GetTitle() );
        TQtControlBarItem *barItem = new TQtControlBarItem(button,b);
        fItems.push(barItem);
        connect(b,SIGNAL(toggled(bool)),barItem,SLOT(Exec(bool)));
        connect(b,SIGNAL(toggled(bool)),barItem,SIGNAL(ClickedItem(bool)));
        connect(barItem,SIGNAL(ClickedItem(bool)),this,SLOT(Clicked(bool)));
      }
    }
  }
  fWidget->show();
}
