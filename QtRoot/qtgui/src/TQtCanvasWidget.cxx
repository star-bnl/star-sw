// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtCanvasWidget.cxx,v 1.9 2013/08/30 16:00:23 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

// Definition of TQtWinget class
// "double-buffere widget

#include "TQtCanvasWidget.h"
#include "TQtWidget.h"
#include <QEvent>
#include <QApplication>

#ifdef WIN32
#include "Windows4Root.h"
#endif

//_____________________________________________________________________________
TQtCanvasWidget::TQtCanvasWidget(QWidget* parent):QMainWindow(parent)
{ 
//   setAttribute(Qt::WA_DeleteOnClose);
}
//_____________________________________________________________________________
void TQtCanvasWidget::ChangeDocking(bool)
{ 
   // The change of the docking may have lead to the TCanvas size change also
   // We should attempt to resize the TCanvas
   // adjustSize();
   ExitSizeEvent(TQtWidget::kFORCESIZE);
   update();
}
//_____________________________________________________________________________
bool TQtCanvasWidget::ExitSizeEvent (int update)
{
  // Disable update during resizing (for the sake of the optimization) 
  QWidget *w = centralWidget();
  if (!w) return FALSE;

  QApplication::sendEvent(w,new QCustomEvent(int(QEvent::User+update)) );
  
  return true;
}
//_____________________________________________________________________________
void TQtCanvasWidget::closeEvent(QCloseEvent *event)
{
   if ( event->spontaneous()) {
      event->ignore();
      emit WMCloseCanvas();
   } else {
      QWidget::closeEvent(event);
   }
}

#ifdef R__WIN32
//_____________________________________________________________________________
bool TQtCanvasWidget::winEvent ( MSG *msg )
{
   // QT 3.x compliant version of the method
   long result;
   return winEvent ( msg , &result);
}
//_____________________________________________________________________________
bool    TQtCanvasWidget::winEvent ( MSG *msg , long * result )
{
   // In your reimplementation of this function, if you want to stop the event 
   // being handled by Qt, return true. If you return FALSE, this native event
   // is passed back to Qt, which translates the event into a Qt event and sends
   // it to the widget. 
  bool res = false;
  switch ( msg->message ) {

  case WM_ENTERSIZEMOVE: { res = ExitSizeEvent(TQtWidget::kENTERSIZEMOVE); break;}
  case WM_EXITSIZEMOVE:  { res = ExitSizeEvent(TQtWidget::kEXITSIZEMOVE);  break;}

  default: break;
  };
  *result = 0;
  return res;
}
#endif
