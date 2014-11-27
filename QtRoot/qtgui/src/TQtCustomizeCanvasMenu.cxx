// @(#)root/qt:$Name:  $:$Id: TQtCustomizeCanvasMenu.cxx,v 1.4 2013/08/30 16:00:24 perev Exp $
// Author: Valeri Fine   12/12/2005
/****************************************************************************
**
** Copyright (C) 2005 by Valeri Fine.  BNL.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/
#include "TQtCustomizeCanvasMenu.h"
#include "TQtContextMenuImp.h"
#include "TQtWidget.h"
#include "TGQt.h"

#include <QContextMenuEvent>
#include <QMouseEvent>
#include <QEvent>
#include <QMenu>

/////////////////////////////////////////////////////////////////////////////////////
//
// TQtCustomizeCanvasMenu class is a Qt event filter.
//
// It catches two events sent to the embedded TCanvas
//
//     QEvent::MouseButtonPress
//     QEvent::ContextMenu
//
//  and emit AboutToShow(QPopupMenu &contextMenu, TContextMenu *rootContextMenu) signal
//
//  This Qt signal can be connected to user's Qt slot to customize 
//  the ROOT Context menu if any
//
// Usage:
//   1. Instantiate and attach the event filter to the TCanvas or TQtWidget object:
//
//      TQtCustomizeCanvasMenu *eventFilter =
//                TQtCustomizeCanvas::installCustomMenu(tQtWidget1);
//
//      where   tQtWidget1 is a TQtWidget pointer
//
//  2. Connect the AboutToShow signal of the TQtCustomizeCanvasMenu object with your
//     Qt Slot
//
//     connect( eventFilter,SIGNAL(AboutToShow(QPopupMenu *,TContextMenu *))
//            , myMainSteeringObject,SLOT(CustomizeIt(QPopupMenu *,TContextMenu *)));
//
//  3. Provide the method to change the "standard" ROOT Context menu:
//
//       void CustomizeIt(QPopupMenu *contextMenu,TContextMenu *rootContyextMenu) 
//       {
//           // Second parameter is optional and may be disregarded.
//           // One can use to garther an extra information about the context
//           contextMenu->insertSeparator();
//           QPopupMenu *customMenu = new QPopupMenu();
//           contextMenu->insertItem("&AtlasDAQ",propertiesMenu);
//           propertiesMenu->insertItem("Idea to customize ROOT menu belongs Andrea Dotti");
//       }
//
//    See: QtRoot/qtExamples/CustomCanvasMenu example foer the working example.
//
//  The idea of the method belongs Andrea Dotti <andrea.dotti@pi.infn.it> from ATLAS
//
/////////////////////////////////////////////////////////////////////////////////////

//_______________________________________________________
TQtCustomizeCanvasMenu *TQtCustomizeCanvasMenu::installCustomMenu(TQtWidget *canvas)
{
  // Install the event filter to customize the embedded TCanvas context menu.
  TQtCustomizeCanvasMenu *filter=0;
  if (canvas) {
     canvas->installEventFilter( filter= new TQtCustomizeCanvasMenu() );
  }
  return filter;
}

//_______________________________________________________
TQtCustomizeCanvasMenu *TQtCustomizeCanvasMenu::installCustomMenu(TCanvas *canvas)
{
  // Install the event filter to customize the TCanvas *cavas context menu.
  //
  // This is static method, provided for convenience.
  // It behaves essentially like the above function.
  //   
  TQtCustomizeCanvasMenu *filter = 0;
  if (canvas) {
     TQtWidget *widget = (TQtWidget  *)TGQt::iwid(canvas->GetCanvasID());
     filter  =  installCustomMenu(widget);
  }
  return 0;
}
//_______________________________________________________
TQtCustomizeCanvasMenu::~TQtCustomizeCanvasMenu() 
{  if (fContextMenu ) delete fContextMenu;  }

//_______________________________________________________
bool TQtCustomizeCanvasMenu::eventFilter( QObject *o, QEvent *e )
{
  // Qt eventFilter for the  to custiomize the ROOT Canvas TContextMenu
  bool filtered = FALSE;
  
  // Event filter to customize the Canvas behavor
  QMouseEvent        *mouseEvent = 0;
  QContextMenuEvent  *contextEvent = 0;
  
  if (e->type() == QEvent::MouseButtonPress) { 
     mouseEvent = (QMouseEvent *)e;
     fPosition = mouseEvent->pos();
  } else
  if (e->type() == QEvent::ContextMenu) {
     contextEvent = (QContextMenuEvent *)e;
     fPosition = contextEvent->pos();
  }
  
  if ( (contextEvent && (contextEvent->reason() == QContextMenuEvent::Mouse ) )
        || (mouseEvent && (mouseEvent->button() == Qt::RightButton )) ) 
  {
     TQtWidget *widget4EmbeddedCanvas = (TQtWidget *)o;
     filtered = mouseEvent ? true : MakeContextMenu(widget4EmbeddedCanvas);
  } 
  return filtered;
}
//_______________________________________________________
bool TQtCustomizeCanvasMenu::MakeContextMenu(TQtWidget *canvas) 
{
   // Create ROOT Context Menu for the embedded TCanvas.
   if (canvas) {
      if ((fCanvasWidget != canvas) ) {
         if (fContextMenu ) { delete fContextMenu;  fContextMenu = 0; }
         fCanvasWidget =  canvas;
      }
      if (!fContextMenu) {
         fContextMenu = new TContextMenu("CustomeContextMenu");
         // See: TContextMenu::Popup
         TQtContextMenuImp *qtImpl = 
                     (TQtContextMenuImp *)fContextMenu->GetContextMenuImp();
                  
         connect(qtImpl,SIGNAL( AboutToShow(QMenu *,TContextMenu *))
               , this,  SIGNAL( AboutToShow (QMenu *,TContextMenu *)));
      }
      TObject *obj = fCanvasWidget->GetSelected();
      if  (obj) {
         fContextMenu->Popup(
//                 fCanvasWidget->GetSelectedX(),fCanvasWidget->GetSelectedY(),
                   fPosition.x(), fPosition.y()
                 , obj,fCanvasWidget->GetCanvas());
      }
   }
   return true;
}
