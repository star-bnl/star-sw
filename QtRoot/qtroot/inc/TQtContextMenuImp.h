// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtContextMenuImp.h,v 1.2 2013/08/30 16:00:27 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// TQtContextMenuImp                                                          //
//                                                                            //
// This class provides an interface to  context sensitive popup menus.        //
// These menus pop up when the user hits  the right mouse button,  and        //
// are destroyed when the menu pops downs.                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TQtContextMenuImp
#define ROOT_TQtContextMenuImp

#include "TContextMenuImp.h"
#include "TContextMenu.h"

#include <qglobal.h>
#include <qobject.h>
#include <QVector>
#define NoQtWebkit

// *-*
// *-* Context Menu is derived from QPopupMenu (since it is special type of PopUp menu
// *-*   with
// *-*
// *-*    TQtMenuItem  fTitle
// *-*    TQtMenuItem  fProperties
// *-*
// *-*   where
// *-*
// *-*     fTitle      is the first item of the menu
// *-*     fProperties is the last one
// *-*     fWindowsObj is a pointer to the parent Windows object
// *-*     ("normal" menu has no direct relation with any Windows objects)
// *-*

class TQtDialog;
class QEvent;
//MOC_SKIP_BEGIN
  class QMenu;
//MOC_SKIP_END

class TQtMenutItem : public QObject {

  Q_OBJECT

private:
  TMethod *fMethod;
  TObject *fObject;
  TContextMenu *fContextMenu;
public:
  TQtMenutItem( TContextMenu *menu,TMethod *m,TObject *o): fMethod(m),fObject(o),fContextMenu(menu){}
  ~TQtMenutItem() { }

public slots:
  void Exec(){ fContextMenu->Action(fObject, fMethod); }
};

class TObjectExecute;
#ifndef NoQtWebkit
class QWebView;
#endif
class TQtContextMenuImp : public QObject, public TContextMenuImp 
{

 Q_OBJECT

 private:

   QMenu   *fPopupMenu;
   QVector<TQtMenutItem *> fItems;
   TObjectExecute  *fExecute;
#ifndef NoQtWebkit
   QWebView        *fHelpWidget;
#endif
   virtual void  ClearProperties();
           void  CreatePopup  ();

   virtual void  UpdateProperties();

 public:

    TQtContextMenuImp(TContextMenu *c=0);
    virtual ~TQtContextMenuImp();
    virtual void       CreatePopup  ( TObject *object );
    virtual void       Dialog       ( TObject *object, TMethod *method );
    virtual void       Dialog       ( TObject *object, TFunction *function);

    virtual void       DisplayPopup ( Int_t x, Int_t y);
            void       DeletePopup();
    QMenu &PopupMenu() const { return *fPopupMenu; }

    virtual bool   event(QEvent *){return FALSE;}
    
 protected slots:
   void Disconnect();

 public slots:

   void  AboutToShow();
   void  CopyCB();
   void  InspectCB();
   void  BrowseCB();
   void  HelpCB();
signals:
   void AboutToShow(QMenu *, TContextMenu *);
    // ClassDef(TQtContextMenuImp,0) //Context sensitive popup menu implementation
};
#endif
