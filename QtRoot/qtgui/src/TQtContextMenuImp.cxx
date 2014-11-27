// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtContextMenuImp.cxx,v 1.7 2013/08/30 16:00:23 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TQtContextMenuImp.h"
#include <QMenu>
#include <QClipboard>
#include <QDebug>
#ifndef NoQtWebkit
#  include <QWebView>
#  include <QNetworkProxy>
#  include <QUrl>
#  if QT_VERSION >= 0x40500
#    include <QtNetwork/QNetworkProxyFactory>
#  endif
#endif
#include <QString>
#include "TGQt.h"
#include "TSystem.h"
#include "TQtLock.h"
#include "TCanvas.h"
#include "TClass.h"

#include "TMethod.h"
#include "TBrowser.h"
#include "TMethodArg.h"
#include "TDataType.h"
#include "TMethodCall.h"
#include "TROOT.h"
#include "TEnv.h"

#include "TContextMenuImp.h"
#include "TObjArray.h"
#include "TApplication.h"
#include "TQtApplication.h"
#include "TQtObjectDialog.h"
#include "TObjectExecute.h"


//______________________________________________________________________________
TQtContextMenuImp::TQtContextMenuImp(TContextMenu *c) :  TContextMenuImp(c), fPopupMenu(0)
,fExecute(0)
#ifndef NoQtWebkit
, fHelpWidget(0)
#endif
{
   // Create the ROOT Context menu implmenation and optional WebView
   // to access the class defininition from ROOT Web site
    fExecute = new TObjectExecute();
}
//______________________________________________________________________________
TQtContextMenuImp::~TQtContextMenuImp()
{
   // destroy the WebView if any
#ifndef NoQtWebkit
   delete fHelpWidget; fHelpWidget = 0;
#endif
   // destroy all menu item first
   foreach (TQtMenutItem *it, fItems) { delete it; }
   DeletePopup();
}
//______________________________________________________________________________
void TQtContextMenuImp::DeletePopup()
{
  if (fPopupMenu) {
    fPopupMenu->disconnect(this);
    QMenu   *m = fPopupMenu; fPopupMenu = 0;
    delete m; 
  }
  ClearProperties();
  if (fExecute) {
    TObjectExecute  *e=fExecute; fExecute = 0; 
    delete e;
  }
}
//______________________________________________________________________________
void TQtContextMenuImp::CreatePopup ( TObject * /*object*/ ) 
{}
//______________________________________________________________________________
void TQtContextMenuImp::CreatePopup () {
  TContextMenu *c;

//*-*   Find the parent canvas window

  if ( (c=GetContextMenu()) ) {
    TCanvas *canvas = (TCanvas*)c->GetSelectedCanvas();
    QWidget *parent = 0;
    if (canvas) {
      parent = (QWidget *)TGQt::iwid(canvas->GetCanvasID());
    }
//*-*  Add a title. 
    if (fPopupMenu) delete fPopupMenu;
    fPopupMenu = 0;
    fPopupMenu = new QMenu("ContextMenu",parent);
    fPopupMenu->setSeparatorsCollapsible(false);
    connect(fPopupMenu,SIGNAL(destroyed()),this,SLOT(Disconnect()));
    connect(fPopupMenu,SIGNAL(aboutToShow () ),this,SLOT( AboutToShow() ));
//    fPopupMenu->setCaption("Title");

//*-*  Include the menu title
    TObject *object = c? c->GetSelectedObject() : 0;
    QString titleBar = fContextMenu->CreatePopupTitle(object);
    fPopupMenu->setTitle(titleBar);
    QAction *action = fPopupMenu->addAction(titleBar);
    QFont af = action->font();
    af.setBold(true);
    action->setFont(af);

    fPopupMenu->addSeparator(); fPopupMenu->addSeparator();
//*-*  Include the standard static items into the context menu
    QAction *a = fPopupMenu->addAction("&Inspect",    this,SLOT(InspectCB()));
    a->setToolTip(tr("Open the ROOT Object Inspector"));
    a = fPopupMenu->addAction("&Copy",       this,SLOT(CopyCB()),QKeySequence::Copy);
    a->setToolTip(tr("Copy the object pointer to the clipboard"));
    a = fPopupMenu->addAction("&Browse",     this,SLOT(BrowseCB()));
    a->setToolTip(tr("Open the ROOT Object Browser"));
  }
}
//______________________________________________________________________________
void TQtContextMenuImp::AboutToShow()
{
   // Slot to propagate the QPopupMenu signal further to the TContextMenu "clients"
   if (fPopupMenu) {
        emit AboutToShow(fPopupMenu,GetContextMenu());
   }
}
//______________________________________________________________________________
void TQtContextMenuImp::ClearProperties()
{ }
//______________________________________________________________________________
void  TQtContextMenuImp::Dialog( TObject *object, TMethod *method )
{
  if ( !( object && method ) ) return;
  TQtObjectDialog *d = new TQtObjectDialog(object,method);
  connect(d,SIGNAL(helpRequested()),this,SLOT(HelpCB()));
  if (d->exec() == QDialog::Accepted )  {
    TObjArray *parList = d->GetParamList();
    if (fExecute) fExecute->Execute(object,method,parList);
    // TContextMenu *c=GetContextMenu();
    //  c->Execute(object,method,parList); 
#ifndef NoQtWebkit
    if (fHelpWidget) fHelpWidget->hide();
#endif
  }
  delete d;
}
//______________________________________________________________________________
void  TQtContextMenuImp::Dialog( TObject *object, TFunction *function )
{
  if ( !( object && function ) ) return;
  TQtObjectDialog *d = new TQtObjectDialog(object,(TMethod *)function);
  connect(d,SIGNAL(helpRequested()),this,SLOT(HelpCB()));
  if (d->exec() == QDialog::Accepted )  {
    TObjArray *parList = d->GetParamList();
    if (fExecute) fExecute->Execute(function,parList);
    // TContextMenu *c=GetContextMenu();
    //  c->Execute(0,function,parList); 
#ifndef NoQtWebkit
    if (fHelpWidget) fHelpWidget->hide();
#endif
  }
  delete d;
}
//______________________________________________________________________________
void TQtContextMenuImp::Disconnect()
{
  TQtLock lock;
   // Popup menu has been destroyed from outside
  if (fPopupMenu) fPopupMenu = 0;
}
//______________________________________________________________________________
void  TQtContextMenuImp::DisplayPopup ( Int_t x, Int_t y)
{
  TContextMenu *m = GetContextMenu();
  if (!m) return;

  CreatePopup();

//*-*   Update a popup

  UpdateProperties();

//*-*   Display Popup
  QWidget *w = (QWidget *)fPopupMenu->parent();
  QPoint pop = QPoint(x,y);
  if (w) pop = w->mapToGlobal(QPoint(x,y));
  fPopupMenu->popup(pop);
}
//______________________________________________________________________________
void TQtContextMenuImp::UpdateProperties()
{
  TContextMenu *c = GetContextMenu();
  TObject *object = c? c->GetSelectedObject() : 0;
  if (object)
  {
    //*-*   Change title
    fPopupMenu->setTitle(fContextMenu->CreatePopupTitle(object));
    
    //*-*  Include the "Properties" item "by canvases"
    fPopupMenu->addSeparator();
    QMenu *propertiesMenu = fPopupMenu->addMenu("&Properties");

    //*-*  Create Menu "Properties"

    TClass *classPtr = NULL;
    TMethod *method  = NULL;

    //*-*  Create a linked list
    TList *methodList = new TList();
    object->IsA()->GetMenuItems( methodList );
    TIter next( methodList );
    foreach (TQtMenutItem *it, fItems) { delete it; }
    fItems.clear();
    while ( ( method = (TMethod *) next () ) ) {

       if ( classPtr != method->GetClass() ) {
          //*-*  Add a separator.
          if (classPtr) propertiesMenu->addSeparator();
          classPtr = method->GetClass();
       }
       //*-*  Create a popup item.
       TQtMenutItem *menuItem = new TQtMenutItem(c,method,object);
       fItems.push_back(menuItem);
       propertiesMenu->addAction(method->GetName(),menuItem,SLOT(Exec()));
    }
    // Delete linked list of methods.
    delete methodList;
  }
}
//______________________________________________________________________________
void TQtContextMenuImp::InspectCB()
{
   // Open the ROOT Object Inspector 
  TContextMenu *c = GetContextMenu();
  TObject *object = c? c->GetSelectedObject() : 0;
  if (object) object->Inspect();
}
//______________________________________________________________________________
void TQtContextMenuImp::BrowseCB()
{
   // Open the ROOT Object Browser 
  TContextMenu *c = GetContextMenu();
  TObject *object = c? c->GetSelectedObject() : 0;
  if (object) new TBrowser(object->GetName(),object);
}
//______________________________________________________________________________
void TQtContextMenuImp::CopyCB()
{
   // Copy the object pointer to the system clipboard
  TContextMenu *c = GetContextMenu();
  TObject *object = c? c->GetSelectedObject() : 0;
  QString className = object->ClassName();
  QClipboard *clipboard = QApplication::clipboard();
  clipboard->setText(
               QString("((%1 *)0x%2)->")
              .arg(className)
              .arg((ulong)object,0,16)
              ,clipboard->supportsSelection()
              ?QClipboard::Selection 
              :QClipboard::Clipboard);
}
//______________________________________________________________________________
void TQtContextMenuImp::HelpCB()
{
  // Pop Web page with the class HTML doc from web site defined by "Browser.StartUrl"
#ifdef  NoQtWebkit
   return;
#endif
  TObject *obj = fContextMenu->GetSelectedObject(); 
  if (obj) { 
     QString clname = obj->ClassName(); 
     QString url = gEnv->GetValue("Browser.StartUrl", "http://root.cern.ch/root/html/"); 
     if (url.endsWith(".html",Qt::CaseInsensitive))  url = url.left(url.lastIndexOf("/")+1);
     if (!url.endsWith("/")) url += "/";
     if (fContextMenu->GetSelectedMethod()) { 
         TString smeth = fContextMenu->GetSelectedMethod()->GetName(); 
         TMethod *method = obj->IsA()->GetMethodAllAny(smeth.Data()); 
         if (method) clname = method->GetClass()->GetName(); 
         url += QString("%1.html#%1:%2").arg(clname).arg(smeth.Data()); 
     } else { 
         url += QString("%1.html").arg(clname); 
     } 
#ifndef NoQtWebkit
     if (!fHelpWidget) {
        // Set proxy
        QString http_proxy = gSystem->Getenv("http_proxy");
        if (!http_proxy.isEmpty()) {
           if (!http_proxy.contains("//")) {
              http_proxy = "http://" + http_proxy;
           }
           QNetworkProxy proxy;
           QUrl url(http_proxy);
           proxy.setType(QNetworkProxy::HttpProxy);
           proxy.setHostName(url.host());
           proxy.setPort(url.port());
           QNetworkProxy::setApplicationProxy(proxy); 
        }
#if QT_VERSION >= 0x40500           
        else {
           // attempt to get the system defined proxy
           QList<QNetworkProxy> list = QNetworkProxyFactory::systemProxyForQuery();
           // Find one that suitable for http:
           for (int i = 0; i < list.size(); ++i) {
              if (list.at(i).type() == QNetworkProxy::HttpProxy) {
                QNetworkProxy::setApplicationProxy(list.at(i));
                break;
              }
           }
        }
#endif
        fHelpWidget = new QWebView;
        fHelpWidget->resize(400,400);
     }
     fHelpWidget->setUrl(url);
     fHelpWidget->show();
#endif
  }
}
