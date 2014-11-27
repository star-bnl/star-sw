// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtRootBrowserImp.cxx,v 1.10 2013/08/30 16:00:25 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtRootBrowserImp                                                    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtRootBrowserImp.h"
#include "TQtRootBrowserAction.h"
#include "TQtRootAction.h"
#include "TGQt.h"

#include "TApplication.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TContextMenu.h"
#include "TBrowser.h"
#include "TFile.h"

#ifdef R__WIN32
#  include "TWinNTSystem.h"
#endif

#include "TQtBrowserImp.h"
#include "TQtIconBrowserImp.h"

#if QT_VERSION < 0x40000
#  include "qpopupmenu.h"
#  include "qiconview.h"
#  include "qhbox.h"
#  include "qmainwindow.h"
#  include "qfiledialog.h"
#  include "qtoolbar.h"
#  include "qdragobject.h" 
#else /* QT_VERSION */
#  include <QMenu>
#  include <q3iconview.h>
#  include <q3hbox.h>
#  include <QMainWindow>
#  include <q3filedialog.h>
#  include <QToolBar>
#  include "q3dragobject.h" 
#  include <QImageWriter>
//Added by qt3to4:
#  include <QActionGroup>
#  include <QDropEvent>
#  include <Q3ValueList>
#endif /* QT_VERSION */

#include "qlabel.h"
#include "qapplication.h"
#include "qpixmap.h"
#include "qsplitter.h"
#include "qmenubar.h"
#include "qmessagebox.h"
#include "qstatusbar.h"
#include "qimage.h"
#include "qclipboard.h"
#include "qapplication.h"
#include "qprinter.h"
#include "qpainter.h"

enum {
   M_NEW_BROWSER,
   M_NEW_CANVAS, 
   M_OPEN_FILE,  
   M_FILE_SAVE,  
   M_FILE_SAVEAS,  
   M_FILE_COPY,  
   M_FILE_PRINT, 
   M_FILE_CLOSE, 
   M_FILE_QUIT,  
   M_VIEW_TOOLBAR,
   M_VIEW_STATBAR,

   M_VIEW_LARGE,  
   M_VIEW_SMALL,  
   M_VIEW_LIST,   
   M_VIEW_DETAIL, 
   M_VIEW_ICONS,  
   M_VIEW_LINEUP, 
   M_VIEW_REFRESH,
   M_HELP_MAIN,   
   M_ABOUT_MAIN
};

//  Create our own version of QMainWindow to catch the QDropEvent
#if QT_VERSION < 0x40000
class RootMainWindows : public QMainWindow {
#else /* QT_VERSION */
class RootMainWindows : public QMainWindow {
#endif /* QT_VERSION */
protected:
   TList *fFileList;
public:
#if QT_VERSION < 0x40000
   RootMainWindows( QWidget* parent=0, WFlags f = WType_TopLevel ) :
      QMainWindow( parent, (const char*)0, f ), fFileList(0){setAcceptDrops(true);}
#else /* QT_VERSION */
   RootMainWindows( QWidget* parent=0, Qt::WindowFlags f = Qt::WType_TopLevel ) :
      QMainWindow( parent, f ), fFileList(0){ setAcceptDrops(true);   }
#endif /* QT_VERSION */
      ~RootMainWindows() {
         if (fFileList) {
            // Clean garbage. 
            // This may cause a crash of other instances of the TBrowser thought :-(((
            TList *garbage =  fFileList; fFileList = 0;
            TList *rootList = gROOT->GetListOfBrowsables();
            TIter next(garbage); TObject *obj = 0;
            while ( (obj = next()) ) { rootList->Remove(obj); delete obj; }
            delete garbage; } 
      }
protected:
   // Overloaded method
   virtual  void dropEvent ( QDropEvent *event )
   {
      QString text;
#if QT_VERSION < 0x40000
      if ( QTextDrag::decode(event, text) ) {
         QString fileName = QUriDrag::uriToLocalFile((const char*)text);
#else /* QT_VERSION */
      if ( Q3TextDrag::decode(event, text) ) {
         QString fileName = Q3UriDrag::uriToLocalFile((const char*)text);
#endif /* QT_VERSION */
         // remove errat "end-of-line" if any
         fileName.remove('\n'); fileName.remove('\r');
         QFileInfo file(fileName);
         if (file.isDir ()) {
#if 0
            // It is too fragile. Let's postpone its implementation. 24.01.2004. V.Fine
            // add it to the list of the browsable
            TSystemDirectory *workdir = new TSystemDirectory("workdir",(const char *)file.absFilePath ());
            if (!fFileList) fFileList = new TList(); fFileList->Add(workdir);
            gROOT->GetListOfBrowsables()->Add(workdir);
#endif
         } else {
             // Check ROOT file
            TFile *rootFile = TFile::Open( (const char *)file.absFilePath () );
            if (rootFile->IsZombie()) delete rootFile;
         }
         event->accept(true);
      } else {
         event->accept(false);
      }
   }
};

TQtBrowserMenuItem_t menu_data[] = {
   {"&New Browser",  M_NEW_BROWSER,  Qt::CTRL+Qt::Key_N, "Open a new ROOT Browser", "" },
   {"New C&anvas",   M_NEW_CANVAS,   Qt::CTRL+Qt::Key_A, "Open a new ROOT TCanvas", "" },
   {"&Open",         M_OPEN_FILE,    Qt::CTRL+Qt::Key_O, "Open a new ROOT file",    "" },
   {"&Save",         M_FILE_SAVE,    Qt::CTRL+Qt::Key_S, "Save the image of the TBrowser frame in the current file",    "hdisk_t.xpm" },
   {"Save &as",      M_FILE_SAVEAS,  0,                  "Save the image of the TBrowser frame in the selected file",   "hdisk_t.xpm" },
   {"&Copy",         M_FILE_COPY,    Qt::CTRL+Qt::Key_C, "Copy the image of the TBrowser frame to the system clipboard",""            },
   {"&Print",        M_FILE_PRINT,   Qt::CTRL+Qt::Key_P, "Print this window up",     "printer_s.xpm"},
   {"Close",         M_FILE_CLOSE,   Qt::ALT+ Qt::Key_F4, "Close this window",       ""          },
   {"&Quit",         M_FILE_QUIT,    Qt::CTRL+Qt::Key_X, "Quit the ROOT application",""          },
   {"&Toolbar",      M_VIEW_TOOLBAR, 0,                  "Show/Hide the toolbar",    ""},
   {"Status &Bar",   M_VIEW_STATBAR, 0,                  "Show/Hide the status bar", ""},

   {"Lar&ge Icons",   M_VIEW_LARGE,  0,                  "Show large icon view",     "tb_bigicons.xpm"},
   {"S&mall Icons",   M_VIEW_SMALL,  0,                  "Show small icon view",     "tb_smicons.xpm" },
   {"&List",          M_VIEW_LIST,   0,                  "Show list view",           "tb_list.xpm"    },
   {"&Details",       M_VIEW_DETAIL, 0,                  "Show detail view",         "tb_details.xpm" },
   {"Arrange &Icons", M_VIEW_ICONS,  0,                  "Arrange &Icons",           ""               },
   {"Lin&e up icons", M_VIEW_LINEUP, 0,                  "Lin&e up icons",           ""               },
   {"&Refresh",       M_VIEW_REFRESH,0,                  "", ""},
   {"&Help",          M_HELP_MAIN,   0,                  "", ""},
   {"&About",         M_ABOUT_MAIN,  0,                  "", ""},
   {0,0,0,"",""}
};

//______________________________________________________________________________
TQtRootBrowserImp::TQtRootBrowserImp(TBrowser *b,bool initFlag): TBrowserImp(b)
  , fBrowserImpID(0), fMenuBar(0), fTreeView(0),fIconView(0),fX(0),fY(0),fWidth(0),fHeight(0)
  , fTitle(""), fViewActions(0)
  { 
  if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtRootBrowserImp::TQtRootBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height,bool initFlag)
 : TBrowserImp(b) 
 , fBrowserImpID(0), fMenuBar(0), fTreeView(0),fIconView(0),fX(0),fY(0),fWidth(width),fHeight(height),fTitle(title)
 , fViewActions(0)
{
  if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtRootBrowserImp::TQtRootBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag)
: TBrowserImp(b) 
, fBrowserImpID(0), fMenuBar(0), fTreeView(0),fIconView(0), fX(x),fY(y),fWidth(width),fHeight(height),fTitle(title)
{
  if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtRootBrowserImp::~TQtRootBrowserImp() 
{
    if (fBrowserImpID) {
      QWidget *w = fBrowserImpID; 
      fBrowserImpID = 0;
      w->deleteLater ();
    }
}
//______________________________________________________________________________
void TQtRootBrowserImp::Add(TObject *obj, const char *caption)
{
   // Add object to iconbox. Class is used to get the associated icons
   // via the mime file (see GetObjPictures()).
   
   Add(obj,caption, -1);
}
//______________________________________________________________________________
void TQtRootBrowserImp::Add(TObject *obj, const char *caption, Int_t check  ) 
{
   Add(obj,caption,kFALSE);
}

//______________________________________________________________________________
void TQtRootBrowserImp::Add(TObject *obj, const char *caption, Bool_t firstFlag) 
{ 
  if (!fBrowserImpID) InitWindow();
  const char *n = caption;
  if (!n) n = obj->GetName();
  if (fTreeView) fTreeView->Add(obj,n);
  // ONly container should be added to the 
  if (fIconView && !firstFlag) fIconView->Add(obj,n);
}
//______________________________________________________________________________
void  TQtRootBrowserImp::AddCheckBox(TObject *obj, Bool_t check)
{  
   // Add a checkbox in the TGListTreeItem corresponding to obj
   // and a checkmark on TGLVEntry if check = kTRUE.
}
//______________________________________________________________________________
void  TQtRootBrowserImp::CheckObjectItem(TObject *obj, Bool_t check)
{ 
   // Check / uncheck the TGListTreeItem corresponding to this
   // object and add a checkmark on TGLVEntry if check = kTRUE.
}
//______________________________________________________________________________
void  TQtRootBrowserImp::RemoveCheckBox(TObject *obj)
{
  // Remove checkbox from TGListTree and checkmark from TGListView.
}
//______________________________________________________________________________
void   TQtRootBrowserImp::SetDrawOption(Option_t *option)
{ 
   // Sets drawing option.
}
//______________________________________________________________________________
Option_t *TQtRootBrowserImp::GetDrawOption() const
{  
   // Returns drawing option
   return "";
}
//______________________________________________________________________________
void TQtRootBrowserImp::BrowseObj(TObject *obj) 
{ 
  if (obj) {
    // Add(obj,0,kTRUE);
    TBrowser *b = Browser(); 
    if (b) {
#ifdef OLD
      obj->Browse(b);
#else
       if (fTreeView) fTreeView->BrowseObj(obj);
       if (fIconView) fIconView->BrowseObj(obj);
#endif
    }
  }
}
//______________________________________________________________________________
void TQtRootBrowserImp::DisplayTotal(Int_t total, Int_t selected)
{
   // Display in statusbar total number of objects and number of
   // selected objects in IconBox.

   char tmp[64];
   const char *fmt;

   if (selected)
      fmt = "%d Object%s, %d selected.";
   else
      fmt = "%d Object%s.";

   sprintf(tmp, fmt, total, (total == 1) ? "" : "s", selected);
   // fBrowserImpID->statusBar()->SetText(tmp, 0);
}

//______________________________________________________________________________
void TQtRootBrowserImp::Disconnect()
{
   if (fBrowserImpID) {
      fBrowserImpID = 0;
      TBrowser *b = Browser(); 
      if (b && b->IsOnHeap() )  delete b;
    }
}
//______________________________________________________________________________
QWidget *TQtRootBrowserImp::GetBrowserID()
{ 
  return fBrowserImpID; 
}
//______________________________________________________________________________
void TQtRootBrowserImp::ExecuteDefaultAction(TObject *obj) {
   TQtRootBrowserAction::Instance()->ExecuteDefaultAction(obj);
}
//______________________________________________________________________________
void TQtRootBrowserImp::Iconify() { if (fBrowserImpID) fBrowserImpID->hide(); }
//______________________________________________________________________________
void TQtRootBrowserImp::RecursiveRemove(TObject * /*obj*/) { }
//______________________________________________________________________________
void TQtRootBrowserImp::Refresh(Bool_t /*flag*/) { }
//______________________________________________________________________________
void TQtRootBrowserImp::Show() { 
   if (!fBrowserImpID) InitWindow();
   if (fBrowserImpID)  {
#if defined(WIN320)
      // raising the window under MS Windows needs some extra effort.
      HWND h = fBrowserImpID->winId();
      SetWindowPos(h,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW);
      SetWindowPos(h,HWND_NOTOPMOST,0,0,0,0,SWP_NOMOVE | SWP_NOSIZE );
#endif
      fBrowserImpID->show(); 
   }
}

//______________________________________________________________________________
Int_t TQtRootBrowserImp::InitWindow()
{
  fBrowserImpID = new RootMainWindows(0,Qt::WDestructiveClose|Qt::WType_TopLevel);
  fBrowserImpID->setName(Browser()->GetName());
  connect(fBrowserImpID,SIGNAL(destroyed()),this,SLOT(Disconnect()));
  fBrowserImpID->setCaption(fTitle);

#if QT_VERSION < 0x40000
  QWidget *central = new QHBox(fBrowserImpID,"CentralRootBrowser");
#else /* QT_VERSION */
  QWidget *central = new Q3HBox(fBrowserImpID,"CentralRootBrowser");
#endif /* QT_VERSION */
  fBrowserImpID->setCentralWidget (central);

  if (fX*fY) fBrowserImpID->setGeometry(fX,fY,fWidth,fHeight);
  else fBrowserImpID->resize(fWidth,fHeight);
  
  // to avoid calling some virtual methods from ctor's, 
  // - create the object 
  // - initialize the windows separately
  fTreeView = new TQtBrowserImp(Browser(),false);
  fIconView = new TQtIconBrowserImp(Browser(),false);

  fTreeView->InitWindow();
  fIconView->InitWindow();

  connect(fTreeView,SIGNAL(OpenFolder(TObject *)),           fIconView,SLOT(Clear(TObject *)));
  connect(fTreeView,SIGNAL(FolderExpanded(TObject *,Bool_t)),fIconView,SLOT(FolderExpanded(TObject *,Bool_t)));
  connect(fTreeView,SIGNAL(ItemOpen(TObject *)),             fIconView,SLOT(BrowseObject(TObject *)));
  connect(fIconView,SIGNAL(SwitchTreeView(Int_t)),           fTreeView,SLOT(SwitchParent(Int_t)));
  connect(fIconView,SIGNAL(ActivateParent(TObject *)),       fTreeView,SLOT(MakeParentActive(TObject *)));
  connect(fIconView,SIGNAL(ActivateObject(TObject *)),       fTreeView,SLOT(ChangeActive(TObject *)));
  connect(fTreeView,SIGNAL(CanBeUpdated(Bool_t)),            fIconView,SLOT(EnableUpdates(Bool_t)));

  

  QSplitter *split = new QSplitter(central);
#if QT_VERSION < 0x40000
  QValueList<int> sizes;
#else /* QT_VERSION */
  Q3ValueList<int> sizes;
#endif /* QT_VERSION */

  sizes.append(  fWidth/3);
  sizes.append(2*fWidth/3);

  QWidget *treeWidget = (QWidget *)fTreeView->GetBrowserID();
  treeWidget->reparent(split,Qt::WStyle_NoBorder,QPoint(0,0));

  QWidget *iconWidget = (QWidget *)fIconView->GetBrowserID();
  iconWidget->reparent(split,Qt::WStyle_NoBorder,QPoint(0,0));
  split->setSizes(sizes);

  MakeActions();
  MakeToolBar();
  MakeStatBar();
  MakeMenu();
  Show();
  return 0;
}
//______________________________________________________________________________
void TQtRootBrowserImp::MakeActions() {
   int i;
   for (i = 0; menu_data[i].fMenuText!=NULL; i++) {
      // skip the separators 
      TQtRootAction *action = new TQtRootAction(fBrowserImpID,menu_data[i]);
      fActions.insert(action->Id(),action);
      connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
   }
   // Create a group action
#if QT_VERSION < 0x40000
   fViewActions = new QActionGroup(fBrowserImpID,"viewActions",true);
   fViewActions->add(fActions[M_VIEW_LARGE] ); fActions[M_VIEW_LARGE] ->setToggleAction ( true );
   fViewActions->add(fActions[M_VIEW_SMALL] ); fActions[M_VIEW_SMALL] ->setToggleAction ( true ); fActions[M_VIEW_SMALL]  ->setOn(true);
   fViewActions->add(fActions[M_VIEW_LIST]  ); fActions[M_VIEW_LIST]  ->setToggleAction ( true );
   fViewActions->add(fActions[M_VIEW_DETAIL]); fActions[M_VIEW_DETAIL]->setToggleAction ( true );  
#else /* QT_VERSION */
   fViewActions = new QActionGroup(fBrowserImpID); fViewActions->setExclusive(true);
   fViewActions->addAction(fActions[M_VIEW_LARGE] ); fActions[M_VIEW_LARGE] ->setCheckable ( true );
   fViewActions->addAction(fActions[M_VIEW_SMALL] ); fActions[M_VIEW_SMALL] ->setCheckable ( true ); fActions[M_VIEW_SMALL]  ->setChecked(true);
   fViewActions->addAction(fActions[M_VIEW_LIST]  ); fActions[M_VIEW_LIST]  ->setCheckable ( true );
   fViewActions->addAction(fActions[M_VIEW_DETAIL]); fActions[M_VIEW_DETAIL]->setCheckable ( true );  
#endif /* QT_VERSION */
   if (fIconView) 
      connect(fIconView,SIGNAL(ResetActionRequest(int)), this,SLOT(ResetAction(int)));

}
//______________________________________________________________________________
void TQtRootBrowserImp::ResetAction(int mode) {
fActions[M_VIEW_LARGE+mode]->
#if QT_VERSION < 0x40000
   setOn(true);
#else
   setChecked(true);
#endif
}
//______________________________________________________________________________
void TQtRootBrowserImp::MakeMenu()
{
  if (!fBrowserImpID) return;
  if (fMenuBar) { delete fMenuBar; fMenuBar = 0; }
    QMenuBar   *mainMenu = fBrowserImpID->menuBar();
    fMenuBar = mainMenu;
//    QPopupMenu *fileMenu = new QPopupMenu;
//    fileMenu->insertItem( "New",  myView, SLOT(newFile()), CTRL+Key_N );
//    fileMenu->insertItem( "Open", myView, SLOT(open()),    CTRL+Key_O );
//    mainMenu->insertItem( "File", fileMenu );

//*-*  Main Canvas menu items

 // Int_t iMainMenuStart = i;
#if QT_VERSION < 0x40000
 QPopupMenu *fileMenu      = new QPopupMenu();
 mainMenu->insertItem("&File",fileMenu);

 QPopupMenu *viewMenu      = new QPopupMenu();
 mainMenu->insertItem("&View",viewMenu);

 QPopupMenu *optionsMenu   = new QPopupMenu();
 mainMenu->insertItem("&Options",optionsMenu);
                                                 mainMenu->insertSeparator();
 QPopupMenu *helpMenu   = new QPopupMenu();
 mainMenu->insertItem("&Help",helpMenu);

//  fileMenu->insertItem("&New Browser",   this,SLOT(NewBrowserCB()),CTRL+Key_N);
  fActions[M_NEW_BROWSER]->addTo(fileMenu); 
  fActions[M_NEW_CANVAS] ->addTo(fileMenu); 
  fActions[M_OPEN_FILE]  ->addTo(fileMenu); 
  fileMenu->                                insertSeparator();
  fActions[M_FILE_SAVE]  ->addTo(fileMenu); 
  fActions[M_FILE_SAVEAS]->addTo(fileMenu); 
  fileMenu->                                insertSeparator();
  fActions[M_FILE_COPY]  ->addTo(fileMenu); 
  fileMenu->                                insertSeparator();
  fActions[M_FILE_PRINT] ->addTo(fileMenu); 
  fileMenu->                                insertSeparator();
  fActions[M_FILE_CLOSE] ->addTo(fileMenu); 
  fActions[M_FILE_QUIT]  ->addTo(fileMenu); 

//*-*   Items for the View
  fActions[M_VIEW_TOOLBAR]->addTo(viewMenu);
  fActions[M_VIEW_STATBAR]->addTo(viewMenu);
  viewMenu->                                 insertSeparator();
  fViewActions            ->addTo(viewMenu);
  viewMenu->                                 insertSeparator();
  fActions[M_VIEW_ICONS]  ->addTo(viewMenu); 
  fActions[M_VIEW_LINEUP] ->addTo(viewMenu); 
  viewMenu->                                 insertSeparator();
  fActions[M_VIEW_REFRESH]->addTo(viewMenu); 

//*-*   Items for the Help

  fActions[M_HELP_MAIN] ->addTo(helpMenu); 
  helpMenu->insertSeparator();
  fActions[M_ABOUT_MAIN]->addTo(helpMenu); 

#else /* QT_VERSION */
 QMenu *fileMenu      = mainMenu->addMenu("&File");
 QMenu *viewMenu      = mainMenu->addMenu("&View");
 QMenu *optionsMenu   = mainMenu->addMenu("&Options");
                                                 mainMenu->addSeparator();
 QMenu *helpMenu      = mainMenu->addMenu("Help");

//  fileMenu->insertItem("&New Browser",   this,SLOT(NewBrowserCB()),CTRL+Key_N);
  fileMenu->clear();
  fileMenu->addAction(fActions[M_NEW_BROWSER]); 
  fileMenu->addAction(fActions[M_NEW_CANVAS] ); 
  fileMenu->addAction(fActions[M_OPEN_FILE]  ); 
  fileMenu->                                addSeparator();
  fileMenu->addAction(fActions[M_FILE_SAVE]  ); 
  fileMenu->addAction(fActions[M_FILE_SAVEAS]); 
  fileMenu->                                addSeparator();
  fileMenu->addAction(fActions[M_FILE_COPY]  ); 
  fileMenu->                                addSeparator();
  fileMenu->addAction(fActions[M_FILE_PRINT] ); 
  fileMenu->                                addSeparator();
  fileMenu->addAction(fActions[M_FILE_CLOSE] ); 
  fileMenu->addAction(fActions[M_FILE_QUIT]  ); 

//*-*   Items for the View
  viewMenu->clear();
  viewMenu->addAction(fActions[M_VIEW_TOOLBAR]);
  viewMenu->addAction(fActions[M_VIEW_STATBAR]);
  viewMenu->                                 addSeparator();
  viewMenu->addActions(fViewActions->actions());
  viewMenu->                                 addSeparator();
  viewMenu->addAction(fActions[M_VIEW_ICONS]  ); 
  viewMenu->addAction(fActions[M_VIEW_LINEUP] ); 
  viewMenu->                                 addSeparator();
  viewMenu->addAction(fActions[M_VIEW_REFRESH]); 

//*-*   Items for the Help

  helpMenu->clear();
  helpMenu->addAction(fActions[M_HELP_MAIN]  ); 
  helpMenu->                                addSeparator();
  helpMenu->addAction(fActions[M_ABOUT_MAIN] ); 

#endif /* QT_VERSION */
 
#if 0
 fOptionMenu = optionsMenu;
#endif


}
//______________________________________________________________________________
void TQtRootBrowserImp::MakeToolBar() 
{
    //---- toolbar. Add the action with the icon to the toolbar
#if QT_VERSION < 0x40000
    fToolBar = new QToolBar(fBrowserImpID);
    fBrowserImpID->addDockWindow(fToolBar);
    fActions[M_FILE_SAVEAS]->addTo(fToolBar); 
    fToolBar->                               addSeparator();
    fActions[M_FILE_PRINT] ->addTo(fToolBar); 
    fToolBar->                               addSeparator();
    fViewActions           ->addTo(fToolBar); 

    // reset Signals/Slots
    fActions[M_VIEW_TOOLBAR]->disconnect(SIGNAL(activated()));
    connect(fActions[M_VIEW_TOOLBAR],SIGNAL(toggled ( bool)), this,SLOT(ToolbarCB(bool)));
    fActions[M_VIEW_TOOLBAR]->setToggleAction (true); 
    fActions[M_VIEW_TOOLBAR]->setOn (true);
#else /* QT_VERSION */
    fToolBar = new QToolBar(fBrowserImpID);
    fBrowserImpID->addToolBar(fToolBar);
    fToolBar->addAction(fActions[M_FILE_SAVEAS] );
    fToolBar->                               addSeparator();
    fToolBar->addAction(fActions[M_FILE_PRINT]  ); 
    fToolBar->                               addSeparator();
    fToolBar->addActions(fViewActions->actions()); 

    // reset Signals/Slots
    fActions[M_VIEW_TOOLBAR]->disconnect(SIGNAL(activated()));
    connect(fActions[M_VIEW_TOOLBAR],SIGNAL(toggled ( bool)), this,SLOT(ToolbarCB(bool)));
    fActions[M_VIEW_TOOLBAR]->setCheckable(true); 
    fActions[M_VIEW_TOOLBAR]->setChecked (true);
#endif /* QT_VERSION */
}
//______________________________________________________________________________
void TQtRootBrowserImp::MakeStatBar() 
{
  fBrowserImpID->statusBar();
  fActions[M_VIEW_STATBAR]->disconnect(SIGNAL(activated()));
  connect(fActions[M_VIEW_STATBAR],SIGNAL(toggled ( bool)), this,SLOT(StatusBarCB(bool)));
  fActions[M_VIEW_STATBAR]->setToggleAction (true); 
  fActions[M_VIEW_STATBAR]->setOn (true);
}
//______________________________________________________________________________
void TQtRootBrowserImp::NewBrowserCB()
{ 
   new TBrowser("browser"); 
}
//______________________________________________________________________________
void TQtRootBrowserImp::NewCanvasCB()
{ 
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,03)
  gROOT->MakeDefCanvas();
#else   
  gROOT->GetMakeDefCanvas()();
#endif  
}
//______________________________________________________________________________
void TQtRootBrowserImp::OpenCB()
{ printf(" TQtRootBrowserImp::OpenCB() \n"); }
//______________________________________________________________________________
void  TQtRootBrowserImp::ProcessMessage()
{
   TQtRootAction *actionSender =  (TQtRootAction *)sender ();
   switch (actionSender->Id()) {
   case M_NEW_BROWSER:   NewBrowserCB(); break;
   case M_NEW_CANVAS:    NewCanvasCB();  break; 
   case M_OPEN_FILE:     OpenCB();       break;  
   case M_FILE_SAVE:     SaveCB();       break;  
   case M_FILE_SAVEAS:   SaveAsCB();     break;  
   case M_FILE_COPY:     CopyCB();       break;  
   case M_FILE_PRINT:    PrintCB();      break; 
   case M_FILE_CLOSE:    CloseCB();      break; 
   case M_FILE_QUIT:     QuitCB();       break;  
   case M_VIEW_LARGE:    LargeCB();      break;  
   case M_VIEW_SMALL:    SmallCB();      break;  
   case M_VIEW_LIST:     ListCB();       break;   
   case M_VIEW_DETAIL:   DetailsCB();    break; 
   case M_VIEW_ICONS:    ArrangeCB();    break;  
   case M_VIEW_LINEUP:   LineupCB();     break; 
   case M_VIEW_REFRESH:  RefreshCB();    break;
   case M_HELP_MAIN:     HelpCB();       break;   
   case M_ABOUT_MAIN :   AboutCB();      break;
   default:
      break;

   };

}
//______________________________________________________________________________
void TQtRootBrowserImp::SetViewMode(int mode) 
{ if (fIconView) fIconView->SetViewMode(mode); }
//______________________________________________________________________________
void TQtRootBrowserImp::SaveCB()
{
   if ( fSaveType.isEmpty() ||  fSaveFileName.isEmpty() )
      SaveAsCB(); 
   else
      QPixmap::grabWidget(GetBrowserID()->topLevelWidget() ).save(fSaveFileName,fSaveType);
}
//______________________________________________________________________________
void TQtRootBrowserImp::SaveAsCB()
{ 
  QString filter;

  // QString defExtension[] = {"C","ps","eps","root","bmp","C"};

  UInt_t i=0;
#if QT_VERSION < 0x40000
  for (i = 0; i < QImageIO::outputFormats().count(); i++ ) 
#else
  QList<QByteArray> formats =  QImageWriter::supportedImageFormats();
  QList<QByteArray>::const_iterator j;
  for (j = formats.constBegin(); j != formats.constEnd(); ++j)
#endif
  {
    if (i) filter +=';';
    filter += "Image ( *.";
#if QT_VERSION < 0x40000
    QString str = QString( QImageIO::outputFormats().at( i ) );
#else
    QString str =  *j; i++;
#endif
    filter += str.lower();
    filter +=");";
  }
  filter +=';';

  QString selectedFilter;

#if QT_VERSION < 0x40000
  QString thatFile = QFileDialog::getSaveFileName(gSystem->WorkingDirectory()
#else /* QT_VERSION */
  QString thatFile = Q3FileDialog::getSaveFileName(gSystem->WorkingDirectory()
#endif /* QT_VERSION */
    , filter, fBrowserImpID, "SaveAs"
    , "Save the this browser widget as"
    , &selectedFilter);

  if (thatFile.isEmpty()) return;

  //  define the file extension
  QString fileNameExtension = QFileInfo(thatFile).extension(FALSE);
  QString saveType = fileNameExtension.upper();

  if (selectedFilter.contains("*.*")) {
     if (!fileNameExtension.isEmpty() ) 
        saveType = TGQt::QtFileFormat(fileNameExtension);
     else 
        saveType = "PNG"; // this is default
  } else {
     saveType = TGQt::QtFileFormat(selectedFilter);
  }

  if (saveType.isEmpty() ) {
     TBrowser *b = Browser();;
     if (b) b->Error("Save As", "no image format is defined");
     return;
  }
  fSaveType      = saveType;
  fSaveFileName  = thatFile;

  QPixmap::grabWidget(GetBrowserID()->topLevelWidget() ).save(fSaveFileName,fSaveType);
}
//______________________________________________________________________________
void TQtRootBrowserImp::StatusBarCB(bool show)
{
   // Show or hide the status bar
   QStatusBar * bar = fBrowserImpID->statusBar();
   if (show) bar->show();
   else      bar->hide();
}

//______________________________________________________________________________
void TQtRootBrowserImp::ToolbarCB(bool show)
{
   // Show or hide toolbar.
   if (show) fToolBar->show();
   else      fToolBar->hide();
}
//______________________________________________________________________________
void TQtRootBrowserImp::PrintCB()
{ 
  QPrinter p;
  if (p.setup()) {
    QPainter pnt(&p);
    pnt.drawPixmap(0,0,QPixmap::grabWidget(GetBrowserID()->topLevelWidget() ));
  }
}

//______________________________________________________________________________
void TQtRootBrowserImp::CloseCB()
{
  TBrowser *b = Browser();
  if (b && b->IsOnHeap() )  delete b;
}

//______________________________________________________________________________
void TQtRootBrowserImp::QuitCB()
{
  gApplication->Terminate(0);
}

// editor menu 
//______________________________________________________________________________
void TQtRootBrowserImp::CopyCB()
{
//  Copy the current gPad to the system clipboard 
  QClipboard *cb = QApplication::clipboard();
  cb->setPixmap(QPixmap::grabWidget(GetBrowserID()));
}

//______________________________________________________________________________
void TQtRootBrowserImp::RefreshCB()
{}
//______________________________________________________________________________
void TQtRootBrowserImp::AboutCB()
{ 
  QMessageBox::aboutQt(0);
  QMessageBox::about(0,"ROOT Browser with Qt interface"
    ,"ROOT Qt interface Copyright (C) 2001-2004, Valeri Fine. Brookhaven National Laboratory. All right reserved.");
}
//______________________________________________________________________________
void TQtRootBrowserImp::HelpCB()
{
  gSystem->Exec("explorer http://root.cern.ch/root/html/ClassIndex.html");
}
