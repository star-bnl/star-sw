// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtCanvasImp.cxx,v 1.26 2013/08/30 16:00:23 perev Exp $
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
// TQtCanvasImp                                                         //
//                                                                      //
// This class creates a main window with menubar, scrollbars and a      //
// drawing area.                                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifdef WIN32
# include "Windows4Root.h"
#endif

#include "TROOT.h"
#include "TBrowser.h"
#include "TSystem.h"
#include "TCanvas.h"
#include "TApplication.h"
#include "TMarker.h"
#include "TStyle.h"
#include "HelpText.h"
#include "TInterpreter.h"
#include "TVirtualPadEditor.h"
#include "TQtCanvas2Html.h"
#include "TEnv.h"
#include "TError.h"

#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)   
#include "TVirtualViewer3D.h"
#endif

#include "TQtCanvasImp.h"
#include "TGQt.h"
#include "TQtWidget.h"
#include "TQtCanvasWidget.h"
#include "TQtRootAction.h"
#include "TQtZoomPadWidget.h"
#include "TQtToolBar.h"

#include <QApplication>
#include <QMenuBar>

#include <QTimer>
#include <QDebug>
#include <QMenu>
#include <QImageWriter>
#include <QFileDialog>
#include <QWhatsThis>
#include <QFileDialog>
#include <QToolBar>
#include <QDockWidget>
#include <QPixmap>

#include <QMessageBox>
#include <QStatusBar>

#include <QLabel>
#include <QSplitter>

#include <QImage>
#include <QClipboard>
#include <QPrinter>

// Canvas menu command ids
enum ERootCanvasCommands {
   kFileNewCanvas,
   kFileOpen,
   kFileSave,
   kFileSaveAs,
   kFileSaveAsRoot,
   kFileSaveAsC,
   kFileSaveAsPS,
   kFileSaveAsEPS,
   kFileSaveAsPDF,
   kFileSaveAsSVG,
   kFileSaveAsGIF,
   kFileSaveAsWeb,
   kFilePrint,
   kFileCloseCanvas,
   kFileQuit,
   kFileExit,

   kEditCut,
   kEditCopy,
   kEditCopyFrame,
   kEditPaste,
   kEditClearPad,
   kEditClearCanvas,
   kEditUndo,
   kEditRedo,

   kViewEditor,
   kViewToolbar,
   kViewEventStatus,
   kViewColors,
   kViewFonts,
   kViewMarkers,
   kViewIconify,
   kViewX3D,
   kViewOpenGL,
   kViewInventorGL,
   kViewZoomer,
   
   kOptionAutoResize,
   kOptionResizeCanvas,
   kOptionMoveOpaque,
   kOptionResizeOpaque,
   kOptionInterrupt,
   kOptionRefresh,
   kOptionAutoExec,
   kOptionStatistics,
   kOptionHistTitle,
   kOptionFitParams,
   kOptionCanEdit,

   kInspectRoot,
   kInspectBrowser,

   kClassesTree,

   kHelpHelp,
   kHelpAbout,
   kHelpOnCanvas,
   kHelpOnMenus,
   kHelpOnGraphicsEd,
   kHelpOnBrowser,
   kHelpOnObjects,
   kHelpOnPS
};

//static const char *gOpenTypes[] = { "ROOT files",   "*.root",
//                                    "All files",    "*",
//                                    0,              0 };


static TQtBrowserMenuItem_t gMenu_Data[] = {
  // { filename,      tooltip,            staydown,  id,              button}
/* File Menu */
  { "&New",       kFileNewCanvas,  Qt::CTRL+Qt::Key_N, "Open the new ROOT TCanvas widget",     "newcanvas.xpm"           },
  { "&Open",      kFileOpen,       Qt::CTRL+Qt::Key_O, "Open a new ROOT TFile",               "open.xpm"                },
  { "&Close",     kFileCloseCanvas,Qt::ALT +Qt::Key_F4,"Close the TCanvas widget and delete the TCanvas object",  "" },
  { "&Save",      kFileSave,       Qt::CTRL+Qt::Key_S, "Save the current canvas image as ... ","save.xpm"            },
  { "Save &As",   kFileSaveAs,         0,              "Save the current canvas image as ... ","hdisk_t.xpm"         },
  { "Save as Web page", kFileSaveAsWeb,   0,           "Save the current canvas as Web site"  ,"hdisk_t.xpm"         },
  { "&Print",     kFilePrint,      Qt::CTRL+Qt::Key_P, "Print the TCanvas image ",             "printer_s.xpm"       },
  { "&Quit",      kFileQuit,       Qt::CTRL+Qt::Key_Q, "Terminate the ROOT application",       ""                    },
  { "E&xit",      kFileExit,       Qt::CTRL+Qt::Key_X, "Exit the ROOT application",            ""                    },
/* Edit Menu */
  { "&Copy",      kEditCopy,       Qt::CTRL+Qt::Key_C, "Copy the image of the TCanvas client area to the system clipboard","" },
  { "Copy &Frame",kEditCopyFrame,  Qt::CTRL+Qt::Key_F, "Copy the image of the TCanvasframe border to the system clipboard","" },
  { "Clear &Pad", kEditClearPad,       0, "", ""},
  { "Clear &Window", kEditClearCanvas, 0, "", ""},

 /* View Menu */
  { "&Pad Editor", kViewEditor,      Qt::CTRL+Qt::Key_E, "", ""},
  { "&Tool bar",   kViewToolbar,     0, "", ""},
  { "&Status Bar", kViewEventStatus, 0, "", ""},
  { "&Explore",    kInspectBrowser, Qt::CTRL+Qt::Key_B, "Start ROOT Browser",               "browser.xpm"                 },
  { "&Zoomer",     kViewZoomer,     Qt::CTRL+Qt::Key_Z, "Attach the zoomer",                ""                            },

  { "&Color Box", kViewColors,       0, "", ""},
  { "&Font Box",  kViewFonts,        0, "", ""},
  { "&Markers",   kViewMarkers,      0, "", ""},
  { "&Iconify",   kViewIconify,      0, "", ""},

  { "OpenGL &3D View", kViewOpenGL,    0, "", ""},
  { "X&3D View",       kViewX3D,       0, "", ""},
  { "Coin3D View",     kViewInventorGL,0, "", ""},

  { "Interrupt", kOptionInterrupt,Qt::ALT+Qt::Key_C , "Send the interruption signal to the application", "interrupt.xpm"},
  { "Re&Fresh",   kOptionRefresh,  Qt::Key_F5,         "Refresh the TCanvas widget",       "refresh2.xpm"                },
  { "&Pad Auto Exec", kOptionAutoExec,         0, "", ""},
  { "&Resize Canvas", kOptionResizeCanvas,     0, "", ""},
  { "&Auto Resize Canvas", kOptionAutoResize,  0, "", ""},
  { "&Statistics", kOptionStatistics,          0, "", ""},
  { "&Histogram title", kOptionHistTitle,      0, "", ""},
  { "Fit &Params", kOptionFitParams,           0, "", ""},
  { "&Can Edit Histograms", kOptionCanEdit,    0, "", ""},

  { "Inspect ROOT",   kInspectRoot,    Qt::Key_F3,         "Inspect the ROOT objectwith the ROOT object inspector", "inspect.xpm" },
  { "&Class Tree",kClassesTree,      0, "", ""},
  { "Start Browser",   kInspectBrowser, Qt::CTRL+Qt::Key_B, "Start ROOT Browser",               "browser.xpm"                 },

  { "About",                 kHelpAbout,        0, "", ""},
  { "Help On Canvas...",     kHelpOnCanvas,     0, "", ""},
  { "Help On Menus...",      kHelpOnMenus,      0, "", ""},
  { "Help On Graphics Editor...",  kHelpOnGraphicsEd, 0, "", ""},
  { "Help On Browser...",    kHelpOnBrowser,    0, "", ""},
  { "Help On Objects...",    kHelpOnObjects,    0, "", ""},
  { "Help On PostScript...", kHelpOnPS,         0, "", ""},
  {0,0,0,"",""}
};

//____________________________________________________
static inline QString QtFileFormat(const char *selector)
{ return QtFileFormat(QString(selector)); }

//____________________________________________________
static inline  QString QtFileFormat(const QString &selector)
{
   // returns Qt file format
   return TGQt::QtFileFormat(selector);
}

//____________________________________________________
static inline QString RootFileFormat(const char *selector)
{  return RootFileFormat(QString(selector)); }
//____________________________________________________
static inline QString RootFileFormat(const QString &selector)
{   
   return TGQt::RootFileFormat(selector);
}
#if  ROOT_VERSION_CODE < ROOT_VERSION(4,03,03)   
static Bool_t glViewerLoadFlag = kFALSE;
#endif

TQtZoomPadWidget  *TQtCanvasImp::fgZoomingWidget = 0;

//______________________________________________________________________________
TQtCanvasImp::TQtCanvasImp()
: QObject(), TCanvasImp()
, fCanvasImpID(0), fEditor(0)
, fMenuBar(0),fOptionMenu(0),fDoubleBuffer(kTRUE) {;}
//______________________________________________________________________________
TQtCanvasImp::TQtCanvasImp(TCanvas *c, const char *name, UInt_t width, UInt_t height,bool initFlag)
:   QObject(), TCanvasImp(c, name, width, height)
 , fCanvasImpID(0),fX(0),fY(0),fWidth(width),fHeight(height)
 , fFileToolBar(0),fToolBar(0),fEditToolBar(0),fEditor(0)
 , fMenuBar(0),fOptionMenu(0), fDoubleBuffer(kTRUE)  
{ 
   if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtCanvasImp::TQtCanvasImp(TCanvas *c, const char *name, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag)
:  QObject(), TCanvasImp(c, name,x,y,width, height)
 , fCanvasImpID(0),fX(x),fY(y),fWidth(width),fHeight(height)
 , fFileToolBar(0),fToolBar(0),fEditToolBar(0),fEditor(0)
 , fMenuBar(0),fOptionMenu(0), fDoubleBuffer(kTRUE)  
{
   if (initFlag) InitWindow();
}
//______________________________________________________________________________
TQtCanvasImp::~TQtCanvasImp()
{ 
  Delete();
}
//______________________________________________________________________________
void TQtCanvasImp::Delete() 
{
   // Delete widget from the proper thread
   if (fCanvasImpID) {
      TCanvas *c =  Canvas();
      if (c) c->DisconnectWidget();
      fCanvasImpID->hide();
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,18,0)
      TVirtualPadEditor* gged = TVirtualPadEditor::GetPadEditor(kFALSE);
      if(gged && gged->GetCanvas() == fCanvas)  gged->Hide();
#endif
      if (c && c->GetCanvasID() != TGQt::iwid(((TGQt *)gVirtualX)->GetSelectedWindow()) ) 
         gVirtualX->SelectWindow ( TGQt::iwid(fCanvasID));
      fCanvasID = 0;
#if ROOT_VERSION_CODE < ROOT_VERSION(5,13,3) 
      // Stolen from the TRootCanvas::Close()
      if (fEditor) fEditor->DeleteEditors();
#endif
#if 0
      if (TVirtualPadEditor::GetPadEditor(kFALSE) != 0)
         TVirtualPadEditor::Terminate();
#else
      delete fEditor; fEditor = 0;
#endif 
      TQtCanvasWidget *wid = fCanvasImpID;
      fCanvasImpID = 0;
      disconnect(wid,SIGNAL(destroyed()),this,SLOT(Disconnect()));
      gVirtualX->CloseWindow();
#if QT_VERSION < 0x40000
      delete wid;
#else
      wid->deleteLater();
#endif
   }
}
//______________________________________________________________________________
void TQtCanvasImp::DrawEventStatus(const char *text, Int_t partidx)
{ 
  // Set text for 'partidx field
  SetStatusText(text,partidx);
} 
//______________________________________________________________________________
void TQtCanvasImp::MakeActions() {
   int i=0;
   while (gMenu_Data[i].fMenuText!=NULL) {
      // skip the separators 
      TQtRootAction *action = new TQtRootAction(fCanvasImpID,gMenu_Data[i]);
      fActions.insert(action->Id(),action);
      connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
      i++;
   }
   MakeToolBarActions(i);
}
//______________________________________________________________________________
void TQtCanvasImp::MakeToolBarActions(Int_t /* firstId */)
{
   fEditToolBar = new TQtToolBar(fCanvasImpID);
}
//______________________________________________________________________________
void TQtCanvasImp::MakeMenu()
{
   if (!fCanvasImpID) return;
   if (fMenuBar) { delete fMenuBar; fMenuBar = 0; }
   QMenuBar   *mainMenu = fCanvasImpID->menuBar();
   fMenuBar = mainMenu;

    //---- toolbar. Add the action with the icon to the toolbar
   fFileToolBar = new QToolBar(fCanvasImpID);
   fToolBar = new QToolBar(fCanvasImpID);
   fCanvasImpID->addToolBar(fFileToolBar);
   fCanvasImpID->addToolBar(fToolBar);
   // fEditToolBar has been created at TQtCanvasImp::MakeToolBarAction()
   fCanvasImpID->addToolBar(fEditToolBar);

   //*-*  Main Canvas menu items

   // Int_t iMainMenuStart = i;
   QMenu *fileMenu      = mainMenu->addMenu("&File");
   QMenu *editMenu      = mainMenu->addMenu("&Edit");
   QMenu *viewMenu      = mainMenu->addMenu("View");
   QMenu *optionsMenu   = mainMenu->addMenu("&Options");
   QMenu *inspectorMenu = mainMenu->addMenu("&Tools");

   mainMenu->addSeparator();

   QMenu *helpMenu     = mainMenu->addMenu("&Help");

   fOptionMenu = optionsMenu;

   //*-*   Items for the File Menu
   fileMenu->clear();
   fileMenu->addAction(fActions[kFileNewCanvas]);              fFileToolBar->addAction(fActions[kFileNewCanvas]);
   fileMenu->addAction(fActions[kFileOpen]);                   fFileToolBar->addAction(fActions[kFileOpen]);
   fileMenu->                                 addSeparator();
   fileMenu->addAction(fActions[kFileCloseCanvas]);
   fileMenu->                                 addSeparator();
   fileMenu->addAction(fActions[kFileSave]);                   fFileToolBar->addAction(fActions[kFileSave]);
   fileMenu->addAction(fActions[kFileSaveAs]);                 fFileToolBar->addAction(fActions[kFileSaveAs]);
   fileMenu->addAction(fActions[kFileSaveAsWeb]);              fFileToolBar->addAction(fActions[kFileSaveAsWeb]);
   fileMenu->                                 addSeparator();
   fileMenu->addAction(fActions[kFilePrint]);                  fFileToolBar->addAction(fActions[kFilePrint]);
   fileMenu->                                 addSeparator();
   fileMenu->addAction(fActions[kFileQuit]);fActions[kFileQuit]->setMenuRole(QAction::QuitRole);
   fileMenu->addAction(fActions[kFileExit]);fActions[kFileExit]->setMenuRole(QAction::QuitRole);

   //*-*   Items for the Edit Menu

   editMenu->clear();
   //  editMenu->insertItem("Undo","&Undo",SLOT(UnDoCB));
   editMenu->addAction(fActions[kEditCopy]);
   editMenu->addAction(fActions[kEditCopyFrame]);
   editMenu->                                  addSeparator();
   editMenu->addAction(fActions[kViewEditor]);
      fActions[kViewEditor]->setCheckable(true);
      fActions[kViewEditor]->setChecked(gStyle->GetShowEditor());
   editMenu->                                  addSeparator();
   QMenu *clearMenu = editMenu->addMenu("C&lear");

   clearMenu->addAction(fActions[kEditClearPad]);
   clearMenu->addAction(fActions[kEditClearCanvas]);

   //*-*   Items for the View

   viewMenu->clear();
   viewMenu->addAction(fActions[kViewEditor]);
   viewMenu->addAction(fActions[kViewToolbar]);   
      fActions[kViewToolbar]->setCheckable(true);
      if (gStyle->GetShowToolBar()) {
         fActions[kViewEventStatus]->setChecked(true);
      } else {
         fActions[kViewEventStatus]->setChecked(false);
         fFileToolBar->hide();
         fToolBar->hide();
         fEditToolBar->hide();
      }
   viewMenu->addAction(fActions[kViewEventStatus]);
      fActions[kViewEventStatus]->setCheckable(true);
      fActions[kViewEventStatus]->setChecked(gStyle->GetShowEventStatus());
   viewMenu->addAction(fActions[kInspectBrowser]);
   viewMenu->addAction(fActions[kViewZoomer]);
      fActions[kViewZoomer]->setCheckable(true);
      fActions[kViewZoomer]->setChecked(false);
   
   viewMenu->                                  addSeparator();
   viewMenu->addAction(fActions[kViewColors]);
   viewMenu->addAction(fActions[kViewFonts]);
   viewMenu->addAction(fActions[kViewMarkers]);
   viewMenu->addAction(fActions[kViewIconify]);
   viewMenu->                                  addSeparator();
   QMenu *viewWithMenu = viewMenu->addMenu("View &3D With");

#ifdef WIN32
   viewWithMenu->addAction(fActions[kViewOpenGL ]);
#else
   viewWithMenu->addAction(fActions[kViewX3D]);
   
#  if ROOT_VERSION_CODE >= ROOT_VERSION(4,01,00)
      fActions[kViewX3D]    ->setEnabled(FALSE);
#  endif
   
   viewWithMenu->addAction(fActions[kViewOpenGL]);
#endif
   // Check whether we haved OpenGL plugin
   char *libRQTGL = gSystem->DynamicPathName("libRQTGL",kTRUE);
   fActions[kViewOpenGL]->setEnabled(libRQTGL);
   delete [] libRQTGL;
   {
      viewWithMenu->addAction(fActions[kViewInventorGL]); 
      // Check whether we haved OpenInventor plugin
      libRQTGL = gSystem->DynamicPathName("libRQIVTGL",kTRUE);
      fActions[kViewInventorGL]->setEnabled(libRQTGL);
      delete [] libRQTGL;
   }
   //*-*   Items for the Options Menu
 
   optionsMenu->clear();
   optionsMenu->addAction(fActions[kViewEventStatus]);
   optionsMenu->addAction(fActions[kOptionAutoExec]);
   optionsMenu->                                  addSeparator();
   optionsMenu->addAction(fActions[kOptionAutoResize]);
   optionsMenu->addAction(fActions[kOptionResizeCanvas]);
   optionsMenu->                                  addSeparator();
   optionsMenu->addAction(fActions[kOptionInterrupt]);             fToolBar->addAction(fActions[kOptionInterrupt]); 
   optionsMenu->addAction(fActions[kOptionRefresh]);               fToolBar->addAction(fActions[kOptionRefresh]); 
   optionsMenu->                                  addSeparator();

   optionsMenu->addAction(fActions[kOptionStatistics]);
   fActions[kOptionStatistics]->setCheckable(true);
   fActions[kOptionStatistics]->setChecked(gStyle->GetOptStat());

   optionsMenu->addAction(fActions[kOptionHistTitle]);
   fActions[kOptionHistTitle]->setCheckable(true);
   fActions[kOptionHistTitle]->setChecked(gStyle->GetOptTitle());

   optionsMenu->addAction(fActions[kOptionFitParams]);
   fActions[kOptionFitParams]->setCheckable(true);
   fActions[kOptionFitParams]->setChecked(gStyle->GetOptFit());

   optionsMenu->addAction(fActions[kOptionCanEdit]);
   fActions[kOptionCanEdit]->setCheckable(true);
   fActions[kOptionCanEdit]->setChecked(gROOT->GetEditHistograms());
   //*-*   Items for the Inspect Menu

   inspectorMenu->clear();
   inspectorMenu->addAction(fActions[kInspectRoot]   );              fToolBar->addAction(fActions[kInspectRoot]); 
   inspectorMenu->addAction(fActions[kInspectBrowser]);              fToolBar->addAction(fActions[kInspectBrowser]); 
   inspectorMenu->addAction(fActions[kClassesTree]);


   helpMenu->clear();
//   fActions[kHelpHelp]        ->addTo(helpMenu);
   helpMenu->addAction(fActions[kHelpOnCanvas]    );
   helpMenu->addAction(fActions[kHelpOnMenus]     );
   helpMenu->addAction(fActions[kHelpOnGraphicsEd]);
   helpMenu->addAction(fActions[kHelpOnBrowser]   );
   helpMenu->addAction(fActions[kHelpOnObjects]   );
   helpMenu->addAction(fActions[kHelpOnPS]        );
   helpMenu->                                   addSeparator();
   helpMenu->addAction(fActions[kHelpAbout]       );fActions[kHelpAbout]->setMenuRole(QAction::AboutRole);
}

//______________________________________________________________________________
void TQtCanvasImp::FitCanvas(){}
//______________________________________________________________________________
void TQtCanvasImp::Lock()
{
   // Lock updating canvas.
  if (!fCanvasImpID) fCanvasImpID->setUpdatesEnabled(FALSE);
}

//______________________________________________________________________________
void TQtCanvasImp::Unlock()
{
   //  Unlock updating canvas.

  if (!fCanvasImpID) fCanvasImpID->setUpdatesEnabled(true);
}
//______________________________________________________________________________
Bool_t TQtCanvasImp::IsLocked() 
{  
   if (fCanvasImpID) return !fCanvasImpID->isUpdatesEnabled();
   return kFALSE; 
}
//______________________________________________________________________________
void TQtCanvasImp::Close() 
{ 
  // if (fCanvasImpID) fCanvasImpID->topLevelWidget()->close();
  Delete();
}
//_____________________________________________________________________________
void  TQtCanvasImp::RaiseWindow()
{
   if (fCanvasImpID) {
      fCanvasImpID->raise();
      fCanvasImpID->show();
   }
}

//_____________________________________________________________________________
void TQtCanvasImp::ReallyDeleteCB()
{
   // [slot] to delete the TCanvasImp from the event loop
   ReallyDelete();
}

//______________________________________________________________________________
void TQtCanvasImp::ReallyDelete()
{
   // Really delete the canvas and this GUI.
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,18,0)
   TVirtualPadEditor* gged = TVirtualPadEditor::GetPadEditor(kFALSE);
   if(gged && gged->GetCanvas() == fCanvas) gged->Hide();
#endif
   if (fCanvasImpID) fCanvasImpID->hide();
   TVirtualPad *savepad = gPad;
   gPad = 0;        // hide gPad from CINT
   gInterpreter->DeleteGlobal(fCanvas);
   gPad = savepad;  // restore gPad for ROOT
//   Delete();
   TCanvas *c = fCanvas; fCanvas = 0;
   delete c;  // will in turn delete this object
}
//______________________________________________________________________________
void TQtCanvasImp::ForceUpdate()
{
   fprintf(stderr," TQtCanvasImp::ForceUpdate();\n");
}

//______________________________________________________________________________
#if ROOT_VERSION_CODE > ROOT_VERSION(4,00,4) 
UInt_t TQtCanvasImp::GetWindowGeometry(Int_t &x, Int_t &y, UInt_t &w, UInt_t &h)
#else
void   TQtCanvasImp::GetWindowGeometry(Int_t &x, Int_t &y, UInt_t &w, UInt_t &h);
#endif   
{
   // Get effective window parameters (with borders and menubar)
   QRect rect = fCanvasImpID->topLevelWidget()->frameGeometry();
   x = rect.x();
   y = rect.y();
   w = rect.width();
   h = rect.height(); 
//   if (!fCanvas->GetShowEditor()) return 0;
//   return fEditorFrame->GetWidth();
#if ROOT_VERSION_CODE > ROOT_VERSION(4,00,4) 
   return 0;
#endif   
}
//______________________________________________________________________________
void TQtCanvasImp::Iconify()
{
  if (fCanvasImpID) fCanvasImpID->topLevelWidget()->showMinimized();
}
//______________________________________________________________________________
Int_t TQtCanvasImp::InitWindow()
{
  if (!fCanvasImpID) 
  {
     // fprintf(stderr,"TQtCanvasImp::InitWindow: ");
    fCanvasImpID = new TQtCanvasWidget();
    fCanvasImpID->setName(gVirtualX->GetName());
    connect(fCanvasImpID,SIGNAL(WMCloseCanvas()),this,SLOT(CloseCB()));
    connect(fCanvasImpID,SIGNAL(destroyed()),this,SLOT(Disconnect()));
//    fCanvasID = (TQtWidget *)TGQt::iwid(gVirtualX->InitWindow(TGQt::iwid(fCanvasImpID)));
    fCanvasID = (TQtWidget *)TGQt::iwid(gVirtualX->InitWindow(0));
    fCanvasID->setParent(fCanvasImpID);
    // printf(" %d \n",  fCanvasID->isTopLevel());
    fCanvasID->SetCanvas(Canvas());
//    fCanvasID->resize(fWidth,fHeight);

#if 0 
    Q3ScrollView *view = new Q3ScrollView (0,"canvasscroll");
    view->addChild(fCanvasID);
    fCanvasImpID->setCentralWidget (view);
#else
    fCanvasImpID->setCentralWidget (fCanvasID);
#endif
 
    int parts[] = {43,7,10,39};
    CreateStatusBar(parts,4);
    fCanvasImpID->statusBar()->hide();
    MakeActions();
    MakeMenu();

    fCanvasImpID->move(fX,fY);
#ifndef WIN32
    // Now try to figure out the full size of the main window.
    // = (Canvas size) + (frame size) + (menu bar size)
    QSize fullSize =   QSize(fWidth,fHeight) 
                     + (fCanvasImpID->frameSize() - fCanvasImpID->size())  + QSize(0,3)
                     + QSize(0,fCanvasImpID->menuBar()->frameSize().height());
    fCanvasImpID->resize(fullSize);
#else
    fCanvasImpID->resize(fWidth,fHeight);
#endif
 
//    fCanvasID->resize(fWidth,fHeight);
  }
  assert(Canvas()->GetCanvasID() == -1 ||  Canvas()->GetCanvasID() != TGQt::iwid(fCanvasID));
  // fprintf(stderr," TQtCanvasImp::InitWindow() %d \n",TGQt::iwid(fCanvasID));
  return TGQt::iwid(fCanvasID);
}
//______________________________________________________________________________
void TQtCanvasImp::Disconnect()
{
  if (fCanvasImpID) {
     fCanvasImpID = 0;
     TCanvas *c = Canvas();
     if (c && c->IsOnHeap() ) {
       c->DisconnectWidget();
       if (c->GetCanvasID() == TGQt::iwid(((TGQt *)gVirtualX)->GetSelectedWindow()) ) 
       {
          // To close QPainter properly
         gVirtualX->SelectWindow(-1);
       }
       ReallyDelete();
       // delete c;
     }
  }
}
//______________________________________________________________________________
void TQtCanvasImp::NewCanvas()
{
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,03)
  gROOT->MakeDefCanvas();
#else   
  gROOT->GetMakeDefCanvas()();
#endif  
 }
//______________________________________________________________________________
void TQtCanvasImp::CreateStatusBar(Int_t nparts)
{
  QStatusBar *statusBar = fCanvasImpID->statusBar();
  Int_t i=0;
  for (i=0;i<nparts;i++) {
    QLabel *l = new QLabel(statusBar);
    statusBar->addWidget(l,1,true);

    // remember to delete later
    fStatusBar.insert(i,l);  
  }
}
//______________________________________________________________________________
void TQtCanvasImp::CreateStatusBar(Int_t *parts, Int_t nparts)
{
  QStatusBar *statusBar = fCanvasImpID->statusBar();
#ifdef WIN32
  statusBar->setSizeGripEnabled(FALSE);
#endif
  // Any number of widgets may be controlled by just
  // one splitter
  QSplitter *split = new QSplitter(statusBar);
  statusBar->addWidget(split,1,FALSE);

  Int_t iField=0;
  for (iField=0; iField<nparts; iField++) {
    QLabel *infoBox = new QLabel(split);
    infoBox->setIndent(3);
    QSize s = infoBox->size();
    s.setWidth(parts[iField]);
    infoBox->resize(s);

    // remember to delete later
    fStatusBar.insert(iField,infoBox);
  }
}
//______________________________________________________________________________
void  TQtCanvasImp::ProcessMessage()
{
   TQtRootAction *actionSender =  (TQtRootAction *)sender ();
   switch (actionSender->Id()) {

   case kFileNewCanvas:    NewCB();    break;
   case kFileOpen:         OpenCB();   break;
   case kFileSave:         SaveCB();   break;
   case kFileSaveAs:       SaveAsCB(); break;
   case kFileSaveAsWeb:    SaveAsWebCB();break;
   case kFilePrint:        PrintCB();  break;
   case kFileCloseCanvas:  CloseCB();  break;
   case kFileQuit:         QuitCB();   break;
   case kFileExit:         QuitCB();   break;
 
   case kEditCut:          CB();            break;
   case kEditCopy:         CopyCB();        break;
   case kEditPaste:        CB();            break;
   case kEditClearPad:     ClearPadCB();    break;
   case kEditClearCanvas:  ClearCanvasCB(); break;
   case kEditUndo:         CB();            break;
   case kEditRedo:         CB();            break;

   case kViewEditor:       EditorCB();      break;
   case kViewToolbar:      ToolBarCB();     break;
   case kViewEventStatus:  EventStatusCB(); break;
   case kViewColors:       ColorsCB();      break;
   case kViewFonts:        FontsCB();       break;
   case kViewMarkers:      MarkersCB();     break;
   case kViewIconify:      IconifyCB();     break;
   case kViewX3D:          X3DViewCB();     break;
   case kViewOpenGL:       GLViewCB();      break;
   case kViewInventorGL:   GLIVViewCB();    break;
   case kViewZoomer:       ZoomCB();        break;

   case kOptionAutoResize:    CB();         break;
   case kOptionResizeCanvas:  CB();         break;
   case kOptionMoveOpaque:    CB();         break;
   case kOptionResizeOpaque:  CB();         break;
   case kOptionInterrupt:  InterruptCB();   break;
   case kOptionRefresh:    RefreshCB();     break;
   case kOptionAutoExec:   PadAutoExecCB(); break;
   case kOptionStatistics: OptStatCB();     break;
   case kOptionHistTitle:  OptTitleCB();    break;
   case kOptionFitParams:  OptFitCB();      break;
   case kOptionCanEdit:    CanEditHistogramsCB(); break;

   case kInspectRoot:      ROOTInspectCB(); break;
   case kInspectBrowser:   BrowserCB();     break;

   case kClassesTree:      FullTreeCB();    break;

   case kHelpAbout:        AboutCB();            break;
   case kHelpOnCanvas:     HelpOnCanvasCB();     break;
   case kHelpOnMenus:      HelpOnMenusCB();      break;
   case kHelpOnGraphicsEd: HelpOnGraphicsEdCB(); break;
   case kHelpOnBrowser:    HelpOnBrowserCB();    break;
   case kHelpOnObjects:    HelpOnObjectsCB();    break;
   case kHelpOnPS:         HelpOnPSCB();         break;

   default:
      break;

   };

}

//______________________________________________________________________________
void TQtCanvasImp::RootExec(const char* /*cmd*/){assert(0);}

//______________________________________________________________________________
void TQtCanvasImp::SetCanvasSize(UInt_t w, UInt_t h)
{ 
   // printf("It is not clear what SetCanvasSize differs from SetWindowSize\n");
   fCanvasImpID->resize(w,h);
}

//______________________________________________________________________________
void TQtCanvasImp::SetWindowPosition(Int_t x, Int_t y)
{
   fCanvasImpID->move(x,y);
}
//______________________________________________________________________________
void TQtCanvasImp::SetWindowSize(UInt_t w, UInt_t h) 
{ 
   fCanvasImpID->resize(w,h);
}
//______________________________________________________________________________
void TQtCanvasImp::SetWindowTitle(const Text_t *newTitle) 
{ 
   fCanvasImpID->setCaption(newTitle);
}
//______________________________________________________________________________
void TQtCanvasImp::SetStatusText(const char *text, Int_t partidx)
{
  // Set Text into the 'npart'-th part of the status bar
  if (Int_t(fStatusBar.size()) > partidx) {
    fStatusBar[partidx]->setText(text);
  }
} 
//______________________________________________________________________________
void TQtCanvasImp::ShowEditor(Bool_t show)
{
   // fprintf(stderr,"QtCanvasImp::ShowEditor %d\n",show);

   // Canvas()->ToggleEditor();  / Toogle call ShowEditor !!!
   TVirtualPad *savedPad = 0;
   savedPad = (TVirtualPad *) gPad;
   gPad = Canvas();
   
   if (show) {
      if (!fActions[kViewEditor]->isOn()) 
         fActions[kViewEditor]->setOn(true);
      if (!fEditor) CreateEditor();
      if (fEditor) fEditor->Show();
   } else {
      if (fActions[kViewEditor]->isOn()) 
         fActions[kViewEditor]->setOn(false);
      if (fEditor) fEditor->Hide();
   }
   if (savedPad) gPad = savedPad;
   QTimer::singleShot(0,fCanvasID, SLOT(Refresh()));
}
//______________________________________________________________________________
void TQtCanvasImp::ShowMenuBar(Bool_t show)
{
  if (show) fMenuBar->show();
  else fMenuBar->hide();
}
//______________________________________________________________________________
void TQtCanvasImp::ShowStatusBar(Bool_t show)
{
  if (show) fCanvasImpID->statusBar()->show(); 
  else      fCanvasImpID->statusBar()->hide(); 
#if 0
  TCanvas *c = Canvas();
  if (c) {
    c->Modified();
    c->Update();
  }
#endif  
}
//______________________________________________________________________________
void TQtCanvasImp::ShowToolBar(Bool_t show)
{
   // Show or hide toolbar.
   if (show) {
       if (!fActions[kViewToolbar]->isOn()) 
          fActions[kViewToolbar]->setOn(true);
       fFileToolBar->show();
       fToolBar->show();
       fEditToolBar->show();
    } else {
       if (fActions[kViewToolbar]->isOn()) 
          fActions[kViewToolbar]->setOn(false);
       fFileToolBar->hide();
       fToolBar->hide();
       fEditToolBar->hide();
    }
}

//______________________________________________________________________________
void  TQtCanvasImp::ShowToolTips(Bool_t show ) 
{
   if (show) {}
}

//______________________________________________________________________________
Bool_t TQtCanvasImp::HasEditor()    const { return fEditToolBar!=0; }
//______________________________________________________________________________
Bool_t TQtCanvasImp::HasMenuBar()   const { return fToolBar!=0; }
//______________________________________________________________________________
Bool_t TQtCanvasImp::HasStatusBar() const { return fStatusBar.size() > 0; }
//______________________________________________________________________________
Bool_t TQtCanvasImp::HasToolBar()   const { return fToolBar!=0; }
//______________________________________________________________________________
Bool_t TQtCanvasImp::HasToolTips()  const { return kFALSE; }
   
//______________________________________________________________________________
void TQtCanvasImp::Show(){
  if (fCanvasImpID) {
     fCanvasImpID->show();
//    gVirtualX->MapRaised(TGQt::iwid(fCanvasImpID));
  }
}
//______________________________________________________________________________
void TQtCanvasImp::NewCB()
{ 
  NewCanvas();
}
//______________________________________________________________________________
void TQtCanvasImp::OpenCB()
{
   static QString thisCintCommand;
   static QString filetypes = "ROOT files (*.root);;";
   if( Canvas()) {
      QString dir = fSaveFileName;
      if (dir.isEmpty()) dir = gSystem->WorkingDirectory(); 
      else               dir = QFileInfo(dir).dirPath();

      QString fOpenFileName = QFileDialog::getOpenFileName (
           fCanvasImpID
         , tr("Open ROOT file ")
         , dir
         , filetypes );
      
      if (!fOpenFileName.isEmpty()){
         thisCintCommand = "{new TFile(\"";
         thisCintCommand += fOpenFileName;
         thisCintCommand +="\",\"update\");}";
         gROOT->ProcessLine((const char *)thisCintCommand);
      }
   }
}

//______________________________________________________________________________
void TQtCanvasImp::SaveCB()
{ 
   if (!(fSaveFileName.isEmpty() ||  fSaveType.isEmpty()) ) 
      SaveFile(fSaveFileName,fSaveType);
   else 
      SaveAsCB();
}

//______________________________________________________________________________
void TQtCanvasImp::SaveAsCB()
{ 
  QString selectedFilter = "all files (*.*)"; 
  QString filter = 
      "C++ macro (*.cpp,*.cxx,*.C);"
     ";Postscript (*.ps);"
     ";Encapsulated Postscript  (*.eps);"
     ";Portable Document Format (*.pdf);"
     ";Scalable Vector Graphics (*.svg);"
     ";Extensible Markup Language (*.xml);"
     ";Graphics Interchange Format (*.gif);"
     ";Web page (*.html);"
     ";ROOT file (*.root);"
     ";Image (";

  UInt_t i=0;
  QList<QByteArray> formats =  QImageWriter::supportedImageFormats();
  QList<QByteArray>::const_iterator j;
  for (j = formats.constBegin(); j != formats.constEnd(); ++j)
  {
    if (i) filter +=',';
    filter += "*.";
    QString str =  *j; i++;
    filter += str.lower();
  }
  filter +=");";
  filter +=";all files (*.*);;";

  QString thatFile = QFileDialog::getSaveFileName(
         fCanvasImpID 
       , tr("Save the selected Canvas/Pad as")
       , gSystem->WorkingDirectory()
       , filter
       , &selectedFilter
       );
  if (thatFile.isEmpty()) return;
  SaveFile(thatFile,selectedFilter);
}


//______________________________________________________________________________
void TQtCanvasImp::SaveAsWebCB()
{ 
  QString filter = "Web page (*.html);";

  QString thatFolder = QFileDialog::getExistingDirectory(
         fCanvasImpID
       , tr("Select the folder to save the Canvas/Pad as Web site")
       , gSystem->WorkingDirectory() );

   if (thatFolder.isEmpty()) return;
   if (fActions[kViewZoomer]->isOn() ) {
       TQtCanvas2Html a(Canvas(),1,(const char *)thatFolder,fgZoomingWidget);
   } else {
       TQtCanvas2Html a(Canvas(),1.8,(const char *)thatFolder);
   }
}


//______________________________________________________________________________
void TQtCanvasImp::SaveFile(const QString &theFile, const QString &selectedFilter)
{
  //
  // selectedFilter contains "*.* - the output format is defined by the file extension
  //                        != *.* - outputformat is defined by the seleted filter
  QString thatFile = theFile;
  QString e;
  bool rootFormatFound = kTRUE;
  Info("SaveFile","Selected filter %s \n", (const char *)selectedFilter);

  //  define the file extension
  QString fileNameExtension = QFileInfo(thatFile).suffix();
  QString  saveType = fileNameExtension.upper();

  if (selectedFilter.contains("*.*")) {
     if (!fileNameExtension.isEmpty() ) {
         saveType =  RootFileFormat(fileNameExtension);
         if (saveType.isEmpty() )  {
            rootFormatFound = kFALSE;
            saveType = QtFileFormat(fileNameExtension);
         }
     } 
  } else {
     saveType =  RootFileFormat(selectedFilter);
     if (saveType.isEmpty() && !selectedFilter.contains("html"))  {
        rootFormatFound = kFALSE;
        saveType = QtFileFormat(fileNameExtension);
     }
     if (saveType.isEmpty() && selectedFilter.contains("html"))  {
        rootFormatFound = kFALSE;
        saveType = "HTML";
     }
  }

  if (saveType.isEmpty() ) {
     TCanvas *c =  Canvas();
     if (c) c->Error("Save As", "no image format is defined");
     return;
  }
  fSaveType      = saveType;
  fSaveFileName  = theFile;

//   if (! thatFile.contains('.'))  thatFile += '.';
//   if (thatFile.at(thatFile.length()-1) == '.')  thatFile += defExtension[i];
  
  Info("TQtCanvasImp::SaveFile","Save %d:<%s> file as \"%s\"\n",rootFormatFound,(const char *)fSaveFileName, (const char *)fSaveType);
  if (fSaveType == "HTML") {
     if (fActions[kViewZoomer]->isOn() ) {
        TQtCanvas2Html a(Canvas(),1,0,fgZoomingWidget);
     } else {
        TQtCanvas2Html a(Canvas());
     }
  } else {
       fCanvasID->Save(fSaveFileName,fSaveType);
  }
}
//____________________________________________________________________________
void TQtCanvasImp::PrintCB()
{ 
  QPrinter p;
  if (p.setup()) {     
    QPixmap *pix = fCanvasID->GetOffScreenBuffer(); //(QPixmap *)TGQt::iwid(c->GetCanvasID());
    QPainter pnt(&p);
    pnt.drawPixmap(0,0,*pix);
  }
}

//______________________________________________________________________________
void TQtCanvasImp::CloseCB()
{
#if 1   
  fCanvasImpID->hide();
  // qDebug() << "TQtCanvasImp::CloseCB()";
  QTimer::singleShot(0, this, SLOT(ReallyDeleteCB()));
#else
  Close();
  delete this;
#endif  
}

//______________________________________________________________________________
void TQtCanvasImp::QuitCB()
{
   if (gROOT->GetClass("TStyleManager"))
     gROOT->ProcessLine("TStyleManager::Terminate()");
   gApplication->Terminate(0);
}

// editor menu 
//______________________________________________________________________________
void TQtCanvasImp::CopyCB()
{
//  Copy the current gPad to the system clipboard 
  QPixmap *p = 0;
  if (gPad == gPad->GetCanvas() ) {
    // Copy the double buffer of the TCanvas
    p = fCanvasID->GetOffScreenBuffer();
  } else {
    // Get the selected TPad only
    p = (QPixmap *)TGQt::iwid(gPad->GetPixmapID());
  }
  if (p) {
     QClipboard *cb = QApplication::clipboard();
     cb->setPixmap(*p);
  }
}

//______________________________________________________________________________
void TQtCanvasImp::CopyFrameCB()
{
  // Copy the entire window including the menu and the status bar
  QClipboard *cb = QApplication::clipboard();
  cb->setPixmap(QPixmap::grabWidget(fCanvasImpID->topLevelWidget()));
}

//______________________________________________________________________________
void TQtCanvasImp::EditorCB()
{
  // Canvas()->EditorBar(); 
   TCanvas *c = Canvas();
   // This is XOR operation
   if (c && (( c->GetShowEditor() && !fActions[kViewEditor]->isOn() )  ||
             (!c->GetShowEditor() &&  fActions[kViewEditor]->isOn() ) 
            ) 
      ) {
          Canvas()->ToggleEditor();
      }
//     ShowEditor(fActions[kViewEditor]->isOn());
}

//______________________________________________________________________________
void TQtCanvasImp::CreateEditor()
{
   // Create editor.
#if 0
   if (TVirtualPadEditor::GetPadEditor(kFALSE) != 0) {
      TVirtualPadEditor::HideEditor();
   }
#endif   
  // fEditorFrame->SetEditable();
  // gPad = Canvas();
   TString show = gEnv->GetValue("Canvas.ShowEditor","false");
   gEnv->SetValue("Canvas.ShowEditor","true");
   gPad = Canvas();
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,20,0)
   fEditor = TVirtualPadEditor::LoadEditor(); //  TVirtualPadEditor::LoadEditor();
#else
   fEditor = TVirtualPadEditor::GetPadEditor(kTRUE); //  TVirtualPadEditor::LoadEditor();
#endif
   if (fEditor) fEditor->SetGlobal(kFALSE);

   // next line is related to the old editor
   if (show == "false") gEnv->SetValue("Canvas.ShowEditor","false");
}
//______________________________________________________________________________
void TQtCanvasImp::ClearPadCB()
{
   gPad->Clear();
   gPad->Modified();
   gPad->Update();
}
//______________________________________________________________________________
void TQtCanvasImp::ClearCanvasCB()
{
   TCanvas *c = Canvas();
   if (!c) return;
   c->Clear();
   c->Modified();
   c->Update();
}

//______________________________________________________________________________
void TQtCanvasImp::BrowserCB()
{ new TBrowser("browser"); }
//______________________________________________________________________________
void TQtCanvasImp::ZoomCB()
{
   // Activate the Qt Zoomer
   
   TQtRootAction *action = (TQtRootAction *)sender();
   if (!fgZoomingWidget && (action->isOn()))  {
       fgZoomingWidget = new TQtZoomPadWidget();
       QWhatsThis::showText(QCursor::pos(),
       tr("<P>Click any <b>TPad</b> object with the <b>middle</b> mouse button to zoom it out"));
   }
   if (action->isOn()) { 
       fgZoomingWidget->Connect(fCanvasID);
   } else {
       fgZoomingWidget->Disconnect(fCanvasID);
   }
}
//______________________________________________________________________________
void TQtCanvasImp::ColorsCB()
{
  TVirtualPad *padsav = gPad->GetCanvas();
  char defcanvas[32];
  strcpy(defcanvas,gROOT->GetDefCanvasName());
  gROOT->SetDefCanvasName("DisplayColors");
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,03)
  gROOT->MakeDefCanvas();
#else   
  (gROOT->GetMakeDefCanvas())();
#endif  
  gROOT->SetDefCanvasName(defcanvas);
  TPad::DrawColorTable();
  gPad->Update();
  padsav->cd();
}
//______________________________________________________________________________
void TQtCanvasImp::FontsCB()
{ printf(" TQtCanvasImp::FontsCB() \n"); }
//______________________________________________________________________________
void TQtCanvasImp::MarkersCB()
{
  TVirtualPad *padsav = gPad->GetCanvas();
  TCanvas *m = new TCanvas("markers","MarkersTypes",600,200);
  TMarker::DisplayMarkerTypes();
  m->Update();
  padsav->cd();
}
//______________________________________________________________________________
void TQtCanvasImp::IconifyCB()
{ Iconify(); }
//______________________________________________________________________________
void TQtCanvasImp::X3DViewCB()
{  
  // Create X3d viewer
  // This entry for X11 only. It is never called under WIN32
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)   
      Error("X3DViewCB","  There is no 3D viewer implementation with the Qt layer for ROOT 4.03 yet !!!\n");
#else   
  gPad->x3d();
#endif  
}
//______________________________________________________________________________
void TQtCanvasImp::GLViewCB()
{  
   // Load Qt-based GL viewer first
   // // static bool loadFlag = true;
   // if (loadFlag) loadFlag = gQt->LoadQt("libRQTGL");
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,03)   
   //  Make sure gPad belong out TCanvas 
   TVirtualPad *thisPad = gPad;
   TCanvas *c = gPad->GetCanvas();
   if (c != Canvas()) thisPad =  Canvas();
   TVirtualViewer3D *viewer = TVirtualViewer3D::Viewer3D(thisPad,"ogl");
   if (viewer) {
      // Create Open GL viewer
      TGQt::SetCoinFlag(0);
      viewer->BeginScene();
      viewer->EndScene();
   }
#else   
   if (! glViewerLoadFlag) {
      glViewerLoadFlag = !gQt->LoadQt("libRQTGL");
   }
   if (glViewerLoadFlag) {
      // Create Open GL viewer
      TGQt::SetCoinFlag(0);
      gPad->x3d("OPENGL");
   }
#endif  
   else {
      fActions[kViewInventorGL]->setEnabled(false);
   }
}
//______________________________________________________________________________
void TQtCanvasImp::GLIVViewCB()
{  
   // Create Open GL viewer
   // Load Qt-based GL viewer first
   // static bool loadFlag = true;
   // if (loadFlag) loadFlag = gQt->LoadQt("libRQIVTGL");
// #if  ROOT_VERSION_CODE < ROOT_VERSION(5,20,0)
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)
#if  ROOT_VERSION_CODE < ROOT_VERSION(5,20,0)
   // Load the Coin library for STAR
   static bool coinWasLoaded = false;
   if (!coinWasLoaded) {
      TString ivrootDir = gSystem->Getenv("IVROOT");
      if (ivrootDir.IsNull() ) {
         ivrootDir = "$ROOT/5.99.99/Coin2/.$STAR_HOST_SYS";
      }
      if (!ivrootDir.IsNull()) {
#  ifndef R__WIN32
         ivrootDir += "/lib/";
         gSystem->ExpandPathName(ivrootDir);
         if (!gSystem->AccessPathName(ivrootDir.Data())) {
            if ( ! (gSystem->Load(ivrootDir+"libSoQt") + 
            gSystem->Load(ivrootDir+"libCoin") 
            /* + gSystem->Load(ivrootDir+"libSmallChange") */)  )
                   coinWasLoaded = true; // Try to load it at once
         }
#  else
/*
         ivrootDir += "/bin/";
         gSystem->ExpandPathName(ivrootDir);
         if (!gSystem->AccessPathName(ivrootDir.Data())) {
            if ( ! (gSystem->Load(ivrootDir+"soqt1.dll") + 
            gSystem->Load(ivrootDir+"coin2.dll") +
            gSystem->Load(iirootDir+"SmallChange1.dll")) )
*/
                   coinWasLoaded = true; // Try to load it at once
//       }
#  endif
      }
   }
#endif
   //  Make sure gPad belong out TCanvas 
   TVirtualPad *thisPad = gPad;
   TCanvas *c = gPad->GetCanvas();
   if (c != Canvas()) thisPad = Canvas();
   TVirtualViewer3D *viewer = TVirtualViewer3D::Viewer3D(thisPad,"oiv");
   if (viewer) {
      // Create Open GL viewer
      TGQt::SetCoinFlag(1);
      viewer->BeginScene();
      viewer->EndScene();
    }
#else
   if (! glViewerLoadFlag) {
      glViewerLoadFlag = !gQt->LoadQt("libRQTGL");
   }
   if (glViewerLoadFlag) {
      // Create Open GL viewer
      TGQt::SetCoinFlag(1);
      gPad->x3d("OPENGL");
   }
#endif
   else { 
      fActions[kViewInventorGL]->setEnabled(false);
   }
}

//______________________________________________________________________________
void TQtCanvasImp::InterruptCB()
{ gROOT->SetInterrupt(); }
//______________________________________________________________________________
void TQtCanvasImp::ModifiedUpdate()
{
   gPad->Modified();
   Canvas()->Modified();
   Canvas()->Update();
}
//______________________________________________________________________________
void TQtCanvasImp::AutoFitCanvasCB()
{}
//______________________________________________________________________________
void TQtCanvasImp::FitCanvasCB()
{}
//______________________________________________________________________________
void TQtCanvasImp::RefreshCB()
{  
   TCanvas *c = Canvas();
   if (!c) return;
   c->Paint();
   ModifiedUpdate();
}
//______________________________________________________________________________
void TQtCanvasImp::MenuBarCb()
{
  // ShowMenuBar(fActions[kViewMenuBar]->isOn());

}
//______________________________________________________________________________
void TQtCanvasImp::ToolBarCB()
{
   ShowToolBar(fActions[kViewToolbar]->isOn());
}
//______________________________________________________________________________
void TQtCanvasImp::EventStatusCB()
{  
   TCanvas *c = Canvas();
   if (!c) return;
   TQtRootAction *action = (TQtRootAction *)sender();
   c->ToggleEventStatus();
   if (c->GetShowEventStatus())
   {
      ShowStatusBar();
      if (!action->isOn()) action->setOn(true);
   }
   else {
      if (action->isOn()) action->setOn(false);
      ShowStatusBar(kFALSE);
   }
   ModifiedUpdate();
}
//______________________________________________________________________________
void TQtCanvasImp::PadAutoExecCB()
{
   TQtRootAction *action = (TQtRootAction *)sender();
   TCanvas *c = Canvas();
   if (c) {
      c->ToggleAutoExec();
      if (c->GetAutoExec() != action->isOn() ) 
         action->setOn(c->GetAutoExec());
      ModifiedUpdate();
   }
}

//______________________________________________________________________________
void TQtCanvasImp::OptStatCB()
{
   TQtRootAction *action = (TQtRootAction *)sender();
   if (gStyle->GetOptStat()) {
      gStyle->SetOptStat(0);
      delete gPad->FindObject("stats");
      if (action->isOn()) action->setOn(false);
   } else {
      gStyle->SetOptStat(1);
      if (!action->isOn()) action->setOn(true);
   }
   ModifiedUpdate();
}
//______________________________________________________________________________
void TQtCanvasImp::OptTitleCB()
{
   TQtRootAction *action = (TQtRootAction *)sender();
   if (gStyle->GetOptTitle()) {
      gStyle->SetOptTitle(0);
      delete gPad->FindObject("title");
      if (action->isOn()) action->setOn(false);
   } else {
      gStyle->SetOptTitle(1);
      if (!action->isOn()) action->setOn(true);
   }
   ModifiedUpdate();
}
//______________________________________________________________________________
void TQtCanvasImp::OptFitCB()
{
   TQtRootAction *action = (TQtRootAction *)sender();
   if (gStyle->GetOptFit()) {
      gStyle->SetOptFit(0);
      if (action->isOn()) action->setOn(false);
   } else {
      gStyle->SetOptFit(1);
      if (!action->isOn()) action->setOn(true);
   }
   ModifiedUpdate();
}
//______________________________________________________________________________
void TQtCanvasImp::CanEditHistogramsCB()
{
   TQtRootAction *action = (TQtRootAction *)sender();
   if (gROOT->GetEditHistograms()) {
      gROOT->SetEditHistograms(kFALSE);
      if (action->isOn()) action->setOn(false);
   } else {
      gROOT->SetEditHistograms(kTRUE);
      if (!action->isOn()) action->setOn(true);
   }
}
  //*-*   Items for the Inspect Menu
//______________________________________________________________________________
void TQtCanvasImp::ROOTInspectCB()
{
   Canvas()->cd();
   gROOT->Inspect();
}
   
   //*-*   Items for the Class Menu
//______________________________________________________________________________
void TQtCanvasImp::FullTreeCB()
{}

//______________________________________________________________________________
void TQtCanvasImp::AboutCB()
{ 
  QMessageBox::aboutQt(0);
  QMessageBox::about(0,"ROOT Canvas with Qt interface"
    ,"ROOT Qt interface Copyright (C) 2001-2002, Valeri Fine. Brookhaven National Laboratory. All right reserved.");
#ifdef R__UNIX
                        TString rootx;
# ifdef ROOTBINDIR
                        rootx = ROOTBINDIR;
# else
                        rootx = gSystem->Getenv("ROOTSYS");
                        if (!rootx.IsNull()) rootx += "/bin";
# endif
                        rootx += "/root -a &";
                        gSystem->Exec(rootx);
#else                        
                        QString str = QString("ROOT ") + QString(gROOT->GetVersion());
                        QString helpAbout = gHelpAbout;
                        helpAbout.replace("\n"," ");
                        HelpOn(str,helpAbout);
#endif
}
//______________________________________________________________________________
void TQtCanvasImp::HelpCB()
{
#ifdef R__WIN32
  gSystem->Exec("explorer http://root.cern.ch/root/html/ClassIndex.html");
#else
  gSystem->Exec("netscape http://root.cern.ch/root/html/ClassIndex.html");
#endif
}
//______________________________________________________________________________
void TQtCanvasImp::HelpOn(const char *title, const char *text) {
    QString body = "<tt><center><b>";
    body += title; body += "</b></center><br>";
    body += text;
    body.replace("\n","<br>");
    QMessageBox::about(0,title,body);
 }
//______________________________________________________________________________
void TQtCanvasImp::HelpOnCanvasCB(){  
   HelpOn("Help On Canvas...",gHelpAbout ); 
}
//______________________________________________________________________________
void TQtCanvasImp::HelpOnMenusCB() { 
   HelpOn("Help On Menus...", gHelpPullDownMenus);
}
//______________________________________________________________________________
void TQtCanvasImp::HelpOnGraphicsEdCB() {
   HelpOn("Help On Graphics Editor...", gHelpGraphicsEditor); 
}
//______________________________________________________________________________
void TQtCanvasImp::HelpOnBrowserCB() {
   HelpOn("Help On Browser...", gHelpBrowser ); 
}
//______________________________________________________________________________
void TQtCanvasImp::HelpOnObjectsCB() { 
   HelpOn("Help On Objects...", gHelpObjects); 
}
//______________________________________________________________________________
void TQtCanvasImp::HelpOnPSCB()      { 
   HelpOn("Help On PostScript...",gHelpPostscript ); 
}
