// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/


#include <Riostream.h>

#include <TROOT.h>
#include <TRint.h>
#include <TVirtualX.h>
#include <TEnv.h>

#include <TFile.h>
#include <TTree.h>
#include <TFrame.h>
#include <TH1.h>
#include <TF1.h>

#include "TQtWidget.h"
#include <qfile.h>
#include <q3toolbar.h>
#include <q3popupmenu.h>
#include <q3vbox.h>
#include <qtimer.h>
#include <q3frame.h>
#include <q3hbox.h>
#include <qsplitter.h>
#include <q3valuelist.h>
#include <q3listview.h>
#include <qtabwidget.h>
#include <q3textbrowser.h>
#include <qstatusbar.h>
#include <qaction.h>
#include <qmenubar.h>
#include <qtooltip.h>
#include <qmessagebox.h>
#include <q3filedialog.h>
#include <qapplication.h>
//Added by qt3to4:
#include <Q3TextStream>
#include <QLabel>
#include <QPixmap>
#include <QDebug>

#include "GShowerPad.h"
#include <TCanvas.h>
//#include <TRandom.h>
#include <TBrowser.h>
#include <TParticle.h>
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)   
#  include "TVirtualViewer3D.h"
#  include "TGQt.h"
#else
#  include <TPadView3D.h>
#endif

#include "RootShower.h"
#include "MyParticle.h"
#include "GTitleFrame.h"
#include "GShowerPad.h"
#include "GButtonFrame.h"
#if 0
#include "RSMsgBox.h"
#else 
#include "TGPicture.h"
#endif

#include "RSAbout.h"
#include "SettingsDlg.h"
#include "RSHelpText.h"
#include "MyEvent.h"
#include "ProgramPath.h"

#include <TGeometry.h>
#include <TView.h>
#include <TBRIK.h>
#include <TMixture.h>
#include <TNode.h>


#include <THtml.h>

#ifndef _CONSTANTS_H_
#include "constants.h"
#endif
// flag to load Qt-based OpenGL Viewer
static bool loadFlag = TRUE;

enum RootShowerMessageTypes {
    M_FILE_OPEN,
    M_FILE_SAVE,
    M_FILE_SAVEAS,
    M_FILE_HTML,
    M_FILE_EXIT,
    M_ZOOM_PLUS,
    M_ZOOM_MOINS,
    M_ZOOM_PLUS2,
    M_ZOOM_MOINS2,

    M_SHOW_PROCESS,
    M_ANIMATE_GIF,
    M_SETTINGS_DLG,
    M_SETTINGS_SAVE,
    M_SHOW_INFOS,
    M_SHOW_3D,
    M_SHOW_TRACK,

    M_VIEW_TOOLBAR,
    M_INSPECT_BROWSER,

    M_HELP_PHYSICS,
    M_HELP_SIMULATION,
    M_HELP_LICENSE,
    M_HELP_ABOUT
};

//___________________________________________________________
const char *xpm_names[] = {
    "open.xpm",
    "save.xpm",
    "saveas.xpm",
    "",
    "settings.xpm",
    "",
    "infos.xpm",
    "view3d.xpm",
    "",
    "browser.xpm",
    "",
    "manual.xpm",
    "help.xpm",
    "license.xpm",
    "about.xpm",
    "",
    "quit.xpm",
    0
};

ToolBarData_t tb_data[] = {
  { "&Open",            "Open Root event file",     kFALSE, M_FILE_OPEN,        NULL },
  { "Save",             "Save event in current Root file",  kFALSE, M_FILE_SAVE,  NULL },
  { "Save &As",         "Save event in Root file",  kFALSE, M_FILE_SAVEAS,      NULL },
  { "",              0,             0,      -1,                 NULL },
  { "&Settings...",     "Event settings",           kFALSE, M_SETTINGS_DLG,     NULL },
  { "",              0,             0,      -1,                 NULL },
  { "&Infos...",        "Infos on current event",   kFALSE, M_SHOW_INFOS,       NULL },
  { "&3D View",         "Open 3D viewer",           kFALSE, M_SHOW_3D,          NULL },
  { "",              0,             0,      -1,                 NULL },
  { "&Browser",         "Start Root browser",       kFALSE, M_INSPECT_BROWSER,  NULL },
  { "",              0,             0,      -1,                 NULL },
  { "&Physics",         "Physics recalls",          kFALSE, M_HELP_PHYSICS,     NULL },
  { "&Simulation",      "RootShower help",          kFALSE, M_HELP_SIMULATION,  NULL },
  { "&License...",      "Display license",          kFALSE, M_HELP_LICENSE,     NULL },
  { "&About",           "About RootShower",         kFALSE, M_HELP_ABOUT,       NULL },
  { "",              0,             0,      -1,                 NULL },
  { "E&xit",            "Exit Application",         kFALSE, M_FILE_EXIT,        NULL },
//  { NULL,            NULL,          0,      0,                  NULL },
  { "Save &Parameters", "Save Parameters",     kFALSE, M_SETTINGS_SAVE,    NULL },
  { "Show &Process",    "Save Parameters",     kFALSE, M_SHOW_PROCESS,     NULL },
  { "Animated &GIF",    "Animated GIF",        kFALSE, M_ANIMATE_GIF,      NULL },
  { "&Show Selected  Track", "Show Selected  Track",    kFALSE, M_SHOW_TRACK,        NULL },
  { "&Create Html Doc", "Show Selected  Track",kFALSE, M_FILE_HTML,        NULL },
  { "&Toolbar",         "Show / hide toolbar", kFALSE, M_VIEW_TOOLBAR,     NULL },
  { "&License...",      "",                    kFALSE, M_HELP_LICENSE,     NULL },
  { "&About...",        "",                    kFALSE, M_HELP_ABOUT,       NULL },
  { NULL,            NULL,          0,      0,                  NULL }
};

RootShower      *gRootShower;
Int_t            gColIndex;
Q3ListViewItem  *gEventListTree; //  top "event" for 
Q3ListViewItem  *gTmpLTI;
Q3ListViewItem  *gLTI[MAX_PARTICLE];

const TGPicture *bpic, *bspic;
const TGPicture *lpic, *lspic;

 static QString filetypes = 
    "ROOT files (*.root);;";
    //"ROOT macros (*.C);"
    //"GIF  files (*.gif);"
    //"PS   files (*.ps);"
    //"EPS  files (*.eps);"
    //"All files (*.*);;";

//_________________________________________________
// RootShower
//

Int_t RootShower::fgDefaultXPosition = 20;
Int_t RootShower::fgDefaultYPosition = 20;


//______________________________________________________________________________
RootShower::RootShower(QWidget *p, UInt_t w, UInt_t h): Q3MainWindow (p)
{
    // Create (the) Event Display.
    //
    // p = pointer to GMainFrame (not owner)
    // w = width of RootShower frame
    // h = width of RootShower frame

    fOk                 = kFALSE;
    fModified           = kFALSE;
    fSettingsModified   = kFALSE;
    fIsRunning          = kFALSE;
    fTimer              = 0;
    fPicIndex           = 1;

    fRootShowerEnv = new TEnv(".rootshowerrc");

    fFirstParticle = fRootShowerEnv->GetValue("RootShower.fFirstParticle", PHOTON);
    fE0            = fRootShowerEnv->GetValue("RootShower.fE0", 10.0);
    fB             = fRootShowerEnv->GetValue("RootShower.fB", 20.000);
    fMaterial      = fRootShowerEnv->GetValue("RootShower.fMaterial", Polystyrene);
    fDimX          = fRootShowerEnv->GetValue("RootShower.fDimX", 100.0);
    fDimY          = fRootShowerEnv->GetValue("RootShower.fDimY", 200.0);
    fDimZ          = fRootShowerEnv->GetValue("RootShower.fDimZ", 100.0);
    fPicNumber     = fRootShowerEnv->GetValue("RootShower.fPicNumber", 24);
    fPicDelay      = fRootShowerEnv->GetValue("RootShower.fPicDelay", 250);
    fPicReset      = fRootShowerEnv->GetValue("RootShower.fPicReset", 1);

    fEventNr = 0;
    fNRun    = 0;

    fLeafPic   = (QPixmap *)gClient->GetPicture("leaf_t.xpm")->GetPicture();
    fBranchPic = (QPixmap *)gClient->GetPicture("branch_t.xpm")->GetPicture();
    // Create a container of the actions
    int i;
    for (i = 0; tb_data[i].fPixmap!=NULL; i++) {
       // skip the separators 
       if (!tb_data[i].fPixmap[0]) continue;
       ShowerAction *action = new ShowerAction(this,tb_data[i]);
       fActions.insert(action->Id(),action);
       connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
    }

    //---- toolbar. Add the action with the icon to the toolbar
    fToolBar = new Q3ToolBar(this);
    addDockWindow(fToolBar);
    for (i = 0; xpm_names[i]; i++) {
        if (xpm_names[i][0]) {
           char *iconname = new char[100];
           sprintf(iconname,":/icons/%s",xpm_names[i]);
           ShowerAction *action = fActions[tb_data[i].fId];
           action->setIconSet( QIcon(iconname ));
           action->addTo(fToolBar);
        }
    }

    // Create menubar and popup menus.
    MakeMenuBarFrame();

    Q3VBox *fVBox = new Q3VBox(this,"central");
    setCentralWidget(fVBox);

    // CREATE TITLE FRAME
    fTitleFrame = new GTitleFrame(fVBox, "ROOT Shower Monte Carlo", "Event Display", 100, 100);
    // CREATE MAIN SCHENE  FRAME
    Q3HBox *fHBox = new Q3HBox(fVBox,"scene");

   // Create the central frame

    QSplitter *split = new QSplitter(fHBox);
    //QValueList<int> sizes;
    //sizes.push_back(w/4); sizes.push_back(w-(w/4));
    //split->setSizes(sizes);

    fSelectionFrame   = new Q3VBox(split,"event");

    // left pane
    // create button frame
    fButtonFrame = new GButtonFrame (fSelectionFrame);
    connect(fButtonFrame,SIGNAL( NextEvent() ),this,SLOT( NextEvent() ));
    connect(fButtonFrame,SIGNAL(SelectEvent()),this,SLOT(SelectEvent()));
    connect(fButtonFrame,SIGNAL( Interrupt() ),this,SLOT( Interrupt() ));

    fEventListTree =  new Q3ListView ( fSelectionFrame);
    fEventListTree->addColumn("Particles"); 
    fEventListTree->addColumn("Energy [Gev]"); 
    connect( fEventListTree, SIGNAL(selectionChanged ( Q3ListViewItem * )),this,SLOT(OnShowSelected(Q3ListViewItem *)));
    gEventListTree = new Q3ListViewItem( fEventListTree, "Event" );

    // CREATE MAIN FRAME
    // Create Display Canvas Tab (where the actual main event is displayed)
    fDisplayFrame = new QTabWidget(split,"mainDisplay");
       // first tab Main Event
       GShowerPad *tFrame = new GShowerPad(fDisplayFrame);
       fDisplayFrame->addTab(tFrame,"Main Event (Shower)");
       cA = tFrame->GetCanvas();

       // Selected track
       GShowerPad *sFrame = new GShowerPad(fDisplayFrame);
       fDisplayFrame->addTab(sFrame,"Selected Track");
       cB = sFrame->GetCanvas();

       // Histograms 
       fEmbeddedCanvas3 = new TQtWidget(fDisplayFrame,"statistics");
       fEmbeddedCanvas3->setSizePolicy(QSizePolicy::Expanding,QSizePolicy::Expanding);
       fDisplayFrame->addTab(fEmbeddedCanvas3,"Statistics");

       // PDG Table
       Q3TextBrowser *fTextView = new Q3TextBrowser(fDisplayFrame);
       fTextView->setWordWrap(Q3TextEdit::NoWrap);
       fTextView->setFamily("Courier");
       fDisplayFrame->addTab(fTextView,"PDG Table");

 // ---- 

    cB->cd();
    fSelection = new TGeometry("selection","selection");
    fSelection->cd();
    sel_detect = new TBRIK("sel_detect","sel_detect","void",fDimX/2.0,fDimY/2.0,fDimZ/2.0);
    sel_detect->SetLineColor(7);
    sel_node = new TNode("SEL_NODE","SEL_NODE","sel_detect");
    sel_node->cd();
    fSelection->Draw();
    cB->GetView()->SetPerspective();
    cB->GetView()->SideView();

    cB->Update();

//  -- third tab: Histograms 
    fEmbeddedCanvas3->GetCanvas()->SetBorderMode(0);
    cC = fEmbeddedCanvas3->GetCanvas();
    cC->SetFillColor(10);
    cC->cd();
    padC = new TPad("padC","Histogram",0.0,0.0,1.0,1.0,10,3,1);
    padC->SetFillColor(10);
    padC->SetBorderMode(0);
    padC->SetBorderSize(0);
    padC->Draw();

    // Creation of histogram for particle's energy loss
    fHisto_dEdX = new TH1F("Statistics","Energy loss for each particle",100,0,0.025); // Max at 25 MeV
    fHisto_dEdX->SetFillColor(38);
    fHisto_dEdX->SetStats(kTRUE);
    fHisto_dEdX->SetXTitle("Energy Loss [GeV]");
    fHisto_dEdX->SetLabelFont(42,"X");
    fHisto_dEdX->SetLabelSize(0.03f, "X");
    fHisto_dEdX->GetXaxis()->SetTitleFont(42);
    fHisto_dEdX->SetYTitle("Number");
    fHisto_dEdX->SetLabelFont(42,"Y");
    fHisto_dEdX->SetLabelSize(0.03f, "Y");
    fHisto_dEdX->GetYaxis()->SetTitleFont(42);

    cC->Update();

    // Create text display Tab
    QString pdgFilename = gSystem->Getenv("ROOTSYS");
    pdgFilename += "/etc/pdg_table.txt";

    QFile file( pdgFilename ); // Read the text from a file
    if ( file.open( QIODevice::ReadOnly ) ) {
        Q3TextStream stream( &file );
        fTextView->setText( stream.read() );
    }

    // Create status bar
    Int_t parts[] = {45, 45, 10};
    CreateStatusBar(parts,sizeof(parts)/sizeof(Int_t));
    SetStatusText("Waiting to start simulation...",0);

    // Finish RootShower for display...
    setCaption("Root Shower Event Display");
    setIconText("Root Shower Event Display");
    move(fgDefaultXPosition, fgDefaultYPosition);
    fEvent = new MyEvent();
    fEvent->Init(0, fFirstParticle, fE0, fB, fMaterial, fDimX, fDimY, fDimZ);
    Initialize(1);
    gROOT->GetListOfBrowsables()->Add(fEvent,"RootShower Event");
 //    gSystem->Load("libTreeViewer");
    gRootShower = this;
    resize( w, h);
}
//______________________________________________________________________________
void RootShower::SetStatusText(const char *text, Int_t partidx)
{
  // Set Text into the 'npart'-th part of the status bar
  if (Int_t(fStatusBar.size()) > partidx) {
    fStatusBar[partidx]->setText(text);
  }
} 
//______________________________________________________________________________
void RootShower::CreateStatusBar(Int_t *parts, Int_t nparts)
{
  QStatusBar *showerStatusBar = statusBar();
#ifdef WIN32
  showerStatusBar->setSizeGripEnabled(TRUE);
#endif
  // Any number of widgets may be controlled by just
  // one splitter
  QSplitter *split = new QSplitter(showerStatusBar);
  showerStatusBar->addWidget(split,1,FALSE);

  fStatusBar.resize(nparts);
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
void RootShower::MakeMenuBarFrame()
{
    // Create menubar and popup menus.
    fMenuBar = menuBar();
// -- File Menu
    fMenuFile = new Q3PopupMenu();

    fActions[M_FILE_OPEN]->addTo(fMenuFile); fActions[M_FILE_OPEN]->setAccel(Qt::CTRL+Qt::Key_O);
//    fMenuFile->insertItem("&Close",  this,SLOT(CloseCB()));
//    fMenuFile->insertItem("&Print",  this,SLOT(PrintCB()),CTRL+Key_P);
    fMenuFile->                    insertSeparator();
    fActions[M_FILE_SAVE]->  addTo(fMenuFile); fActions[M_FILE_SAVE]->  setEnabled(false);
    fActions[M_FILE_SAVEAS]->addTo(fMenuFile); fActions[M_FILE_SAVEAS]->setEnabled(false);
                                               fActions[M_FILE_SAVEAS]->setAccel(Qt::CTRL+Qt::Key_A);

    fMenuFile->                                 insertSeparator();
    fActions[M_FILE_EXIT]->addTo(fMenuFile); fActions[M_FILE_EXIT]->setAccel(Qt::CTRL+Qt::Key_X);
// -- Test Menu
    fMenuTest = new Q3PopupMenu();
    fMenuTest->insertItem("Simulation Settings...");
    fMenuTest->insertSeparator(); fMenuTest->insertSeparator();

    fActions[M_SETTINGS_DLG]-> addTo(fMenuTest);
    fActions[M_SETTINGS_SAVE]->addTo(fMenuTest);
    fActions[M_SHOW_PROCESS]-> addTo(fMenuTest); fActions[M_SHOW_PROCESS] ->setToggleAction (true);
                                                 fActions[M_SHOW_PROCESS]->setOn (false);
                                                 fActions[M_SHOW_PROCESS]->disconnect(SIGNAL(activated()));
    fActions[M_ANIMATE_GIF]->  addTo(fMenuTest); fActions[M_ANIMATE_GIF] ->setToggleAction (true);
                                                 fActions[M_ANIMATE_GIF]->setOn (false);
                                                 fActions[M_ANIMATE_GIF]->disconnect(SIGNAL(activated()));

    fActions[M_SHOW_INFOS]->   addTo(fMenuTest); fActions[M_SHOW_INFOS]->  setEnabled(false);

    fMenuTest->insertSeparator();

    fActions[M_SHOW_3D]->   addTo(fMenuTest); fActions[M_SHOW_3D]->   setEnabled(false);
    fActions[M_SHOW_TRACK]->addTo(fMenuTest); fActions[M_SHOW_TRACK]->setEnabled(false);
// -- Inpsect Menu
    fMenuInspect = new Q3PopupMenu();
    fMenuInspect->insertItem("Simulation Tools...");
    fMenuInspect->insertSeparator(); fMenuInspect->insertSeparator();

    fActions[M_INSPECT_BROWSER]->addTo(fMenuInspect); 
    fActions[M_FILE_HTML]->      addTo(fMenuInspect); 
// -- View Menu
    fMenuView = new Q3PopupMenu();
    fActions[M_VIEW_TOOLBAR]->addTo(fMenuView); fActions[M_VIEW_TOOLBAR]->setToggleAction (true);
                                                fActions[M_VIEW_TOOLBAR]->setOn (true);
    fActions[M_VIEW_TOOLBAR]->disconnect(SIGNAL(activated()));
    connect(fActions[M_VIEW_TOOLBAR],SIGNAL(toggled ( bool)), this,SLOT(ShowToolBar(bool)));
// ---  Help menu 
    fMenuHelp = new Q3PopupMenu();
    fActions[M_HELP_PHYSICS]->   addTo(fMenuHelp); 
    fActions[M_HELP_SIMULATION]->addTo(fMenuHelp); 
    fMenuHelp->insertSeparator();
    fActions[M_HELP_LICENSE]->   addTo(fMenuHelp); 
    fActions[M_HELP_ABOUT]->     addTo(fMenuHelp); 

// -- Main Menu

    fMenuBar->insertItem("&File",fMenuFile);
    fMenuBar->insertItem("&Event",fMenuTest);
    fMenuBar->insertItem("&Tools...",fMenuInspect);
    fMenuBar->insertItem("&View",fMenuView);
    fMenuBar->insertItem("&Help",fMenuHelp);
}

//______________________________________________________________________________
void RootShower::ShowToolBar(bool show)
{
   // Show or hide toolbar.
   if (show) fToolBar->show();
   else      fToolBar->hide();
}

//______________________________________________________________________________
RootShower::~RootShower()
{
    // Destroy RootShower object. Delete all created widgets
    // GUI MEMBERS
}

//______________________________________________________________________________
void RootShower::setDefaultPosition(Int_t x, Int_t y)
{
    // Set the default position on the screen of new RootShower instances.
    fgDefaultXPosition = x;
    fgDefaultYPosition = y;
}
//______________________________________________________________________________
void RootShower::CloseWindow()
{
    // Got close message for this RootShower. The EventDislay and the
    // application will be terminated.

#if 0
    if(fModified) {
        new RootShowerMsgBox(gClient->GetRoot(),this, 400, 200);
        if ( fOk ) {
            fRootShowerEnv->SetValue("RootShower.fFirstParticle",fFirstParticle);
            fRootShowerEnv->SetValue("RootShower.fE0",fE0);
            fRootShowerEnv->SetValue("RootShower.fB",fB);
            fRootShowerEnv->SetValue("RootShower.fMaterial",fMaterial);
            fRootShowerEnv->SetValue("RootShower.fDimX",fDimX);
            fRootShowerEnv->SetValue("RootShower.fDimY",fDimY);
            fRootShowerEnv->SetValue("RootShower.fDimZ",fDimZ);
            fRootShowerEnv->SaveLevel(kEnvLocal);
            cout << " Saving stuff .... " << endl;
#ifdef R__WIN32
            gSystem->Exec("del .rootshowerrc");
            gSystem->Rename(".rootshowerrc.new",".rootshowerrc");
#endif
        }
    }
#endif
    cout << "Terminating RootShower" << endl;
    gApplication->Terminate(0);
}
//______________________________________________________________________________
void  RootShower::ProcessMessage() {
    // Handle messages send to the RootShower object.
    //Window_t wdummy;
    //int ax, ay;
    // TRootHelpDialog *hd;
    // Char_t  strtmp[80];
    ShowerAction *actionSender =  (ShowerAction *)sender ();
    switch (actionSender->Id()) {
       case M_FILE_OPEN:
          if(! fIsRunning) {
             QString selectedFilter;
             QString dir = fSaveFileName;
             if (dir.isEmpty()) dir = gSystem->WorkingDirectory(); 
             else               dir = QFileInfo(dir).dirPath();

             QString fOpenFileName = Q3FileDialog::getOpenFileName (dir
                , filetypes, this, "Open"
                , "Open ROOT file "
                , &selectedFilter);
             if (!fOpenFileName.isEmpty()) OnOpenFile(fOpenFileName);
          }
          break;

       case M_FILE_HTML:
          {
             THtml html;
             html.SetSourceDir(".");
             html.MakeClass("MyParticle");
             html.MakeClass("MyDetector");
             html.MakeClass("EventHeader");
             html.MakeClass("MyEvent");
             html.MakeIndex();
          }
          break;

       case M_FILE_SAVE:
          if(!fIsRunning)  break;
          // If the file is known then use it
          if (!fSaveFileName.isEmpty()) {
             OnSaveFile(fSaveFileName);
             break;
          }
          // otherwise apply "Save As"
       case M_FILE_SAVEAS:
          if(!fIsRunning) {
             QString selectedFilter;
             QString dir = fSaveFileName;
             if (dir.isEmpty()) dir = gSystem->WorkingDirectory(); 
             fSaveFileName = Q3FileDialog::getSaveFileName(dir
                , filetypes, this, "SaveAs"
                , "Save the selected Canvas/Pad as"
                , &selectedFilter);
             if (!fSaveFileName.isEmpty()) {
                QFileInfo  fi(fSaveFileName);
                if (fi.extension().isEmpty()) fSaveFileName += ".root";
                OnSaveFile(fSaveFileName);
             }
          }
          break;

       case M_FILE_EXIT:
          CloseWindow();   // this also terminates theApp
          break;

       case M_SETTINGS_DLG:
          {
             if(fIsRunning) break;
             SettingsDialog *dialog = new SettingsDialog(this, 400, 200);
             if(dialog->exec() == QDialog::Accepted) {
                fSettingsModified =  kTRUE;
                fEvent->Init(0, fFirstParticle, fE0, fB, fMaterial, fDimX, fDimY, fDimZ);
                Initialize(0);
                Modified();
                SettingsModified(kFALSE);
             }
             delete dialog;
             break;
          }

       case M_SETTINGS_SAVE:
          fRootShowerEnv->SetValue("RootShower.fFirstParticle",fFirstParticle);
          fRootShowerEnv->SetValue("RootShower.fE0",fE0);
          fRootShowerEnv->SetValue("RootShower.fB",fB);
          fRootShowerEnv->SetValue("RootShower.fMaterial",fMaterial);
          fRootShowerEnv->SetValue("RootShower.fDimX",fDimX);
          fRootShowerEnv->SetValue("RootShower.fDimY",fDimY);
          fRootShowerEnv->SetValue("RootShower.fDimZ",fDimZ);
          fRootShowerEnv->SaveLevel(kEnvLocal);
#ifdef R__WIN32
          gSystem->Exec("del .rootshowerrc");
          gSystem->Rename(".rootshowerrc.new",".rootshowerrc");
#endif
          Modified(kFALSE);
          break;

       case M_SHOW_INFOS:
          if(fIsRunning) break;
          ShowInfos();
          break;

       case M_INSPECT_BROWSER:
          new TBrowser();
          break;

       case M_HELP_PHYSICS:
          {
             QMessageBox::information(0,"Help on Physics",physics_txt,QMessageBox::Ok);
             // hd = new TRootHelpDialog(this, str, 620, 350);
             //gVirtualX->TranslateCoordinates( GetId(), GetParent()->GetId(),
             //   (Int_t)(GetWidth() - 620) >> 1,
             //   (Int_t)(GetHeight() - 350) >> 1,
             //   ax, ay, wdummy);
             //hd->Move(ax, ay);
          }
          break;

       case M_HELP_SIMULATION:
          QMessageBox::information(0,"Help on Simulation",simulation_txt,QMessageBox::Ok);
          break;

       case M_HELP_LICENSE:
          QMessageBox::information(0,"RootShower License",gHelpLicense,QMessageBox::Ok);
          break;

       case M_HELP_ABOUT:
          RootShowerAbout::About(this, 400, 200);
          QMessageBox::aboutQt(0);
          QMessageBox::about(0,"RootShower with Qt interface"
             ,"ROOT Qt interface<p>Copyright © 2001-2003, Valeri Fine. Brookhaven National Laboratory.<br>All right reserved.<p>See: http://root.bnl.gov");

          break;

       case M_SHOW_3D:
          if(fIsRunning) break;
          
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,03)
          {
             TVirtualViewer3D *viewer = TVirtualViewer3D::Viewer3D(cA,"ogl");
             if (viewer  ) {
                loadFlag = FALSE;
                // Create Open GL viewer
                TGQt::SetCoinFlag(0);
                viewer->BeginScene();
                viewer->EndScene();
             }
         }
#else   
          // Load Qt-based GL viewer first
          if (loadFlag) loadFlag = gSystem->Load("libRQTGL");
           cA->x3d("OpenGL");
          // set "wired" view
          if ( cA->GetView3D() ) cA->GetView3D()->ExecuteEvent(kKeyPress,'w',1);
#endif          
             break;

       case M_SHOW_TRACK:
          if(fIsRunning) break;
          {
             fDisplayFrame->setCurrentPage (1);
             Q3ListViewItem *item;
             if ((item = fEventListTree->selectedItem () ) != 0)
                OnShowSelected(item);
          }
          break;
    } // switch parm1
}
//______________________________________________________________________________
void RootShower::Initialize(Int_t set_angles)
{
    Interrupt(kFALSE);
    gEventListTree = 0;
    fEventListTree->clear();
    gEventListTree = new Q3ListViewItem( fEventListTree, "Event" );

//    gClient->NeedRedraw(fEventListTree);

    cB->cd();
    cB->SetFillColor(1);
    cB->Clear();
    fSelection->cd();
    delete sel_node;
    delete sel_detect;
    sel_detect = new TBRIK("sel_detect","sel_detect","void",fDimX/2.0,fDimY/2.0,fDimZ/2.0);
    sel_detect->SetLineColor(7);
    sel_node = new TNode("SEL_NODE","SEL_NODE","sel_detect");
    sel_node->cd();
    fSelection->Draw();
    cB->GetView()->SetPerspective();
    if(set_angles)
        cB->GetView()->SideView();
    cB->cd();
    cB->Update();

    cA->cd();
    cA->SetFillColor(1);
    cA->Clear();
    fEvent->GetDetector()->GetGeometry()->Draw();
    cA->GetView()->SetPerspective();
    if(set_angles)
        cA->GetView()->SideView();
    cA->cd();
    cA->Update();

    SetStatusText("",1);

}

//______________________________________________________________________________
void RootShower::produce()
{
    Int_t     local_num,local_last,local_end;
    Int_t     old_num;
    Char_t    strtmp[80];

    // Check if some Event parameters have changed
    if((fEvent->GetHeader()->GetDate() != fEventTime) ||
       (fEvent->GetDetector()->GetMaterial() != fMaterial) ||
       (fEvent->GetHeader()->GetPrimary() != fFirstParticle) ||
       (fEvent->GetHeader()->GetEnergy() != fE0) ||
       (fEvent->GetB() != fB)) {
        fEventNr++;
        fNRun = 0;
    }
    fEvent->SetHeader(fEventNr, fNRun++, fEventTime, fFirstParticle, fE0);
    fEvent->Init(0, fFirstParticle, fE0, fB, fMaterial, fDimX, fDimY, fDimZ);

    fEvent->GetDetector()->GetGeometry()->cd();
    Interrupt(kFALSE);
    old_num = -1;
    // loop events until user interrupt or until all particles are dead
    while((!IsInterrupted()) && (fEvent->GetNAlives() > 0)) {
        qApp->processEvents();  // handle GUI events
        if(fEvent->GetTotal() > old_num) {
            sprintf(strtmp,"Simulation running, particles : %4d, please wait...",fEvent->GetTotal());
            old_num = fEvent->GetTotal();
            SetStatusText(strtmp,0);
            // Update display here to not slow down too much...
        }
        local_last = fEvent->GetLast();
        local_num = 0;
        local_end = kFALSE;
        while((!IsInterrupted()) && (local_end == kFALSE) && (local_num < (local_last + 1))) {
             qApp->processEvents();  // handle GUI events
            // Update display here if fast machine...
            if(fEvent->GetParticle(local_num)->GetStatus() != DEAD) {
                if(fEvent->Action(local_num) == DEAD)
                    local_end = kTRUE;
                if(fEvent->GetParticle(local_num)->GetStatus() == CREATED)
                    fEvent->GetParticle(local_num)->SetStatus(ALIVE);
            }
            local_num ++;
        }
    }
    fActions[M_SHOW_INFOS] ->setEnabled(true);
    fActions[M_SHOW_3D]    ->setEnabled(true);
    fActions[M_FILE_SAVEAS]->setEnabled(true);
    fActions[M_FILE_SAVE]  ->setEnabled(true);
}

//______________________________________________________________________________
void RootShower::OnShowerProduce()
{
    Int_t     i,gifindex;
    Char_t    gifname[80];
    SetStatusText("",1);

    // animation logo handling
    if(fPicReset > 0) fPicIndex = 1;
    // animation timer
    if (!fTimer)  {
       fTimer = new QTimer(this);
       connect(fTimer,SIGNAL(timeout()),this,SLOT(HandleTimer()));
    }
    fTimer->start(fPicDelay);

    fIsRunning = kTRUE;
    fHisto_dEdX->Reset();
    produce();
    Interrupt(kFALSE);
    gifindex = 0;
    int total = fEvent->GetTotal();
    for(i=0;i<=total;i++) {
        qApp->processEvents();  // handle GUI events
        if(IsInterrupted()) break;
        // if particle has no child, represent it by a leaf,
        // otherwise by a branch
        if(fEvent->GetParticle(i)->GetChildId(0) == 0) {
            gLTI[i]->setPixmap(0,*fLeafPic);
        }
        else {
            gLTI[i]->setPixmap(0,*fBranchPic);
        }
        // Show only charged and massive particles...
        if((fEvent->GetParticle(i)->GetPdgCode() != PHOTON) &&
           (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_E) &&
           (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_MUON) &&
           (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_TAU) &&
           (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_E) &&
           (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_MUON) &&
           (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_TAU) ) {
            // Fill histogram for particle's energy loss
            fHisto_dEdX->Fill(fEvent->GetParticle(i)->GetELoss());
            fEvent->GetTrack(i)->Draw();
            // show track by track if "show process" has been choosen
            // into the menu
            if(fActions[M_SHOW_PROCESS]->isOn()) {
                cA->Modified();
                cA->Update();
                // create one gif image by step if "Animated GIF"
                // has been choosen into the menu
                if(fActions[M_ANIMATE_GIF]->isOn()) {
                    sprintf(gifname,"event_%04d.gif",gifindex);
                    cA->SaveAs(gifname);
                    gifindex++;
                }
            }
        }
    }
//    AppendPad();
    cA->cd();
    cA->Modified();
    cA->Update();

    padC->cd();
    // do not fit if not enough particles
    if(fHisto_dEdX->GetEntries() > 10) {
       fHisto_dEdX->Fit("landau","L");
       TF1 *f1 = fHisto_dEdX->GetFunction("landau");
       //delete fit function is fit is a non sense
       if (f1 && f1->GetNDF() > 0) {
          f1->SetLineColor(kRed);
          f1->SetLineWidth(1);
       } else {
          delete f1;
       }
    }
    fHisto_dEdX->Draw();
    padC->Modified();
    padC->Update();
    cC->Update();
    padC->cd();
    padC->SetFillColor(16);
    padC->GetFrame()->SetFillColor(10);
    padC->Draw();
    padC->Update();

    // Open first list tree items
    fEventListTree->setOpen(gEventListTree,true);
    fEventListTree->setOpen(gLTI[0],true);
    fTimer->stop();
    fIsRunning = kFALSE;
    if(fPicReset > 0)
        fTitleFrame->ChangeRightLogo(1);
}

//______________________________________________________________________________
void RootShower::HighLight(Q3ListViewItem *item)
{ if (item) item->listView()->setCurrentItem(item); }
//______________________________________________________________________________
void RootShower::OnShowSelected(Q3ListViewItem *item)
{
    // Shows track which has been selected into the list tree
    Int_t i,retval;
    fDisplayFrame->setCurrentPage (1);

    cB->cd();
    cB->SetFillColor(1);
    cB->SetBorderMode(0);
    cB->SetBorderSize(0);
    cB->Clear();
    cB->cd();
    // draw geometry
    fSelection->Draw();
    cB->GetView()->SetPerspective();
    cB->cd();
    cB->Update();
    fSelection->cd();
    retval = -1;
    for(i=0;i<=fEvent->GetTotal();i++) {
        if(gLTI[i] == item) {
            retval = i;
            break;
        }
    }
    if((retval > -1) &&
       (fEvent->GetParticle(i)->GetPdgCode() != PHOTON) &&
       (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_E) &&
       (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_MUON) &&
       (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_TAU) &&
       (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_E) &&
       (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_MUON) &&
       (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_TAU) ) {
        fEvent->GetTrack(retval)->Draw();
    }
    cB->cd();
    cB->Modified();
    cB->Update();
}

//______________________________________________________________________________
void RootShower::OnOpenFile(const Char_t *filename)
{
    // Opens a root file into which a previous event
    // has been saved.
    Char_t   strtmpBuffer[80];
    Char_t   *strtmp = &strtmpBuffer[0];
    Int_t  i;
    TFile *f = new TFile(filename);
    TTree *tree;
    TBranch *branch;
    SetStatusText("",1);

    fEvent->Init(0, fFirstParticle, fE0, fB, fMaterial, fDimX, fDimY, fDimZ);
    fHisto_dEdX->Reset();
    tree = (TTree *)f->Get("RootShower");
    if(tree == NULL) return;
    branch = tree->GetBranch("Event");
    branch->SetAddress(&fEvent);
    tree->GetEntry(0, 1);
    f->Close();
    // take back detector dimensions for selection geometry
    fEvent->GetDetector()->GetDimensions(&fDimX, &fDimY, &fDimZ);
    Initialize(1);

    for(i=0;i<=fEvent->GetTotal();i++) {
        gTmpLTI = new Q3ListViewItem(gEventListTree, fEvent->GetParticle(i)->GetName());
        sprintf(strtmp,"%1.2f GeV",fEvent->GetParticle(i)->Energy());
//        QToolTip::add(gTmpLTI,strtmp);
        gLTI[i] = gTmpLTI;

        if(fEvent->GetParticle(i)->GetChildId(0) == 0) 
           gLTI[i]->setPixmap(0,*fLeafPic);
        else 
           gLTI[i]->setPixmap(0,*fBranchPic);

        if((fEvent->GetParticle(i)->GetPdgCode() != PHOTON) &&
           (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_E) &&
           (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_MUON) &&
           (fEvent->GetParticle(i)->GetPdgCode() != NEUTRINO_TAU) &&
           (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_E) &&
           (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_MUON) &&
           (fEvent->GetParticle(i)->GetPdgCode() != ANTINEUTRINO_TAU) ) {
            // Fill histogram for particle's energy loss
            fHisto_dEdX->Fill(fEvent->GetParticle(i)->GetELoss());
            fEvent->GetTrack(i)->Draw();
        }
    }
    // Reparent each list tree item regarding the
    // corresponding particle relations
    for(i=1;i<=fEvent->GetTotal();i++) {
       gEventListTree->takeItem(gLTI[i]);
       gLTI[fEvent->GetParticle(i)->GetFirstMother()]->insertItem(gLTI[i]);
    }
    fEventListTree->setOpen(gEventListTree,true);
    fEventListTree->setOpen(gLTI[0],true);
//    fClient->NeedRedraw(fEventListTree);
//    AppendPad();

    sprintf(strtmp,"Done - Total particles : %d - Waiting for next simulation",
                    fEvent->GetTotal());
    SetStatusText(strtmp,0);
    padC->cd();
    // do not fit if not enough particles
    if(fHisto_dEdX->GetEntries() > 10) {
       fHisto_dEdX->Fit("landau","L");
       TF1 *f1 = fHisto_dEdX->GetFunction("landau");
       //delete fit function is fit is a non sense
       if (f1 && f1->GetNDF() > 0) {
          f1->SetLineColor(kRed);
          f1->SetLineWidth(1);
       } else {
          delete f1;
       }
    }
    fHisto_dEdX->Draw();
    padC->Modified();
    padC->Update();
    cC->Update();
    padC->cd();
    padC->SetFillColor(16);
    padC->GetFrame()->SetFillColor(10);
    padC->Draw();
    padC->Update();

    cA->cd();
    cA->Modified();
    cA->Update();

    fActions[M_SHOW_INFOS] ->setEnabled(true);
    fActions[M_SHOW_3D]    ->setEnabled(true);
    fActions[M_FILE_SAVEAS]->setEnabled(true);
    fActions[M_FILE_SAVE]  ->setEnabled(true);
    fButtonFrame           ->SetState(GButtonFrame::kAllActive);
}

//______________________________________________________________________________
void RootShower::OnSaveFile(const Char_t *filename)
{
    // Saves current event into a Root file
    TFile *hfile;

    hfile = new TFile(filename,"RECREATE","Root Shower file");
    hfile->SetCompressionLevel(9);

    TTree *hTree = new TTree("RootShower","Root Shower tree");
    hTree->Branch("Event", "MyEvent", &fEvent, 8000, 2);
    hTree->Fill();  //fill the tree
    hTree->Write();
    hTree->Print();
    hfile->Close();
}

//______________________________________________________________________________
void RootShower::ShowInfos()
{
    // Gives infos on current event
    Char_t Msg[500];
    Double_t dimx,dimy,dimz;

    fEvent->GetDetector()->GetDimensions(&dimx, &dimy, &dimz);

    sprintf(Msg, "<tt><center><b>Some information about the current shower:</b></center><br>");
    sprintf(Msg, "%s  Target material ...... : %s<br>", Msg,
            fEvent->GetDetector()->GetMaterialName());
    sprintf(Msg, "%s  Dimensions of the target:<br>", Msg);
    sprintf(Msg, "%s  X .................... : %1.2e [cm]    <br>", Msg, dimx);
    sprintf(Msg, "%s  Y .................... : %1.2e [cm]    <br>", Msg, dimy);
    sprintf(Msg, "%s  Z .................... : %1.2e [cm]    <br>", Msg, dimz);
    sprintf(Msg, "%s  Magnetic field ....... : %1.2e [kGauss]<br>", Msg,
            fEvent->GetB());
    sprintf(Msg, "%s  Initial particle ..... : %s<br>", Msg,
            fEvent->GetParticle(0)->GetName());
    sprintf(Msg, "%s  Initial energy ....... : %1.2e [GeV]<br>", Msg,
            fEvent->GetHeader()->GetEnergy());
    sprintf(Msg, "%s  Total Energy loss .... : %1.2e [GeV]", Msg,
            fEvent->GetDetector()->GetTotalELoss());

    QMessageBox::information(0,"Infos on current shower",Msg,QMessageBox::Ok);
}

//______________________________________________________________________________
void RootShower::HandleTimer()
{
   // Logo animation timer handling.

   if(fPicIndex > fPicNumber) fPicIndex = 1;
   fTitleFrame->ChangeRightLogo(fPicIndex);
   fPicIndex++;
}

//______________________________________________________________________________
Int_t RootShower::DistancetoPrimitive(Int_t px, Int_t py)
{
    // Compute distance from point px,py to objects in event
    Int_t i;
    Int_t dist = 9999;

    if(fEvent->GetTotal() <= 0) return 0;
    // Browse every track and get related particle infos.
    for(i=0;i<fEvent->GetTotal();i++) {
        dist = fEvent->GetTrack(i)->DistancetoPrimitive(px, py);
        if (dist < 2) {
            gPad->SetSelected((TObject*)fEvent->GetParticle(i));
            SetStatusText(fEvent->GetParticle(i)->GetObjectInfo(px, py),1);
            gPad->SetCursor(kPointer);
            return 0;
        }
    }
    gPad->SetSelected((TObject*)gPad->GetView());
    return 0;
}
//______________________________________________________________________________
void RootShower::NextEvent()
{
   Char_t  strtmp[80];
   fDisplayFrame->setCurrentPage (0);
   Initialize(0);
   SetStatusText("Simulation running, please wait...",0);

   fButtonFrame->SetState(GButtonFrame::kNoneActive);
   fActions[M_SETTINGS_DLG]->setEnabled(false);
   OnShowerProduce();
   fButtonFrame->SetState(GButtonFrame::kAllActive);
   fActions[M_SETTINGS_DLG]->setEnabled(true);
   sprintf(strtmp,"Done - Total particles : %d - Waiting for next simulation",
      fEvent->GetTotal());
   SetStatusText(strtmp,0);
}
//______________________________________________________________________________
void RootShower::SelectEvent()
{
   fDisplayFrame->setCurrentPage (1);
   Q3ListViewItem *item;
   if ((item = fEventListTree->selectedItem () ) != 0)
      OnShowSelected(item);
}
