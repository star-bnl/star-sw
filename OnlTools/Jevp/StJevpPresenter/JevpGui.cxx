///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include "JevpGui.h"
//#include "StJevpPool/StJevpUtils/EvpUtil.h"
//#include "StJevpPool/StJevpUtils/ServerStatus.h"
#include "TQtRootSlot.h"
#include "TROOT.h"
#include "TEnv.h"

//#include "StRoot/StEEmcPool/muEztPanitkin/EEqaPresenter.h"
#include "JevpLogic.h"
#include "ReferenceWidget.h"
#include "TQtWidget.h"
#include "TQtZoomPadWidget.h"

#include "qapplication.h"
#include "qtabwidget.h"
#include "qpushbutton.h"
#include "qmenubar.h"
#include "qlayout.h"
#include "qtooltip.h"
#include "qpixmap.h"
#include "qlineedit.h"
#include "qlabel.h"
#include "qcursor.h"
#include "qtimer.h"


#include "q3vbox.h"
#include "q3hbox.h"
#include "q3mainwindow.h"
#include "q3popupmenu.h"
#include "qicon.h"
#include "q3progressbar.h"
#include "q3toolbar.h"
#include <QCustomEvent>
#include <Q3Frame>
#include <stack>

#include "EventInfo.h"
#include "ServerInfo.h"
#include "TriggerDetectorBitsInfo.h"
#include "RunDialog.h"
#include "EvpMain.h"

JevpLogic *Logic;

char* mystrcat(const char* a, const char* b) {
  char* txt = new char[1024];
  sprintf(txt,"%s%s",a,b);
  return txt;
}

// Suspend signal emitting until this class is deleted...
class  PresenterSuspend {
private:
  std::stack<QObject *> fWidgets;

public:
  PresenterSuspend(){};
  
  ~PresenterSuspend(){    
    while (!fWidgets.empty()) {
      fWidgets.top()->blockSignals(false);
      fWidgets.pop();
    }
  }
  
  void operator<<(QObject *o) {
    o->blockSignals(true); 
    fWidgets.push(o);
  }
};

JevpScreenWidget::JevpScreenWidget(char *tabname, char *plotname, u_int combo_index, QTabWidget *menu) : TQtWidget(menu, tabname) {
   
  //SetDoubleBuffer(0);
  printf("Is Double Buffered? %d",IsDoubleBuffered());

  plots = new TList();
  jevpPlots = new TList();
  parentMenu = menu;
  this->combo_index = combo_index;
  printf("Creating JevpScreen for %s %s\n",plotname,tabname);
  plot = new std::string(plotname);

  
}

JevpPlot *JevpScreenWidget::getJevpPlot(char *name) {

  CP;
  JevpPlot *curr = (JevpPlot *)jevpPlots->First();
  while(curr) {
    CP;
    if(strcmp(curr->GetPlotName(), name) == 0) return curr;
    
    CP;
    curr = (JevpPlot *)jevpPlots->After(curr);	  
    CP;
  }
  CP;
  return NULL;
}

JevpScreenWidget::~JevpScreenWidget() {
  printf("Deleting JevpScreen for %s\n",plot->c_str());
  delete plot;
}

void JevpScreenWidget::mousePressEvent(QMouseEvent *e)
{
  int wide, deep;
  DisplayNode *node = Logic->getCanvasDescriptor(combo_index);
  
  const char *tmp = node->parent->getProperty("wide");
  if(tmp) wide = atoi(tmp);
  else wide = 1;

  tmp = node->parent->getProperty("deep");
  if(tmp) deep = atoi(tmp);
  else deep = 1;
 
  double xx = (double)e->x() / (double)width();
  xx *= wide;
  int x = (int)xx;

  double yy = (double)e->y() / (double)height();
  yy *= deep;
  int y = (int)yy; 
  
  int nn = y * wide + x;

  if(nn < 0) return;
  if(nn > node->nSiblings()) return;
  
  DisplayNode *hnode = node;
  for(int i=0;i<nn;i++) {
    hnode = hnode->next;
  }
  
  printf("Got a mouse press event...%d %d  %s:\n",x,y,hnode->name);
  
  if(hnode->name) {
    ReferenceWidget *ref = new ReferenceWidget(Logic, hnode->name);
    ref->exec();
    printf("Returned....\n");
    delete ref;
  }
}

void JevpGui::fillTab(QTabWidget *tab, u_int idx)
{
  PresenterSuspend suspend;
  //int isCanvasMe = 0;
  //int isCanvasChild = 0;

  //char *myname = logic->getTab(idx, &isCanvasMe);
  DisplayNode *mynode = logic->getTab(idx);
  u_int child_idx = logic->getTabChildIdx(idx);
  if(!mynode) return;

  //char *childname = logic->getTab(child_idx, &isCanvasChild);
  DisplayNode *childnode = logic->getTab(child_idx);
  if(!childnode) return;

  if(childnode->leaf) {
    printf("create widget: fillTab idx=%d, name=%s (canvas)\n",child_idx,childnode->name
);    
    JevpScreenWidget *nwidget = new JevpScreenWidget(mynode->name, childnode->name, child_idx, tab);

    logic->screens->Add((TObject *)nwidget);

    suspend << nwidget;

    LOG(DBG, "nwidget = 0x%x",nwidget);

    if(currentScreen == NULL) {   // Assume very first tab is the screen tab at first...
      currentScreen = nwidget;
    }

    tab->addTab(nwidget, mynode->name);
    //QToolTip::add(nwidget,"<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
    // mZoomer->Connect(nwidget);
  
    char lab[100];
    sprintf(lab, "%s - not available",nwidget->plot->c_str());
    //logic->DrawPlot(nwidget);
  }
  else {
    QTabWidget *nwidget = new QTabWidget();
    suspend << nwidget;

    // We want to know when the tabs are selected...
    connect(nwidget, SIGNAL(currentChanged(QWidget *)), this, SLOT(tabChanged(QWidget*)));

    tab->addTab(nwidget, mynode->name);
    fillTab(nwidget, child_idx);
  }
  
  u_int next_idx = logic->getTabNextIdx(idx);
  fillTab(tab, next_idx);
}

void JevpGui::buildTabs(QWidget *container)
{
  PresenterSuspend suspend;

  LOG(DBG, "In buildTabs.  Setting current screen to NULL");
  
  currentScreen = NULL;
  QTabWidget *maintab = new QTabWidget(container);
  suspend << maintab;

  LOG(DBG, "fillTab currentScreen = 0x%x",currentScreen);

  fillTab(maintab, 1);

  LOG(DBG, "after fillTab currentScreen = 0x%x",currentScreen);

  // Need to know when the tab is selected...
  connect(maintab, SIGNAL(currentChanged(QWidget *)), this, SLOT(tabChanged(QWidget*)));
}


static TQtBrowserMenuItem_t gMenu_Data[] = {
  // { text, id, accelerator, tooltip, icon }
  /* File Menu */
  { "&Change To Run", kFileChangeToRun, 0, "Re-analyse old run ", ""},
  { "&Live", kFileLive, 0, "Analyze current run", ""},
  { "&Print",     kFilePrint,    Qt::CTRL+Qt::Key_P, "Print the TCanvas image ",            ":/printer.xpm" },
  { "&Print All", kFilePrintAll, 0,                  "Print the TCanvas image ",            ":/printer.xpm" },
  { "onlprinter2",kOnlPrinter2 , 0,                  "Send last print to onlpinter2",       ":/printer.xpm" },
  { "About",      kHelpAbout,      0, "", ""},
  { "&Update",    kUpdate,       Qt::CTRL+Qt::Key_U, "Update current ",                      ":/update.xpm"  },
  { "&AutoUpdate",kAutoUpdate,   Qt::CTRL+Qt::Key_A, "Automatically update eventy 10 sec",   ":/update.xpm"  },
  { "View Toolbar",     kToolBar,        0,          "show toolbar ",                        "" },
  {0,0,0,"",""}
};



//------------------------------------------------------------------------
JevpGui::JevpGui(JevpLogic *logic, bool isRefWindow) :  
  Q3MainWindow(), mWidth(900), mHight(500), mStrLive(" Live  "), mStrFile(" File  "), 
  mStrRun("Running"), mStrStop("Stopped"), fGuiRefreshRate(5000)
{
  fMenuBar = NULL;
  mZoomer = NULL;
  fProgressBar = NULL;
  mCentralWidget = NULL;
  fToolBar = NULL;
  fTab = NULL;
  this->logic = logic;
  Logic = logic;
  
  
  setAttribute(Qt::WA_DeleteOnClose);
  setWindowModality(Qt::WindowModal);

  PresenterSuspend blockWidgets;
  blockWidgets << this;

  // Some preparations here
  // Here is starting directory name
  SetDefaults();

  setUsesTextLabel(true); // use the text labels for the tool bar buttons
  

  blockWidgets << (mCentralWidget = new Q3HBox(this));
  mCentralWidget->setMargin(0);
  mCentralWidget->setSpacing(0); 
  setCentralWidget(mCentralWidget);

  //
  MakeActions();

  MakeMenuBar();

  printf("then...\n");

  if(isRefWindow) {
    ShowToolBar(false);
  }
  //
  // Mother Frame for canvas tabs is defined here
  //

  printf("Further\n");

  // MakeConnectionFrame();

  // Create ZoomWidget 
  // mZoomer = new TQtZoomPadWidget(0);
  // mZoomer->SetZoomFactor(3);

  printf("tabs..\n");

  //
  // Define Tabs
  //
  // main tab holding
  buildTabs(mCentralWidget);

  printf("Done building tabs\n");

  fActions[kAutoUpdate]->setOn(true);



  
 //  for(int i=0;;i++) {
//     char *tname = logic->getTab(i,0);
//     if(tname == NULL) break;

//     QTabWidget *toptab = new QTabWidget(fTab);
//     blockWidgets << toptab;
//     toptab->setMargin(0);
//     fTab->addTab(toptab, tname);
    
//     for(int j=1;;j++) {
//       tname = logic->getTab(i,j);
//       if(tname == NULL) break;

//       TQtWidget *w = new TQtWidget(toptab);
//       blockWidgets << w;
      
//       QToolTip::add(w,"<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
//       toptab->addTab(w, tname);
//       toptab->showPage(w);
//       mZoomer->Connect(w);
//       connect(toptab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );
//     }
//     fTab->showPage(toptab);
//   }  
    


  /*
  if(isRefWindow)
    setCaption("Histogram Reference");
  else {
    setCaption("STAR Histogram Presenter");

    // Adjust the GUI refresh rate msec.
    fGuiRefreshRate = gEnv->GetValue("Online.GuiRefreshRate",100);
    fActions[kAutoUpdate]->blockSignals(true);
    fActions[kAutoUpdate]->setOn(true);
    fActions[kAutoUpdate]->blockSignals(false);
  }
  */

  // create only one TQTWidget
  // Loop over upper level Tabs
  //  for(int i=0;i<MAX_TABS;i++) {
  //     if((nSubTabs[i] != 0) && (nSubTabs[i]<=MAX_SUBTABS)) {
  //       QTabWidget *topTab = new QTabWidget(fStaticTab);
  // #if QT_VERSION >= 0x40000
  //       blockWidgets << topTab;
  // #endif /* QT4 */
  //       topTab->setMargin(0);
  //       // Define upper level tab
  //       fStaticTab->addTab(topTab,TabNames[i][0]);
  //       // Add subTabs for current Tab
  //       // Note that we started with 1                         
  //       for(int j=1;j<=nSubTabs[i];j++) {
  // 	TQtWidget* w = new TQtWidget(topTab);
  // #if QT_VERSION >= 0x40000
  // 	blockWidgets << w;
  // #endif /* QT4 */
  //         QToolTip::add(w,"<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
  //         topTab->addTab( w ,TabNames[i][j]);
  //         topTab->showPage(w);
  //         mZoomer->Connect(w);
  //         connect(topTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );
  //       }
  //     } else {
  //       TQtWidget* w = new TQtWidget(fStaticTab);
  // #if QT_VERSION >= 0x40000
  //       blockWidgets << w;
  // #endif /* QT4 */
  //       QToolTip::add(w,"<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
  //       fStaticTab->addTab( w ,TabNames[i][0]);
  //       fStaticTab->showPage(w);
  //       mZoomer->Connect(w);
  //     }
  //   }

  //   fTab->showPage(fStaticTab);

  //   if(isRefWindow)
  //     setCaption("Histogram Reference");
  //   else {
  //     setCaption("STAR Histogram Presenter");

  //     // Adjust the GUI refresh rate msec.
  //     fGuiRefreshRate = gEnv->GetValue("Online.GuiRefreshRate",100);
  //     fActions[kAutoUpdate]->blockSignals(true);
  //     fActions[kAutoUpdate]->setOn(true);
  //     fActions[kAutoUpdate]->blockSignals(false);
  //   }

  // connect(fTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  // connect(fDynamicTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  // connect(fStaticTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  connect(qApp,SIGNAL(lastWindowClosed()),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit()));
  // connect(this,SIGNAL(destroyed()), mZoomer, SLOT(close()));

  //emit update(GetCanvas(), GetTabId(), GetSubTabId() );
  //QTabWidget* first = (QTabWidget*) fStaticTab->currentPage();
  //first->setCurrentPage(2);
  ///first->currentChanged( first->currentPage() );
  //emit currentChanged(first);
  printf("Resize\n");
  resize(mWidth,mHight);
  printf("Done with resize\n");

  SetWindowName("Live...");
}


//----------------------------------------------------------------
JevpGui::~JevpGui()
{}

//----------------------------------------------------------------
void JevpGui::SetDefaults()
{
  // Set printout level. 0 - silent, 1 - Verbose
  SetDebugLevel(0);
  //  strcpy(mDefaultPSFilePath,"/d/histoes/");
  //strcpy(mDefaultPSFilePath,"/home/panitkin/online/histoes/");
  //mDefaultPSFilePath = "/aa/online/histoes/";
  //    mDefaultPSFilePath = "/home_local/panitkin/online/histoes/";
  //  mDefaultPSFilePath = "/b/histos/";
  // Define GUI lay out.
}
//----------------------------------------------------------------
void JevpGui::CloseWindow()
{
  qApp->closeAllWindows();
}
//----------------------------------------------------------------
void JevpGui::DefineLayouts()
{  }
//______________________________________________________________________________

// connect make "actions" from menu id's and connect to the process message slot
void JevpGui::MakeActions() {
  int i=0;
  while (gMenu_Data[i].fMenuText!=NULL) {
    // skip the separators 
    TQtRootAction *action = new TQtRootAction(this, gMenu_Data[i]);
    fActions.insert(action->Id(),action);
    connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
    i++;
  }
}
//----------------------------------------------------------------
void JevpGui::MakeMenuBar()
{  
  if (fMenuBar) { delete fMenuBar; fMenuBar = 0; }
   
  QMenuBar   *mainMenu = menuBar();
  fMenuBar = mainMenu;

  // File menu
  Q3PopupMenu *fileMenu      = new Q3PopupMenu();
  mainMenu->insertItem("&File",fileMenu);

  fActions[kFileChangeToRun]->addTo(fileMenu);
  fActions[kFileLive]->addTo(fileMenu);
  fActions[kFileLive]->setToggleAction(true);
  fActions[kFileLive]->setOn(true);
  printf("MakeMenuBar\n");
 
  fileMenu->insertSeparator();

  fActions[kFilePrint]->addTo(fileMenu); 
  fActions[kFilePrintAll]->addTo(fileMenu); 
  fActions[kOnlPrinter2]->addTo(fileMenu); 

  // Input Menu
  Q3PopupMenu *inputMenu     = new Q3PopupMenu();
  mainMenu->insertItem("&Input",inputMenu);
  fActions[kUpdate]->addTo(inputMenu);
  fActions[kAutoUpdate]->addTo(inputMenu);
  fActions[kAutoUpdate]->setToggleAction(true);
  fActions[kAutoUpdate]->setOn(true);

  // Option Menu
  Q3PopupMenu *optionMenu     = new Q3PopupMenu();
  mainMenu->insertItem("&Option",optionMenu);
  fActions[kToolBar]->addTo(optionMenu);
  fActions[kToolBar]->setToggleAction(true);
  fActions[kToolBar]->setOn(true);

  mainMenu->insertSeparator();

  // Help menu
  Q3PopupMenu *helpMenu  = new Q3PopupMenu();
  mainMenu->insertItem("&Help",helpMenu);
  helpMenu->insertSeparator();
  fActions[kHelpAbout]->addTo(helpMenu);

  // add text to actions icons
  fActions[kFilePrint]->setText("Print");
  fActions[kFilePrintAll]->setText("Print all");
  fActions[kOnlPrinter2]->setText("onlPrinter2");
  //fActions[kReference]->setText("Reference");
  fActions[kUpdate]->setText("Update");
  fActions[kAutoUpdate]->setText("AutoUpdate");

  // create tool bar
  if (fToolBar) { delete fToolBar; fToolBar = 0;}
  fToolBar = new Q3ToolBar(this);

  addDockWindow(fToolBar);
  // populate toolbar

  fToolBar->addSeparator();
  fActions[kUpdate]->addTo(fToolBar);
  fActions[kAutoUpdate]->addTo(fToolBar);
  fToolBar->addSeparator();
  fActions[kFilePrint]->addTo(fToolBar);
  fActions[kFilePrintAll]->addTo(fToolBar);
  fActions[kOnlPrinter2]->addTo(fToolBar);
}

//______________________________________________________________________________

//-------------------------------------------------------------------
void JevpGui::MakeConnectionFrame()
{
  // Connection Frame
  //
  // Mother Connection frame defined here
  //
  Q3VBox *leftPane = new Q3VBox(centralWidget());
  leftPane->setMaximumSize(230,32767);
  mEventInfo = new EventInfo(leftPane);
  mServerInfo = new ServerInfo(leftPane);


  // fStarLogo = new  QPushButton(QIcon(":/starlogo_1.xpm"),"",leftPane);
  // fStarLogo->setToolTip("Experiment shutdown. Don't push this button!");
  fProgressBar = new Q3ProgressBar(leftPane,"Progress");

  //fProgressBar->setMaximumSize(leftPane->width(),25);
  fProgressBar->setProgress(0,10);
  
  mBitsFrame = new Q3Frame(0,"Trigger/ Detector-Bits");
  mTriggerDetectorBitsInfo = new TriggerDetectorBitsInfo(mBitsFrame);
  mBitsFrame->adjustSize();
}

//-------------------------------------------------------------------
void JevpGui::MakePrintFrame()
{
  // Connection Frame
/*
  fDummyPrintFrame = new QGroupFrame(fConnectionFrame, "Output");
  fCleanUp->Add(fDummyPrintFrame);
  fConnectionFrame->AddFrame(fDummyPrintFrame,fL200);

  fSaveButton = new QTextButton(fDummyPrintFrame, "&Save ", 165);
  fCleanUp->Add(fSaveButton);
  fSaveButton->Connect("Clicked()", "PresenterGui", this, "WritePSFile()");
  fSaveButton->SetToolTipText("Save PS File with histograms");
  fDummyPrintFrame->AddFrame(fSaveButton, fL203);

  fPrintButton = new QTextButton(fDummyPrintFrame, "&Print ", 16);
  fCleanUp->Add(fPrintButton);
  fPrintButton->Connect("Clicked()", "PresenterGui", this, "DoPrintButton()");
  fPrintButton->SetToolTipText("Print PS file");
  fDummyPrintFrame->AddFrame(fPrintButton, fL203);
*/

}
//--------------------------------------------------------------
// Converts string to integer
//
int JevpGui::IntFromTString(const TString &ts)
{
  int t;
  std::string s(ts.Data());

  std::istringstream iss(s);
  iss >> t;
  return t;
}
//--------------------------------------------------------------
//Converts string to float
//
float JevpGui::FloatFromTString(const TString &ts)
{
  float t;
  std::string s(ts.Data());

  std::istringstream iss(s);
  iss >> t;
  return t;
}

//_____________________________________________________________________
int JevpGui::ParseString (const TString &tChain, TObjArray &Opt)
{
  Int_t nParsed = 0;
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  //Comma separated tokens
  TRegexp separator("[^,]+");
  TString Tag, opt, nopt;

  while ((begin < tChain.Length()) && (index != kNPOS) )
    {
      // loop over given Chain options
      index = tChain.Index(separator,&end,begin);

      if (index >= 0)
        {
	  //csp TString substring(tChain(index,end));
	  index = end;
	  TString substring(tChain(begin,end));

	  if(mDebugLevel) {
	    cout<<"begin:"<<begin<<" end:"<<end<<" index:"<<index<<" T:"<<substring.Data()<<endl;
	  }
	  Opt.Add(new TObjString(substring.Data()));
	  nParsed++;
        }
      begin += end+1;
    }
  return nParsed;
}

//--------------------------------------------------------------
void JevpGui::DoLiveButton()
{
  // Handle Live button click.
  // Connect to mmap file
//   if(mDebugLevel) {
//     cout<<"Live Button Pressed  "<< this << endl;
//   } 
//   if (fActions[kLive]->isOn()) { 
//       emit live();
//       emit update(GetCanvas(), GetTabId(), GetSubTabId() );
//   }

//mEvpClient->Stop();   // will restart automatically
  //mEvpClient->SetSource();
}


//-------------------------------------------------------------
void JevpGui::DoFileButton()
{
  // Handle File button click.
  if(mDebugLevel) {
    cout<<"File Button Pressed"<<endl;
  }
  //  fActions[kLive]->setOn(false);
  emit file();
}
//---------------------------------------------------------------
void JevpGui::DoAutoUpdateButton()
{
  if(mDebugLevel) {
    cout<<"AutoUpdate Button Pressed"<<endl;
  }
  UpdatePlots();
}
//---------------------------------------------------------------
void JevpGui::DoBitsButton() {
    mBitsFrame->show();
    mBitsFrame->raise();
}
//-------------------------------------------------------------
void JevpGui::DoUpdateButton()
{
  // Handle File button click.
  if(mDebugLevel) {
    cout<<"Update Button Pressed"<<endl;
  }
  
  UpdatePlots();
  //  emit nextEvent();
  //  updateRequest();
}
//---------------------------------------------------------------
void JevpGui::DoPrintButton()
{
  // Send earlier defined PS file to the online printer
#if 0
  char PrintCommand[1024];
  //  sprintf(PrintCommand,"%s","lp -d starhp1_p ");
  sprintf(PrintCommand,"%s","lp -d onlprinter2 ");
  cout<<PrintCommand<<"  "<<mHistoPSFile<<endl;
  strcat(PrintCommand,mHistoPSFile);
  cout<<PrintCommand<<endl;
  gSystem->Exec(PrintCommand);
#endif
}
//______________________________________________________________________________
void JevpGui::ProcessMessage()
{
  printf("Processing:  %d\n",fActions[kFileLive]->isOn());

   TQtRootAction *actionSender =  (TQtRootAction *)sender ();
   switch (actionSender->Id()) {

     //case kFileSave:         SaveCB();         break;
     //case kFileSaveAs:       SaveAsCB();       break;
   case kFileChangeToRun:  ChangeToRun(); break;
   case kFileLive:         ChangeToLive(); break;
   case kFilePrint:        PrintCB();        break;
   case kFilePrintAll:     PrintAllCB();     break;
     //   case kLive:             DoLiveButton();   break;
     // case kFile:             DoFileButton();   break;
   case kUpdate:           DoUpdateButton(); break;
   case kAutoUpdate:       DoAutoUpdateButton(); break;
     //case kBits:             DoBitsButton(); break;
   case kToolBar:          ToolBarCB();      break;
     //case kFileExit:         QuitCB();         break;
   case kHelpAbout:        AboutCB();        break;
   case kOnlPrinter2:      onlPrinter2();    break;
     //case kReference:        OpenReference();  break;
 
   default:                                  break;

   };
   printf("Processed:  %d\n",fActions[kFileLive]->isOn());
}
//______________________________________________________________________________
void JevpGui::SaveCB()
{
  emit save();
}
//______________________________________________________________________________
void JevpGui::SaveAsCB()
{
  setEnabled(true);
  emit saveAs();
  setEnabled(true);
}
//______________________________________________________________________________
void JevpGui::ChangeToRun()
{
  printf("Change to Run...\n");
  
  //char *name = getRunNumberDialog();

  RunDialog *mydialog = new RunDialog();
  int ret = mydialog->exec();
  printf("my dialog returned %d\n",ret);

  char *newname = NULL;

  if(ret) {
    newname = mydialog->getResult();
  }

  delete mydialog;

  if(!newname) return;

  fActions[kFileLive]->setOn(0);
  fActions[kFileLive]->setEnabled(1);


  char *name = new char[40];
  strcpy(name, newname);
  static char windowname[100];
  sprintf(windowname, "Run: %s",name);
  SetWindowName(windowname);
  
  printf("run name=%s\n",name);

  int newport = logic->LaunchRun(name);
  printf("newport = %d\n",newport);
  if(newport <= 0) return;   // no new connection...

  if(evpMain->serverport != JEVP_PORT) {
    logic->killServer();
  }
  logic->ConnectToServerPort(newport, 60);

  delete name;
}

void JevpGui::ChangeToLive()
{
  printf("Change to live...\n");
  int x;

  SetWindowName("Live Run");

  // This is the new result after QT sets/unsets the checkbox...
  x = fActions[kFileLive]->isOn();
  fActions[kFileLive]->setEnabled(x);

  logic->killServer();
  logic->ConnectToServerPort(JEVP_PORT,5);
}



void JevpGui::PrintCB()
{
#ifdef NOJML
  TCanvas* cc = 0;
  if ( fTab->currentPage() == fStaticTab ) {
    cc = GetCanvas();
    sprintf(mPsName,"%s/run%s_tab%dsubTab%d.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().ascii(),GetTabId(),GetSubTabId());
  } else {
    cc = GetGroupCanvas();
    sprintf(mPsName,"%s/run%s_%s.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().ascii(),GetGroupWidget()->name());
  }
  if ( cc ) {
    cc->Print(mPsName);
  }
#endif
}
//______________________________________________________________________________
void JevpGui::PrintAllCB()
{
#ifdef NOJML
  sprintf(mPsName,"%s/run%s.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().ascii());
  emit printAll(mPsName);
#endif
}
//______________________________________________________________________________
void JevpGui::onlPrinter2() 
{
  if ( strcmp(mPsName,"") ) {    
    char cmd[1014];
    sprintf(cmd,"lp -d onlprinter2 %s &",mPsName);
    cout << "print command: " << cmd << endl;
    gSystem->Exec(cmd);
  }
}
//______________________________________________________________________________
void JevpGui::OpenReference()
{
  emit openReference();
}
//______________________________________________________________________________
void JevpGui::QuitCB()
{
   CloseWindow();
}
//______________________________________________________________________________
void  JevpGui::AboutCB()
{
  //   new DGHelp("/home_local/panitkin/online/messages/about.message");
}
//______________________________________________________________________________
int JevpGui::GetTabId() 
{ 
	return fStaticTab->currentPageIndex (); 
}
//______________________________________________________________________________
int JevpGui::GetSubTabId() {
// remember that subTabs starts with 1	
  QWidget *currentWidget = fStaticTab->currentPage();
  QTabWidget *subTab = dynamic_cast<QTabWidget*>(currentWidget);
  if (subTab) 
     return subTab->currentPageIndex()+1;
  else 
    return 0;
} 
//______________________________________________________________________________
// TCanvas* JevpGui::GetCanvas() 
// { 
//   QWidget *currentWidget = fStaticTab->currentPage();
//   QTabWidget *subTab = dynamic_cast<QTabWidget*>(currentWidget);
//   if (subTab) 
//     return ((TQtWidget*)subTab->currentPage())->GetCanvas();
//   else 
//     return ((TQtWidget*)fStaticTab->currentPage())->GetCanvas();;
// }

//______________________________________________________________________________
void  JevpGui::SetWindowName(const char* displayText)
{ 
  setCaption(displayText); 
}



//______________________________________________________________________________
void JevpGui::tabChanged(QWidget* q) {
//   mTab  = GetTabId();
//   mSubTab =GetSubTabId();
//   emit tab( mTab );
//   emit subTab( mSubTab );
//   emit canvas( GetCanvas() );
//   updateRequest();
  
  JevpScreenWidget *widget = dynamic_cast<JevpScreenWidget *>(q);

  if(widget) {
    printf("tab changed to %s\n",widget->plot->c_str());
    char lab[100];
    sprintf(lab, "%s - not available",widget->plot->c_str());
    printf("tabchanged:  lab=%s\n",lab);


    widget->setUpdatesEnabled(false);
    logic->DrawPlot(widget);
    widget->setUpdatesEnabled(true);
    widget->update();

    currentScreen = widget;

    // widget->show();
    //widget->Refresh();

    // printf("tab index: %d   screen from tab index: %s\n",
// 	   widget->parentMenu->currentIndex(), 
// 	   ((JevpScreenWidge
//	     t *)(widget->parentMenu->currentWidget()))->plot->c_str());
  }
  else {
    QTabWidget *tab = dynamic_cast<QTabWidget *>(q);
    if(!tab) {
      printf("tab changed to something else (impossible?)\n");
      return;
    }

    QWidget *q = tab->currentWidget();
    tabChanged(q);
  }
}
//______________________________________________________________________________
void JevpGui::ShowToolBar(bool show)
{
   // Show or hide toolbar.
   if (show) {
     if (!fActions[kToolBar]->isOn()) 
       fActions[kToolBar]->setOn(true);
       fToolBar->show();
    } else {
      if (fActions[kToolBar]->isOn()) 
	fActions[kToolBar]->setOn(false);
      fToolBar->hide();
    }
}
//______________________________________________________________________________
void JevpGui::ToolBarCB()
{
   ShowToolBar(fActions[kToolBar]->isOn());
}
//______________________________________________________________________________
void JevpGui::setEventInfo(int run, int event, int count, int token,  unsigned int triggerBits,  unsigned int detectorBits, unsigned int triggerBitsRun , unsigned int detectorBitsRun ) {
  mEventInfo->setRunNumber(run);
  mEventInfo->setEventNumber(event);
  mEventInfo->setEventCounter(count);
  mEventInfo->setTokenNumber(token);
  mEventInfo->setDetectorBits(detectorBits);
  mEventInfo->setTriggerBits(triggerBits);
  mEventInfo->setDetectorBitsRun(detectorBitsRun);
  mEventInfo->setTriggerBitsRun(triggerBitsRun);
  mTriggerDetectorBitsInfo->setDetectorBits(detectorBits);
  mTriggerDetectorBitsInfo->setTriggerBits(triggerBits);
  mTriggerDetectorBitsInfo->setDetectorBitsRun(detectorBitsRun);
  mTriggerDetectorBitsInfo->setTriggerBitsRun(triggerBitsRun);

}
//______________________________________________________________________________
void JevpGui::updateRequest() {
  int pro = fProgressBar->progress();
  pro++;
  if (pro>10) pro=0;
  fProgressBar->setProgress(pro);

//   if ( fTab->currentPage() == fStaticTab ) { 
//     emit update(GetCanvas(), GetTabId(), GetSubTabId() );
//     return;
//   }  
//   if ( fTab->currentPage() == fDynamicTab ) { 
//     TQtWidget* widget = GetGroupWidget();
//     if (widget) {
//       emit update( widget->GetCanvas(), widget->name() );
//       return;
//     }
//   }
//  emit update(); 
}
//______________________________________________________________________________
void JevpGui::UpdatePlots() 
{

  if(logic->socket == NULL) {
    printf("Updating plots but socket is NULL... returning...\n");
  }
  else {
    
    // Event loop via singleShot timer
    static int nupdates=0;
    if(nupdates % 100 == 0) {
      LOG(DBG,"%d updates",nupdates);
    }
    nupdates++;


    //char lab[100];
    //sprintf(lab, "%s - not available",currentScreen->plot->c_str());

    logic->DrawPlot(currentScreen);
  }
  
  if (fActions[kAutoUpdate]->isOn()) {
    QTimer::singleShot(fGuiRefreshRate,this,SLOT(UpdatePlots()));
  }
}

  //______________________________________________________________________________
void JevpGui::setServerInfo(ServerStatus* ss) {
#ifdef NOJML
  mServerInfo->setRequestTime( ss->getRequestTime() );
  mServerInfo->setReceiveTime( ss->getReceiveTime() );
  mServerInfo->setRequestType( ss->getRequestType() );
  mServerInfo->setReceiveType( ss->getReceiveType() );
#endif
}
  //______________________________________________________________________________
  // adds a new QTabWidget to the dynamic tabs;
  // void JevpGui::addGroupTab(const char* name) {
  //   //cout << " adding " << groupName << endl;
  //   QTabWidget* tab = new QTabWidget(fDynamicTab,name);
  //   tab->setMargin(0);
  //   fDynamicTab->addTab(tab,name);
  //   fDynamicTab->showPage(tab);
  //   connect(tab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  //   cout << __PRETTY_FUNCTION__ << " " << name <<endl;
  // }
  //______________________________________________________________________________ 
  // adds a new QWidget to that last QTabWidget in the dynamic tabs
  // void JevpGui::addGroup(const char* name) {
  // //cout << " adding " << name << endl;
  //   int n = fDynamicTab->count();
  //  if ( n ==0 ) return ; // tab had no group jet
  //   QTabWidget* tab = dynamic_cast<QTabWidget*>(fDynamicTab->page(n-1));  // get the last group tab
  //   if  (!tab) return;
  //   TQtWidget* subTab = new TQtWidget(tab,name);
  //   tab->addTab( subTab ,name);
  //   tab->setEnabled(true);
  //   tab->showPage( subTab );
  //   mZoomer->Connect(subTab);
  //   cout << __PRETTY_FUNCTION__ << " " << name <<endl;
  // }
  //______________________________________________________________________________ 
  // adds a new QWidget to that last QTabWidget in the dynamic tabs
  // void JevpGui::removeGroupTabs() {
  //   //cout << __PRETTY_FUNCTION__ << endl;
  //   setEnabled(false);
  //   QTabWidget* tab;
  //   while ( fDynamicTab->count() ) {
  //     tab = dynamic_cast<QTabWidget*>(fDynamicTab->page(0));
  //     if (!tab) {
  //       cout << __PRETTY_FUNCTION__ << " ### can remove non QTabWidget " << endl;
  //       setEnabled(true);
  //      return ;
  //     }
  //     while ( tab->count() ) {
  //       QWidget* subTab = tab->page(0);
  //       tab->removePage( subTab );
  //       delete subTab;
  //     }
  //     fDynamicTab->removePage(tab); 
  //     delete tab;
  //   }
  //   setEnabled(true);
  // }
  //______________________________________________________________________________ 
  // TQtWidget* JevpGui::GetGroupWidget() {
  //   QTabWidget* tab = dynamic_cast<QTabWidget*>(fDynamicTab->currentPage());
  //   if  (!tab) return 0;
  //   TQtWidget* sub = dynamic_cast<TQtWidget*>(tab->currentPage());
  //   return sub;
  // }
  //______________________________________________________________________________ 
  // TCanvas* JevpGui::GetGroupCanvas() {
  //   QTabWidget* tab = dynamic_cast<QTabWidget*>(fDynamicTab->currentPage());
  //   if  (!tab) return 0;
  //   TQtWidget* sub = dynamic_cast<TQtWidget*>(tab->currentPage());
  //   if ( !sub ) return 0;
  //   return sub->GetCanvas();
  // }
