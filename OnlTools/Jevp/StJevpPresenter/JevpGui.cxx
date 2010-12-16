///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include "JevpGui.h"
//#include "StJevpPool/StJevpUtils/EvpUtil.h"
//#include "StJevpPool/StJevpUtils/ServerStatus.h"
#include "TQtRootSlot.h"
#include "TROOT.h"
#include <TSocket.h>
#include <TClass.h>
#include <TLine.h>
#include "TEnv.h"

//#include "StRoot/StEEmcPool/muEztPanitkin/EEqaPresenter.h"
#include "ReferenceWidget.h"
#include "TQtWidget.h"
#include "TQtZoomPadWidget.h"
#include <qinputdialog.h>

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

#if QT_VERSION < 0x40000
#  include <qfiledialog.h>
#else /* QT4 */
#  include <q3filedialog.h>
#endif /* QT4 */

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

JevpGui *Logic;

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
  DisplayNode *node = Logic->jl_getCanvasDescriptor(combo_index);
  
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

  //char *myname = jl_getTab(idx, &isCanvasMe);
  DisplayNode *mynode = jl_getTab(idx);
  u_int child_idx = jl_getTabChildIdx(idx);
  if(!mynode) return;

  //char *childname = jl_getTab(child_idx, &isCanvasChild);
  DisplayNode *childnode = jl_getTab(child_idx);
  if(!childnode) return;

  if(childnode->leaf) {
    printf("create widget: fillTab idx=%d, name=%s (canvas)\n",child_idx,childnode->name
);    
    JevpScreenWidget *nwidget = new JevpScreenWidget(mynode->name, childnode->name, child_idx, tab);

    jl_screens->Add((TObject *)nwidget);

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
    //jl_DrawPlot(nwidget);
  }
  else {
    QTabWidget *nwidget = new QTabWidget();
    suspend << nwidget;

    // We want to know when the tabs are selected...
    connect(nwidget, SIGNAL(currentChanged(QWidget *)), this, SLOT(tabChanged(QWidget*)));

    tab->addTab(nwidget, mynode->name);
    fillTab(nwidget, child_idx);
  }
  
  u_int next_idx = jl_getTabNextIdx(idx);
  fillTab(tab, next_idx);
}

void JevpGui::deleteTabs(QTabWidget *tab)
{  
  for(int idx=tab->currentIndex(); idx >= 0; idx=tab->currentIndex()) {

    QWidget *widget = tab->currentWidget();
    
    widget->blockSignals(true);
    
    QTabWidget *ctab = dynamic_cast<QTabWidget *>(widget);
    JevpScreenWidget *screen = dynamic_cast<JevpScreenWidget *>(widget);

    LOG("JEFF", "deleteTabs:  idx=%d tab=%d screen=%d",idx, tab ? 1 : 0, screen ? 1 :0);

    tab->removeTab(idx);

    if(ctab) {
      deleteTabs(ctab);
      delete ctab;
    }
    if(screen) {
      delete screen;
    }
  }
}

void JevpGui::switchTabs(const char *newdisplay) {
  CP;
  int ret = jl_displayFile->setDisplay((char *)newdisplay);
  if(ret == -1) {
    LOG(ERR, "No display %s",newdisplay);
  }

  CP;
  evpMain->display = (char *)newdisplay;

  CP;
  LOG("JEFF", "Calling deleteTabs for root");

  PresenterSuspend suspend;
  suspend << rootTab;

  deleteTabs(rootTab);
  LOG("JEFF", "Back from deletTabs for root");
  CP;
  LOG("JEFF", "Calling buildTabs");
  buildTabs();
  LOG("JEFF", "Back from buildTabs");
}

void JevpGui::buildTabs()
{
  PresenterSuspend suspend;

  LOG(DBG, "In buildTabs.  Setting current screen to NULL");
  
  currentScreen = NULL;
 
  suspend << rootTab;

  LOG(DBG, "fillTab currentScreen = 0x%x",currentScreen);

  fillTab(rootTab, 1);

  LOG(DBG, "after fillTab currentScreen = 0x%x",currentScreen);

  // Need to know when the tab is selected...
  connect(rootTab, SIGNAL(currentChanged(QWidget *)), this, SLOT(tabChanged(QWidget*)));
}


static TQtBrowserMenuItem_t gMenu_Data[] = {
  // { text, id, accelerator, tooltip, icon }
  /* File Menu */
  { "Change Histogram Set", kFileChangeHistogramSet, 0, "Change Histogram Set", ""},
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
JevpGui::JevpGui() : Q3MainWindow(), mWidth(900), mHight(500), mStrLive(" Live  "), mStrFile(" File  "), mStrRun("Running"), mStrStop("Stopped"), fGuiRefreshRate(5000)
{
}

void JevpGui::gui_JevpGui(JevpGui *logic, bool isRefWindow)
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
  rootTab = new QTabWidget(mCentralWidget);
  buildTabs();

  printf("Done building tabs\n");

  fActions[kAutoUpdate]->setOn(true);



  
 //  for(int i=0;;i++) {
//     char *tname = jl_getTab(i,0);
//     if(tname == NULL) break;

//     QTabWidget *toptab = new QTabWidget(fTab);
//     blockWidgets << toptab;
//     toptab->setMargin(0);
//     fTab->addTab(toptab, tname);
    
//     for(int j=1;;j++) {
//       tname = jl_getTab(i,j);
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

  fActions[kFileChangeHistogramSet]->addTo(fileMenu);
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
   case kFileChangeHistogramSet:  ChangeHistogramSet(); break;
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
void JevpGui::ChangeHistogramSet()
{
  bool ok;

  QString text = QInputDialog::getText(this, 
				       tr("New Histogram Set Name"),
				       tr("New Histogram Set Name:"), 
				       QLineEdit::Normal,
				       tr(evpMain->display), &ok);

  if(ok && !text.isEmpty()) {
    printf("Change Histogram set to %s\n", (const char *)text);
    switchTabs((const char *)text);
  }

  
}

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

  int newport = jl_LaunchRun(name);
  printf("newport = %d\n",newport);
  if(newport <= 0) return;   // no new connection...

  if(evpMain->serverport != JEVP_PORT) {
    jl_killServer();
  }
  jl_ConnectToServerPort(newport, 60);

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

  jl_killServer();
  jl_ConnectToServerPort(JEVP_PORT,5);
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
    jl_DrawPlot(widget);
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

  if(jl_socket == NULL) {
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

    jl_DrawPlot(currentScreen);
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



//// PRESENTER_CONNECT

void JevpGui::pc_PresenterConnect(JevpGui* gui, JevpGui* pre) 
{

    pc_mGui = gui;
    pc_mPresenter = pre;

  connect(pc_mGui,SIGNAL(live()), this, SLOT(live()) ); 
  connect(pc_mGui,SIGNAL(file()), this, SLOT(file()) ); 
  connect(pc_mGui,SIGNAL(update(TCanvas*, int, int )), this, SLOT(update(TCanvas*, int, int )) ); 
  connect(pc_mGui,SIGNAL(update(TCanvas*, const char* )), this, SLOT(update(TCanvas*, const char*)) ); 
  connect(pc_mGui,SIGNAL(update()), this, SLOT(update()) ); 
  connect(pc_mGui,SIGNAL(save()), this, SLOT(save()) ); 
  connect(pc_mGui,SIGNAL(saveAs()), this, SLOT(saveAs()) ); 
  connect(pc_mGui,SIGNAL(print()), this, SLOT(print()) ); 
  connect(pc_mGui,SIGNAL(openReference()), this, SLOT(openReference()) ); 

  connect(pc_mGui,SIGNAL( tab(int) ),         this, SLOT( setTab(int)) ); 
  connect(pc_mGui,SIGNAL( subTab(int) ),      this, SLOT( setSubTab(int)) ); 
  connect(pc_mGui,SIGNAL( canvas(TCanvas*) ), this, SLOT( setCanvas(TCanvas*)) ); 

  connect(this, SIGNAL( pc_signalEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ), pc_mGui, SLOT( setEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ) ); 
  connect(this, SIGNAL( pc_signalServerInfo(ServerStatus*) ), pc_mGui, SLOT( setServerInfo(ServerStatus*) ) ); 
  connect(this, SIGNAL( pc_updateRequest() ), pc_mGui, SLOT( updateRequest() ) );


  //connect(pc_mPresenter, SIGNAL( addGroupTab(const char*) ), pc_mGui, SLOT( addGroupTab(const char*) ) );
  //connect(pc_mPresenter, SIGNAL( addGroup(const char*) ), pc_mGui, SLOT( addGroup(const char*) ) );
  //connect(pc_mPresenter, SIGNAL( removeGroupTabs()), pc_mGui, SLOT( removeGroupTabs()) );
  connect(pc_mPresenter, SIGNAL( setEnabled(bool)) , pc_mGui, SLOT( setEnabled(bool) ) );

  // Gui --> Presenter
  connect(pc_mGui,SIGNAL(printAll(const char*)), pc_mPresenter, SLOT(printAll(const char*)) );
  //connect(pc_mGui,SIGNAL(nextEvent()), pc_mPresenter, SLOT(NextEvent()) );
//  connect(qApp,SIGNAL(lastWindowClosed()),pc_mPresenter,ClosePresenter()));
  pc_mCanvas = 0;
}


void JevpGui::pc_save() {
  pc_mPresenter->jl_Save("");
}

void JevpGui::pc_saveAs() {
  // QString dir(EvpUtil::GetOutputPath());
  QString dir("");

  QString filter("*.root");
#if QT_VERSION < 0x40000
  QFileDialog dialog( dir, filter, pc_mGui, "", true );
#else /* QT4 */
  Q3FileDialog dialog( dir, filter, pc_mGui, "", true );
#endif /* QT4 */
  dialog.exec();
  if (!dialog.selectedFile().isEmpty()) {
    pc_mPresenter->jl_Save( dialog.selectedFile().ascii() );
  }
}


void JevpGui::pc_live() {
  //cout << "liveButton" << endl;
  pc_mPresenter->jl_Stop();   // will restart automatically
  //pc_mPresenter->jl_SetSource();
}

void JevpGui::pc_file() {
  //cout << "fileButton" << endl;
  //QString dir(EvpUtil::GetOutputPath());
  QString dir("/home/jml");

  QString caption("File dialog");
#if QT_VERSION < 0x40000
  QFileDialog dialog(dir, QString(), pc_mGui,caption);
#else /* QT4 */
  Q3FileDialog dialog(dir, QString(), pc_mGui,caption);
#endif /* QT4 */
  dialog.setCaption(caption);
  dialog.addFilter("*.root");
  //  dialog.addFilter("*.map");
  dialog.exec();

  QString file = dialog.selectedFile();
  QString mapFile = file;
  int iret = 0;
//  if ( file.find(".map") < 0 ) {   // must be root file, only *.root and *.map are allowed
//    mapFile.replace(".root",".map");
//    iret = EvpUtil::Root2Map(file,mapFile);
//  } 

  if (iret) {
#if QT_VERSION < 0x40000
    cerr << "### error ### Can not open file : " << mapFile << endl;
#else /* QT4 */
    cerr << "### error ### Can not open file : " << mapFile.toStdString() << endl;
#endif /* QT4 */
    return;
  }

  //  pc_mPresenter->jl_SetSource( mapFile.ascii() );
  emit pc_updateRequest();
}

void JevpGui::pc_openReference() {
  cout << "Opening reference" << endl;
  //JevpGui* gui2 = new JevpGui(true);
  //gui2->resize(500,500);
  //gui2->show();
//  EvpUtil::ReadCanvasDefinitions();
  //EvpPresenter* presenter2 = new EvpPresenter(EvpUtil::mReference);
  //JevpGui* presenter2 = new JevpGui();
  //PresenterConnect* con2 = new PresenterConnect(gui2,presenter2);

//  QString file = "/home/dkettler/test/run10029077.root";
//  QString mapFile = file;
//  mapFile.replace(".root",".map");
//  int iret = EvpUtil::Root2Map(file,mapFile);
//  
//  if (iret) {
//    cerr << "### error ### Can not open file : " << mapFile << endl;
//    return;
//  }

//  QString mapFile = "/a/pplot/histos/run10031084.map";
//  presenter2->SetSource( EvpUtil::mReference );
}
   
void JevpGui::pc_update() {
//   if ( pc_mPresenter->jl_serverStatus()->diffTimeInSec() >120.) {
//     if ( pc_mPresenter->jl_Status() ) {
//       live();
//       std::cout << "live again "<< endl;
//     }
//   }
  emit pc_signalEventInfo(pc_mPresenter->jl_run(),0,0,0,0,0,0,0);
  //  emit pc_signalServerInfo(pc_mPresenter->jl_serverStatus());
}


void JevpGui::pc_update(TCanvas* canvas, int tab, int subTab) {
  update();
  printf("In update? a \n");
  if (canvas) pc_mPresenter->jl_Draw(canvas,pc_mTab,pc_mSubTab);
}

void JevpGui::pc_update(TCanvas* canvas, const char* name) {
  update();
  printf("In update? b\n");
  if (canvas) pc_mPresenter->jl_Draw(canvas, name );
}

void JevpGui::pc_print() {
//   //cout << "print" << endl;
//   int tab = pc_mGui->GetTabId();
//   int subTab = pc_mGui->GetSubTabId();
//   TCanvas* cc = pc_mGui->GetCanvas();
//   pc_mPresenter->jl_Print(cc,tab,subTab);
}
   
   
void JevpGui::pc_setTab(int t) { pc_mTab = t;} 
void JevpGui::pc_setSubTab(int t) { pc_mSubTab = t;} 
void JevpGui::pc_setCanvas(TCanvas* t) { pc_mCanvas = t;} 



////////////////JevpLogic///////////////////


int JevpGui::jl_ConnectToServerPort(int port, int ntries)
{
  // First disconnect from current server, if sensible.
  if(jl_socket != NULL) {
    jl_socket->Close();
    jl_socket = NULL;
  }
  
  evpMain->serverport = port;
  TSocket *nsocket;
  for(int i=0;i<ntries;i++) {
    nsocket = new TSocket(evpMain->server, evpMain->serverport);
    if(nsocket->IsValid()) {
      jl_socket = nsocket;
      return 0;
    }

    printf("Error Connecting (%dth of %d tries): %d\n", i, ntries, nsocket->GetErrorCode());
    sleep(1);
  }

  return -1;
}

int JevpGui::jl_LaunchRun(char *runNumber) {
  EvpMessage msg;
  msg.setCmd((char *)"launch");
  msg.setSource((char *)"presenter");
  msg.setArgs(runNumber);
  jl_send(&msg);


  // Get response...
  TMessage *mess;
  int ret = jl_socket->Recv(mess);
  if(ret == 0) {  // disconnect
    printf("Server disconnected?\n");
    exit(0);
  }
  
  //int x = (int)mess->GetClass();
  if(strcmp(mess->GetClass()->GetName(), "EvpMessage") != 0) {
    printf("Didn't get a EvpMessage class\n");
    exit(0);
  }

  EvpMessage *response = (EvpMessage *)mess->ReadObject(mess->GetClass());

  printf("Got a response: %s %s\n",response->cmd,response->args);

  if(strcmp(response->cmd, "launch") != 0) {
    printf("Didn't get a launch command...");
    return 0;
  }

  int port = atoi(response->args);
  
  return port;
}

void JevpGui::jl_killServer() {
  EvpMessage msg;
  msg.setCmd((char *)"kill");
  jl_send(&msg);

  jl_socket->Close();
  jl_socket = NULL;
}


void JevpGui::jl_JevpLogic() 
{
  jl_mLastDrawnCanvas = NULL;
  // We need to set up the sockets...
  jl_screens = new TList();

  jl_socket = NULL;

  //   if(evpMain->server) {
  //     socket = new TSocket(evpMain->server,  evpMain->serverport);
  //     if(!socket->IsValid()) {
  //       socket->NetError("connect: ",socket->GetErrorCode());
  //       exit(0);
  //     }
  //   }

  if(jl_ConnectToServerPort(evpMain->serverport,5) < 0) return;
  
  if(evpMain->display == NULL) {
    printf("Need to specify the display\n");
    exit(0);
  }

  if(!evpMain->server || evpMain->displayFile==1) {
    // Read Display from file...

    jl_displayFile = new DisplayFile();
    if(jl_displayFile->Read(evpMain->display) < 0) {
      printf("Error reading display file: %s\n",evpMain->display);
      exit(0);
    }
    
    jl_displayFile->dump();
  }
  else {
    // Read display from server...
    
    EvpMessage msg;
    msg.setCmd((char *)"display_desc");
    msg.setArgs(evpMain->display);
    jl_send(&msg);
    
    // get response...
    //printf("Waiting for tab data from server...\n");
    TMessage *mess;
    int ret = jl_socket->Recv(mess);
    if(ret == 0) {  // disconnect
      printf("Server disconnected?\n");
      exit(0);
    }
    
    //printf("Got something ret=%d mess=0x%x\n",ret,mess);
    int x = (int)mess->GetClass();

    printf("--->0x%x\n",x);// , x[0],x[1],x[2],x[3]);

    if(strcmp(mess->GetClass()->GetName(), "EvpMessage") != 0) {
      printf("Didn't get a DisplayDefSender class\n");
      exit(0);
    }

    EvpMessage *tabdata = (EvpMessage *)mess->ReadObject(mess->GetClass());

    //   printf("....cmd was %s\n",tabdata->cmd);

    if(tabdata->args == NULL) {
      printf("No display '%s' found...\n", evpMain->display);
      exit(0);
    }

    jl_displayFile = new DisplayFile();

    //    printf("asdf\n");
    //    printf("tabdata 0x%x\n",tabdata->args);
    //    printf("%c%c%c\n",tabdata->args[0],tabdata->args[1],tabdata->args[2]);

    jl_displayFile->ReadBuff(tabdata->args, strlen(tabdata->args));
    
    jl_displayFile->setDisplay(evpMain->display);
    // printf("I just got the display file: \n");
    // jl_displayFile->dump();
    // printf("Done dumping it...\n");
    
    delete tabdata;


  }


  // Connect();
  // Some preparations here
  // Here is starting directory name
  SetDebugLevel(0);
}

void JevpGui::jl_JevpLogic(const char* file) {
  jl_mLastDrawnCanvas = 0;
  // Some preparations here
  // Here is starting directory name
  SetDebugLevel(0);
}

//----------------------------------------------------------------
//JevpGui::jl_~JevpLogic() {
//}


//--------------------------------------------------------------
// void EvpPresenter::SetSource(const char* file)  {
//   if ( mDebugLevel) cout << __PRETTY_FUNCTION__ << endl; 
//   emit setEnabled(false);
//   Disconnect();
//   sprintf(mMapFile,"%s",file);
//   Connect();
//   emit setEnabled(true);
// }
//--------------------------------------------------------------



//--------------------------------------------------------------
// void EvpPresenter::ReconfigureTabs() {
//   cout << __PRETTY_FUNCTION__ << endl;
//   emit removeGroupTabs();
//   addGroupTabs();  //tmp->remove();
// }

//--------------------------------------------------------------


void JevpGui::jl_DrawPlot(JevpScreenWidget *screen) {
  char tmp[256];
  tmp[0] = '\0';

  screen->setUpdatesEnabled(false);   // keep double buffer in place...

  int combo_index = screen->combo_index;
  TCanvas *gcc = screen->GetCanvas();

  //printf("sleeping 5\n");
  //sleep(5);

  //int isCanvas=0;
  
  printf("drawplot....combo_index = %d\n",combo_index);

  // I need to use the display def version to get a 
  // real object rather than a string...
  CP;
  DisplayNode *thetab = jl_displayFile->getTab(combo_index);
  CP;
  if(!thetab->leaf) {
    sprintf(tmp, "No histo for index=%d",combo_index);
    LOG("JEFF", "Cross of death: %s",tmp);
    jl_CrossOfDeath(screen, tmp);
    CP;
    return;
  }

  //   void *thetab = jl_displayFile->getTab(combo_index, &isCanvas);
  //   if(!isCanvas) {
  //     sprintf(tmp, "No canvas for index=%d",combo_index);
  //     jl_CrossOfDeath(screen, tmp);
  //     return;
  //   }

  
  CP;
  // CanvasDescriptor *cd = (CanvasDescriptor *)thetab;
  int nplots = thetab->nSiblings() + 1;
  screen->Clear();
  // gcc->Clear();
  
  int wide = thetab->getIntParentProperty("wide");
  if(wide <= 0) wide = 1;
  int deep = thetab->getIntParentProperty("deep");
  if(deep <= 0) deep = 1;
  int scaley = thetab->getIntParentProperty("scaley");
  if(scaley <= 0) scaley = 0;

  CP;
  gcc->Divide(wide, deep);
  //sleep(3);

  double maxY = -9999;

  DisplayNode *hd = thetab;
  for(int i=0;i<nplots;i++) {   // First get plots!
    char tmp[256];
    JevpPlot *plot = jl_getPlotFromServer(hd->name,  tmp);
  
    if(plot) {
      screen->addPlot(plot);
      screen->addJevpPlot(plot);
      double my = plot->getMaxY();
      if(my > maxY) maxY = my;
    }

    hd = hd->next;   
  }

  CP;

  // Now plot them
  hd = thetab;
  CP;
  for(int i=0;i<nplots;i++) {
    JevpPlot *plot = screen->getJevpPlot(hd->name);
    gcc->cd(i+1);

    if(plot == NULL) {
      CP;
      LOG("JEFF", "Cross of death: %s",tmp);
      sprintf(tmp, "Server Doesn't Currently Have Plot %s",hd->name);
      jl_CrossOfDeath(screen, tmp);     // Some error...
    }
    else {
      CP;
      if(scaley) {
	plot->setMaxY(maxY * 1.2);
      }
      
      CP;
      plot->draw();
      //delete plot;
    }
    CP;
    //printf("drew %s\n",hd->name);

    hd = hd->next;    
    // sleep(3);
  }

  CP;

  screen->setUpdatesEnabled(true);   // reenable the update
  gcc->Update();
  screen->update();                  // and trigger!

  CP;
  //showDirectories();
}
    
void JevpGui::jl_saveExistingPlot(JevpPlot *plot)
{
  plot->refid = -plot->refid;
  jl_writePlotToServer(plot);
}

void JevpGui::jl_writePlotToServer(JevpPlot *plot)
{
  TMessage mess(kMESS_OBJECT);
  mess.WriteObject(plot);
  jl_socket->Send(mess);
}

void JevpGui::jl_deletePlot(JevpPlot *plot) {
  EvpMessage msg;
  msg.setCmd((char *)"deleteplot");
  char str[256];
  sprintf(str, "%s %d",plot->GetPlotName(), plot->refid);
  msg.setArgs(str);
  jl_send(&msg);
}

void JevpGui::jl_swapRefsOnServer(char *name, int idx1, int idx2)
{
  EvpMessage msg;
  msg.setCmd((char *)"swaprefs");
  char str[256];
  sprintf(str, "%s %d %d",name, idx1, idx2);
  msg.setArgs(str);
  jl_send(&msg);
}

JevpPlot *JevpGui::jl_getPlotFromServer(char *name, char *error)
{
  error[0] = '\0';   // clear error...
  
  // Ask server for plot...
  EvpMessage msg;
  msg.setCmd("getplot");
  msg.setArgs(name);
  jl_send(&msg);
  
  // get response...
  printf("Waiting for plot from server...\n");
  TMessage *mess;
  int ret = jl_socket->Recv(mess);
  if(ret == 0) {  // disconnect
    printf("Server disconnected?\n");
    sprintf(error, "Can't get plot: %s  (no server)",name);

    return NULL;
  }
  
  if(strcmp(mess->GetClass()->GetName(), "EvpMessage") == 0) {
    // There was no valid object...
    EvpMessage *msg = (EvpMessage *)mess->ReadObject(mess->GetClass());
    sprintf(error, "Can't get plot: (%s)", msg->args);
    
    delete msg;
    delete mess;
    return NULL;
  }

  if(strcmp(mess->GetClass()->GetName(), "JevpPlot") == 0) {
    JevpPlot *plot = (JevpPlot *)mess->ReadObject(mess->GetClass());

    delete mess;
    return plot;
  }

  printf("Invalid message type %s\n",mess->GetClass()->GetName());
  sprintf(error, "Invalid message type %s",mess->GetClass()->GetName());
  delete mess;
  return NULL;
}


void JevpGui::jl_CrossOfDeath(JevpScreenWidget *screen, char *str) {

  TLine* a = new TLine(0.,0.,1.,1.);
  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,str);

  // This is how we free the memory...
  a->SetBit(kCanDelete);
  b->SetBit(kCanDelete);
  t->SetBit(kCanDelete);
  screen->addPlot(a);
  screen->addPlot(b);
  screen->addPlot(t);

  a->SetLineColor(2);
  b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad...
  a->Draw();
  b->Draw();
  t->Draw();

  //gcc->Update();
  //cout << __PRETTY_FUNCTION__ << endl;
  return;
}
 

void JevpGui::jl_Draw(TCanvas* gcc, int  tab, int subTab) {
  //   if(EvpUtil::hGroupName[tab][subTab] != "") {
  //     Draw(gcc, EvpUtil::hGroupName[tab][subTab]);
  //     return;
  //   }

  //   if ( mDebugLevel) {
  //     cout << "Draw tab/subtab : " << tab << "/" << subTab << endl;
  //   }
  //   if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][subTab])==0 ) { jl_CrossOfDeath(gcc); return; }
  //   if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][subTab])==0 ) { jl_CrossOfDeath(gcc); return; }
  //   if (!mfile) return;
  
  //   if ( mLastDrawnCanvas != gcc ) needsUpdate = true;
  //   if ( !needsUpdate ) return;
  
  // //  GenericFile* gen = new GenericFile(mfile);
  // //  EvpUtil::DisplayOneCanvas(gen,gcc,tab,subTab);
  // //  delete gen;
  //   EvpUtil::DisplayOneCanvas(mfile,gcc,tab,subTab);
  //   mLastDrawnCanvas =  gcc;
}

void JevpGui::jl_Draw(TCanvas* gcc, const char* group) {
  //   if ( mDebugLevel ) {
  //     cout << "Draw group : " << group << endl;
  //   }
  //   HistogramGroup* hg = mGroups.read(mfile,group);
  //   if ( !gcc ) {
  //     cout <<  __PRETTY_FUNCTION__ << " no canvas " << endl;
  //     return; 
  //   } 
  //   if ( !hg ) { 
  //     cout <<  __PRETTY_FUNCTION__ << " no histogram group " << endl;
  //     return;
  //   }
  //   if ( mLastDrawnCanvas != gcc ) needsUpdate = true;
  //   if ( !needsUpdate ) return;
  
  //   hg->draw(gcc);
  //   mLastDrawnCanvas = gcc;
}



void JevpGui::jl_Save(const char* file){
  //   char filename[1024];
  //   //cout << " fileame " << file << endl;
  //   if ( strcmp(file,"")==0 ) {
  //     sprintf(filename,"%s/run%d.root",EvpUtil::GetOutputPath(),mRS->getRunNumber());
  //   } else {
  //     if ( strcmp(file,gSystem->BaseName(file))==0) { // no path given, add default path
  //       sprintf(filename,"%s/%s",EvpUtil::GetOutputPath(),file);
  //     } else { // use filename as given
  //       sprintf(filename,"%s",file);
  //     }
  //   }
  //   cout << " filename " << filename << endl;
  //   if(mfile->mapFile()) {
  //     TMapFile* mapfile = (TMapFile*) mfile->file();
  //     EvpUtil::Map2Root(mapfile,filename);
  //   } else {
  //     TFile* rfile = (TFile*) mfile->file();
  //     rfile->Write(filename);
  //   }
}

void JevpGui::jl_SaveAll(){
  //   char filename[1024];
  //   char cmd[1024];
  //   char title[1024];
  //   char cname[1024];
  //   //if ( true) return ;
  //   sprintf(filename,"run%d.map",mRS->getRunNumber() );
  //   sprintf(title,"End of run action : run%d",mRS->getRunNumber());
  //   sprintf(cname,"%d",(int)time(0));
  //   sprintf(cmd,"/bin/cp %s %s/%s",mMapFile,EvpUtil::GetOutputPath(),filename);
  //   //cout << cmd << endl;
  //   gSystem->Exec(cmd);
  
  //   gSystem->Sleep(1000);
  //   sprintf(cmd,"pwd; endOfRunAction.csh %s/%s & ",EvpUtil::GetOutputPath(),filename);
  //   cout << cmd << endl;
  //   gSystem->Exec(cmd);

}


void JevpGui::jl_WriteCurrent(int i, int j){
  //   char psFilename[1024];
  //   char pdfFilename[1024];
  //   sprintf(psFilename,"%s/run%d_tab%d_%d.ps",EvpUtil::GetOutputPath(),mRS->getRunNumber(),i,j);
  //   sprintf(pdfFilename,"%s/run%d_tab%d_%d.pdf",EvpUtil::GetOutputPath(),mRS->getRunNumber(),i,j);
  //   WriteCurrentCanvasToPSFile(psFilename,i,j);

  //   char cmd[1024];
  //   sprintf(cmd,"/usr/bin/convert %s %s",psFilename, pdfFilename);
  //   int iret = gSystem->Exec(cmd);
  //   if ( !iret ) {
  //     cout << " ### error ### writing cureent canvas failed " << endl;
  //     cout << " ### error ### psFilename: " << psFilename << endl;
  //     cout << " ### error ### pdfFilename: " << pdfFilename << endl;
  //     cout << " ### error ### error-code: " << iret << endl;
  //   }
  //   /*
  //   if (iret==0) {
  //     PGMessage* pg = new PGMessage("Writing current canvas:",false,0xffffff); 
  //     pg->AddLine(psFilename);
  //     pg->AddLine(pdfFilename);
  //     pg->AddLine("O.K");
  //     pg->Layout();
  //   } else {
  //     PGMessage* pg = new PGMessage("### error ### Writing current canvas:",false,0xff0000); 
  //     pg->AddLine(psFilename);
  //     pg->AddLine(pdfFilename);
  //     pg->AddLine("return error");
  //     pg->Layout();
  //   }
  //   */

}    


void JevpGui::jl_WriteCurrentCanvasToPSFile(const char* filename, int tab, int subTab){
  //   // Find active canvas indexes
  //   int i = tab;
  //   int j = subTab;
  //   //cout << filename << endl;
  //   int type = 111; //portrait ps
  //   //int type =112; //landscape ps
  //   //int type =113; //eps

  //   char *name = "Saving Canvas in PS File";
  //   int canvasWidth  = EvpUtil::mCanvasWidth;
  //   int canvasHeight = EvpUtil::mCanvasHeight;


  //   TCanvas* c1 = new TCanvas(name, name, canvasWidth, canvasHeight);
  //   TPaveLabel title(0.1,0.96,0.9,0.99,mHistoPSFile);

  //   TDatime ftime;
  //   TPaveLabel date(0.7,0.01,0.9,0.03,ftime.AsString());


  //   //cout<<filename<<endl;
  //   TPostScript *ps = new TPostScript(filename,type);


  //   c1->cd();
  //   title.Draw();
  //   date.Draw();
  //   EvpUtil::DisplayOneCanvas(mfile,c1,i,j);
 
  //   c1->Update();

  //   ps->Close();
  //   c1->Close();

  //   //gSystem->Exec("ghostview test.ps");
  //   delete ps;
  //   //delete plotPad;
  //   delete c1;

}



void JevpGui::jl_printAll(const char* filename) {
  //   cout << __PRETTY_FUNCTION__ << " " <<  filename << endl;

  //   gROOT->SetBatch(kTRUE);
  //   TCanvas *cc = new TCanvas("printAllCanvas","printAllCanvas",EvpUtil::mCanvasWidth,EvpUtil::mCanvasHeight);
  //   cc->cd();
  //   //TPostScript ps(filename,111);
  //   char openPs[1024]; 
  //   char printPs[1024];
  //   char closePs[1024];
  //   sprintf(openPs,"%s[",filename);
  //   sprintf(printPs,"%s",filename);
  //   sprintf(closePs,"%s]",filename);

  //   cc->Print(openPs);
  //   //DisplayRunStatus();
  //   cc->Print(printPs);
  //   for ( int tab=0; tab<EvpUtil::mNumberOfTabs; tab++) {
  //     for ( int subTab=0; subTab<EvpUtil::mNumberOfSubTabs[tab]; subTab++) {
  //       if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][subTab])==0 ) continue;
  //       if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][subTab])==0 )   continue;
  //       EvpUtil::DisplayOneCanvas(mfile,cc,tab,subTab,true);
  //       cc->Print(printPs);
  //     }
  //   }
  //   mGroups.print(cc,printPs);
  //   cc->Print(closePs);
  //   //mGroups.display(cc);
  //   //ps.Close();
  //   cout << filename << " written " << endl;
  //   delete cc;
}


void JevpGui::jl_Print(TCanvas* gcc, int tab, int sub) {
  //   char psFilename[1024];
  //   sprintf(psFilename,"%s/run%d_tab%d_%d.ps",EvpUtil::GetOutputPath(),mRS->getRunNumber(),tab,sub);
  //   gROOT->SetBatch(kTRUE);
  //   TCanvas* cc = new TCanvas(psFilename,psFilename,EvpUtil::mCanvasWidth,EvpUtil::mCanvasHeight);
  //   cc->cd();
  //   if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[tab][sub])==0 ) { jl_CrossOfDeath(gcc); return; }
  //   if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[tab][sub])==0 ) { jl_CrossOfDeath(gcc); return; }
  //   EvpUtil::DisplayOneCanvas(mfile,cc,tab,sub);
  //   cc->Update();
  //   cc->Print(psFilename);
  //   cc->Close(); 
  //   gROOT->SetBatch(kFALSE);
  //   char cmd[1024];
  //   sprintf(cmd,"lp -d onlprinter2 %s",psFilename);
  //   sprintf(cmd,"ls %s",psFilename);
  //   gSystem->Exec(cmd);
}



void JevpGui::jl_addGroupTabs() {
  //     GroupMap groupMap( mGroups );
  //     char name[1024];
  //     for( GroupMapIterator mapIter = groupMap.begin(); mapIter != groupMap.end(); mapIter++) {
  //       //cout << (*mapIter).first.c_str() << endl;
  //       sprintf(name,"%s",(*mapIter).first.c_str());
  //       //PR((*mapIter).second.numberOfActiveGroups());
  //       if ( (*mapIter).second.numberOfActiveGroups() ) {
  // 	emit addGroupTab( name );
  // 	for( GroupIterator groupIter = (*mapIter).second.begin(); groupIter != (*mapIter).second.end(); groupIter++) {
  // 	  //cout << "\t " << (*groupIter)->subGroupName() << " " << (*groupIter)->id() << endl;
  // 	  if ( (*groupIter)->active() ) {
  // 	    //sprintf(name,"%s#%s#%s",(*groupIter)->subGroupName(),(*groupIter)->triggerName(),(*groupIter)->detectorName());
  // 	    //emit addGroup( name );
  // 	    emit addGroup( (*groupIter)->id() );
  // 	  }
  // 	}
  //       }
  //     }
}
#if 0
//______________________________________________________________________________ 
void JevpGui::jl_ClosePresenter() 
{
  // Qt [slot] to terminate the application
  Stop();
}

#endif

u_int JevpGui::jl_getTabBase()
{
  return DisplayFile::getTabBase();
}

// Gets the multiplier to access the final
u_int JevpGui::jl_getTabDepthMult(u_int idx)
{
  return DisplayFile::getTabDepthMult(idx);
}

u_int JevpGui::jl_getTabNextIdx(u_int idx)
{
  return DisplayFile::getTabNextIdx(idx);
}

u_int JevpGui::jl_getTabChildIdx(u_int idx)
{
  return DisplayFile::getTabChildIdx(idx);
}

u_int JevpGui::jl_getTabIdxAtDepth(u_int idx, u_int depth)
{
  return DisplayFile::getTabIdxAtDepth(idx, depth);
}

int JevpGui::jl_send(TObject *msg) {
  TMessage mess(kMESS_OBJECT);
  
  mess.WriteObject(msg);
  jl_socket->Send(mess);
  return 0;
}


void JevpGui::jl_showDirectories()
{
  printf("gDirectories ls()-------->\n");
  gDirectory->ls();
  printf("screen objects ls()------>\n");
  
  TIter next(jl_screens);
  int i=0;
  JevpScreenWidget *widget;
  while((widget = (JevpScreenWidget *)next())) {
    printf("Widget: %d\n",i++);
    widget->GetCanvas()->ls();
  }
}
