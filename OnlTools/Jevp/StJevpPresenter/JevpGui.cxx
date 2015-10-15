///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include "JevpGui.h"
#include "TQtRootSlot.h"
#include "TROOT.h"
#include "TRint.h"
#include <TSocket.h>
#include <TClass.h>
#include <TLine.h>
#include "TEnv.h"
#include "ReferenceWidget.h"
#include "ZoomWidget.h"
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
#include <mcheck.h>
#include <RTS/include/SUNRT/clockClass.h>

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

JevpGui *gJevpGui;

#define CM showMyMemory(__LINE__)

void showMyMemory(int line) {
  FILE *f = fopen("/proc/self/stat", "ro");
  if(!f) {
    LOG("JEFF", "Can't read stat");
    return;
  }

  int pid;
  char comm[256];
  char state;
  int ppid,pgrp,session,tty_nr,tpgid;
  unsigned long int flags,minflt,cminflt,majflt,cmajflt,utime,stime;
  long int cutime,cstime,priority,nice, place, itrealvalue;
  unsigned long int starttime,vsize;
  long int rss;
  
  fscanf(f, "%d %s %c %d %d %d %d %d %lu %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld%ld %ld %lu %lu %ld",
	 &pid, comm, &state, 
	 &ppid, &pgrp, &session, &tty_nr, &tpgid,
	 &flags, &minflt, &cminflt, &majflt, &cmajflt, &utime, &stime,
	 &cutime, &cstime, &priority, &nice, &place, &itrealvalue, &starttime, 
	 &vsize, &rss);
  
  LOG("JEFF", "(%d) Mem: virt %lu rss %ld",line, vsize, rss);
  fclose(f);
}

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
  //printf("Is Double Buffered? %d",IsDoubleBuffered());

  plots = new TList();
  jevpPlots = new TList();
  parentMenu = menu;
  this->combo_index = combo_index;
  LOG(DBG,"Creating JevpScreenWidget for %s %s\n",plotname,tabname);
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
  LOG(DBG,"Deleting JevpScreen for %s\n",plot->c_str());
  delete plot;
}

void JevpScreenWidget::mousePressEvent(QMouseEvent *e)
{
  if(e->buttons() & Qt::RightButton) {
    int wide, deep;
    DisplayNode *node = gJevpGui->jl_getCanvasDescriptor(combo_index);
    
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


#ifdef USE_QTIP
    QToolTip::showText(e->globalPos(), hnode->name);
#else

    // Add a popup menu...
    Q3PopupMenu *pop = new Q3PopupMenu(this);
    pop->insertItem(hnode->name,NULL,1,1);
    pop->insertItem("Reference Plot...",NULL,2,2);
    pop->insertItem("Zoom Plot...",3,3);
    
    int ret = pop->exec(QCursor::pos());
    {
      if(ret == 1) {
	LOG("JEFF", "Do nothing...");
      }
      else if (ret == 2) {
	LOG("JEFF", "Do reference plots");
	if(hnode->name) {
	  ReferenceWidget *ref = new ReferenceWidget(gJevpGui, hnode->name);
	  ref->exec();
	  LOG(DBG,"Returned....\n");
	  delete ref;
	}
      }
      else if (ret == 3) {
	LOG("JEFF", "Do zoom plot");
	if(hnode->name) {
	  ZoomWidget *ref = new ZoomWidget(gJevpGui, hnode->name);
	  ref->exec();
	  LOG(DBG,"Returned....\n");
	  delete ref;
	}
      }
    }

    delete pop;
#endif

    //LOG("JEFF","Got a mouse press event...%d %d  %s:\n",x,y,hnode->name);
  }
}

void JevpScreenWidget::mouseDoubleClickEvent(QMouseEvent *e)
{
  int wide, deep;
  DisplayNode *node = gJevpGui->jl_getCanvasDescriptor(combo_index);
  
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
  
  LOG(DBG,"Got a mouse press event...%d %d  %s:\n",x,y,hnode->name);
  
  if(hnode->name) {
    ReferenceWidget *ref = new ReferenceWidget(gJevpGui, hnode->name);
    ref->exec();
    LOG(DBG,"Returned....\n");
    delete ref;
  }
}

void JevpGui::fillTab(QTabWidget *tab, u_int idx)
{
  RtsTimer_root clock;


  LOG(DBG, "Filltab %d",idx);

  PresenterSuspend suspend;

  DisplayNode *mynode = jl_getTab(idx);
  LOG("JEFF", "getTab(%d)=0x%x",idx, mynode);
  u_int child_idx = jl_getTabChildIdx(idx);
  if(!mynode) {
    LOG(DBG, "Didn't find mynode for %d",idx);
    return;
  }

  LOG(DBG, "Mynode is %s",mynode->name);

  DisplayNode *childnode = jl_getTab(child_idx);
  LOG("JEFF", "getTab(%d)=0x%x",child_idx, childnode);

  if(!childnode) {
    LOG(DBG, "Didn't find child in %s for %d",mynode->name,child_idx);
    return;
  }

  if(childnode->leaf) {
    LOG(DBG,"create widget: fillTab idx=%d, name=%s (canvas)\n",child_idx,childnode->name);    
    clock.record_time();
    JevpScreenWidget *nwidget = new JevpScreenWidget(mynode->name, childnode->name, child_idx, tab);
    double t1 = clock.record_time();

    jl_screens->Add((TObject *)nwidget);

    suspend << nwidget;

    LOG(DBG, "nwidget = 0x%x",nwidget);

    if(currentScreen == NULL) {   // Assume very first tab is the screen tab at first...
      currentScreen = nwidget;
    }
    
    double t2 = clock.record_time();
    suspend << tab;

    tab->addTab(nwidget, mynode->name);
  
    char lab[100];
    sprintf(lab, "%s - not available",nwidget->plot->c_str());

    double t3 = clock.record_time();

    LOG(DBG, "fill leaf (%s) %lf %lf %lf",lab,t1,t2,t3);
  }
  else {
    clock.record_time();
    QTabWidget *nwidget = new QTabWidget();
    suspend << nwidget;

    double t1 = clock.record_time();
    // We want to know when the tabs are selected...
    connect(nwidget, SIGNAL(currentChanged(QWidget *)), this, SLOT(tabChanged(QWidget*)));

    tab->addTab(nwidget, mynode->name);
    fillTab(nwidget, child_idx);

    double t2 = clock.record_time();

    LOG(DBG, "Fill tab: (%s) %lf %lf",mynode->name,t1,t2);
  }
  
  u_int next_idx = jl_getTabNextIdx(idx);
  fillTab(tab, next_idx);
}

// set leave summary = 1 to leave the summary page...
void JevpGui::deleteTabs(QTabWidget *tab, int leaveSummary)
{  
  RtsTimer_root c1;
  c1.record_time();
  RtsTimer_root clock;
  clock.record_time();
  int tabs = 0;

  int ntabs = tab->count() - 1;
  for(int idx=ntabs; idx >= (leaveSummary ? 1 : 0); idx--) {
    char buff[100];
    strcpy(buff, tab->tabText(idx));
    tabs++;
    clock.record_time();

    QWidget *widget = tab->widget(idx);
    
    widget->blockSignals(true);
    
    QTabWidget *ctab = dynamic_cast<QTabWidget *>(widget);
    JevpScreenWidget *screen = dynamic_cast<JevpScreenWidget *>(widget);

    LOG(DBG, "deleteTabs:  idx=%d tab=%d screen=%d",idx, tab ? 1 : 0, screen ? 1 :0);

    double t1 = clock.record_time();

    tab->removeTab(idx);
    double t2 = clock.record_time();
    double t3=0;
    double t4 = 0;
    double t5 = 0;
    if(ctab) {
      deleteTabs(ctab);
      t5 = clock.record_time();
      delete ctab;
      t3 = clock.record_time();
    }
    if(screen) {
      delete screen;
      t4 = clock.record_time();
    }

    LOG(DBG, "(tab #%d (%s) - %lf %lf (%lf %lf) %lf",tabs,buff,t1,t2,t5,t3,t4);
  }

  double t6=c1.record_time();
  LOG(DBG, "(tab -------  (%lf))",t6);
}

void JevpGui::switchTabs(const char *newdisplay, const char *newbuilderlist) {
  CP;

  RtsTimer_root clock;
  clock.record_time();

  int ret = jl_displayFile->setDisplay((char *)newdisplay);
  if(ret == -1) {
    LOG(ERR, "No display %s",newdisplay);
  }

  static char evpMainDisplay[40];
  strcpy(evpMainDisplay, newdisplay);
  evpMain->display = evpMainDisplay;

  LOG("JEFF", "Setting servertags to %s",newbuilderlist);
  jl_displayFile->setServerTags(newbuilderlist);

  double t1 = clock.record_time();

  // jl_displayFile->dump();

  CP;
  LOG(DBG, "Calling deleteTabs for root");

  PresenterSuspend suspend;
  suspend << rootTab;
  deleteTabs(rootTab,1);
  
  LOG(DBG, "Back from deletTabs for root");
  CP;

  double t2 = clock.record_time();

  LOG(DBG, "Calling buildTabs");
  buildTabs(0);
  LOG(DBG, "Back from buildTabs");

  double t3 = clock.record_time();
  LOG(DBG, "changed tabs: %lf %lf %lf",t1,t2,t3);
}

void JevpGui::buildTabs(int addSummary)
{
  PresenterSuspend suspend;

  currentScreen = NULL;
 
  suspend << rootTab;

  int firstTab = 1;
  if(addSummary == 1) {
    firstTab = 1;
  }
  else {
    firstTab = 2;
  }

  fillTab(rootTab, firstTab);

  LOG(DBG, "after fillTab currentScreen = 0x%x",currentScreen);

  // Need to know when the tab is selected...
  connect(rootTab, SIGNAL(currentChanged(QWidget *)), this, SLOT(tabChanged(QWidget*)));
}


static TQtBrowserMenuItem_t gMenu_Data[] = {
  // { text, id, accelerator, tooltip, icon }
  /* File Menu */
  { "Change Histogram Set", kFileChangeHistogramSet, 0, "Change Histogram Set", ""},
  { "Ignore Server Tags", kFileIgnoreServerTags, 0, "Ignore Server Tags", ""},
  { "&Change To Run", kFileChangeToRun, 0, "Re-analyse old run ", ""},
  { "&Live", kFileLive, 0, "Analyze current run", ""},
  { "&Print",     kFilePrint,    Qt::CTRL+Qt::Key_P, "Print the TCanvas image ",            ":/printer.xpm" },
  { "&Print All", kFilePrintAll, 0,                  "Print the TCanvas image ",            ":/printer.xpm" },
//   { "onlprinter2",kOnlPrinter2 , 0,                  "Send last print to onlpinter2",       ":/printer.xpm" },
  { "About",      kHelpAbout,      0, "", ""},
  { "&Update",    kUpdate,       Qt::CTRL+Qt::Key_U, "Update current ",                      ":/update.xpm"  },
  { "&AutoUpdate",kAutoUpdate,   Qt::CTRL+Qt::Key_A, "Automatically update eventy 10 sec",   ":/update.xpm"  },
  { "View Toolbar",     kToolBar,        0,          "show toolbar ",                        "" },
  {0,0,0,"",""}
};



//------------------------------------------------------------------------
JevpGui::JevpGui() : Q3MainWindow(), mWidth(900), mHight(500), mStrLive(" Live  "), mStrFile(" File  "), mStrRun("Running"), mStrStop("Stopped"), fGuiRefreshRate(5000)
{
  gJevpGui = this;
  serverTags = (char *)malloc(10);
  strcpy(serverTags, "");

  refreshTimer = new QTimer(this);
  connect(refreshTimer, SIGNAL(timeout()), this, SLOT(refreshTimerFired()));
  refreshTimer->start(2500);
}




//----------------------------------------------------------------
JevpGui::~JevpGui()
{
  if(serverTags) free(serverTags);
}

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

  fActions[kFileIgnoreServerTags]->addTo(fileMenu);
  fActions[kFileIgnoreServerTags]->setToggleAction(true);
  fActions[kFileIgnoreServerTags]->setOn(false);

  fActions[kFileChangeToRun]->addTo(fileMenu);
  fActions[kFileLive]->addTo(fileMenu);
  fActions[kFileLive]->setToggleAction(true);
  fActions[kFileLive]->setOn(true);
  LOG(DBG,"MakeMenuBar\n");
 
  fileMenu->insertSeparator();

  fActions[kFilePrint]->addTo(fileMenu); 
  fActions[kFilePrintAll]->addTo(fileMenu); 
  //fActions[kOnlPrinter2]->addTo(fileMenu); 

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
  fActions[kToolBar]->setOn(false);

  mainMenu->insertSeparator();

  // Help menu
  Q3PopupMenu *helpMenu  = new Q3PopupMenu();
  mainMenu->insertItem("&Help",helpMenu);
  helpMenu->insertSeparator();
  fActions[kHelpAbout]->addTo(helpMenu);

  // add text to actions icons
  fActions[kFilePrint]->setText("Print");
  fActions[kFilePrintAll]->setText("Print all");
  //fActions[kOnlPrinter2]->setText("onlPrinter2");
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
  //fActions[kOnlPrinter2]->addTo(fToolBar);
  fToolBar->hide();
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
  tabChanged(rootTab);
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
  tabChanged(rootTab);
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
  LOG(DBG,"Processing:  %d\n",fActions[kFileLive]->isOn());

   TQtRootAction *actionSender =  (TQtRootAction *)sender ();
   switch (actionSender->Id()) {

     //case kFileSave:         SaveCB();         break;
     //case kFileSaveAs:       SaveAsCB();       break;
   case kFileChangeHistogramSet:  ChangeHistogramSet(); break;
   case kFileIgnoreServerTags: IgnoreServerTags(); break;
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
   LOG(DBG,"Processed:  %d\n",fActions[kFileLive]->isOn());
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

  QStringList items;
  int i=0;
  char *disp = jl_displayFile->getDisplay(i);
  while(disp) {
    items << tr(disp);
    i++;
    disp = jl_displayFile->getDisplay(i);
  }

  QString text = QInputDialog::getItem(this, 
				       tr("New Histogram Set Name"),
				       tr("New Histogram Set Name:"), 
				       items,
				       0, false, &ok);

  if(ok && !text.isEmpty()) {
    LOG(DBG,"Change Histogram set to %s\n", (const char *)text);
  }
  else return;

  switchTabs((const char *)text, (const char *)serverTags);
  CP;
}

void JevpGui::IgnoreServerTags()
{
  if (!fActions[kFileIgnoreServerTags]->isOn()) {
    fActions[kFileIgnoreServerTags]->setOn(false);
    LOG(DBG, "Ignore servertags 1");
    jl_displayFile->ignoreServerTags = 0;
  }
  else {
    fActions[kFileIgnoreServerTags]->setOn(true);
    jl_displayFile->ignoreServerTags = 1;
    LOG(DBG, "Ignore servertags off");
  }

  char *disp = jl_displayFile->getDisplayName();
  switchTabs((const char *)disp, (const char *)serverTags);
  CP;
}

void JevpGui::ChangeToRun()
{
  LOG(DBG,"Change to Run...\n");
  
  //char *name = getRunNumberDialog();

  RunDialog *mydialog = new RunDialog();
  int ret = mydialog->exec();
  LOG(DBG,"my dialog returned %d\n",ret);

  char *newname = NULL;

  if(ret) {
    newname = mydialog->getResult();
  }
  
  char *name = new char[40];
  strcpy(name, newname);

  delete mydialog;

  if(!newname) return;

  fActions[kFileLive]->setOn(0);
  fActions[kFileLive]->setEnabled(1);



  static char windowname[100];
  sprintf(windowname, "Reanalysing Existing Run: %s",name);
  SetWindowName(windowname);
  
  LOG(DBG,"run name=%s\n",name);

  jl_LaunchRun(name);


  delete name;
}

void JevpGui::ChangeToLive()
{
  LOG(DBG,"Change to live...\n");
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
  char args[100];

  sprintf(args, "OnlPrinter2 %d %d",
	  jl_displayFile->getDisplayIdx(), 
	  currentScreen->combo_index);
	 
  EvpMessage msg;
  msg.setCmd((char *)"print");
  msg.setSource((char *)"presenter");
  msg.setArgs(args);
  jl_send(&msg);
}
//______________________________________________________________________________
void JevpGui::PrintAllCB()
{
  EvpMessage msg;
  char args[100];

  sprintf(args, "OnlPrinter2 %d 0",
	  jl_displayFile->getDisplayIdx());
	 

  msg.setCmd((char *)"print");
  msg.setSource((char *)"presenter");
  msg.setArgs(args);
  jl_send(&msg);
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
  JevpScreenWidget *widget = dynamic_cast<JevpScreenWidget *>(q);

  if(widget) {
    LOG(DBG,"tab changed to %s\n",widget->plot->c_str());
    char lab[100];
    sprintf(lab, "%s - not available",widget->plot->c_str());
    LOG(DBG,"tabchanged:  lab=%s\n",lab);


    widget->setUpdatesEnabled(false);
    jl_DrawPlot(widget);
    widget->setUpdatesEnabled(true);
    widget->update();

    currentScreen = widget;
  }
  else {
    QTabWidget *tab = dynamic_cast<QTabWidget *>(q);
    if(!tab) {
      LOG(ERR,"tab changed to something else (impossible?)\n");
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

// Gets the run status from the server
// updates the window name...
int JevpGui::updateRunStatus()
{
  RtsTimer_root clock;
  clock.record_time();

  EvpMessage msg;
  msg.setCmd((char *)"GetStatus");
  msg.setSource((char *)"presenter");
  msg.setArgs((char *)"");
  jl_send(&msg);

  CP;
  TMessage *mess;
  int ret = jl_socket->Recv(mess);
  if(ret == 0) {  // disconnect
    CP;
    LOG(ERR,"Server disconnected?\n");
    exit(0);
  }
  
  double t1=clock.record_time();
  CP;
  if(strcmp(mess->GetClass()->GetName(), "RunStatus") != 0) {
    LOG(ERR,"Didn't get a RunStatus class: got %s\n",mess->GetClass()->GetName());
    exit(0);
  }
 
  CP;
  RunStatus *rs = (RunStatus *)mess->ReadObject(mess->GetClass());
  delete mess;
  double t2=clock.record_time();

  //LOG("JEFF", "Ethernet: RunStatus (response=%lf decode=%lf)",t1,t2);
  
  int isLive = fActions[kFileLive]->isOn();
  int secs = time(NULL) - rs->timeOfLastChange;
  char winlab[120];
  sprintf(winlab, "%s:  Run #%d  (%s for %d seconds)",
	  isLive ? "     Live" : "Reanalyse", rs->run, rs->status, secs);

  SetWindowName(winlab);

  delete rs;
  return 0;
}

// Gets the tags from the server
// returns true if the tabs changed
int JevpGui::updateServerTags()
{
  RtsTimer_root clock;
  clock.record_time();

  CP;
  // First check for a change to the serverTags...
  EvpMessage msg;
  msg.setCmd((char *)"getServerTags");
  msg.setSource((char *)"presenter");
  msg.setArgs((char *)"");
  jl_send(&msg);

  CP;
  TMessage *mess;
  int ret = jl_socket->Recv(mess);
  if(ret == 0) {  // disconnect
    LOG(ERR,"Server disconnected?\n");
    exit(0);
  }

  double t1 = clock.record_time();
  CP;
  if(strcmp(mess->GetClass()->GetName(), "EvpMessage") != 0) {
    LOG(ERR,"Didn't get a EvpMessage class\n");
    exit(0);
  }
  
  EvpMessage *response = (EvpMessage *)mess->ReadObject(mess->GetClass());
  
  CP;
  
  double t2 = clock.record_time();

  LOG(DBG, "Ethernet: getServerTags (response=%lf decode=%lf)",t1,t2);

  char *args = (char *)response->getArgs();
  

  LOG(DBG, "The server tags are: %s  (old are %s)",args,serverTags);
  CP;

  ret = 0;
  if(strcmp(serverTags, args) != 0) {
    CP;
    LOG(DBG, "Changing server tags from %s to %s",serverTags, args);
    free(serverTags);
    serverTags = (char *)malloc(strlen(args) + 1);
    strcpy(serverTags, args);
    ret = 1;
  }
  CP;

  delete mess;
  CP;
  delete response;
  CP;

  LOG(DBG, "updateservertags = %d",ret);
  return ret;
}

void JevpGui::UpdatePlots() 
{
  CP;
  RtsTimer_root clock;
  
  //CM;
  if(jl_socket == NULL) {
    LOG(ERR,"Updating plots but socket is NULL... returning...\n");
  }
  else {
    
    // Event loop via singleShot timer
    static int nupdates=0;
    if(nupdates % 100 == 0) {
      LOG(DBG,"%d updates",nupdates);
    }
    nupdates++;

    // Get run status and handle window name...
    updateRunStatus();
    
    if(updateServerTags() || !currentScreen) {  // Did the server tags change?  If so, update tab structure
      CP;
      clock.record_time();
      switchTabs(evpMain->display, serverTags);
      CP;
      double t1 = clock.record_time();
      LOG("JEFF", "Ethernet: switchTags:   (%lf)",t1);
    }
    else {  // if not, update visible plots and redraw...
      CP;
      clock.record_time();
 
      jl_DrawPlot(currentScreen);
  
      double t1 = clock.record_time();
      if(t1 > .25)
	LOG("JEFF", "Ethernet: Drawplots %s: (%lf)",currentScreen->plot->c_str(),t1);
    }
    
  }

//   if (fActions[kAutoUpdate]->isOn()) {
//     QTimer::singleShot(fGuiRefreshRate,this,SLOT(UpdatePlots()));
//   }
  CP;
}

void JevpGui::refreshTimerFired()
{ 
  //  LOG("JEFF", "Timer fired...");
  if(fActions[kAutoUpdate]->isOn()) {
    //LOG("JEFF", "calling update...");
    UpdatePlots();
    tabChanged(rootTab);
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

void JevpGui::pc_save() {
  jl_Save("");
}

void JevpGui::pc_saveAs() {
  // QString dir(EvpUtil::GetOutputPath());
  QString dir("");

  QString filter("*.root");
#if QT_VERSION < 0x40000
  QFileDialog dialog( dir, filter, this, "", true );
#else /* QT4 */
  Q3FileDialog dialog( dir, filter, this, "", true );
#endif /* QT4 */
  dialog.exec();
  if (!dialog.selectedFile().isEmpty()) {
    jl_Save( dialog.selectedFile().ascii() );
  }
}


void JevpGui::pc_live() {
  //cout << "liveButton" << endl;
  jl_Stop();   // will restart automatically
  //pc_mPresenter->jl_SetSource();
}

void JevpGui::pc_file() {
  //cout << "fileButton" << endl;
  //QString dir(EvpUtil::GetOutputPath());
  QString dir("/home/jml");

  QString caption("File dialog");
#if QT_VERSION < 0x40000
  QFileDialog dialog(dir, QString(), this,caption);
#else /* QT4 */
  Q3FileDialog dialog(dir, QString(), this,caption);
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
  emit pc_signalEventInfo(jl_run(),0,0,0,0,0,0,0);
  //  emit pc_signalServerInfo(pc_mPresenter->jl_serverStatus());
}


void JevpGui::pc_update(TCanvas* canvas, int tab, int subTab) {
  update();
  LOG(DBG,"In update? a \n");
  if (canvas) jl_Draw(canvas,pc_mTab,pc_mSubTab);
}

void JevpGui::pc_update(TCanvas* canvas, const char* name) {
  update();
  LOG(DBG,"In update? b\n");
  if (canvas) jl_Draw(canvas, name );
}

void JevpGui::pc_print() {
//   //cout << "print" << endl;
//   int tab = GetTabId();
//   int subTab = GetSubTabId();
//   TCanvas* cc = GetCanvas();
//  jl_Print(cc,tab,subTab);
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

    LOG(ERR,"Error Connecting (%dth of %d tries): %d\n", i, ntries, nsocket->GetErrorCode());
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
}

void JevpGui::jl_killServer() {
  EvpMessage msg;
  msg.setCmd((char *)"kill");
  jl_send(&msg);

  jl_socket->Close();
  jl_socket = NULL;
}

void JevpGui::readDisplayFromServer()
{
  if(!evpMain->server || evpMain->displayFile==1) {
    // Read Display from file...
    LOG(DBG, "reading display from file: %s",evpMain->displayFile);

    jl_displayFile = new DisplayFile();
    if(jl_displayFile->Read(evpMain->display) < 0) {
      LOG(ERR,"Error reading display file: %s\n",evpMain->display);
      exit(0);
    }
    
    //jl_displayFile->dump();
  }
  else {
    // Read display from server...
    
    LOG(DBG, "Reading display from server!");

    RtsTimer_root clock;
    clock.record_time();

    EvpMessage msg;
    msg.setCmd((char *)"display_desc");
    msg.setArgs(evpMain->display);
    jl_send(&msg);
    
    // get response...
    //printf("Waiting for tab data from server...\n");
    TMessage *mess;
    int ret = jl_socket->Recv(mess);
    if(ret == 0) {  // disconnect
      LOG(ERR,"Server disconnected?\n");
      exit(0);
    }

    double t1 = clock.record_time();

    //printf("Got something ret=%d mess=0x%x\n",ret,mess);
    long x = (int)mess->GetClass();

    LOG(DBG,"--->0x%x\n",x);// , x[0],x[1],x[2],x[3]);

    if(strcmp(mess->GetClass()->GetName(), "EvpMessage") != 0) {
      LOG(ERR,"Didn't get a DisplayDefSender class\n");
      exit(0);
    }

    CP;
    EvpMessage *tabdata = (EvpMessage *)mess->ReadObject(mess->GetClass());

    //   printf("....cmd was %s\n",tabdata->cmd);

    if(tabdata->args == NULL) {
      LOG(ERR,"No display '%s' found...\n", evpMain->display);
      exit(0);
    }

    double t2 = clock.record_time();


    CP;
    jl_displayFile = new DisplayFile();

    CP;
    jl_displayFile->ReadBuff(tabdata->args, strlen(tabdata->args));

    double t3 = clock.record_time();
    CP;
    
    jl_displayFile->setDisplay(evpMain->display);
    CP;
    
    double t4 = clock.record_time();
    LOG(DBG, "Ethernet: set display %s (%lf %lf %lf %lf)",
	evpMain->display, t1,t2,t3,t4);

    delete tabdata;
  }

}


void JevpGui::init()
{
  CP;
  jl_mLastDrawnCanvas = NULL;

  jl_screens = new TList();
  jl_socket = NULL;

  CP;
  if(jl_ConnectToServerPort(evpMain->serverport,5) < 0) return;
  CP;

  readDisplayFromServer();

  if(evpMain->display == NULL) {
    LOG(ERR,"Need to specify the display\n");
    exit(0);
  }

  SetDebugLevel(0);


  fMenuBar = NULL;
  mZoomer = NULL;
  fProgressBar = NULL;
  mCentralWidget = NULL;
  fToolBar = NULL;
  fTab = NULL;

  LOG("JEFF", "Presenter pid=%d",getpid());

  CP;
  setAttribute(Qt::WA_DeleteOnClose);
  setWindowModality(Qt::WindowModal);

  CP;
  PresenterSuspend blockWidgets;
  blockWidgets << this;

  CP;
  // Some preparations here
  // Here is starting directory name
  SetDefaults();

  CP;
  setUsesTextLabel(true); // use the text labels for the tool bar buttons
  

  blockWidgets << (mCentralWidget = new Q3HBox(this));
  mCentralWidget->setMargin(0);
  mCentralWidget->setSpacing(0); 
  setCentralWidget(mCentralWidget);

  //
  MakeActions();

  MakeMenuBar();


  ShowToolBar(true);
    
  //
  // Define Tabs
  //
  // main tab holding
  CP;
  rootTab = new QTabWidget(mCentralWidget);
  CP;

  //  jl_displayFile->getDisplay(0);

  jl_displayFile->setServerTags("");
  buildTabs(1);

  CP;
  LOG(DBG,"Done building tabs\n");

  // this is enough!
  //fActions[kAutoUpdate]->setOn(false);
  fActions[kAutoUpdate]->setOn(true);
 
  connect(qApp,SIGNAL(lastWindowClosed()),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit()));
  connect(qApp,SIGNAL(lastWindowClosed()),this, SLOT(jl_ClosePresenter()));
  resize(mWidth,mHight);

  SetWindowName("Live...");

  show();
  


  
  connect(this,SIGNAL(update()), this, SLOT(update()) ); 

  connect(this, SIGNAL( pc_signalEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ), this, SLOT( setEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ) ); 
  connect(this, SIGNAL( pc_signalServerInfo(ServerStatus*) ), this, SLOT( setServerInfo(ServerStatus*) ) ); 

  //  rootTab->setCurrentIndex(0);
  //  update();
  tabChanged(rootTab);

  pc_mCanvas = 0;
}


void JevpGui::jl_DrawPlot(JevpScreenWidget *screen) {
  CP;

  static int x;

  char tmp[256];
  tmp[0] = '\0';

  CP;
  screen->setUpdatesEnabled(false);   // keep doublebuffer in place...

  CP;
  double maxY = -9999;
  DisplayNode *hd = NULL;
  screen->Clear();
  TCanvas *gcc = screen->GetCanvas();
  CP;


  int wide=0;
  int deep=0;
  int scaley=0;
  int nplots=0;

  int combo_index = screen->combo_index;
  LOG(DBG,"drawplot....combo_index = %d x=%d\n",combo_index, x++);

  // I need to use the display def version to get a 
  // real object rather than a string...
  CP;
  DisplayNode *thetab = jl_displayFile->getTab(combo_index);
  CP;
  if(!thetab->leaf) {
    sprintf(tmp, "No histo for index=%d",combo_index);
    LOG(DBG, "Cross of death: %s",tmp);
    jl_CrossOfDeath(screen, tmp);
    CP;
    goto redraw;
  }

  hd = thetab;

  CP;
  nplots = thetab->nSiblings() + 1;

  // gcc->Clear();
  
  wide = thetab->getIntParentProperty("wide");
  if(wide <= 0) wide = 1;
  deep = thetab->getIntParentProperty("deep");
  if(deep <= 0) deep = 1;
  scaley = thetab->getIntParentProperty("scaley");
  if(scaley <= 0) scaley = 0;

  LOG(DBG, "wide = %d deep = %d scaley = %d nplots=%d", wide, deep, scaley, nplots);

  CP;



//   if(x % 2 == 0) {
//     jl_CrossOfDeath(screen, "Every other update!");
//     goto redraw;
//   }

  gcc->Divide(wide, deep);
    
  for(int i=0;i<nplots;i++) {   // First get plots!
    char tmp[256];
      
    JevpPlot *plot = jl_getPlotFromServer(hd->name,  tmp);
      
    if(plot) {
      LOG(DBG, "Got plot %s : %s",hd->name,plot->GetPlotName());
      screen->addPlot(plot);
      screen->addJevpPlot(plot);
      double my = plot->getMaxY();
      if(my > maxY) maxY = my;
    }
    else {
      LOG(DBG, "No plot for %s",hd->name);
    }
      
    hd = hd->next;   
  }
  CP;
    
  // Now plot them
  hd = thetab;
  CP;
  for(int i=0;i<wide*deep;i++) {
      
    gcc->cd(i+1);
      
    if(i<nplots) {
      JevpPlot *plot = screen->getJevpPlot(hd->name);
      if(plot == NULL) {
	CP;
	LOG(DBG, "Cross of death: %s",tmp);
	sprintf(tmp, "Server Doesn't Currently Have Plot %s",hd->name);
	gcc->cd(i+1);
	jl_CrossOfDeath(screen, tmp);     // Some error...
      }
      else {
	CP;
	if(scaley) {
	  plot->setMaxY(maxY * 1.2);
	}
	  
	CP;
	gcc->cd(i+1);

	if(!plot->needsdata || plot->isDataPresent()) {
	  plot->draw();
	}
	else {
	  char tmp[255];
	  sprintf(tmp,"No data for plot: %s",hd->name);
	  jl_NoDataPresent(screen, tmp);
	}
	
	//delete plot;
      }
      CP;
      //printf("drew %s\n",hd->name);
	
      hd = hd->next;    
      // sleep(3);
    }
    else {
      gcc->cd(i+1);
      jl_DrawEmptySpace(screen, (char *)" ");
      //jl_CrossOfDeath(screen, "blah...");
    }
  }
  CP;

 redraw:
  screen->setUpdatesEnabled(true);   // reenable the update
  gcc->Update();
  //screen->update();                  // and trigger!

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

  RtsTimer_root clock;
  clock.record_time();

  // Ask server for plot...
  EvpMessage msg;
  msg.setCmd("getplot");
  msg.setArgs(name);
  jl_send(&msg);

  // get response...
  LOG(DBG,"Waiting for plot from server...\n");
  TMessage *mess;

  int ret = jl_socket->Recv(mess);

  double t1 = clock.record_time();

  LOG(DBG, "size received = %d",ret);

  if(ret == 0) {  // disconnect
    LOG(ERR,"Server disconnected?\n");
    sprintf(error, "Can't get plot: %s  (no server)",name);

    return NULL;
  }

  LOG(DBG, "Message class: %s",mess->GetClass()->GetName());

  if(strcmp(mess->GetClass()->GetName(), "EvpMessage") == 0) {
    // There was no valid object...
    EvpMessage *msg = (EvpMessage *)mess->ReadObject(mess->GetClass());
    sprintf(error, "Can't get plot: (%s)", msg->args);
    LOG(DBG, "error?");

    delete msg;
    delete mess;
    return NULL;
  }

  if(strcmp(mess->GetClass()->GetName(), "JevpPlot") == 0) {
    JevpPlot *plot = (JevpPlot *)mess->ReadObject(mess->GetClass());

    double t2 = clock.record_time();
    if(t2 > .25)
      LOG("JEFF", "Ethernet: JevpPlot (%lf %lf)",t1,t2);

    delete mess;
    return plot;
  }

  LOG(ERR,"Invalid message type %s\n",mess->GetClass()->GetName());
  sprintf(error, "Invalid message type %s",mess->GetClass()->GetName());
  delete mess;
  return NULL;
}


void JevpGui::jl_NoDataPresent(JevpScreenWidget *screen, char *str) {
  //  TLine* a = new TLine(0.,0.,1.,1.);
  //  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,str);

  // This is how we free the memory...
  //a->SetBit(kCanDelete);
  //b->SetBit(kCanDelete);
  //t->SetBit(kCanDelete);
  // screen->addPlot(a);
  // screen->addPlot(b);
  screen->addPlot(t);

  // a->SetLineColor(2);
  // b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad...
  // a->Draw();
  // b->Draw();
  t->Draw();

  //gcc->Update();
  //cout << __PRETTY_FUNCTION__ << endl;
  return;
}

void JevpGui::jl_CrossOfDeath(JevpScreenWidget *screen, char *str) {

  TLine* a = new TLine(0.,0.,1.,1.);
  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,str);

  // This is how we free the memory...
  //a->SetBit(kCanDelete);
  //b->SetBit(kCanDelete);
  //t->SetBit(kCanDelete);
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

void JevpGui::jl_DrawEmptySpace(JevpScreenWidget *screen, char *str) {

  TText* t = new TText(0.5,0.5,str);

  // This is how we free the memory...
  //t->SetBit(kCanDelete);
  screen->addPlot(t);

  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad.
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


//______________________________________________________________________________ 
void JevpGui::jl_ClosePresenter() 
{
  LOG("JEFF", "Exitint....");
  // Qt [slot] to terminate the application
  gApplication->Terminate();
}

void JevpGui::closeEvent(QCloseEvent *e)
{
  LOG("JEFF", "Done with close");
  gApplication->Terminate();
}

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
  RtsTimer_root clock;
  clock.record_time();

  TMessage mess(kMESS_OBJECT);
  
  mess.WriteObject(msg);
  jl_socket->Send(mess);

  double t1=clock.record_time();
  //  LOG("JEFF", "Ethernet: send()   (%lf)",t1);

  return 0;
}


void JevpGui::jl_showDirectories()
{
  LOG(DBG,"gDirectories ls()-------->\n");
  gDirectory->ls();
  LOG(DBG,"screen objects ls()------>\n");
  
  TIter next(jl_screens);
  int i=0;
  JevpScreenWidget *widget;
  while((widget = (JevpScreenWidget *)next())) {
    printf("Widget: %d\n",i++);
    widget->GetCanvas()->ls();
  }
}
