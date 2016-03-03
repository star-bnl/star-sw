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

#  include <qfiledialog.h>

#include <QMainWindow>
#include <QMenu>
#include "qicon.h"
#include <QProgressBar>
#include <QToolBar>
#include <QCustomEvent>
#include <QFrame>
#include <stack>

#include "EventInfo.h"
#include "ServerInfo.h"
#include "TriggerDetectorBitsInfo.h"
#include "RunDialog.h"
#include "EvpMain.h"

JevpGui *gJevpGui;

#define LOG_DRAW_TIME 0.0
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



void JevpGui::fillTab(QTabWidget *tab, u_int idx)
{
    RtsTimer_root clock;


    LOG(DBG, "Filltab %d",idx);

    PresenterSuspend suspend;

    DisplayNode *mynode = jl_getTab(idx);
    // LOG("JEFF", "getTab(%d)=0x%x",idx, mynode);
    u_int child_idx = jl_getTabChildIdx(idx);
    if(!mynode) {
	LOG(DBG, "Didn't find mynode for %d",idx);
	return;
    }

    LOG(DBG, "Mynode is %s",mynode->name);

    DisplayNode *childnode = jl_getTab(child_idx);
    // LOG("JEFF", "getTab(%d)=0x%x",child_idx, childnode);

    if(!childnode) {
	LOG(DBG, "Didn't find child in %s for %d",mynode->name,child_idx);
	return;
    }

    if(childnode->leaf) {
	//LOG("JEFF","create widget: fillTab idx=%d, name=%s (canvas)\n",child_idx,childnode->name);    

	JevpScreenWidget *nwidget = new JevpScreenWidget(childnode->name, child_idx, tab);
	suspend << nwidget;
	suspend << tab;
	tab->addTab(nwidget, mynode->name);
	nwidget->init();
	jl_screens->Add((TObject *)nwidget);
	
	if(currentScreen == NULL) {   // Assume very first tab is the screen tab at first...
	    currentScreen = nwidget;
	}

	if(idx == 1) {
	    summaryScreen = nwidget;
	}

	//tab->addTab(new QLabel(mynode->name), mynode->name);

	//char lab[100];
	//sprintf(lab, "%s - not available",nwidget->name->c_str());

	double t3 = clock.record_time();

	//LOG(DBG, "fill leaf  %lf %lf %lf",t1,t2,t3);
    }
    else {
	clock.record_time();
	QTabWidget *nwidget = new QTabWidget();
	suspend << nwidget;

	double t1 = clock.record_time();
	// We want to know when the tabs are selected...

	//LOG("JEFF", "connect %p", nwidget);
	connect(nwidget, SIGNAL(currentChanged(int)), this, SLOT(tabChanged()));

	tab->addTab(nwidget, mynode->name);
	fillTab(nwidget, child_idx);

	double t2 = clock.record_time();

	//LOG(DBG, "Fill tab: (%s) %lf %lf",mynode->name,t1,t2);
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
	currentScreen = summaryScreen;
    }

    fillTab(rootTab, firstTab);

    LOG(DBG, "after fillTab currentScreen = 0x%x",currentScreen);

    // Only do this the first time...
    if(addSummary == 1) {
	//LOG("JEFF", "connect root tab %p", rootTab); 
	connect(rootTab, SIGNAL(currentChanged(int)), this, SLOT(tabChanged()));
    }
}

//------------------------------------------------------------------------
JevpGui::JevpGui() : QMainWindow(), mWidth(900), mHight(500), mStrLive(" Live  "), mStrFile(" File  "), mStrRun("Running"), mStrStop("Stopped"), fGuiRefreshRate(5000)
{
  gJevpGui = this;
  serverTags = (char *)malloc(10);
  strcpy(serverTags, "");

  refreshTimer = new QTimer(this);
  connect(refreshTimer, SIGNAL(timeout()), this, SLOT(refreshTimerFired()));
  refreshTimer->start(10000);
}


//----------------------------------------------------------------
JevpGui::~JevpGui()
{
    if(serverTags) free(serverTags);
}

//----------------------------------------------------------------
void JevpGui::SetDefaults()
{
    SetDebugLevel(0);
}
//----------------------------------------------------------------
void JevpGui::CloseWindow()
{
    qApp->closeAllWindows();
}
//----------------------------------------------------------------

QAction *JevpGui::createAction(const char *name, bool checkable, bool checked) {
    QAction *action = new QAction(name, this);
    action->setCheckable(checkable);
    if(checkable) {
	action->setChecked(checked);
    }
    //connect(action, SIGNAL(triggered()), this, SLOT(ProcessAction(action)));
    return action;
}

void JevpGui::MakeMenuBar()
{  
    //printf("MakeMenuBar update\n");

    QMenu *fileMenu = menuBar()->addMenu("&file");
    QMenu *inputMenu = menuBar()->addMenu("&Input");
    QMenu *optionMenu = menuBar()->addMenu("&Option");
    QMenu *helpMenu = menuBar()->addMenu("&Help");
    
    AutoUpdateAction = createAction("Auto Update", true);
    IgnoreServerTagsAction = createAction("Ignore Server Tags", true);

    QAction *UpdateAction = createAction("Update");
    QAction *PrintAction = createAction("Print");
    QAction *PrintAllAction = createAction("Print All");

    fileMenu->addAction(createAction("Change Histogram Set"));
    fileMenu->addAction(IgnoreServerTagsAction);
    fileMenu->addSeparator();
    fileMenu->addAction(PrintAction);
    fileMenu->addAction(PrintAllAction);

    inputMenu->addAction(UpdateAction);
    inputMenu->addAction(AutoUpdateAction);

    optionMenu->addAction(createAction("View Toolbar", true));
    helpMenu->addAction(createAction("About"));
    
    connect(menuBar(), SIGNAL(triggered(QAction *)), this, SLOT(ProcessAction(QAction *)));

    //    connect(fileMenu, SIGNAL(triggered(QAction *)), this, SLOT(ProcessAction(QAction *)));
    //connect(inputMenu, SIGNAL(triggered(QAction *)), this, SLOT(ProcessAction(QAction *)));
    //connect(optionMenu, SIGNAL(triggered(QAction *)), this, SLOT(ProcessAction(QAction *)));
    //connect(helpMenu, SIGNAL(triggered(QAction *)), this, SLOT(ProcessAction(QAction *)));

    // create tool bar
    ftoolbar = addToolBar(tr("Toolbar"));
    ftoolbar->addAction(UpdateAction);
    ftoolbar->addAction(AutoUpdateAction);
    ftoolbar->addSeparator();
    ftoolbar->addAction(PrintAction);
    ftoolbar->addAction(PrintAllAction);
    //    connect(ftoolbar, SIGNAL(actionTriggered(QAction *)), this, SLOT(ProcessAction(QAction *)));

    // // populate toolbar

    // fToolBar->addSeparator();
    // UpdateAction->addTo(fToolBar);
    // AutoUpdateAction->addTo(fToolBar);
    // fToolBar->addSeparator();
    // PrintAction->addTo(fToolBar);
    // PrintAllAction->addTo(fToolBar);
    // //fActions[kOnlPrinter2]->addTo(fToolBar);
    // fToolBar->hide();
}

//______________________________________________________________________________
void JevpGui::ProcessAction(QAction *action)
{
    //printf("ProcessAction: %s\n", action->text().toAscii().data());
    fflush(stdout);

    if(action->text() == "Change Histogram Set") {
	ChangeHistogramSet();
    }
    else if(action->text() == "Ignore Server Tags") {
	IgnoreServerTags();
    }
    else if(action->text() == "Print") {
	PrintCB();
    }
    else if(action->text() == "Print All") {
	PrintAllCB();
    }
    else if(action->text() == "Update") {
	UpdatePlots();
    }
    else if(action->text() == "Auto Update") {
	UpdatePlots();
    }
    else if(action->text() == "View Toolbar") {
	ToolBarCB();
    }
    else if(action->text() == "About") {
	AboutCB();
    }
    else {
	LOG(ERR, "Invalid action: %s", action->text().toAscii().data());
    }
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
    if (IgnoreServerTagsAction->isOn()) {
	//IgnoreServerTagsAction->setOn(false);
	LOG(DBG, "Ignore servertags 1");
	jl_displayFile->ignoreServerTags = 1;
    }
    else {
	//IgnoreServerTagsAction->setOn(true);
	jl_displayFile->ignoreServerTags = 0;
	LOG(DBG, "Ignore servertags off");
    }
    
    LOG("JEFF", "Ignore server tags: %d", jl_displayFile->ignoreServerTags);

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

    //fActions[kFileLive]->setOn(0);
    //fActions[kFileLive]->setEnabled(1);



    static char windowname[100];
    sprintf(windowname, "Reanalysing Existing Run: %s",name);
    SetWindowName(windowname);
  
    LOG(DBG,"run name=%s\n",name);

    jl_LaunchRun(name);


    delete name;
    
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
void  JevpGui::SetWindowName(const char* displayText)
{ 
  setCaption(displayText); 
}

void JevpGui::updateScreen(JevpScreenWidget *screen) {
    LOG("JEFF","Screen changed to %s",screen->name->c_str());
    RtsTimer_root clock;

    clock.record_time();
 
    DisplayNode *displayTab = screen->getDisplayTab(jl_displayFile);
    screen->DownloadAllPlotsFromServer(jl_socket, displayTab);
    double t1 = clock.record_time();
    screen->DrawOnScreen(displayTab);	  
    double t2 = clock.record_time();

    if(t1 > LOG_DRAW_TIME)
	LOG("JEFF", "Draw time %s: (%lf eth, %lf draw, %lf total)\n",screen->name->c_str(),t1,t2, t1+t2);


    CP;
    currentScreen = screen;
    CP;
    return;
}

void JevpGui::tabChanged() {
    QTabWidget *tab = (QTabWidget *)QObject::sender();
    tabChanged(tab);
}


//______________________________________________________________________________
void JevpGui::tabChanged(QTabWidget *tab) {
    QTabWidget *child;
    QWidget *curr = tab->currentWidget();
    while((child = dynamic_cast<QTabWidget *>(curr))) {
	curr = child->currentWidget();
    }
    
    JevpScreenWidget *screen = dynamic_cast<JevpScreenWidget *>(curr);
    if(!screen) {
	LOG(ERR, "Changed to tab, but not a JevpScreenWidget!");
	return;
    }
    
    updateScreen(screen);
}

//______________________________________________________________________________
void JevpGui::ShowToolBar(bool show)
{
    ftoolbar->show();
   // // Show or hide toolbar.
   // if (show) {
   //     if (!fActions[kToolBar]->isOn()) 
   // 	   fActions[kToolBar]->setOn(true);
   //     fToolBar->show();
   // } else {
   //     if (fActions[kToolBar]->isOn()) 
   // 	   fActions[kToolBar]->setOn(false);
   //     fToolBar->hide();
   // }
}
//______________________________________________________________________________
void JevpGui::ToolBarCB()
{
    //ShowToolBar(fActions[kToolBar]->isOn());
    ftoolbar->show();
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
  
  //double t1=clock.record_time();
  CP;
  if(strcmp(mess->GetClass()->GetName(), "RunStatus") != 0) {
    LOG(ERR,"Didn't get a RunStatus class: got %s\n",mess->GetClass()->GetName());
    exit(0);
  }
 
  CP;
  RunStatus *rs = (RunStatus *)mess->ReadObject(mess->GetClass());
  delete mess;
  //double t2=clock.record_time();

  //LOG("JEFF", "Ethernet: RunStatus (response=%lf decode=%lf)",t1,t2);
  
  //int isLive = fActions[kFileLive]->isOn();
  //isLive = true;
  int secs = time(NULL) - rs->timeOfLastChange;
  char winlab[120];
  sprintf(winlab, "%s:  Run #%d  (%s for %d seconds)","Live",rs->run, rs->status, secs);

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
	    updateScreen(currentScreen);
	}
    
    }

    //   if (fActions[kAutoUpdate]->isOn()) {
    //     QTimer::singleShot(fGuiRefreshRate,this,SLOT(UpdatePlots()));
    //   }
    CP;
}

void JevpGui::refreshTimerFired()
{ 
    if(AutoUpdateAction->isOn()) {
       	LOG("JEFF", "Auto Update Starting...");
	UpdatePlots();
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
    // QString dir("");
    // QString filter("*.root");

    QFileDialog dialog(this, tr("caption"), tr(""), tr("*.root"));

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
  QFileDialog dialog(this, tr("caption"), tr(""), tr("*.root"));
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

    cerr << "### error ### Can not open file : " << mapFile.toStdString() << endl;
    return;
  }
}



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
  return 0;
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

    mZoomer = NULL;
    fProgressBar = NULL;
    mCentralWidget = NULL;
    ftoolbar = NULL;
    //fTab = NULL;

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
    //setUsesTextLabel(true); // use the text labels for the tool bar buttons
  

    CP;

    mCentralWidget = new QWidget(this);
    blockWidgets << mCentralWidget;
    QVBoxLayout *layout = new QVBoxLayout(mCentralWidget);
    CP;
    //mCentralWidget
    layout->setMargin(0);
    CP;
    //mCentralWidget->setSpacing(0); 
    layout->setSpacing(0);
    CP;
    setCentralWidget(mCentralWidget);

    CP;
    //
    //MakeActions();

    CP;
    MakeMenuBar();
    layout->addWidget(ftoolbar);
    CP;
    ShowToolBar(true);
    
    //
    // Define Tabs
    //
    // main tab holding
    CP;
    rootTab = new QTabWidget(mCentralWidget);
    layout->addWidget(rootTab);
    CP;

    //  jl_displayFile->getDisplay(0);

    jl_displayFile->setServerTags("");
    buildTabs(1);

    CP;
    LOG(DBG,"Done building tabs\n");

    // this is enough!
    //fActions[kAutoUpdate]->setOn(false);
    //fActions[kAutoUpdate]->setOn(true);
    CP;
  
    connect(qApp,SIGNAL(lastWindowClosed()),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit()));
    connect(qApp,SIGNAL(lastWindowClosed()),this, SLOT(jl_ClosePresenter()));

    CP;
    resize(mWidth,mHight);

    CP;
    SetWindowName("Live...");
    CP;
    show();
    CP;


  
    connect(this,SIGNAL(update()), this, SLOT(update()) ); 
    CP;
    connect(this, SIGNAL( pc_signalEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ), this, SLOT( setEventInfo(int,int,int,int, unsigned int, unsigned int,unsigned int, unsigned int) ) ); 
    CP;
    connect(this, SIGNAL( pc_signalServerInfo(ServerStatus*) ), this, SLOT( setServerInfo(ServerStatus*) ) ); 
    CP;
    //  rootTab->setCurrentIndex(0);
    //  update();


    ///xxxxx
    //tabChanged(rootTab);
    tabChanged(rootTab);


    CP;
    pc_mCanvas = 0;
    CP;
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

    //double t1=clock.record_time();
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
