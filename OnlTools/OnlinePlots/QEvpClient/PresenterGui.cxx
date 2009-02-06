///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include "PresenterGui.h"
#include "EvpUtil.h"
#include "ServerStatus.h"
#include "TQtRootSlot.h"
#include "TROOT.h"
#include "TEnv.h"

//#include "StRoot/StEEmcPool/muEztPanitkin/EEqaPresenter.h"
#include "EvpPresenter.h"
#include "TQtWidget.h"
#include "TQtZoomPadWidget.h"

#include "qapplication.h"
#include "qtabwidget.h"
#include "qvbox.h"
#include "qhbox.h"
#include "qmainwindow.h"
#include "qpopupmenu.h"
#include "qvbuttongroup.h"
#include "qpushbutton.h"
#include "qmenubar.h"
#include "qlayout.h"
#include "qtooltip.h"
#include "qiconset.h"
#include "qpixmap.h"
#include "qtoolbar.h"
#include "qlineedit.h"
#include "qlabel.h"
#include "qcursor.h"
#include "qprogressbar.h"
#include "qtimer.h"

#include "EventInfo.h"
#include "ServerInfo.h"
#include "TriggerDetectorBitsInfo.h"


char* mystrcat(const char* a, const char* b) {
  char* txt = new char[1024];
  sprintf(txt,"%s%s",a,b);
  return txt;
}

static TQtBrowserMenuItem_t gMenu_Data[] = {
  // { filename,      tooltip,            staydown,  id,              button}
/* File Menu */
  { "&Save",      kFileSave,     Qt::CTRL+Qt::Key_S, "Save histograms in root file",        mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/save.xpm")    },
  { "Save &As",   kFileSaveAs,   0,                  "Save histograms in root file as ... ",mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/hdisk_t.xpm") },
  { "&Print",     kFilePrint,    Qt::CTRL+Qt::Key_P, "Print the TCanvas image ",            mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/printer.xpm") },
  { "&Print All", kFilePrintAll, 0,                  "Print the TCanvas image ",            mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/printer.xpm") },
  { "onlprinter2",kOnlPrinter2 , 0,                  "Send last print to onlpinter2",       mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/printer.xpm") },
  { "&Reference", kReference,    Qt::CTRL+Qt::Key_R, "Open reference plots ... ",           mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/fileopen.xpm") },
  { "E&xit",      kFileExit,     Qt::CTRL+Qt::Key_X, "Exit the ROOT application",           mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/quit.xpm")    },

  { "About",      kHelpAbout,      0, "", ""},

  { "&Live",      kLive,         Qt::CTRL+Qt::Key_L, "Connect to current run ... ",          mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/connect.xpm")  },
  { "&File",      kFile,         Qt::CTRL+Qt::Key_F, "Open datafile or directory ... ",      mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/fileopen.xpm") },
  { "&Update",    kUpdate,       Qt::CTRL+Qt::Key_U, "Update current ",                      mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/update.xpm")   },
  { "&AutoUpdate",kAutoUpdate,   Qt::CTRL+Qt::Key_A, "Automatically update eventy 10 sec",   mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/update.xpm")   },  
  { "&bits",kBits,   Qt::CTRL+Qt::Key_B, "Show Trigger and Detector bits",                   mystrcat( gEnv->GetValue("Online.plotsDir","."),"/images/bits.xpm")   },
  { "View Toolbar",     kToolBar,        0, "show toolbar ",                            "" },

  {0,0,0,"",""}
};

//------------------------------------------------------------------------
PresenterGui::PresenterGui(bool isRefWindow) : 
  QMainWindow( 0, "example application main window", WDestructiveClose | WGroupLeader ), 
  mWidth(400), mHight(500), mStrLive(" Live  "), mStrFile(" File  "), mStrRun("Running"), mStrStop("Stopped")
  ,fGuiRefreshRate(10)
{
 
  // Some preparations here
  // Here is starting directory name
  SetDefaults();
  //setSizePolicy(QSizePolicy::Minimum);

  // Create test main frame. A QMainFrame is a top level window.

//  connect(qApp,SIGNAL(lastWindowClosed () ), qApp, SLOT(quit ()) );
  connect(qApp,SIGNAL(lastWindowClosed()),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit()));

  setUsesTextLabel(true); // use the text labels for the tool bar buttons
  
  mCentralWidget = new QHBox(this);
  mCentralWidget->setMargin(0);
  setCentralWidget(mCentralWidget);


  MakeActions();
  MakeMenuBar();

  if(isRefWindow) {
    ShowToolBar(false);
  }
  //
  // Mother Frame for canvas tabs is defined here
  //


  MakeConnectionFrame();


  // Create ZoomWidget 
  mZoomer = new TQtZoomPadWidget(0);
  mZoomer->SetZoomFactor(3);
  connect(this,SIGNAL(destroyed()), mZoomer, SLOT(close()));
  
  //
  // Define Tabs
  //
  // main tab holding
  fTab = new QTabWidget(mCentralWidget);
  fTab->setMargin(0);
  connect(fTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  // tab for dynamically defined groups histograms
  fDynamicTab = new QTabWidget(fTab);
  fDynamicTab->setMargin(0);
  connect(fDynamicTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  fTab->addTab(fDynamicTab,"Extra");
  // tab for statically defined histograms
  fStaticTab = new QTabWidget(fTab);
  fStaticTab->setMargin(0);
  connect(fStaticTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  fTab->addTab(fStaticTab,"Standard");
 
  // create only one TQTWidget
  // Loop over upper level Tabs
  for(int i=0;i<MAX_TABS;i++) {
    if((nSubTabs[i] != 0) && (nSubTabs[i]<=MAX_SUBTABS)) {
      QTabWidget *topTab = new QTabWidget(fStaticTab);
      topTab->setMargin(0);
      connect(topTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
      // Define upper level tab
      fStaticTab->addTab(topTab,TabNames[i][0]);
      // Add subTabs for current Tab
      // Note that we started with 1                         
      for(int j=1;j<=nSubTabs[i];j++) {
	TQtWidget* w = new TQtWidget(topTab);
        QToolTip::add(w,"<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
	topTab->addTab( w ,TabNames[i][j]);
	topTab->showPage(w);
	mZoomer->Connect(w);
      }
    } else {
	TQtWidget* w = new TQtWidget(fStaticTab);
        QToolTip::add(w,"<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
	fStaticTab->addTab( w ,TabNames[i][0]);
	fStaticTab->showPage(w);
	mZoomer->Connect(w);
    }
  }

 fTab->showPage(fStaticTab);

  if(isRefWindow)
    setCaption("Histogram Reference");
  else {
    setCaption("STAR Histogram Presenter");

    fActions[kLive]->setOn(true);
    emit live();
    // Adjust the GUI refresh rate msec.
    fGuiRefreshRate = gEnv->GetValue("Online.GuiRefreshRate",100);
    fActions[kAutoUpdate]->setOn(true);
  }
  
  
  //emit update(GetCanvas(), GetTabId(), GetSubTabId() );
  //QTabWidget* first = (QTabWidget*) fStaticTab->currentPage();
  //first->setCurrentPage(2);
  ///first->currentChanged( first->currentPage() );
  //emit currentChanged(first);
  

  QTimer::singleShot (0,this,SLOT(GetNextEvent()));
}


//----------------------------------------------------------------
PresenterGui::~PresenterGui()
{}

//----------------------------------------------------------------
void PresenterGui::SetDefaults()
{
  // Set printout level. 0 - silent, 1 - Verbose
  SetDebugLevel(0);
  //  strcpy(mDefaultPSFilePath,"/d/histoes/");
  //strcpy(mDefaultPSFilePath,"/home/panitkin/online/histoes/");
  //mDefaultPSFilePath = "/aa/online/histoes/";
  //    mDefaultPSFilePath = "/home_local/panitkin/online/histoes/";
  mDefaultPSFilePath = "/b/histos/";
  // Define GUI lay out.

  // Upper Level Tab Names
  TabNames[0][0]="Trigger";
  TabNames[1][0]="DAQ";
  TabNames[2][0]="L3";
  TabNames[3][0]="TPC";
  TabNames[4][0]="SVT";
  TabNames[5][0]="BEMC";
  TabNames[6][0]="FTPC";
  TabNames[7][0]="TOF";
  TabNames[8][0]="BBC";
  TabNames[9][0]="FPD";
  TabNames[10][0]="ETOW";
  TabNames[11][0]="ESMD";
  TabNames[12][0]="EEMC trig";
  TabNames[13][0]="SSD";
  TabNames[14][0]="ZDC SMD";
  TabNames[15][0]="PMD";
  //
  // Second Level Tab Names ( Differen for different subsystems)
  // Note that second index starts from 1. Index 0 is taken by
  // upper level Tab
  //

  // Trigger
  nSubTabs[0]=4;// Number of subTabs for this tab


  //New For 2005 from Misha Kopytine
  TabNames[0][1]="CTB slats and Killer Bits";// SubTab name
  TabNames[0][2]="CTB Algorithm";// SubTab name
  TabNames[0][3]="ZDC Vertex";// SubTab name
  TabNames[0][4]="ZDC Charge";// SubTab name
  //TabNames[0][5]="TriggerId";// Akio said it's defunct

  //TabNames[0][1]="Trigger Detectors 1";// SubTab name
  //  TabNames[0][3]="ZDC Timing 1";// SubTab name
  //TabNames[0][4]="ZDC Timing 2";// SubTab name
  //    TabNames[0][3]="ZDC Charge";// SubTab name
  //TabNames[0][4]="CTB Hits";// SubTab name
  //TabNames[0][6]="ZDC Charge";// SubTab name
  //TabNames[0][5]="BEMC Trigger";// From Alex Stolpovsky
  //    TabNames[0][5]="TriggerId";
  //TabNames[0][7]="Study";

  //DAQ
  nSubTabs[1]=4;// Number of subTabs for this tab
  TabNames[1][1]="Buffer Sizes";
  TabNames[1][2]="Buffer Fractions";
  TabNames[1][3]="Time Series";
  TabNames[1][4]="Bunch Crossing Counter";

  //L3
  nSubTabs[2]=3;
  TabNames[2][1]="L3 Vertex 1";
  TabNames[2][2]="L3 Vertex 2";
  TabNames[2][3]="L3 Tracks ";



  //TPC
  nSubTabs[3]=7;// Number of subTabs for this tab.Increment if you add subtabs
  TabNames[3][1]="TPC Global";
  TabNames[3][2]="Sectors 1-12";
  TabNames[3][3]="Sectors 13-24";
  //  TabNames[3][4]="Laser Trg";
  TabNames[3][4]="ChargeStep, West";
  TabNames[3][5]="ChargeStep, East";
  TabNames[3][6]="Drift Velocity";
  TabNames[3][7]="Charge Phi";

  //SVT
  nSubTabs[4]=5;
  //   TabNames[4][1]="SVT Total Occupancy";
  TabNames[4][1]="SVT West and East";
  TabNames[4][2]="SVT Receiver Occupancy East";
  TabNames[4][3]="SVT Mean Anode-Time bucket";
  TabNames[4][4]="SVT Receiver Occupancy West";
  TabNames[4][5]="SVT Anode vs Hybrid";

  //BEMC and BSMD
  nSubTabs[5]=15;
  TabNames[5][1]="Status";
  TabNames[5][2]="Towers";
  TabNames[5][3]="SMD/PSD";
  TabNames[5][4]="Trigger";
  TabNames[5][5]="Jet";
  TabNames[5][6]="BTOW ADC";
  TabNames[5][7]="JetPatch HighTower Spectra";
  TabNames[5][8]="JetPatch PatchSum Spectra";
  TabNames[5][9]="DSM Level-0 Input";
  TabNames[5][10]="DSM Level-1 Input";
  TabNames[5][11]="DSM Level-2 Input";
  TabNames[5][12]="SMD FEE Sum";
  TabNames[5][13]="Trigger corruption";
  TabNames[5][14]="PSD FEE Sum";
  TabNames[5][15]="BPRS ADC";
  //FTPC
  nSubTabs[6]=3;
  TabNames[6][1]="FTPC Global";
  TabNames[6][2]="FTPC Timebins";
  TabNames[6][3]="FTPC Charge";
  //TOF
  // Jing Liu comments the following:
  //nSubTabs[7]=2;
  //TabNames[7][1]="TOFp Global";
  //TabNames[7][2]="TOFp Test";
  // Jing Liu changes the following, for run8 and future runs, 02/25/2008:
  nSubTabs[7]=7;
  TabNames[7][1]="upvpd";
  TabNames[7][2]="TrayHitmap-west";  
  TabNames[7][3]="TrayHitmap-east";  
  TabNames[7][4]="ToT Tray01_30";
  TabNames[7][5]="ToT Tray31_60";
  TabNames[7][6]="ToT Tray61_90";
  TabNames[7][7]="ToT Tray91_120";

  //BBC
  nSubTabs[8]=6;
  TabNames[8][1]="BBC Hitmap";
  TabNames[8][2]="BBC Multiplicity";
  TabNames[8][3]="BBC ADCsum";
  TabNames[8][4]="BBC TDC East";
  TabNames[8][5]="BBC TDC West";
  TabNames[8][6]="BBC Vertex";

  //FPD
  nSubTabs[9]=6;
  TabNames[9][1]="East AdcSum";
  TabNames[9][2]="West AdcSum";
  TabNames[9][3]="East Hitmap";
  TabNames[9][4]="West Hitmap";
  TabNames[9][5]="East weighted Hitmap";
  TabNames[9][6]="West weighted Hitmap";


  //ETOW   <===== DAQ
  nSubTabs[10]=6;
  TabNames[10][1]="Jet Patch QA";
  TabNames[10][2]="Corrupt";
  TabNames[10][3]="Frequency";
  TabNames[10][4]="Crates";
  TabNames[10][5]="Hot Tw";
  TabNames[10][6]="Mult >thr";
    
  //ESMD  <===== DAQ
  nSubTabs[11]=12;
  TabNames[11][1]="corrupt";
  TabNames[11][2]="pmt freq";
  TabNames[11][3]="sect 12+1";
  TabNames[11][4]="sect 2+3";
  TabNames[11][5]="sect 4+5";
  TabNames[11][6]="sect 6+7";
  TabNames[11][7]="sect 8+9";
  TabNames[11][8]="sect 10+11";
  TabNames[11][9]="U freq";
  TabNames[11][10]="V freq";
  TabNames[11][11]="U mult>thr";
  TabNames[11][12]="V mult>thr";
    
  //ETOW   <===== trig
  nSubTabs[12]=11;
  TabNames[12][1]="DSM-0 Hank's";
  TabNames[12][2]="DSM-0 HT";
  TabNames[12][3]="DSM-0 TP";
  TabNames[12][4]="DSM-1 HT";
  TabNames[12][5]="DSM-1 TP";
  TabNames[12][6]="DSM 2+3 HT";
  TabNames[12][7]="DSM-2 JP";
  TabNames[12][8]="JP sum";
  TabNames[12][9]="JP freq";
  TabNames[12][10]="JP Adj";
  TabNames[12][11]="Et Tot";
    

  //SSD
  nSubTabs[13]=8;
  TabNames[13][1]="Event Size and Pulse";
  TabNames[13][2]="Mean Occupancy";
  TabNames[13][3]="Charge Matching";
  TabNames[13][4]="Ladder Occupancy 1";
  TabNames[13][5]="Ladder Occupancy 2";
  TabNames[13][6]="Ladder Occupancy 3";
  TabNames[13][7]="Ladder Occupancy 4";
  TabNames[13][8]="Ladder Occupancy 5";
  //ZDCSMD
  nSubTabs[14]=1;
  TabNames[14][1]="ZDC SMD";
  //PMD
  nSubTabs[15]=9;
  TabNames[15][1]="1-6";
  TabNames[15][2]="7-12";
  TabNames[15][3]="13-18";
  TabNames[15][4]="19-24";
  TabNames[15][5]="25-30";
  TabNames[15][6]="31-36";
  TabNames[15][7]="37-42";
  TabNames[15][8]="43-48";
  TabNames[15][9]="Chain vs Channel";
  
  
  // define embedded canvas
  // gEc = new TRootEmbeddedCanvas(lCanvasName, fF5, mHight,mHight);

  //fCleanUp->Add(gEc);

  //
  //Initialise database upload flag
  //
  file_uploaded = 0;//flag if pdf file was uploaded into database; yes=1, no=0, error=-1

}
//----------------------------------------------------------------
void PresenterGui::CloseWindow()
{
     qApp->closeAllWindows();
}
//----------------------------------------------------------------
void PresenterGui::DefineLayouts()
{  }
//______________________________________________________________________________
void PresenterGui::MakeActions() {
   int i=0;
   while (gMenu_Data[i].fMenuText!=NULL) {
      // skip the separators 
      TQtRootAction *action = new TQtRootAction(this,gMenu_Data[i]);
      fActions.insert(action->Id(),action);
      connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
      i++;
   }
}
//----------------------------------------------------------------
void PresenterGui::MakeMenuBar()
{  
   if (fMenuBar) { delete fMenuBar; fMenuBar = 0; }
   QMenuBar   *mainMenu = menuBar();
   fMenuBar = mainMenu;

   // File Menue
   QPopupMenu *fileMenu      = new QPopupMenu();
   mainMenu->insertItem("&File",fileMenu);
   fActions[kFileSave]->addTo(fileMenu); 
   fActions[kFileSaveAs]->addTo(fileMenu); 
   fActions[kFilePrint]->addTo(fileMenu); 
   fActions[kFilePrintAll]->addTo(fileMenu); 
   fActions[kOnlPrinter2]->addTo(fileMenu); 
   fActions[kReference]->addTo(fileMenu);
   fileMenu->insertSeparator();
   fActions[kFileExit]->addTo(fileMenu); 

   // Input Menue
   QPopupMenu *inputMenu      = new QPopupMenu();
   mainMenu->insertItem("&Input",inputMenu);
   fActions[kLive]->addTo(inputMenu); 
   fActions[kLive]->setToggleAction(true);
   fActions[kLive]->setOn(false);
   fActions[kFile]->addTo(inputMenu); 
   fActions[kUpdate]->addTo(inputMenu);
   fActions[kAutoUpdate]->addTo(inputMenu);
   fActions[kAutoUpdate]->setToggleAction(true);
   fActions[kAutoUpdate]->setOn(true);
   fActions[kBits]->setOn(false);
   fActions[kBits]->addTo(inputMenu);

   // Option Menue
   QPopupMenu *optionMenu      = new QPopupMenu();
   mainMenu->insertItem("&Option",optionMenu);
   fActions[kToolBar]->addTo(optionMenu);
   fActions[kToolBar]->setToggleAction(true);
   fActions[kToolBar]->setOn(true);

   mainMenu->insertSeparator();
   QPopupMenu *helpMenu   = new QPopupMenu();
   mainMenu->insertItem("&Help",helpMenu);
   helpMenu->insertSeparator();
   fActions[kHelpAbout]->addTo(helpMenu);

   // Option Menue

   // add text to actions icons
   fActions[kFile]->setText("Open");
   fActions[kFileSave]->setText("Save");
   fActions[kFileSaveAs]->setText("Save as");
   fActions[kFilePrint]->setText("Print");
   fActions[kFilePrintAll]->setText("Print all");
   fActions[kOnlPrinter2]->setText("onlPrinter2");
   //fActions[kReference]->setText("Reference");
   fActions[kFileExit]->setText("Exit");
   fActions[kLive]->setText("Live");
   fActions[kUpdate]->setText("Update");
   fActions[kAutoUpdate]->setText("AutoUpdate");
   fActions[kBits]->setText("bits");

   // create tool bar
   if (fToolBar) { delete fToolBar; fToolBar = 0;}
   fToolBar = new QToolBar(this);
   addDockWindow(fToolBar);
   // populate toolbar
   fActions[kFileExit]->addTo(fToolBar);
   fToolBar->addSeparator();
   fActions[kLive]->addTo(fToolBar); 
   fActions[kFile]->addTo(fToolBar); 
   fActions[kUpdate]->addTo(fToolBar);
   fActions[kAutoUpdate]->addTo(fToolBar);
   fToolBar->addSeparator();
   fActions[kBits]->addTo(fToolBar);
   fToolBar->addSeparator();
   fActions[kFileSave]->addTo(fToolBar); 
   fActions[kFileSaveAs]->addTo(fToolBar);
   fActions[kFilePrint]->addTo(fToolBar);
   fActions[kFilePrintAll]->addTo(fToolBar);
   fActions[kOnlPrinter2]->addTo(fToolBar);
}


//______________________________________________________________________________

//-------------------------------------------------------------------
void PresenterGui::MakeConnectionFrame()
{
  // Connection Frame
  //
  // Mother Connection frame defined here
  //

  QVBox *leftPane = new QVBox(centralWidget());
  leftPane->setMaximumSize(230,32767);
  mEventInfo = new EventInfo(leftPane);
  mServerInfo = new ServerInfo(leftPane);



  //fStarLogo = new  QPushButton(QIconSet(QPixmap("../images/starlogo_1.xpm")),"",leftPane);
  //connect(fStarLogo, SIGNAL(clicked()) ,this, SLOT(DoUpdateButton()) );
  // QToolTip::add(fStarLogo,"Experiment shutdown. Don't push this button!");
  fProgressBar = new QProgressBar(leftPane,"Progress");
  //fProgressBar->setMaximumSize(leftPane->width(),25);
  fProgressBar->setProgress(0,10);
  
  mBitsFrame = new QFrame(0,"Trigger/ Detector-Bits");
  mTriggerDetectorBitsInfo = new TriggerDetectorBitsInfo(mBitsFrame);
  mBitsFrame->adjustSize();
}

//-------------------------------------------------------------------
void PresenterGui::MakePrintFrame()
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
int PresenterGui::IntFromTString(const TString &ts)
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
float PresenterGui::FloatFromTString(const TString &ts)
{
  float t;
  std::string s(ts.Data());

  std::istringstream iss(s);
  iss >> t;
  return t;
}

//_____________________________________________________________________
int PresenterGui::ParseString (const TString &tChain, TObjArray &Opt)
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
void PresenterGui::DoLiveButton()
{
  // Handle Live button click.
  // Connect to mmap file
  if(mDebugLevel) {
    cout<<"Live Button Pressed  "<< this << endl;
   } 
  emit live();
  fActions[kLive]->setOn(true);
  emit update(GetCanvas(), GetTabId(), GetSubTabId() );

//mEvpClient->Stop();   // will restart automatically
  //mEvpClient->SetSource();
}


//-------------------------------------------------------------
void PresenterGui::DoFileButton()
{
  // Handle File button click.
  if(mDebugLevel) {
    cout<<"File Button Pressed"<<endl;
  }
  fActions[kLive]->setOn(false);
  emit file();
}
//---------------------------------------------------------------
void PresenterGui::DoAutoUpdateButton()
{
  if(mDebugLevel) {
    cout<<"AutoUpdate Button Pressed"<<endl;
  }
  GetNextEvent();
}
//---------------------------------------------------------------
void PresenterGui::DoBitsButton() {
    mBitsFrame->show();
    mBitsFrame->raise();
}
//-------------------------------------------------------------
void PresenterGui::DoUpdateButton()
{
  // Handle File button click.
  if(mDebugLevel) {
    cout<<"Update Button Pressed"<<endl;
  }
  emit nextEvent();
  updateRequest();
}
//---------------------------------------------------------------
void PresenterGui::DoPrintButton()
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
void  PresenterGui::ProcessMessage()
{
   TQtRootAction *actionSender =  (TQtRootAction *)sender ();
   switch (actionSender->Id()) {

   case kFileSave:         SaveCB();         break;
   case kFileSaveAs:       SaveAsCB();       break;
   case kFilePrint:        PrintCB();        break;
   case kFilePrintAll:     PrintAllCB();     break;
   case kLive:             DoLiveButton();   break;
   case kFile:             DoFileButton();   break;
   case kUpdate:           DoUpdateButton(); break;
   case kAutoUpdate:       DoAutoUpdateButton(); break;
   case kBits:             DoBitsButton(); break;
   case kToolBar:          ToolBarCB();      break;
   case kFileExit:         QuitCB();         break;
   case kHelpAbout:        AboutCB();        break;
   case kOnlPrinter2:      onlPrinter2();    break;
   case kReference:        OpenReference();  break;
 
   default:                                  break;

 };
}
//______________________________________________________________________________
void  PresenterGui::SaveCB()
{
  emit save();
}
//______________________________________________________________________________
void  PresenterGui::SaveAsCB()
{
  setEnabled(true);
  emit saveAs();
  setEnabled(true);
}
//______________________________________________________________________________
void  PresenterGui::PrintCB()
{
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
}
//______________________________________________________________________________
void  PresenterGui::PrintAllCB()
{
  sprintf(mPsName,"%s/run%s.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().ascii());
  emit printAll(mPsName);
}
//______________________________________________________________________________
void  PresenterGui::onlPrinter2() 
{
  if ( strcmp(mPsName,"") ) {    
    char cmd[1014];
    sprintf(cmd,"lp -d onlprinter2 %s &",mPsName);
    cout << "print command: " << cmd << endl;
    gSystem->Exec(cmd);
  }
}
//______________________________________________________________________________
void  PresenterGui::OpenReference()
{
  emit openReference();
}
//______________________________________________________________________________
void  PresenterGui::QuitCB()
{
   CloseWindow();
}
//______________________________________________________________________________
void  PresenterGui::AboutCB()
{
  //   new DGHelp("/home_local/panitkin/online/messages/about.message");
}
//______________________________________________________________________________
int PresenterGui::GetTabId() 
{ 
	return fStaticTab->currentPageIndex (); 
}
//______________________________________________________________________________
int PresenterGui::GetSubTabId() {
// remember that subTabs starts with 1	
  QWidget *currentWidget = fStaticTab->currentPage();
  QTabWidget *subTab = dynamic_cast<QTabWidget*>(currentWidget);
  if (subTab) 
     return subTab->currentPageIndex()+1;
  else 
    return 0;
} 
//______________________________________________________________________________
TCanvas* PresenterGui::GetCanvas() 
{ 
  QWidget *currentWidget = fStaticTab->currentPage();
  QTabWidget *subTab = dynamic_cast<QTabWidget*>(currentWidget);
  if (subTab) 
    return ((TQtWidget*)subTab->currentPage())->GetCanvas();
  else 
    return ((TQtWidget*)fStaticTab->currentPage())->GetCanvas();;
}

//______________________________________________________________________________
void  PresenterGui::SetWindowName(const char* displayText)
{ setCaption(displayText); }

//______________________________________________________________________________
void PresenterGui::tabChanged(QWidget* q) {
  mTab  = GetTabId();
  mSubTab =GetSubTabId();
  emit tab( mTab );
  emit subTab( mSubTab );
  emit canvas( GetCanvas() );
  updateRequest();
}
//______________________________________________________________________________
void PresenterGui::ShowToolBar(bool show)
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
void PresenterGui::ToolBarCB()
{
   ShowToolBar(fActions[kToolBar]->isOn());
}
//______________________________________________________________________________
void PresenterGui::setEventInfo(int run, int event, int count, int token,  unsigned int triggerBits,  unsigned int detectorBits, unsigned int triggerBitsRun , unsigned int detectorBitsRun ) {
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
void PresenterGui::updateRequest() {
  int pro = fProgressBar->progress();
  pro++;
  if (pro>10) pro=0;
  fProgressBar->setProgress(pro);

  if ( fTab->currentPage() == fStaticTab ) { 
    emit update(GetCanvas(), GetTabId(), GetSubTabId() );
    return;
  }  
  if ( fTab->currentPage() == fDynamicTab ) { 
    TQtWidget* widget = GetGroupWidget();
    if (widget) {
      emit update( widget->GetCanvas(), widget->name() );
      return;
    }
  }
  emit update(); 
}
//______________________________________________________________________________
void PresenterGui::GetNextEvent() 
{
   // Event loop via singleShot timer
   if (fActions[kAutoUpdate]->isOn()) {
      emit nextEvent();
      updateRequest();
      QTimer::singleShot (fGuiRefreshRate,this,SLOT(GetNextEvent()));
   }
}

//______________________________________________________________________________
void PresenterGui::setServerInfo(ServerStatus* ss) {
  mServerInfo->setRequestTime( ss->getRequestTime() );
  mServerInfo->setReceiveTime( ss->getReceiveTime() );
  mServerInfo->setRequestType( ss->getRequestType() );
  mServerInfo->setReceiveType( ss->getReceiveType() );
}
//______________________________________________________________________________
// adds a new QTabWidget to the dynamic tabs;
void PresenterGui::addGroupTab(const char* name) {
  //cout << " adding " << groupName << endl;
  QTabWidget* tab = new QTabWidget(fDynamicTab,name);
  tab->setMargin(0);
  connect(tab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  fDynamicTab->addTab(tab,name);
  fDynamicTab->showPage(tab);
  cout << __PRETTY_FUNCTION__ << " " << name <<endl;
}
//______________________________________________________________________________ 
// adds a new QWidget to that last QTabWidget in the dynamic tabs
void PresenterGui::addGroup(const char* name) {
//cout << " adding " << name << endl;
  int n = fDynamicTab->count();
 if ( n ==0 ) return ; // tab had no group jet
  QTabWidget* tab = dynamic_cast<QTabWidget*>(fDynamicTab->page(n-1));  // get the last group tab
  if  (!tab) return;
  TQtWidget* subTab = new TQtWidget(tab,name);
  tab->addTab( subTab ,name);
  tab->setEnabled(true);
  tab->showPage( subTab );
  mZoomer->Connect(subTab);
  cout << __PRETTY_FUNCTION__ << " " << name <<endl;
}
//______________________________________________________________________________ 
// adds a new QWidget to that last QTabWidget in the dynamic tabs
void PresenterGui::removeGroupTabs() {
  //cout << __PRETTY_FUNCTION__ << endl;
  setEnabled(false);
  QTabWidget* tab;
  while ( fDynamicTab->count() ) {
    tab = dynamic_cast<QTabWidget*>(fDynamicTab->page(0));
    if (!tab) {
      cout << __PRETTY_FUNCTION__ << " ### can remove non QTabWidget " << endl;
      setEnabled(true);
     return ;
    }
    while ( tab->count() ) {
      QWidget* subTab = tab->page(0);
      tab->removePage( subTab );
      delete subTab;
    }
    fDynamicTab->removePage(tab); 
    delete tab;
  }
  setEnabled(true);
}
//______________________________________________________________________________ 
TQtWidget* PresenterGui::GetGroupWidget() {
  QTabWidget* tab = dynamic_cast<QTabWidget*>(fDynamicTab->currentPage());
  if  (!tab) return 0;
  TQtWidget* sub = dynamic_cast<TQtWidget*>(tab->currentPage());
  return sub;
}
//______________________________________________________________________________ 
TCanvas* PresenterGui::GetGroupCanvas() {
  QTabWidget* tab = dynamic_cast<QTabWidget*>(fDynamicTab->currentPage());
  if  (!tab) return 0;
  TQtWidget* sub = dynamic_cast<TQtWidget*>(tab->currentPage());
  if ( !sub ) return 0;
  return sub->GetCanvas();
}

