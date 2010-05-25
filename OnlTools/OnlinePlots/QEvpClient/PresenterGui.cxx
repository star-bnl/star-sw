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

#include <QApplication>
#include <QtGui/QTabWidget>
#include <QtGui/QPushButton>
#include <QtGui/QMenuBar>
#include <QtGui/QLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QToolTip>
#include <QtGui/QPixmap>
#include <QtGui/QLineEdit>
#include <QtGui/QLabel>
#include <QtGui/QCursor>
#include <QTimer>

#  include <QtGui/QMenu>
#  include <QtGui/QIcon>
#  include <QtGui/QProgressBar>
#  include <QtGui/QToolBar>
#  include <QCustomEvent>
#  include <QtGui/QFrame>
#  include <stack>

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
  { "&Save",      kFileSave,     Qt::CTRL+Qt::Key_S, "Save histograms in root file",        ":/save.xpm"    },
  { "Save &As",   kFileSaveAs,   0,                  "Save histograms in root file as ... ",":/hdisk_t.xpm" },
  { "&Print",     kFilePrint,    Qt::CTRL+Qt::Key_P, "Print the TCanvas image ",            ":/printer.xpm" },
  { "&Print All", kFilePrintAll, 0,                  "Print the TCanvas image ",            ":/printer.xpm" },
  { "onlprinter2",kOnlPrinter2 , 0,                  "Send last print to onlpinter2",       ":/printer.xpm" },
  { "&Reference", kReference,    Qt::CTRL+Qt::Key_R, "Open reference plots ... ",           ":/fileopen.xpm"},
  { "E&xit",      kFileExit,     Qt::CTRL+Qt::Key_X, "Exit the ROOT application",           ":/quit.xpm"    },
  { "About",      kHelpAbout,      0, "", ""},

  { "&Live",      kLive,         Qt::CTRL+Qt::Key_L, "Connect to current run ... ",          ":/connect.xpm" },
  { "&File",      kFile,         Qt::CTRL+Qt::Key_F, "Open datafile or directory ... ",      ":/fileopen.xpm"},
  { "&Update",    kUpdate,       Qt::CTRL+Qt::Key_U, "Update current ",                      ":/update.xpm"  },
  { "&AutoUpdate",kAutoUpdate,   Qt::CTRL+Qt::Key_A, "Automatically update eventy 10 sec",   ":/update.xpm"  },
  { "&bits",kBits,   Qt::CTRL+Qt::Key_B, "Show Trigger and Detector bits",                   ":/bits.xpm"    },
  { "View Toolbar",     kToolBar,        0, "show toolbar ",                            "" },

  {0,0,0,"",""}
};
class  PresenterSuspend {
   // class to suspend signal emitting to complete initialization
   private:
         std::stack<QObject *> fWidgets;
   public:
        PresenterSuspend(){}
       ~PresenterSuspend(){
#if 1          
          while (!fWidgets.empty()) {
             fWidgets.top()->blockSignals(false);
             fWidgets.pop();
          }
#endif
        }
       void operator<<(QObject *o){
#if 1
          o->blockSignals(true); fWidgets.push(o);
#endif
       }
   };
//------------------------------------------------------------------------
PresenterGui::PresenterGui(bool isRefWindow) : 

  QMainWindow(),
  mWidth(400), mHight(500), mStrLive(" Live  "), mStrFile(" File  "), mStrRun("Running"), mStrStop("Stopped")
  ,fGuiRefreshRate(10)
{
   setAttribute(Qt::WA_DeleteOnClose);
   setWindowModality(Qt::WindowModal);
   PresenterSuspend blockWidgets;
   blockWidgets << this;

  // Some preparations here
  // Here is starting directory name
  SetDefaults();
  //setSizePolicy(QSizePolicy::Minimum);

  // Create test main frame. A QMainFrame is a top level window.

//  connect(qApp,SIGNAL(lastWindowClosed () ), qApp, SLOT(quit ()) );

  setToolButtonStyle(Qt::ToolButtonTextUnderIcon); // use the text labels for the tool bar buttons
  
  blockWidgets << (mCentralWidget = new QWidget(this) );
  QHBoxLayout *layout = new  QHBoxLayout (mCentralWidget );
  layout->setMargin(0);
  layout->setSpacing(0); 
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

  //
  // Define Tabs
  //
  // main tab holding
  fTab = new QTabWidget(mCentralWidget);
  layout->addWidget(fTab);
  

  blockWidgets << fTab;

  fTab->setMargin(0);
  // tab for dynamically defined groups histograms
  fDynamicTab = new QTabWidget(fTab);

  blockWidgets << fDynamicTab;

  fDynamicTab->setMargin(0);
  fTab->addTab(fDynamicTab,"Extra");
  // tab for statically defined histograms
  fStaticTab = new QTabWidget(fTab);

  blockWidgets << fStaticTab;

  fStaticTab->setMargin(0);
  fTab->addTab(fStaticTab,"Standard");

  // create only one TQTWidget
  // Loop over upper level Tabs
  for(int i=0;i<MAX_TABS;i++) {
    if((nSubTabs[i] != 0) && (nSubTabs[i]<=MAX_SUBTABS)) {
      QTabWidget *topTab = new QTabWidget(fStaticTab);

      blockWidgets << topTab;

      topTab->setMargin(0);
      // Define upper level tab
      fStaticTab->addTab(topTab,TabNames[i][0]);
      // Add subTabs for current Tab
      // Note that we started with 1                         
      for(int j=1;j<=nSubTabs[i];j++) {
         TQtWidget* w = new TQtWidget(topTab);

         blockWidgets << w;

        w->setToolTip("<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
        topTab->addTab( w ,TabNames[i][j]);
        topTab->showPage(w);
        mZoomer->Connect(w);
        connect(topTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );
      }
    } else {
      TQtWidget* w = new TQtWidget(fStaticTab);

      blockWidgets << w;

      w->setToolTip("<P>Click over any TPad with the <b>middle</b> mouse button to <b>zoom</b>");
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

    // Adjust the GUI refresh rate msec.
    fGuiRefreshRate = gEnv->GetValue("Online.GuiRefreshRate",100);
    fActions[kAutoUpdate]->blockSignals(true);
    fActions[kAutoUpdate]->setOn(true);
    fActions[kAutoUpdate]->blockSignals(false);
  }

  connect(fTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  connect(fDynamicTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  connect(fStaticTab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
  connect(qApp,SIGNAL(lastWindowClosed()),TQtRootSlot::CintSlot(),SLOT(TerminateAndQuit()));
  connect(this,SIGNAL(destroyed()), mZoomer, SLOT(close()));

  //emit update(GetCanvas(), GetTabId(), GetSubTabId() );
  //QTabWidget* first = (QTabWidget*) fStaticTab->currentPage();
  //first->setCurrentPage(2);
  ///first->currentChanged( first->currentPage() );
  //emit currentChanged(first);

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
  //TabNames[4][0]="SVT";
  TabNames[4][0]="VPD";
  TabNames[5][0]="BEMC Expert";
  TabNames[6][0]="BEMC Shift";
  TabNames[7][0]="FTPC";
  TabNames[8][0]="TOF";
  TabNames[9][0]="BBC";
  TabNames[10][0]="FPD";
  TabNames[11][0]="ETOW";
  TabNames[12][0]="ESMD";
  TabNames[13][0]="EEMC trig";
  TabNames[14][0]="EEMC Shift";
  TabNames[15][0]="MTD";
  TabNames[16][0]="ZDC SMD";
  TabNames[17][0]="PMD";
  //
  // Second Level Tab Names ( Differen for different subsystems)
  // Note that second index starts from 1. Index 0 is taken by
  // upper level Tab
  //

  // Trigger
  nSubTabs[0]=4;// Number of subTabs for this tab
  TabNames[0][1]="ZDC";    // SubTab name
  TabNames[0][2]="ZDC Seg";// SubTab name
  TabNames[0][3]="ZDC Sums";
  TabNames[0][4]="Bunch Crossing Counter";

  //New For 2005 from Misha Kopytine
  //TabNames[0][1]="CTB slats and Killer Bits";// SubTab name
  //TabNames[0][2]="CTB Algorithm";// SubTab name
  //TabNames[0][3]="ZDC Vertex";// SubTab name
  //TabNames[0][4]="ZDC Charge";// SubTab name
  //
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
  nSubTabs[1]=3;// Number of subTabs for this tab
  TabNames[1][1]="Buffer Sizes";
  TabNames[1][2]="Buffer Fractions";
  TabNames[1][3]="Time Series";

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
  //nSubTabs[4]=5;
  nSubTabs[4]=2;
  //   TabNames[4][1]="SVT Total Occupancy";
  TabNames[4][1]="VPD Lo";
  TabNames[4][2]="VPD Hi";
  //TabNames[4][1]="SVT West and East";
  //TabNames[4][2]="SVT Receiver Occupancy East";
  //TabNames[4][3]="SVT Mean Anode-Time bucket";
  //TabNames[4][4]="SVT Receiver Occupancy West";
  //TabNames[4][5]="SVT Anode vs Hybrid";

  //BEMC and BSMD
  nSubTabs[5]=10;
  TabNames[5][1]="Status";
  TabNames[5][2]="Towers";
  TabNames[5][3]="SMD/PSD";
  TabNames[5][4]="Trigger";
  TabNames[5][5]="Jet";
  TabNames[5][6]="BTOW ADC";
  TabNames[5][7]="JetPatch HighTower Spectra";
  TabNames[5][8]="JetPatch PatchSum Spectra";
  // TabNames[5][9]="DSM Level-0 Input";
  TabNames[5][9]="DSM Level-1 Input";
  TabNames[5][10]="DSM Level-2 Input";
  //  TabNames[5][11]="SMD FEE Sum";
  // TabNames[5][12]="Trigger corruption";
  // TabNames[5][13]="PSD FEE Sum";
  // TabNames[5][14]="BPRS ADC";
  
  //BEMC Shift
  nSubTabs[6]=7;
  TabNames[6][1]="Tower ADC";
  TabNames[6][2]="ADC Eta Vs Phi";
  TabNames[6][3]="SMD FEE Sum";
  TabNames[6][4]="PSD FEE SUM";
  TabNames[6][5]="BPRS ADC";
  TabNames[6][6]="HT/TP FEE Out";
  TabNames[6][7]="EMU Vs FEE Out";
  

  //FTPC
  nSubTabs[7]=3;
  TabNames[7][1]="FTPC Global";
  TabNames[7][2]="FTPC Timebins";
  TabNames[7][3]="FTPC Charge";
  //TOF
  // Jing Liu comments the following:
  //nSubTabs[7]=2;
  //TabNames[7][1]="TOFp Global";
  //TabNames[7][2]="TOFp Test";
  // Jing Liu changes the following, for run8 and future runs, 02/25/2008:
  nSubTabs[8]=11;
  TabNames[8][1]="upvpd";
  TabNames[8][2]="Error check";  
  TabNames[8][3]="TrayHitmap 01-30";  
  TabNames[8][4]="TrayHitmap 31-60";  
  TabNames[8][5]="TrayHitmap 61-90";  
  TabNames[8][6]="TrayHitmap 91-120";  
  TabNames[8][7]="L1 Multiplicity";  
  TabNames[8][8]="Tray 01-30 L0";  
  TabNames[8][9]="Tray 31-60 L0";  
  TabNames[8][10]="Tray 61-90 L0";  
  TabNames[8][11]="Tray 91-120 L0";  

  //BBC
  nSubTabs[9]=6;
  TabNames[9][1]="BBC Hitmap";
  TabNames[9][2]="BBC Multiplicity";
  TabNames[9][3]="BBC ADCsum";
  TabNames[9][4]="BBC TDC East";
  TabNames[9][5]="BBC TDC West";
  TabNames[9][6]="BBC Vertex";

  //FPD
  nSubTabs[10]=6;
  TabNames[10][1]="East AdcSum";
  TabNames[10][2]="West AdcSum";
  TabNames[10][3]="East Hitmap";
  TabNames[10][4]="West Hitmap";
  TabNames[10][5]="East weighted Hitmap";
  TabNames[10][6]="West weighted Hitmap";


  //ETOW   <===== DAQ
  nSubTabs[11]=4;
  TabNames[11][1]="Jet Patch QA";
  TabNames[11][2]="Corrupt";
  //TabNames[11][3]="Frequency";
  //TabNames[11][4]="Crates";
  TabNames[11][3]="Hot Tw";
  TabNames[11][4]="Mult >thr";
    
  //ESMD  <===== DAQ
  nSubTabs[12]=6;
  TabNames[12][1]="corrupt";
  TabNames[12][2]="pmt freq";
  //  TabNames[12][3]="sect 12+1";
  // TabNames[12][4]="sect 2+3";
  //TabNames[12][5]="sect 4+5";
  //TabNames[12][6]="sect 6+7";
  //TabNames[12][7]="sect 8+9";
  // TabNames[12][8]="sect 10+11";
  TabNames[12][3]="U freq";
  TabNames[12][4]="V freq";
  TabNames[12][5]="U mult>thr";
  TabNames[12][6]="V mult>thr";
   
  //ETOW   <===== trig
  nSubTabs[13]=10;
  // TabNames[13][1]="DSM-0 Hank's";
  TabNames[13][1]="DSM-0 HT";
  TabNames[13][2]="DSM-0 TP";
  TabNames[13][3]="DSM-1 HT";
  TabNames[13][4]="DSM-1 TP";
  TabNames[13][5]="DSM 2+3 HT";
  TabNames[13][6]="DSM-2 JP";
  TabNames[13][7]="JP sum";
  TabNames[13][8]="JP freq";
  TabNames[13][9]="JP Adj";
  TabNames[13][10]="Et Tot";
    
  //EEMC SHIFT
  nSubTabs[14]=10;
  TabNames[14][1]="Tower ADC";
  TabNames[14][2]="ADC Eta Vs Phi";
  TabNames[14][3]="SMD-ADC 12S1-1P1";
  TabNames[14][4]="SMD-ADC 2S1-3P1";
  TabNames[14][5]="SMD-ADC 4S1-5P1";
  TabNames[14][6]="SMD-ADC 6S1-7P1";
  TabNames[14][7]="SMD-ADC 8S1-9P1";
  TabNames[14][8]="SMD-ADC 10S1-11P1";
  TabNames[14][9]="HT/TP FEE Out";
  TabNames[14][10]="EMU Vs FEE Out";
  

  //MTD 
  nSubTabs[15]=2;
  TabNames[15][1]="MTD hits";
  TabNames[15][2]="MTD trigger info."; 
  //SSD
  //nSubTabs[15]=8;
  //TabNames[15][1]="Event Size and Pulse";
  //TabNames[15][2]="Mean Occupancy";
  //TabNames[15][3]="Charge Matching";
  //TabNames[15][4]="Ladder Occupancy 1";
  //TabNames[15][5]="Ladder Occupancy 2";
  //TabNames[15][6]="Ladder Occupancy 3";
  //TabNames[15][7]="Ladder Occupancy 4";
  //TabNames[15][8]="Ladder Occupancy 5";

  //ZDCSMD
  nSubTabs[16]=1;
  TabNames[16][1]="ZDC SMD";
  //PMD
  nSubTabs[17]=9;
  TabNames[17][1]="1-6";
  TabNames[17][2]="7-12";
  TabNames[17][3]="13-18";
  TabNames[17][4]="19-24";
  TabNames[17][5]="25-30";
  TabNames[17][6]="31-36";
  TabNames[17][7]="37-42";
  TabNames[17][8]="43-48";
  TabNames[17][9]="Chain vs Channel";
  
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
//     gROOT->ProcessLine(".q");
     
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
      fActions[action->Id()] = action;
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
   QMenu *fileMenu      = fMenuBar->addMenu("&File");
   
   fileMenu->addAction( fActions[kFileSave]);
   fileMenu->addAction( fActions[kFileSaveAs]);
   fileMenu->addAction(fActions[kFilePrint]);
   fileMenu->addAction(fActions[kFilePrintAll]);
   fileMenu->addAction(fActions[kOnlPrinter2]);
   fileMenu->addAction(fActions[kReference]);
   fileMenu->                                 insertSeparator();
   fileMenu->addAction(fActions[kFileExit]);

   // Input Menue
   QMenu *inputMenu     = fMenuBar->addMenu("&Input");

   inputMenu->addAction(fActions[kLive]);
      fActions[kLive]->setCheckable(true);
      fActions[kLive]->setChecked(true);
   inputMenu->addAction(fActions[kFile]);
   inputMenu->addAction(fActions[kUpdate]);
   inputMenu->addAction(fActions[kAutoUpdate]);
      fActions[kAutoUpdate]->setCheckable(true);
      fActions[kAutoUpdate]->setChecked(true);
   inputMenu->addAction(fActions[kBits]);
      fActions[kBits]->setCheckable(false);
       
   // Option Menue
   QMenu *optionMenu     = fMenuBar->addMenu("&Option");

   optionMenu->addAction(fActions[kToolBar]);
      fActions[kToolBar]->setCheckable(true);
      fActions[kToolBar]->setChecked(true);
      
   mainMenu->insertSeparator();
   QMenu *helpMenu  = fMenuBar->addMenu("&Help");
   helpMenu->insertSeparator();
   helpMenu->addAction(fActions[kHelpAbout]);

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
   addToolBar(fToolBar);

   // populate toolbar
   fToolBar->addAction(fActions[kFileExit]);
   fToolBar->addSeparator();
   fToolBar->addAction(fActions[kLive]);
   fToolBar->addAction(fActions[kFile]);
   fToolBar->addAction(fActions[kUpdate]);
   fToolBar->addAction(fActions[kAutoUpdate]);
   fToolBar->addSeparator();
   fToolBar->addAction(fActions[kBits]);
   fToolBar->addSeparator();
   fToolBar->addAction(fActions[kFileSave]);
   fToolBar->addAction(fActions[kFileSaveAs]);
   fToolBar->addAction(fActions[kFilePrint]);
   fToolBar->addAction(fActions[kFilePrintAll]);
   fToolBar->addAction(fActions[kOnlPrinter2]);
}


//______________________________________________________________________________

//-------------------------------------------------------------------
void PresenterGui::MakeConnectionFrame()
{
  // Connection Frame
  //
  // Mother Connection frame defined here
  //
  
  QBoxLayout *topLayout = dynamic_cast<QBoxLayout *>(centralWidget()->layout());
  QVBoxLayout *leftPane = new QVBoxLayout;
  topLayout->addLayout(leftPane);
  //l eftPane->setSizeConstraint ( QLayout::SetMaximumSize);
  // leftPane->setMaximumSize(230);
  leftPane->addWidget(mEventInfo = new EventInfo(this));
  leftPane->addWidget( mServerInfo = new ServerInfo(this));
  mServerInfo->setFixedWidth(mEventInfo->width());

  //fStarLogo = new  QPushButton(QIcon(":/starlogo_1.xpm"),"",leftPane);
  //connect(fStarLogo, SIGNAL(clicked()) ,this, SLOT(DoUpdateButton()) );
  // QToolTip::add(fStarLogo,"Experiment shutdown. Don't push this button!");
  leftPane->addStretch();
  leftPane->addWidget(fProgressBar = new QProgressBar(this));
  fProgressBar-> setSizePolicy(QSizePolicy::Fixed,QSizePolicy::Preferred);
  fProgressBar->setFixedWidth(mEventInfo->width());
  //fProgressBar->setMaximumSize(leftPane->width(),25);
  fProgressBar->setValue(0);
  fProgressBar->setRange(0,10);
  
  mBitsFrame = new QFrame(0,"Trigger/ Detector-Bits");
  mBitsFrame->setWindowTitle ("Trigger/ Detector-Bits");
  
  mTriggerDetectorBitsInfo = new TriggerDetectorBitsInfo(mBitsFrame);

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
void  PresenterGui::TurnLive(bool on)
{
   fActions[kLive]->setOn(on);
}

//--------------------------------------------------------------
void PresenterGui::DoLiveButton()
{
  // Handle Live button click.
  // Connect to mmap file
  if(mDebugLevel) {
    cout<<"Live Button Pressed  "<< this << endl;
  } 
  if (fActions[kLive]->isOn()) { 
      emit live();
      emit update(GetCanvas(), GetTabId(), GetSubTabId() );
  }

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
    sprintf(mPsName,"%s/run%s_tab%dsubTab%d.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().toAscii().data(),GetTabId(),GetSubTabId());
  } else {
    cc = GetGroupCanvas();
    sprintf(mPsName,"%s/run%s_%s.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().toAscii().data(),GetGroupWidget()->name());
  }
  if ( cc ) {
    cc->Print(mPsName);
  }
}
//______________________________________________________________________________
void  PresenterGui::PrintAllCB()
{
  sprintf(mPsName,"%s/run%s.ps",EvpUtil::GetOutputPath(),mEventInfo->run->text().toAscii().data());
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
  int pro = fProgressBar->value();
  pro++;
  if (pro>10) pro=0;
  fProgressBar->setValue(pro);

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
  fDynamicTab->addTab(tab,name);
  fDynamicTab->showPage(tab);
  connect(tab, SIGNAL( currentChanged(QWidget*)), this, SLOT(tabChanged(QWidget*)) );  
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

