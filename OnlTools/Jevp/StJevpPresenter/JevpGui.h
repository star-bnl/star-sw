#ifndef _JEVPGUI_H_
#define _JEVPGUI_H_

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TRegexp.h>
#include <string>
#include <fstream>
#include <sstream>

#include <TROOT.h>
#include <TSystem.h>
#include <TMessage.h>

#include <TQtRootAction.h>
#include <TSocket.h>

#include <TText.h>

#include <qobject.h> 
#include <TQtWidget.h>
#include <qtabwidget.h>
#include "Jevp/StJevpPlot/JevpPlot.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "Jevp/StJevpPlot/EvpMessage.h"
#include "Jevp/StJevpPlot/DisplayDefs.h"


#  include <q3intdict.h> 
#  include <q3mainwindow.h>
#  include <Q3Frame>

//class JevpLogic;



#if QT_VERSION < 0x40000
  class QToolBar;
  class QHBox;
  class QProgressBar;
#else /* QT4 */
  class Q3ToolBar;
  class Q3HBox;
  class Q3ProgressBar;
#endif /* QT4 */

class QTimer;
class QTabWidget ;
class QPushButton;
class QButton;
class TCanvas;
class QMenuBar;
class QWidget;
class QLineEdit;
class EventInfo;
class ServerInfo;
class ServerStatus;
class TriggerDetectorBitsInfo;
class TCanvas;
class TObject;
class TQtWidget;
class TQtZoomPadWidget;


using namespace std;

enum { kFileChangeHistogramSet, kFileIgnoreServerTags, kFileChangeToRun, kFileLive, kFilePrint, kFilePrintAll, kHelpAbout, kUpdate, kAutoUpdate, kToolBar, kOnlPrinter2 }; 

class JevpScreenWidget : public  TQtWidget {
public:
  QTabWidget *parentMenu;
  std::string *plot;
  u_int combo_index;
  TList *plots;
  TList *jevpPlots;
 
  void addJevpPlot(JevpPlot *mplot) {  
    jevpPlots->Add(mplot); 
    LOG(DBG, "jevpPlots has %d entries",jevpPlots->GetSize());
  }
  JevpPlot *getJevpPlot(char *name);
  void addPlot(TObject *mplot) { 
    //mplot->SetBit(kCanDelete); 
    plots->Add(mplot); 
    LOG(DBG, "plots has %d entries",plots->GetSize());
  };

  void Clear() { 
    LOG(DBG, "Clearing Screen Widget...");
    TListIter next(plots);
    TObject *o;

    while((o = (TObject *)next())) {
      LOG(DBG, "Deleting an object... %s can %d must %d",o->GetName(),o->TestBit(kCanDelete),o->TestBit(kMustCleanup));
      delete o;
    }

    plots->Clear(); 
    jevpPlots->Clear();
    //printf("more clear\n");
    GetCanvas()->Clear(); 
    //LOG("JEFF", "Cleared canvas");

    LOG(DBG, "Done Clearing Screen Widget");
  };
  JevpScreenWidget(char *tabname, char *plotname, u_int combo_index, QTabWidget *menu);
  virtual ~JevpScreenWidget();

  //TObject *getPlot
  
  void mouseDoubleClickEvent(QMouseEvent *e);

  void mousePressEvent(QMouseEvent *e);
};


// ************************************************************************
class JevpGui : public Q3MainWindow {
     
  Q_OBJECT

 public:
    EventInfo* mEventInfo;
    ServerInfo* mServerInfo;
    TriggerDetectorBitsInfo* mTriggerDetectorBitsInfo;
    QPushButton* fStarLogo;

private:
    QTimer *refreshTimer;

    QTabWidget *rootTab;

    int mWidth;
    int mHight;
    char mPsName[1024];

    char *serverTags;

#if QT_VERSION < 0x40000
    QFrame* mBitsFrame;
#else /* QT4 */
    Q3Frame* mBitsFrame;
#endif /* QT4 */
    QTabWidget* fTab;
    QTabWidget* fStaticTab;
    QTabWidget* fDynamicTab;
#if QT_VERSION < 0x40000
    QToolBar* fToolBar;
#else /* QT4 */
    Q3ToolBar* fToolBar;
#endif /* QT4 */
    QLineEdit* mRun;
    QLineEdit* mCount;
    QLineEdit* mEvent;
    QLineEdit* mToken;
#if QT_VERSION < 0x40000
    QHBox* mCentralWidget;
    QProgressBar* fProgressBar;
#else /* QT4 */
    Q3HBox* mCentralWidget;
    Q3ProgressBar* fProgressBar;
#endif /* QT4 */
    //Array of tabs
    // Fist index upper_level tabs
    // Second index subtabs
    //
    // Number of Subtabs for a given Tab
    //int nSubTabs[MAX_TABS];
    //sep

    // char *TabNames[MAX_TABS][MAX_SUBTABS];

    // int  nx[MAX_TABS][MAX_SUBTABS];      // number of horizontal pads on a canvas
    // int  ny[MAX_TABS][MAX_SUBTABS];      // number of vertical pads on a canvas
    //int  nHist[MAX_TABS][MAX_SUBTABS];   // number of histograms for a canvas
    //const char* hNames[MAX_TABS][MAX_SUBTABS][MAX_PADS];  // histogram names
    //TString myNames[MAX_TABS][MAX_SUBTABS][MAX_PADS]; // new array for histogram Names

    int mTab;
    int mSubTab;

    TQtZoomPadWidget *mZoomer;
    //-----------------------------------

    //TString mDefaultPSFilePath; // default path for PS files
    //TString mHistoPSFile;       // file name for PostScript file
    //TString mHistoGIFFile;       // file name for GIF file
    //TString mHistoPDFFile;       // file name for PDF file
    //TString mHistoFileNameShort;  // file name without extension, but with path

    int mDebugLevel;


    int IntFromTString(const TString &ts);
    float FloatFromTString(const TString &ts);
    int ParseString (const TString &tChain, TObjArray &Opt);

    int file_uploaded; // flag if pdf file was uploaded into database; yes=1, no=0, error=-1

    TString mStrLive;
    TString mStrFile;
    TString mStrRun;
    TString mStrStop;

    Q3IntDict<TQtRootAction> fActions;

    QMenuBar *fMenuBar;
    
    Int_t   fGuiRefreshRate; // msec.

    void deleteTabs(QTabWidget *tab,int leaveSummary = 0);
    void switchTabs(const char *newdisplay, const char *newbuilderlist);
    void buildTabs(int addSummary);

    void fillTab(QTabWidget *tab, u_int idx);

    JevpScreenWidget *currentScreen;
  

 protected:
    void StopEventLoop();
 public:

    JevpGui();
    void gui_JevpGui(JevpGui *logic, bool isRefWindow = false);
    virtual ~JevpGui();

    // getters for static tabs
    int GetTabId();
    int GetSubTabId();
    int GetWidth() { return mWidth;}
    int GetHight() { return mHight; }
    //TCanvas* GetCanvas();
    // getters for dynamic tabs
    //TQtWidget* GetGroupWidget();
    //TCanvas* GetGroupCanvas();

    void  MakeActions();

    int updateRunStatus();
    int updateServerTags();

public slots:
     void  ProcessMessage();
// void  TurnLive(bool on=true);
     void SaveCB();
     void SaveAsCB();
     void PrintCB();
     void PrintAllCB();
     void QuitCB();
     void AboutCB();
     void ToolBarCB();
     void ChangeHistogramSet();
     void IgnoreServerTags();
     void ChangeToRun();
     void ChangeToLive();

     void tabChanged(QWidget* w=0);
     void SetWindowName(const char* displayText);
     void ShowToolBar(bool show);
     void setEventInfo(int run, int event, int count, int token,  unsigned int triggerBits,  unsigned int detectorBits, unsigned int triggerBitsRun , unsigned int detectorBitsRun );
     void setServerInfo(ServerStatus*);
     void onlPrinter2();
     void OpenReference();
     //void updateRequest();
    void CloseWindow(void);
    void DefineLayouts(void);
    void DoLiveButton(void);
    void DoFileButton(void);
    void DoBitsButton(void);
    void DoUpdateButton(void);
    void DoAutoUpdateButton(void);
    void DoPrintButton(void);
    int  GetDebugLevel(void){return mDebugLevel;}
    //    TString GetHistoPSFilePath(void){return mDefaultPSFilePath;}
    // TString GetHistoPSFile(void){return mHistoPSFile;}
    void MakeConnectionFrame(void);
    void MakePrintFrame(void);
    void MakeMenuBar(void);
    void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
    void SetDefaults(void);

    //    void SetHistoPSFile(TString lHistoPSFile){mHistoPSFile =lHistoPSFile;}
    //    void removeGroupTabs(); // removed recursively all group tabs
    //void addGroupTab(const char*); // adds a new QTabWidget to the dynamic tabs;
    //void addGroup(const char*);    // adds a new QWidget to that last QTabWidget in the dynamic tabs
    void UpdatePlots();
    void refreshTimerFired();

    signals:
      void save();
      void saveAs();
      void live();
      void file();
      void update();
      void update(TCanvas* cc, const char* );
      void update(TCanvas* cc, int tab, int subTab);
      void autoUpdate();
      void print();
      void printAll(const char*);
      void tab(int);
      void subTab(int);
      void canvas(TCanvas*);
      //   void updatePlots();
      void openReference();


      //////////////////////////////  PresenterConnect

 private:


  int pc_mTab;
  int pc_mSubTab;
  TCanvas* pc_mCanvas;
 public:

 protected:
  void closeEvent(QCloseEvent *e);

 public slots:
 void pc_save();
 void pc_saveAs();
 void pc_live();
 void pc_file();
 void pc_update();
 void pc_update(TCanvas* cc, int tab, int subTab);
 void pc_update(TCanvas*, const char*);
 void pc_print();
 void pc_setTab(int);
 void pc_setSubTab(int);
 void pc_setCanvas(TCanvas*);
 void pc_openReference();
 
 signals:
 void pc_signalEventInfo(int, int, int, int, unsigned int, unsigned int, unsigned int, unsigned int);
 void pc_signalServerInfo(ServerStatus*);
 //void pc_updateRequest();



 ///// JevpLogic..............................

 private:

  void jl_showDirectories();

  int jl_send(TObject *msg);
  // Server related fields...

  DisplayFile *jl_displayFile;

  RunStatus* jl_mRS;
  
  bool jl_mGo; // set mGo=false to top the event loop 
  int jl_tab;
  int jl_subTab;
  int jl_tabLast ; // initial default: Tab    =0
  int jl_subTabLast ; // initial default: subTab =1
  int jl_runNumber;
  int jl_runNumberLast;
  int jl_evtNumber;
  unsigned int jl_mTriggerBits;
  unsigned int jl_mDetectorBits;
  unsigned int jl_mTriggerBitsRun;
  unsigned int jl_mDetectorBitsRun;
  int jl_evtNumberLast;
  int jl_evtCounter;
  int jl_evtCounterLast;

  bool jl_needsUpdate;
  char displayText[1024];
  int jl_mDebugLevel;

 
 TString mHistoPSFile;       // file name for PostScript file
 TString mHistoGIFFile;       // file name for GIF file
 TString mHistoPDFFile;       // file name for PDF file
 TString mHistoFileNameShort;  // file name without extension, but with path

 char mMapFile[1024];

 signals:
 void jl_removeGroupTabs();
 void jl_addGroupTab(const char*);
 void jl_addGroup(const char*);
 void jl_setEnabled(bool);
 public:

  TSocket *jl_socket;
  void init();
  void readDisplayFromServer();

 int jl_file_uploaded; // flag if pdf file was uploaded into database; yes=1, no=0, error=-1

 int jl_ConnectToServerPort(int port, int ntries); // reconnects to different port...
 void jl_killServer();                 // sends kill command
 int jl_LaunchRun(char *runNumber);   // run number is pathname w/o /a

 // int jl_event() { return mRS->run; }
 bool jl_event (QEvent *e) {return QObject::event(e);} 
 int jl_run() { return jl_mRS->run; }
 //int jl_token() { return mRS->getToken(); }
 //int jl_counter() { return mRS->getEventCounter(); }
 //unsigned int jl_triggerBits() { return mRS->getTriggerBits(); }
 //unsigned int jl_detectorBits() { return mRS->getDetectorBits(); }
 //unsigned int jl_triggerBitsRun() { return mRS->getTriggerBitsRun(); }
 //unsigned int jl_detectorBitsRun() { return mRS->getDetectorBitsRun(); }

 RunStatus*    jl_runStatus() { return jl_mRS;}

 public slots:
   //void jl_Connect();
 void jl_printAll(const char* filename);
 void jl_ClosePresenter();
 u_int jl_getTabBase();
 u_int jl_getTabDepthMult(u_int idx);
 u_int jl_getTabNextIdx(u_int idx);
 u_int jl_getTabChildIdx(u_int idx);
 u_int jl_getTabIdxAtDepth(u_int idx, u_int depth);
 u_int jl_getFinalTabIdx(u_int idx) {
   int fi=0;
   int depth=0;
   while((fi = jl_getTabIdxAtDepth(idx, depth)) > 0) {
     depth++;
   }
   return fi;
 }
 u_int jl_getPenultimateTabIdx(u_int idx) {
   int pi=0;
   int fi=0;
   int depth=1;
   while((fi = jl_getTabIdxAtDepth(idx, depth)) > 0) {
     depth++;
   }
   if(depth >=2) pi = jl_getTabIdxAtDepth(idx, depth-2);
   return pi;
 }

 void jl_DrawPlot(JevpScreenWidget *screen);
 JevpPlot *jl_getPlotFromServer(char *name, char *error);
 void jl_swapRefsOnServer(char *name, int idx1, int idx2);
 void jl_saveExistingPlot(JevpPlot *plot);
 void jl_deletePlot(JevpPlot *plot);
 void jl_writePlotToServer(JevpPlot *plot);

DisplayNode *jl_getCanvasDescriptor(u_int combo_idx) {
   DisplayNode *node = jl_displayFile->getTab(combo_idx);
   if(node == NULL) {
     return NULL;
   }
   if(!node->leaf) return NULL;
   return node;
 }

 DisplayNode *jl_getTab(u_int combo_idx) {
   DisplayNode *node = jl_displayFile->getTab(combo_idx);
   LOG(DBG, "getTab(%d) --> %s",combo_idx, node ? node->name : "null");
   return node;
 }
 // i,j,k,l,m,n ---> i + 50*j + (50^2)*k + (50^3)*l etc...
 // returns NULL for no tab
 // isCanvas is a return value, type = 0 for tab, 1 for isCanvas
 char *jl_getTabName(u_int combo_idx) {
   DisplayNode *node = jl_displayFile->getTab(combo_idx);
   if(!node) return NULL;
   else return node->name;
 } 

 public:
 void jl_Save(const char* file);
 void jl_Disconnect();
 bool jl_Status() { return jl_mGo; }
 void jl_Start() { jl_mGo = true; }
 void jl_Stop() { jl_mGo = false; }
 void jl_Print(TCanvas* cc, int tab, int sub);
 void jl_Draw(TCanvas*, int  tab, int subTab);
 void jl_Draw(TCanvas*, const char* group);
 void jl_SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
 void jl_WriteCurrent(int i, int j);
 void jl_WriteCurrentCanvasToPSFile(const char* file, int tab, int subTag);
 void jl_SaveAll();
 void jl_NoDataPresent(JevpScreenWidget *screen, char *label="No plot");
 void jl_CrossOfDeath(JevpScreenWidget *screen, char *label="No plot");
 void jl_DrawEmptySpace(JevpScreenWidget *screen, char *label=" ");
 void jl_ReconfigureTabs();
 private: 
 void jl_addGroupTabs();
 TCanvas* jl_mLastDrawnCanvas;

 public:
 TList *jl_screens;  // list of JevpScreenWidgets...
};
//************************************************************************




#endif


