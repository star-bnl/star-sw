#ifndef PresenterGui_h
#define PresenterGui_h

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
#include "EvpUtil.h"
#include "TQtRootAction.h"
#include <qintdict.h> 
#include <qmainwindow.h>

class EvpPresenter;
class QMainWindow;
class QTabWidget ;
class QPushButton;
class QButton;
class TCanvas;
class QMenuBar;
class QWidget;
class QToolBar;
class QLineEdit;
class QHBox;
class QProgressBar;

class EventInfo;
class ServerInfo;
class ServerStatus;
class TriggerDetectorBitsInfo;
class TCanvas;
class TObject;
class TQtWidget;
class TQtZoomPadWidget;


using namespace std;

enum { kFileSave, kFileSaveAs, kFilePrint, kFilePrintAll, kFileExit, kHelpAbout, kLive, kFile, kUpdate, kAutoUpdate, kToolBar, kOnlPrinter2, kBits, kReference}; 

// ************************************************************************
class PresenterGui : public QMainWindow
{

  Q_OBJECT
 public:
    EventInfo* mEventInfo;
    ServerInfo* mServerInfo;
    TriggerDetectorBitsInfo* mTriggerDetectorBitsInfo;
    QPushButton* fStarLogo;
		  
private:
    int mWidth;
    int mHight;
    char mPsName[1024];

    QFrame* mBitsFrame;
    QTabWidget* fTab;
    QTabWidget* fStaticTab;
    QTabWidget* fDynamicTab;
    QToolBar* fToolBar;
    QLineEdit* mRun;
    QLineEdit* mCount;
    QLineEdit* mEvent;
    QLineEdit* mToken;
    QHBox* mCentralWidget;
    QProgressBar* fProgressBar;
    //Array of tabs
    // Fist index upper_level tabs
    // Second index subtabs
    //
    // Number of Subtabs for a given Tab
    int nSubTabs[MAX_TABS];
    //sep

    char *TabNames[MAX_TABS][MAX_SUBTABS];

    int  nx[MAX_TABS][MAX_SUBTABS];      // number of horizontal pads on a canvas
    int  ny[MAX_TABS][MAX_SUBTABS];      // number of vertical pads on a canvas
    int  nHist[MAX_TABS][MAX_SUBTABS];   // number of histograms for a canvas
    //const char* hNames[MAX_TABS][MAX_SUBTABS][MAX_PADS];  // histogram names
    TString myNames[MAX_TABS][MAX_SUBTABS][MAX_PADS]; // new array for histogram Names

    int mTab;
    int mSubTab;

    TQtZoomPadWidget *mZoomer;
    //-----------------------------------

    TString mDefaultPSFilePath; // default path for PS files
    TString mHistoPSFile;       // file name for PostScript file
    TString mHistoGIFFile;       // file name for GIF file
    TString mHistoPDFFile;       // file name for PDF file
    TString mHistoFileNameShort;  // file name without extension, but with path

    int mDebugLevel;


    int IntFromTString(const TString &ts);
    float FloatFromTString(const TString &ts);
    int ParseString (const TString &tChain, TObjArray &Opt);

    int file_uploaded; // flag if pdf file was uploaded into database; yes=1, no=0, error=-1

    TString mStrLive;
    TString mStrFile;
    TString mStrRun;
    TString mStrStop;
    QIntDict<TQtRootAction> fActions;
    QMenuBar *fMenuBar;
    
    Int_t   fGuiRefreshRate; // msec.
 protected:
    void StopEventLoop();
 public:

    PresenterGui(bool isRefWindow = false);
    virtual ~PresenterGui();

    // getters for static tabs
    int GetTabId();
    int GetSubTabId();
    int GetWidth() { return mWidth;}
    int GetHight() { return mHight; }
    TCanvas* GetCanvas();
    // getters for dynamic tabs
    TQtWidget* GetGroupWidget();
    TCanvas* GetGroupCanvas();

    void  MakeActions();

    void customEvent( QCustomEvent * e );


    
public slots:
     void  ProcessMessage();
     void SaveCB();
     void SaveAsCB();
     void PrintCB();
     void PrintAllCB();
     void QuitCB();
     void AboutCB();
     void ToolBarCB();
     void tabChanged(QWidget* w=0);
     void SetWindowName(const char* displayText);
     void ShowToolBar(bool show);
     void setEventInfo(int run, int event, int count, int token,  unsigned int triggerBits,  unsigned int detectorBits, unsigned int triggerBitsRun , unsigned int detectorBitsRun );
     void setServerInfo(ServerStatus*);
     void onlPrinter2();
     void OpenReference();
     void updateRequest();
    void CloseWindow(void);
    void DefineLayouts(void);
    void DoLiveButton(void);
    void DoFileButton(void);
    void DoBitsButton(void);
    void DoUpdateButton(void);
    void DoAutoUpdateButton(void);
    void DoPrintButton(void);
    int  GetDebugLevel(void){return mDebugLevel;}
    TString GetHistoPSFilePath(void){return mDefaultPSFilePath;}
    TString GetHistoPSFile(void){return mHistoPSFile;}
    void MakeConnectionFrame(void);
    void MakePrintFrame(void);
    void MakeMenuBar(void);
    void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
    void SetDefaults(void);
    void SetHistoPSFile(TString lHistoPSFile){mHistoPSFile =lHistoPSFile;}
    void removeGroupTabs(); // removed recursively all group tabs
    void addGroupTab(const char*); // adds a new QTabWidget to the dynamic tabs;
    void addGroup(const char*);    // adds a new QWidget to that last QTabWidget in the dynamic tabs
    void GetNextEvent();

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
      void nextEvent();
      void openReference();
};
//************************************************************************




#endif


