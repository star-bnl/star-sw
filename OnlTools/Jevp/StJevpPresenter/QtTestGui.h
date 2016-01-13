#ifndef _QTTESTGUI_H_
#define _QTTESTGUI_H_

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


#  include <q3intdict.h> 
#  include <q3mainwindow.h>
#  include <Q3Frame>

class Q3ToolBar;
class Q3HBox;
class Q3ProgressBar;


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



// ************************************************************************
class QtTestGui : public Q3MainWindow {
     
    Q_OBJECT


 private:
    TCanvas* pc_mCanvas;
    QTimer *refreshTimer;

    int mWidth;
    int mHight;
  
    Q3HBox *mCentralWidget;
 
    Int_t   fGuiRefreshRate; // msec.
    
 protected:
     void StopEventLoop();

 public:

    QtTestGui();
    void gui_QtTestGui(QtTestGui *logic, bool isRefWindow = false);
    virtual ~QtTestGui();
    
    int GetWidth() { return mWidth;}
    int GetHight() { return mHight; }
 
    void init(); 


 public slots:
    void ProcessMessage();
    void jl_ClosePresenter();

    void CloseWindow(void);
    void DefineLayouts(void);
 
    void refreshTimerFired();
    void UpdatePlots();

 signals:
    
    void jl_setEnabled(bool);
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
    void openReference();
  

    //////////////////////////////  PresenterConnect


 

};
//************************************************************************




#endif


