#ifndef EvpPresenter_h
#define EvpPresenter_h

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#include <Rtypes.h>
#include <TROOT.h>
#include <TApplication.h>
//#include <TVirtualX.h>
#include <TSystem.h>
#include <TStyle.h>


#include <TCanvas.h>
#include <TH1.h>
//#include <TROOT.h>
#include <TH2.h>
#include <TProfile.h>
#include <TRandom.h>

#include <TMapFile.h>

#include <TNamed.h>
#include <TPostScript.h>
#include <TPaveLabel.h>
#include <TDatime.h>
// includes for string parsing
#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TRegexp.h>
#include <string>
#include <fstream>
#include <sstream>

#include "EvpUtil.h"
#include "RunStatus.h"
#include "ServerStatus.h"
#include <qobject.h>
#include "GroupCollection.h"


#include <list> 
using namespace std;

class EndOfRunAction;

template<class T> struct isFinished : public unary_function<T,bool> {
  bool operator() ( const T& t) const {
     cout << " isFinished  " << t->GetState() << endl; 
    if (t->GetState()>2 ) {
      delete t;
      return true;
    }
    return false;
  }
};



void* evpPresenterThreadFunction(void*);


// ************************************************************************
class EvpPresenter : public QObject {

  Q_OBJECT



 private:

 TMapFile *mfile;
 RunStatus* mRS;
 ServerStatus* mSS;
 GroupCollection mGroups;

 bool mGo; // set mGo=false to top the event loop 
  int tab;
  int subTab;
  int tabLast ; // initial default: Tab    =0
  int subTabLast ; // initial default: subTab =1
  int runNumber;
  int runNumberLast;
  int evtNumber;
  unsigned int mTriggerBits;
  unsigned int mDetectorBits;
  unsigned int mTriggerBitsRun;
  unsigned int mDetectorBitsRun;
  int evtNumberLast;
  int evtCounter;
  int evtCounterLast;

  bool needsUpdate;
  char displayText[1024];
  int mDebugLevel;

 
 TString mHistoPSFile;       // file name for PostScript file
 TString mHistoGIFFile;       // file name for GIF file
 TString mHistoPDFFile;       // file name for PDF file
 TString mHistoFileNameShort;  // file name without extension, but with path

 char mMapFile[1024];

 signals:
 void removeGroupTabs();
 void addGroupTab(const char*);
 void addGroup(const char*);
 void setEnabled(bool);
 public:

 EvpPresenter();
 virtual ~EvpPresenter();

 int file_uploaded; // flag if pdf file was uploaded into database; yes=1, no=0, error=-1

 void SetSource(const char* file = EvpUtil::mMapFilePath);

 int event() { return mRS->getEventNumber(); }
 bool event (QEvent *e) {return QObject::event(e);} 
 int run() { return mRS->getRunNumber(); }
 int token() { return mRS->getToken(); }
 int counter() { return mRS->getEventCounter(); }
 unsigned int triggerBits() { return mRS->getTriggerBits(); }
 unsigned int detectorBits() { return mRS->getDetectorBits(); }
 unsigned int triggerBitsRun() { return mRS->getTriggerBitsRun(); }
 unsigned int detectorBitsRun() { return mRS->getDetectorBitsRun(); }

 RunStatus*    runStatus() { return mRS;}
 ServerStatus* serverStatus() { return mSS;}

 public slots:
 void Connect();
 void printAll(const char* filename);
 void NextEvent();


 public:
 void Save(const char* file);
 void Disconnect();
 bool Status() { return mGo; }
 void Start() { mGo = true; }
 void Stop() { mGo = false; }
 void Print(TCanvas* cc, int tab, int tab);
 void Draw(TCanvas*, int  tab, int subTab);
 void Draw(TCanvas*, const char* group);
 void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
 void SetDefaults(void);
 void WriteCurrent(int i, int j);
 void WriteCurrentCanvasToPSFile(const char* file, int tab, int subTag);
 void SaveAll();
 void CrossOfDeath(TCanvas* gcc);
 void ReconfigureTabs();
 private: 
 void addGroupTabs();
 TCanvas* mLastDrawnCanvas;
};
//************************************************************************

#endif







/***************************************************************************
 *
 * $Id: EvpPresenter.h,v 1.2 2009/01/31 00:30:33 fine Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EvpPresenter.h,v $
 * Revision 1.2  2009/01/31 00:30:33  fine
 * Major clean up: Remove the redundant time thread, redundant methods, add protection agaist of crashes, introdcue the new rootrc paramater: Online.GuiRefreshRate, load some shared in batch mode only
 *
 * Revision 1.1  2009/01/23 16:16:05  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.3  2008/03/06 18:05:28  fine
 * Supress thge hiden method compilation warning
 *
 * Revision 1.2  2007/03/08 15:09:11  laue
 * Do not redraw same canvas if no new data arrived
 *
 * Revision 1.1  2007/02/27 15:24:11  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

