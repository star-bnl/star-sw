#ifndef _JEVP_LOGIC_H_
#define _JEVP_LOGIC_H_

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

//#include <TQtRootAction.h>

#include <Rtypes.h>
#include <TROOT.h>
#include <TApplication.h>
//#include <TVirtualX.h>
#include <TSystem.h>
#include <TStyle.h>
#include <TSocket.h>

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

#include <TQtRootAction.h>

#include "Jevp/StJevpPlot/RunStatus.h"
#include "Jevp/StJevpServer/DisplayDefs.h"

#include "JevpGui.h"
#include <qobject.h>
#include <QEvent>


#include <list> 
using namespace std;

class EndOfRunAction;
class JevpPlot;

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
class JevpLogic : public QObject {

  Q_OBJECT

 private:


  void showDirectories();

  int send(TObject *msg);
  // Server related fields...

  DisplayFile *displayFile;

  RunStatus* mRS;
  
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

  TSocket *socket;
 JevpLogic();
 JevpLogic(const char*);
 virtual ~JevpLogic();

 int file_uploaded; // flag if pdf file was uploaded into database; yes=1, no=0, error=-1

 int ConnectToServerPort(int port, int ntries); // reconnects to different port...
 void killServer();                 // sends kill command
 int LaunchRun(char *runNumber);   // run number is pathname w/o /a

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

 public slots:
   //void Connect();
 void printAll(const char* filename);

 u_int getTabBase();
 u_int getTabDepthMult(u_int idx);
 u_int getTabNextIdx(u_int idx);
 u_int getTabChildIdx(u_int idx);
 u_int getTabIdxAtDepth(u_int idx, u_int depth);
 u_int getFinalTabIdx(u_int idx) {
   int fi=0;
   int depth=0;
   while((fi = getTabIdxAtDepth(idx, depth)) > 0) {
     depth++;
   }
   return fi;
 }
 u_int getPenultimateTabIdx(u_int idx) {
   int pi=0;
   int fi=0;
   int depth=1;
   while((fi = getTabIdxAtDepth(idx, depth)) > 0) {
     depth++;
   }
   if(depth >=2) pi = getTabIdxAtDepth(idx, depth-2);
   return pi;
 }

 void DrawPlot(JevpScreenWidget *screen);
 JevpPlot *getPlotFromServer(char *name, char *error);
 void swapRefsOnServer(char *name, int idx1, int idx2);
 void saveExistingPlot(JevpPlot *plot);
 void deletePlot(JevpPlot *plot);
 void writePlotToServer(JevpPlot *plot);

DisplayNode *getCanvasDescriptor(u_int combo_idx) {
   DisplayNode *node = displayFile->getTab(combo_idx);
   if(node == NULL) {
     return NULL;
   }
   if(!node->leaf) return NULL;
   return node;
 }

 DisplayNode *getTab(u_int combo_idx) {
   DisplayNode *node = displayFile->getTab(combo_idx);
   LOG("JEFF", "getTab(%d) --> %s",combo_idx, node ? node->name : "null");
   return node;
 }
 // i,j,k,l,m,n ---> i + 50*j + (50^2)*k + (50^3)*l etc...
 // returns NULL for no tab
 // isCanvas is a return value, type = 0 for tab, 1 for isCanvas
 char *getTabName(u_int combo_idx) {
   DisplayNode *node = displayFile->getTab(combo_idx);
   if(!node) return NULL;
   else return node->name;
 } 

 public:
 void Save(const char* file);
 void Disconnect();
 bool Status() { return mGo; }
 void Start() { mGo = true; }
 void Stop() { mGo = false; }
 void Print(TCanvas* cc, int tab, int sub);
 void Draw(TCanvas*, int  tab, int subTab);
 void Draw(TCanvas*, const char* group);
 void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
 void WriteCurrent(int i, int j);
 void WriteCurrentCanvasToPSFile(const char* file, int tab, int subTag);
 void SaveAll();
 void CrossOfDeath(JevpScreenWidget *screen, char *label="No plot");
 void ReconfigureTabs();
 private: 
 void addGroupTabs();
 TCanvas* mLastDrawnCanvas;

 public:
 TList *screens;  // list of JevpScreenWidgets...
};
//************************************************************************

#endif







/***************************************************************************
 *
 * $Id: JevpLogic.h,v 1.1 2010/12/07 14:25:23 jml Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: JevpLogic.h,v $
 * Revision 1.1  2010/12/07 14:25:23  jml
 * adding Jevp
 *
 * Revision 1.11  2010/10/29 15:48:21  jml
 * updates
 *
 * Revision 1.10  2010/10/20 14:49:56  jml
 * new display defs defintion
 *
 * Revision 1.9  2010/08/09 20:11:41  jml
 * reanalyze
 *
 * Revision 1.8  2010/08/05 15:35:05  jml
 * updates to client for reanalyze mode
 *
 * Revision 1.7  2010/07/01 14:31:44  jml
 * blah
 *
 * Revision 1.6  2010/03/29 15:16:16  jml
 * more changes..
 *
 * Revision 1.5  2010/03/19 17:58:16  jml
 * modify to scripted version, add hlt
 *
 * Revision 1.4  2010/02/16 22:05:28  jml
 * getting to compile on new evp
 *
 * Revision 1.3  2010/01/19 19:20:02  jml
 * old changes
 *
 * Revision 1.2  2009/11/03 21:30:17  jml
 * adding refs
 *
 * Revision 1.1  2009/10/20 20:38:08  jml
 * getting up to date...
 *
 * Revision 1.4  2009/05/29 18:17:05  fine
 * merge Qt3 and Qt4  versions
 *
 * Revision 1.3  2009/02/04 03:43:10  dkettler
 * Addes Reference Plot Option
 *
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

