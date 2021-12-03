#pragma once

#include <stdlib.h>
#include <stdio.h>

#include <TROOT.h>
#include <TSystem.h>
#include <TMessage.h>
#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TRegexp.h>
#include <TSocket.h>
#include "Jevp/StJevpViewer/TGTab2.h"

#include "Jevp/StJevpPlot/JevpPlot.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "Jevp/StJevpPlot/EvpMessage.h"
#include "Jevp/StJevpPlot/DisplayDefs.h"

class EthClient {
 private:
  TSocket *socket;
  DisplayFile *displayFile;
  
 public:
  int tabs_valid;  // note:
                   //        tabsvalid=1, connected==NULL   -->  No connection
                   //        tabsvalid=0, connected!=NULL   -->  In process of connecting...
                   //        tabsvalid=1, connected!=NULL   -->  normal operation
                   //        tabsvalid=0, connected==NULL   -->  Disconnecting...
  time_t statusChangeTime;
  RunStatus lastRunStatus;
  char *serverTags;
  char myDisplayName[64];

  EthClient();

  bool connected();
  bool connectToServer(const char *server, int port);
  void send(const char *cmd, const char *args);
  void send(TObject *msg);
  TObject *receive();
  TSocket *getSocket() { return socket; }

  // Display functions...
  void readDisplayFromServer(const char *displayName="shift");
  // These are all the same
  //void getCanvasDescriptor(uint32_t combo_idx);
  //void getTab(uint32_t combo_idx);
  DisplayNode *getTabDisplayLeaf(uint32_t combo_idx);
  DisplayNode *getTabDisplayBranch(uint32_t combo_idx);
  DisplayNode *getTabDisplayBranchOrLeaf(uint32_t combo_idx);

  char *getTabName(uint32_t combo_idx);
  DisplayFile *display() { return displayFile; }

  int getRun();
  RunStatus *getRunStatus();
  JevpPlot *getPlotFromServer(char *name);
  int updateServerTags();   // returns zero if no change...
  
  // Reference Plots...
  void swapRefsOnServer(char *name, int idx1, int idx2);
  void saveExistingPlot(JevpPlot *plot);
  void deletePlot(JevpPlot *plot);
  void writePlotToServer(JevpPlot *plot);
};
