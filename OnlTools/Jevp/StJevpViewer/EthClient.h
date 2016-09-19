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
    EthClient();
    void connectToServer(const char *server, int port);
    void send(const char *cmd, const char *args);
    void send(TObject *msg);
    TObject *receive();
    TSocket *getSocket() { return socket; }

    // Display functions...
    void readDisplayFromServer(const char *displayName="shift");
    // These are all the same
    //void getCanvasDescriptor(u_int combo_idx);
    //void getTab(u_int combo_idx);
    DisplayNode *getTabDisplayNode(u_int combo_idx);
    char *getTabName(u_int combo_idx);
    DisplayFile *display() { return displayFile; }

    int getRun();
    RunStatus *getRunStatus();
    JevpPlot *getPlotFromServer(char *name);

    // Reference Plots...
    void swapRefsOnServer(char *name, int idx1, int idx2);
    void saveExistingPlot(JevpPlot *plot);
    void deletePlot(JevpPlot *plot);
    void writePlotToServer(JevpPlot *plot);
};
