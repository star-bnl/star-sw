#pragma once

#include <TGClient.h>
#include <TCanvas.h>
#include <TF1.h>
#include <TRandom.h>
#include <TGButton.h>
#include <TGFrame.h>
#include <TRootEmbeddedCanvas.h>
#include <RQ_OBJECT.h>
#include <TObject.h>
#include "Jevp/StJevpViewer/TGTab2.h"
#include <THashTable.h>
#include <TList.h>
#include <TText.h>
#include <TClass.h>
#include <TLine.h>

#include "EthClient.h"
#include "Jevp/StJevpPlot/JevpPlot.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "Jevp/StJevpPlot/EvpMessage.h"
#include "Jevp/StJevpPlot/DisplayDefs.h"




class JevpViewer;
class CanvasFrame;

class EthServers {
 public:
  EthClient shift;
  EthClient l4;
};
  
class MenuHelper {
 public:
    JevpViewer *parent;

    TGPopupMenu *file;
    TGPopupMenu *input;
  
    MenuHelper(JevpViewer *p);
    void buildMenu(TGMainFrame *frame);
};

class TabHelper : public TObject {
    RQ_OBJECT("TabHelper");

 public:
    JevpViewer *parent;
    
    //DisplayFile *displayFile;

    TGTab2 *mainTabs;
    TGTab2 *shiftTabs;
    TGTab2 *l4Tabs;
    
    TabHelper(JevpViewer *p);
    void buildTabs(TGTab2 *main);
    void rebuildTabs();
    
    //CanvasFrame *getCurrentContainer();
    TGCompositeFrame *getCurrentContainer();

    void deleteTab(TGTab2 *tab, int doDelete=1);

    // Add all below combo_index to tab
    void fillTab(TGTab2 *tab, EthClient *client, u_int combo_index);

    // Slots...  
    void tabSelected(Int_t id);

    ClassDef(TabHelper, 0);
};


class JevpViewer : public TObject {
    RQ_OBJECT("JevpViewer");
 public:
    EthServers eth;	
    int SERVERPORT;
    int initauto;
 private:
    TGMainFrame         *fMain;
    
 
    //EthClient           shiftServer;
    //EthClient           l4Server;

    MenuHelper *menu;
    TabHelper *tabs;

    TTimer *timer;

public:
    JevpViewer(const TGWindow *p,UInt_t w,UInt_t h, char *args);
    virtual ~JevpViewer();
    void parseArgs(char *args);

    // Worker functions...
    void update();
    void updateRunStatus();
    void updateServerTags();
    void updateCurrentPlot();

    // Slots...
    void doMenu(Int_t);
    void ExitViewer();
    void refreshTimerFired();

    static void entryPoint(char *args);
    ClassDef(JevpViewer, 0);
};


// This grabs plots from the server
// This draws plots on the canvas
//
class JevpPlotInfo : public TObject {
    RQ_OBJECT("JevpPlotInfo");
 public:
   int combo_index;
   EthClient *ethclient;
 private:  
    // These are owned and stored
    // needed for screen to stay valid despite never being touched again...
    THashTable *jevpPlots;
    TList *plotItems;

    //int npads;
 
    time_t cleanTime;
    
    // These are needed during the draw routines..
    //DisplayNode *displayTab;
    //EthClient *ethclient;
    //TSocket *server;
    //TCanvas *canvas;
    CanvasFrame *myCanvasFrame;

    // Helpers...
    void deleteItems();

    void addJevpPlot(JevpPlot *plot);
    JevpPlot *getJevpPlot(const char *name);
    void addPlotItem(TObject *mplot);
    
    void downloadPlot(const char *name);
    void downloadAllPlots();
    void drawEmptySpace();
    void drawNoDataPresent(const char *name);
    void drawCrossOfDeath(const char *name);

 public:
    int npads;

    JevpPlotInfo(CanvasFrame *parent, EthClient *ethclient, int combo_index);
    virtual ~JevpPlotInfo();

    void DrawOnScreen();
    JevpPlot *getPlotAtIdx(int idx);

    
    DisplayNode *getDisplay() { 
      LOG("JEFF", "getTabDisplayNode from getDisplay()");
      DisplayNode *dnode = ethclient->getTabDisplayLeaf(combo_index); 
      if(dnode == NULL) LOG("JEFF", "returning null");
      
      return dnode;
    };
    

    ClassDef(JevpPlotInfo, 0);
};


class CanvasFrame : public TGCompositeFrame {
    RQ_OBJECT("CanvasFrame");

 public:
    int id;
    JevpPlotInfo *plotInfo;
    
    CanvasFrame(const TGWindow *p, EthClient *server, int id);
    virtual ~CanvasFrame();

    void DoEvent(Int_t cmd, Int_t x, Int_t y, TObject *o);

    TRootEmbeddedCanvas *canvas;
    ClassDef(CanvasFrame, 0);
};



class DummyFrame : public TGCompositeFrame {
  RQ_OBJECT("DummyFrame")

public:
  TRootEmbeddedCanvas *canvas;

  // void DoDraw();
  DummyFrame(const TGWindow *p, UInt_t w, UInt_t h);
  ClassDef(DummyFrame, 0);
};
