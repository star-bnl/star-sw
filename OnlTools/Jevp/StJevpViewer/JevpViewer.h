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

class MenuHelper {
 public:
    JevpViewer *parent;

    TGPopupMenu *file;
    TGPopupMenu *input;
    //TGPopupMenu *option;
    //TGPopupMenu *help;

    MenuHelper(JevpViewer *p);
    void buildMenu(TGMainFrame *frame);
};

//class ToolbarHelper {
//    buildToolbar();
//};



class TabHelper : public TObject {
    RQ_OBJECT("TabHelper");

 public:
    JevpViewer *parent;
    DisplayFile *displayFile;

    TGTab2 *mainTabs;
    
    TabHelper(JevpViewer *p);
    void buildTabs(TGMainFrame *frame, DisplayFile *displayFile);
    void rebuildTabs();
    
    CanvasFrame *getCurrentContainer();
    
    void deleteTab(TGTab2 *tab, int doDelete=1);
    void fillTab(TGTab2 *tab, u_int combo_index);

    // Slots...  
    void tabSelected(Int_t id);

    ClassDef(TabHelper, 0);
};


class JevpViewer : public TObject {
    RQ_OBJECT("JevpViewer");
	
 private:
    TGMainFrame         *fMain;
    EthClient           server;

    MenuHelper *menu;
    TabHelper *tabs;

    TTimer *timer;

    char *serverTags=NULL;

public:
    JevpViewer(const TGWindow *p,UInt_t w,UInt_t h);
    virtual ~JevpViewer();

    // Worker functions...
    void update();
    void updateRunStatus();
    void updateServerTags();
    void updateCurrentPlot();

    // Slots...
    void doMenu(Int_t);
    void ExitViewer();
    void refreshTimerFired();

    static void entryPoint();
    ClassDef(JevpViewer, 0);
};


// This grabs plots from the server
// This draws plots on the canvas
//
class JevpPlotInfo : public TObject {
    RQ_OBJECT("JevpPlotInfo");

 private:  
    // These are owned and stored
    // needed for screen to stay valid despite never being touched again...
    THashTable *jevpPlots;
    TList *plotItems;

    //int npads;
    int combo_index;
    time_t cleanTime;
    
    // These are needed during the draw routines..
    DisplayNode *displayTab;
    TSocket *server;
    TCanvas *canvas;
    

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

    JevpPlotInfo(int combo_index);
    virtual ~JevpPlotInfo();

    void DrawOnScreen(TCanvas *c, TSocket *s, DisplayFile *f);
    JevpPlot *getPlotAtIdx(int idx);

    DisplayNode *getDisplay() { return displayTab; };

    ClassDef(JevpPlotInfo, 0);
};


class CanvasFrame : public TGCompositeFrame {
    RQ_OBJECT("CanvasFrame");

 public:
    int id;
    JevpPlotInfo *plotInfo;
    
    CanvasFrame(const TGWindow *p, int id);
    virtual ~CanvasFrame();

    void DoEvent(Int_t cmd, Int_t x, Int_t y, TObject *o);

    TRootEmbeddedCanvas *canvas;
    ClassDef(CanvasFrame, 0);
};


