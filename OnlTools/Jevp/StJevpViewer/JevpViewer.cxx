#include "JevpViewer.h"

#include <stdio.h>

#include <TROOT.h>
#include <TStyle.h>
#include <TGTab2.h>
#include <TGMenu.h>
#include <TApplication.h>

#include "EthClient.h"
#include "ZoomFrame.h"
#include <Jevp/StJevpPlot/JevpPlot.h>
#include <Jevp/StJevpPlot/EvpMessage.h>

#include <RTS/include/SUNRT/clockClass.h>

//#include <assert.h>


ClassImp(JevpViewer);
ClassImp(CanvasFrame);
ClassImp(JevpPlotInfo);
ClassImp(TabHelper);

MenuHelper::MenuHelper(JevpViewer *p) {
    file = NULL;
    input = NULL;
    //option = NULL;
    //help = NULL;
    
    parent = p;
}

void MenuHelper::buildMenu(TGMainFrame *frame) {
    int mid=0;
    
    file = new TGPopupMenu(gClient->GetRoot());
    input = new TGPopupMenu(gClient->GetRoot());
    //option = new TGPopupMenu(gClient->GetRoot());
    //help = new TGPopupMenu(gClient->GetRoot());
   
    //file->AddEntry("Change Histogram Set", ++mid);
    file->AddEntry("Ignore Server Tags", ++mid);
    file->AddSeparator();
    file->AddEntry("Print", ++mid);
    file->AddEntry("Print All", ++mid);

    input->AddEntry("Update", ++mid);
    input->AddEntry("Auto Update", ++mid);
    input->CheckEntry(mid);
   
    //option->AddEntry("View Toolbar", ++mid);
    //help->AddEntry("About", ++mid);
    		   
    TGMenuBar *menubar = new TGMenuBar(frame, 100, 20, kHorizontalFrame);
    TGLayoutHints *lh = new TGLayoutHints(kLHintsTop|kLHintsLeft,0,4,0,0);
    
    menubar->AddPopup("File", file, lh);
    menubar->AddPopup("Input", input, lh);
    // menubar->AddPopup("Option", option, lh);
    //menubar->AddPopup("help", help, lh);

    file->Connect("Activated(Int_t)", "JevpViewer", parent, "doMenu(Int_t)");
    input->Connect("Activated(Int_t)", "JevpViewer", parent, "doMenu(Int_t)");
    // option->Connect("Activated(Int_t)", "JevpViewer", parent, "doMenu(Int_t)");
    // help->Connect("Activated(Int_t)", "JevpViewer", parent, "doMenu(Int_t)");

    frame->AddFrame(menubar, lh);
}

// void ToolbarHelper::buildToolBar() {
//     // Add buttons...
//     TGCompositeFrame *buttonframe = new TGCompositeFrame(fMain, 2000, 20, kHorizontalFrame);
//     //buttonframe->SetLayoutManager(new TGMatrixLayout(buttonframe, 1, 2));
//     update = new TGTextButton(buttonframe, "update", 1);
//     //update->Resize(600, update->GetDefaultHeight());
   
//     autoupdate = new TGTextButton(buttonframe, "auto update", 2);
//     //autoupdate->Resize(600, autoupdate->GetDefaultHeight());
//     update->Connect("Clicked()", "JevpViewer", this, "DoButton()");
//     autoupdate->Connect("Clicked()", "JevpViewer", this, "DoButton()");
//     autoupdate->AllowStayDown(kTRUE);
//     buttonframe->AddFrame(update, new TGLayoutHints(kLHintsLeft|kLHintsExpandX));
//     buttonframe->AddFrame(autoupdate, new TGLayoutHints(kLHintsLeft | kLHintsExpandX));
//     fMain->AddFrame(buttonframe, new TGLayoutHints(kLHintsTop|kLHintsLeft));
// }

TabHelper::TabHelper(JevpViewer *p) {
    mainTabs = NULL;

    parent = p;
}

void TabHelper::buildTabs(TGMainFrame *frame, DisplayFile *displayFile) {
    this->displayFile = displayFile;

    mainTabs = new TGTab2(frame, 1200, 900);
    fillTab(mainTabs, 1);
    
    mainTabs->Connect("Selected(Int_t)", "TabHelper", this, "tabSelected(Int_t)");

    frame->AddFrame(mainTabs, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0,0,0,0));
}

void TabHelper::rebuildTabs() {
    deleteTab(mainTabs, 0);    // Delete subtabs, but not this tab...
    
    fillTab(mainTabs, 1);
    mainTabs->SetTab(0);
    parent->update();
}


// Delete the contents of each tab (and subtab)
void TabHelper::deleteTab(TGTab2 *tab, int doDelete) {
    int n= tab->GetNumberOfTabs();
    
    for(int i=n-1;i>=0;i--) {
	TGCompositeFrame *content = tab->GetTabContainer(i);
	tab->RemoveTab(i, kFALSE);

	TGTab2 *subtab = dynamic_cast<TGTab2 *>(content);
	if(subtab) {
	    deleteTab(subtab);
	}
	else {
	    delete content;
	}
    }
    
    if(doDelete) {
	delete tab;
    }
}

void TabHelper::fillTab(TGTab2 *tab, UInt_t idx)
{
    DisplayNode *mynode = displayFile->getTab(idx);
    if(!mynode) return;
        
    // First get my child and my tabname
    u_int child_idx = displayFile->getTabChildIdx(idx);
    DisplayNode *childnode = displayFile->getTab(child_idx);
    if(!childnode) {
	return;        // no children!   This is a bug right?
    }
 
    if(!childnode->leaf) {   // It's another tab!
	TGTab2 *childtab = new TGTab2(tab, 1200, 900);
	tab->AddTab(mynode->name, childtab);
	fillTab(childtab, child_idx);
	childtab->Connect("Selected(Int_t)", "TabHelper", this, "tabSelected(Int_t)");
    }
    else {                   // It's a histo!
	CanvasFrame *frame = new CanvasFrame(tab, child_idx);
	tab->AddTab(mynode->name, frame);
    }

    u_int next_idx = displayFile->getTabNextIdx(idx);
    fillTab(tab, next_idx);
}

CanvasFrame *TabHelper::getCurrentContainer() {
    TGTab2 *tab = mainTabs;

    while(tab) {
	TObject *ptr = (TObject *)tab->GetCurrentContainer();

	CanvasFrame *frame = dynamic_cast<CanvasFrame *>(ptr);
	if(frame) {
	    return frame;
	}
	tab = dynamic_cast<TGTab2 *>(ptr);
    }
    
    return NULL;
}

void TabHelper::tabSelected(Int_t id) {
    parent->update();
}

JevpViewer::JevpViewer(const TGWindow *p,UInt_t w,UInt_t h) {
    // Create a main frame

    gROOT->SetEditHistograms(kFALSE);
    gROOT->SetBit(kNoContextMenu | kCannotPick);
    gStyle->SetBit(kNoContextMenu | kCannotPick);

    serverTags = NULL;
    server.connectToServer("evp.starp.bnl.gov", 8499);
    server.readDisplayFromServer();
    
    server.display()->setServerTags("");
    server.display()->setDisplay(server.display()->getDisplayNodeFromName("shift"));
    server.display()->updateDisplayRoot();

    //server.display()->ignoreServerTags = 0;
    //server.display()->dump();
    //printf("\n\n\n---------------------\n");
   
    fMain = new TGMainFrame(p,w,h);
    fMain->Connect("CloseWindow()", "JevpViewer", this, "ExitViewer()");
    fMain->SetBit(kNoContextMenu | kCannotPick);

    menu = new MenuHelper(this);
    menu->buildMenu(fMain);
   
    tabs = new TabHelper(this);
    tabs->buildTabs(fMain, server.display());

       // Set a name to the main frame
    fMain->SetWindowName("Simple Example");

    // Map all subwindows of main frame
    fMain->MapSubwindows();

    // Initialize the layout algorithm
    fMain->Resize(fMain->GetDefaultSize());

    // Map main frame
    fMain->MapWindow();
    
    update();

    // This is how our loop runs!
    timer = new TTimer();
    timer->Connect("Timeout()", "JevpViewer", this, "refreshTimerFired()");
    timer->Start(5000, kFALSE);                   // false repeats forever!
}

JevpViewer::~JevpViewer() {
   // Clean up used widgets: frames, buttons, layout hints
   fMain->Cleanup();
   delete fMain;
}

void JevpViewer::entryPoint() {
    // Popup the GUI...
    
    rtsLogLevel((char *)WARN);
    rtsLogOutput(RTS_LOG_STDERR);

    new JevpViewer(gClient->GetRoot(),1200,900);
}

void JevpViewer::ExitViewer() {
    gApplication->Terminate(0);
}

void JevpViewer::doMenu(Int_t x) {

    TGPopupMenu *p = (TGPopupMenu *)gTQSender;

    const char *menu = p->GetEntry(x)->GetLabel()->Data();

    int toggle = 0;

    if(strcmp(menu, "Ignore Server Tags") == 0) {
	toggle = 1;
	if(p->IsEntryChecked(x)) {    // Going to NOT checked
	    server.display()->ignoreServerTags = 0;
	}
	else {
	    server.display()->ignoreServerTags = 1;
	}
	tabs->rebuildTabs();
	fMain->MapSubwindows();
	
	// Map main frame
	fMain->MapWindow();
    }
    else if(strcmp(menu, "Print") == 0) {
	printf("print()\n");
    }
    else if (strcmp(menu, "Print All") == 0) {
	printf("printAll\n");
    }
    else if (strcmp(menu, "Update") == 0) {
	update();
    }
    else if (strcmp(menu, "Auto Update") == 0) {
	toggle = 1;
    }
    if(toggle) {
	if(p->IsEntryChecked(x)) p->UnCheckEntry(x);
	else p->CheckEntry(x);
    }
}

void JevpViewer::refreshTimerFired()
{
    LOG(NOTE, "timerFired");

    Int_t id = menu->input->GetEntry("Auto Update")->GetEntryId();
    if(menu->input->IsEntryChecked(id)) {
	update();
    }
}

void JevpViewer::update() {
    
    updateRunStatus();
    updateServerTags();
    updateCurrentPlot();

 
}

void JevpViewer::updateRunStatus() {
    RunStatus *rs = server.getRunStatus();
    
    int secs = time(NULL) - rs->timeOfLastChange;
    
    char winlab[120];
    sprintf(winlab, "%s:  Run #%d  (%s for %d seconds)","Live",rs->run, rs->status, secs);
    fMain->SetWindowName(winlab);
    
    delete rs;
}

void JevpViewer::updateServerTags() {
    server.send("getServerTags","");
    EvpMessage *msg = dynamic_cast<EvpMessage *>(server.receive());
    if(!msg) {
	LOG(CRIT, "Can't get server tags\n");
	exit(0);
    }

    const char *args = msg->getArgs();
    if(!serverTags || (strcmp(serverTags, args) != 0)) {

	LOG(NOTE, "Changing serverTags from %p to %p",  serverTags, args);
	
	if(serverTags) {
	    free(serverTags);
	}

	serverTags = (char *)malloc(strlen(args) + 1);
	strcpy(serverTags, args);

	server.display()->setServerTags(serverTags);
	server.display()->updateDisplayRoot();
	tabs->rebuildTabs();
    }

    delete msg;
} 

void JevpViewer::updateCurrentPlot() {
    CanvasFrame *frame = tabs->getCurrentContainer();
    if(!frame) {
	LOG(NOTE,"update: don't have a current frame");
	return;
    }
    frame->canvas->GetCanvas()->SetEditable(kTRUE);
    frame->plotInfo->DrawOnScreen(frame->canvas->GetCanvas(), server.getSocket(), server.display());
    frame->canvas->GetCanvas()->SetEditable(kFALSE);
}


CanvasFrame::CanvasFrame(const TGWindow *p, int id) : TGCompositeFrame(p, 50, 50) {
    SetBit(kNoContextMenu | kCannotPick);
    
    this->id = id;
    canvas = new TRootEmbeddedCanvas("DrawCanvas", this, 50, 50);
    canvas->SetBit(kNoContextMenu | kCannotPick);

    Disconnect();
    TCanvas *c = canvas->GetCanvas();
    c->SetBit(kNoContextMenu | kCannotPick);
    c->Connect("ProcessedEvent(Int_t,Int_t,Int_t,TObject*)", "CanvasFrame", this, "DoEvent(Int_t,Int_t,Int_t,TObject*");
    
    plotInfo = new JevpPlotInfo(id);
    
    AddFrame(canvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10,10,10,1) );
}

CanvasFrame::~CanvasFrame() {
    if(canvas) delete canvas;
}

void CanvasFrame::DoEvent(Int_t cmd, Int_t x, Int_t y, TObject *o) {
    TObject *p = (TObject *)gTQSender;

    static TGPopupMenu *menu = NULL;
    static int plotx;
    static int ploty;

    TCanvas *source_canvas = dynamic_cast<TCanvas *>(p);
    if(source_canvas) {
	
	switch(cmd) {
	case kButton3Down:
	    plotx = x;
	    ploty = y;
	    break;
	case kButton3Up:
	    break;
	default:
	    return;
	}

	int n = plotInfo->npads;
	int w = source_canvas->GetWw();
	int h = source_canvas->GetWh();
	
	int wide = plotInfo->getDisplay()->getIntParentProperty("wide");
	int deep = plotInfo->getDisplay()->getIntParentProperty("deep");
	
	int wk = plotx / (w/wide);
	int hk = ploty / (h/deep);
	
	int pn = wk + hk * wide + 1;
	
	if(pn < 1) pn = 1;
	if(pn > plotInfo->npads) pn = plotInfo->npads;
	JevpPlot *p = plotInfo->getPlotAtIdx(pn);
	if(!p) {
	    LOG(DBG,"plot is null");
	    return;
	}
	
	switch(cmd) {
	case kButton3Down:
	    {
		menu = new TGPopupMenu(canvas);
		
		char *name = p->GetPlotName();
		menu->AddEntry(name, 1);
		menu->AddSeparator();
		menu->AddEntry("Magnify", 2);
		
		menu->PlaceMenu(x,y,kFALSE,kFALSE);
	    }
	    break;

	case kButton3Up: 
	    {
		if(!menu) return;

		void *xx;
		void *&yy = xx;
		int id = menu->EndMenu(yy);
		delete menu;
		menu = NULL;
	
		if(id == 2) {
		    ZoomFrame *zf = new ZoomFrame(NULL, p);
		}
	    }
	    break;  
	}
    }
}



JevpPlotInfo::JevpPlotInfo(int combo_index) {
    this->combo_index = combo_index;
    jevpPlots = new THashTable();
    plotItems = new TList();
    npads = 0;
    cleanTime = 0;
    
    displayTab = NULL;
    server = NULL;
    canvas = NULL;
}

JevpPlotInfo::~JevpPlotInfo() {
    deleteItems();
    
    delete plotItems;
    delete jevpPlots;
}

void JevpPlotInfo::deleteItems() {
    THashTableIter nextp(jevpPlots);
    TListIter nexti(plotItems);
    
    TObject *o;

    while((o = nextp())) {
	LOG(NOTE, "Deleting an plot... %s can %d must %d",o->GetName(),o->TestBit(kCanDelete),o->TestBit(kMustCleanup));
	delete o;
    }
    
    while((o = nexti())) {
	LOG(NOTE,"Deleting an item... %s can %d must %d",o->GetName(),o->TestBit(kCanDelete),o->TestBit(kMustCleanup));
	delete o;
    }
    
    plotItems->Clear();
    jevpPlots->Clear();
}

void JevpPlotInfo::addJevpPlot(JevpPlot *plot) {
    plot->GetPlotName();
    jevpPlots->Add(plot);
}

void JevpPlotInfo::addPlotItem(TObject *item) {
    plotItems->Add(item);
}

JevpPlot *JevpPlotInfo::getJevpPlot(const char *name) {
    return (JevpPlot *)jevpPlots->FindObject(name);
}

void JevpPlotInfo::downloadPlot(const char *name) {
    RtsTimer_root clock;
    clock.record_time();

    // Ask server for plot...
    EvpMessage msg;
    msg.setCmd("getplot");
    msg.setArgs(name);
    TMessage mess(kMESS_OBJECT);
    mess.WriteObject(&msg);
    server->Send(mess);
    

    TMessage *rmsg;
    int ret = server->Recv(rmsg);
    
    double t1 = clock.record_time();
    
    if(ret == 0) {  // disconnect
	LOG(ERR,"Server disconnected?\n");
	return;
	//return NULL;
    }

    LOG(DBG, "Message class: %s",rmsg->GetClass()->GetName());
    if(strcmp(rmsg->GetClass()->GetName(), "EvpMessage") == 0) {
	// There was no valid object...
	EvpMessage *msg = (EvpMessage *)rmsg->ReadObject(rmsg->GetClass());
	LOG(ERR, "No valid plot for %s", name);
	
	delete msg;
	delete rmsg;
	return;
	//return NULL;
    }

    if(strcmp(rmsg->GetClass()->GetName(), "JevpPlot") == 0) {
	JevpPlot *plot = (JevpPlot *)rmsg->ReadObject(rmsg->GetClass());

	double t2 = clock.record_time() + t1;
	if(t2 > .25)
	    LOG("JEFF", "Download plot %s took %lf seconds (%lf for ethernet)",name, t2, t1);
	
	delete rmsg;
	
	//LOG("JEFF", "Got plot: %s", name);

	addJevpPlot(plot);
	return;
	//return plot;
    }

    LOG(ERR,"Invalid message type: (%s)\n",rmsg->GetClass()->GetName());
    delete rmsg;
    //return NULL;
}

void JevpPlotInfo::downloadAllPlots() {
    deleteItems();
    int nplots=0;

    RtsTimer_root clock;   
    clock.record_time();
  
    DisplayNode *ctab = displayTab;
    
    while(ctab) {
	nplots++;
       	downloadPlot(ctab->name);
	ctab = ctab->next;
    }

    double t = clock.record_time();
    LOG("JEFF", "Downloaded all plots[%d].  %d plots in %lf seconds", combo_index, nplots, t);
}


void JevpPlotInfo::drawEmptySpace()
{
    drawNoDataPresent("Empty Space");
}

void JevpPlotInfo::drawNoDataPresent(const char *name) {
    TText* t = new TText(0.5, 0.5, name);
    t->SetBit(kNoContextMenu | kCannotPick);

    addPlotItem(t);
    t->SetTextColor(3);
    t->SetTextAlign(22);
    t->Draw();
}

void JevpPlotInfo::drawCrossOfDeath(const char *name) {
    TLine* a = new TLine(0.,0.,1.,1.);
    a->SetBit(kNoContextMenu | kCannotPick);
    TLine* b = new TLine(0.,1.,1.,0.);
    b->SetBit(kNoContextMenu | kCannotPick);
    TText* t = new TText(0.5,0.5,name);
    t->SetBit(kNoContextMenu | kCannotPick);

    addPlotItem(a);
    addPlotItem(b);
    addPlotItem(t);

    a->SetLineColor(2);
    b->SetLineColor(2);
    t->SetTextColor(3);
    t->SetTextAlign(22);

    a->Draw();
    b->Draw();
    t->Draw();
}

void JevpPlotInfo::DrawOnScreen(TCanvas *c, TSocket *s, DisplayFile *display) {
    canvas = c;
    server = s;
    displayTab = display->getTab(combo_index);

    downloadAllPlots();

    //LOG("JEFF", "DrawOnScreen[%d] winid = %lu",combo_index, winId());
    //tcanvas->setUpdatesEnabled(false);
    //assert(displayTab->leaf);
    
  
    DisplayNode *ctab = displayTab;

    RtsTimer_root clock2;
    RtsTimer_root clock;   
    clock.record_time();
    
    int nplots = ctab->nSiblings() + 1;  
    int wide = ctab->getIntParentProperty("wide");
    int deep = ctab->getIntParentProperty("deep");
    int scaley = ctab->getIntParentProperty("scaley");
    double maxY = 0;

    // If scaling all plots the same, calculate the maxy
    //LOG("JEFF", "Scaley=%d",scaley);
    if(scaley>0) {
	while(ctab) {
	    
	    JevpPlot *plot = getJevpPlot(ctab->name);
	    
	    if(!plot) {
		continue;
	    }

	    
	    double my = plot->getMaxY();
	    
	    if(my > maxY) maxY = my;
	    
	    ctab = ctab->next;
	    
	}
    }
    
    //LOG("JEFF", "wide = %d deep = %d scaley = %d (%lf) nplots=%d", wide, deep, scaley, maxY, nplots);
    if(npads == 0) {
	canvas->Clear();
	canvas->Divide(wide, deep);
	npads = wide*deep;
    }

    

    ctab = displayTab;
    LOG(NOTE, "screenwidget prep: %lf", clock.record_time());

    for(int pad=1;pad <= wide*deep; pad++) {
	canvas->cd(pad);
	canvas->SetBit(kNoContextMenu);

	if(!ctab) {
	    drawEmptySpace();
	    continue;          // don't try to go to the next one!
	}
	
       	JevpPlot *plot = getJevpPlot(ctab->name);

	if(!plot) {
	    LOG(NOTE, "Didn't get plot %s", ctab->name);
	}
	else {
	    //LOG(NOTE, "%s: %p %d %d", ctab->name, plot, plot->needsdata, plot->isDataPresent());
	}
	if(plot && (!plot->needsdata || plot->isDataPresent())) {
	    
	    plot->draw();
	}
	else {
	    
	    char tmp[256];
	    sprintf(tmp, "No data for plot: %s", ctab->name);
	    drawNoDataPresent(tmp);
	}
	
	ctab = ctab->next;
    }
	
    LOG(NOTE, "screenwidget draw: %lf", clock.record_time());
    
    

    //canvas->setUpdatesEnabled(true);
    clock2.record_time();
    canvas->Resize();
    LOG(NOTE, "resize: %lf", clock2.record_time());
    canvas->Modified();
    LOG(NOTE, "mod: %lf", clock2.record_time());
    canvas->Update();
    LOG(NOTE, "upd: %lf", clock2.record_time());
    
    LOG(NOTE, "screenwidget update: %lf", clock.record_time());
    

    double t = clock.record_time();
    cleanTime = time(NULL);
    
    //LOG(NOTE, "Draw idx=%d. %d plots in %lf seconds\n", combo_index, nplots, t);
}

JevpPlot *JevpPlotInfo::getPlotAtIdx(int idx)
{  
    if(!displayTab) {
	return NULL;
    }

    DisplayNode *ctab = displayTab;
       
    int i=1;
    while(ctab) {
	if(i == idx) {
	    return getJevpPlot(ctab->name);
	}
	
	ctab = ctab->next;
	i++;
    }

    return NULL;
}


/*   Mouse events:

   kNoEvent       =  0,
   kButton1Down   =  1, kButton2Down   =  2, kButton3Down   =  3, kKeyDown  =  4,
   kButton1Up     = 11, kButton2Up     = 12, kButton3Up     = 13, kKeyUp    = 14,
   kButton1Motion = 21, kButton2Motion = 22, kButton3Motion = 23, kKeyPress = 24,
   kButton1Locate = 41, kButton2Locate = 42, kButton3Locate = 43, kESC      = 27,
   kMouseMotion   = 51, kMouseEnter    = 52, kMouseLeave    = 53,
   kButton1Double = 61, kButton2Double = 62, kButton3Double = 63
*/
