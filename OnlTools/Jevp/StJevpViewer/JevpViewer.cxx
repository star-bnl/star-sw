#include "Jevp/StJevpViewer/JevpViewer.h"



#include <stdio.h>

#include <TROOT.h>
#include <TStyle.h>
#include "Jevp/StJevpViewer/TGTab2.h"
#include <TGMenu.h>
#include <TApplication.h>

#include "EthClient.h"
#include "ZoomFrame.h"
#include <Jevp/StJevpPlot/JevpPlot.h>
#include <Jevp/StJevpPlot/EvpMessage.h>

#include <RTS/include/SUNRT/clockClass.h>

//#include <assert.h>

EthClient *gshifteth;

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
    
    if(parent->initauto) input->CheckEntry(mid);
   
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

void TabHelper::buildTabs(TGTab2 *mainT) {

  // We don't care if we are connecting or disconnected, 
  // just what the present state actually is!
  mainTabs = mainT;

  //LOG("JEFF", "start building tabs");
 
  shiftTabs = new TGTab2(mainTabs, 1200, 900);
  mainTabs->AddTab("shift", shiftTabs);
  if(parent->eth.shift.connected()) {
    fillTab(shiftTabs, &parent->eth.shift, 1);
    parent->eth.shift.tabs_valid = 1;
  }
  else {
    //if(parent->eth.shift.tabs_valid != 1) {
      parent->eth.shift.tabs_valid = 1;
      
      DummyFrame *f = new DummyFrame(shiftTabs, 1200, 900);
      LOG("JEFF", "Dummy shift: %p", f);
      TCanvas *c = f->canvas->GetCanvas();
      
      TText *t = new TText(0.5, 0.5, "Shift JevpServer not connected!");
      t->SetBit(kNoContextMenu | kCannotPick);
      //addPlotItem(t);
      t->SetTextColor(3);
      t->SetTextAlign(22);
      t->Draw();
    
      shiftTabs->AddTab("No connection", f);
      // }
  }

  l4Tabs = new TGTab2(mainTabs, 1200, 900);
  mainTabs->AddTab("l4", l4Tabs);
  if(parent->eth.l4.connected()) {
    fillTab(l4Tabs, &parent->eth.l4, 1);
    parent->eth.shift.tabs_valid = 1;
  }
  else {
    //if(parent->eth.l4.tabs_valid != 1) {
      parent->eth.l4.tabs_valid = 1;
      DummyFrame *f = new DummyFrame(l4Tabs, 1200, 900);
      //LOG("JEFF", "Dummy l4 = %p", f);
      TCanvas *c = f->canvas->GetCanvas();
      
      TText *t = new TText(0.5, 0.5, "L4 JevpServer not connected");
      t->SetBit(kNoContextMenu | kCannotPick);
      //addPlotItem(t);
      t->SetTextColor(3);
      t->SetTextAlign(22);
      t->Draw();
      
      l4Tabs->AddTab("No connection", f);
      // Add invalid tab
      //}
  }

  //  LOG("JEFF", "done filling tabs");

  // end of building tabs...

}

void TabHelper::rebuildTabs() {
  //LOG("JEFF", "rebuildtabs()");
 
  deleteTab(mainTabs, 0);    // Delete subtabs, but not this tab...
  buildTabs(mainTabs);
  mainTabs->SetTab(0);

  //LOG("JEFF", "Set tab %p", parent);
  //parent->update();
  //LOG("JEFF", "updated");
 
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

void TabHelper::fillTab(TGTab2 *tab, EthClient *ethclient, UInt_t idx)
{
  
  //  LOG("JEFF", "idx=%d ", idx);
  DisplayNode *mynode = ethclient->getTabDisplayBranch(idx);
  //LOG("JEFF", "getTabDisplayBranch: %s", mynode ? mynode->name : "none");
  if(!mynode) return;
        
  // First get my child and my tabname
  u_int child_idx = ethclient->display()->getTabChildIdx(idx);
  DisplayNode *childnode = ethclient->display()->getTab(child_idx);
  if(!childnode) {
    LOG("JEFF", "NULL in filltab?");
    return;        // no children!   This is a bug right?
  }
 
  if(!childnode->leaf) {   // It's another tab!
    TGTab2 *childtab = new TGTab2(tab, 1200, 900);
    //LOG("JEFF", "adding tab=%s", mynode->name);
    tab->AddTab(mynode->name, childtab);
    fillTab(childtab, ethclient, child_idx);
    childtab->Connect("Selected(Int_t)", "TabHelper", this, "tabSelected(Int_t)");
  }
  else {                   // It's a histo!
    //LOG("JEFF", "adding name=%s", mynode->name);
    CanvasFrame *frame = new CanvasFrame(tab, ethclient, child_idx);
    tab->AddTab(mynode->name, frame);
  }

  u_int next_idx = ethclient->display()->getTabNextIdx(idx);
  fillTab(tab, ethclient, next_idx);
}

TGCompositeFrame *TabHelper::getCurrentContainer() {
  TGTab2 *tab = mainTabs;
  
  while(tab) {
    TObject *ptr = (TObject *)tab->GetCurrentContainer();
      
    TGCompositeFrame *frame = dynamic_cast<TGCompositeFrame *>(ptr);
    DummyFrame *df = dynamic_cast<DummyFrame *>(ptr);
    CanvasFrame *cf = dynamic_cast<CanvasFrame *>(ptr);
    
    if(df || cf) {
      return frame;
    }
    tab = dynamic_cast<TGTab2 *>(ptr);
  }
    
  return NULL;
}

void TabHelper::tabSelected(Int_t id) {
  //  LOG("JEFF", "Selected tab %d\n", id);
  parent->update();
  //LOG("JEFF", "Selected tab %d done", id);
}

void JevpViewer::parseArgs(char *args) {
  SERVERPORT = 8499;
  initauto = 1;
  char myargs[512];
  strcpy(myargs, args);
  
  char *tok = strtok(myargs, " ");
  while(tok) {
    if(strcmp(tok, "-padd") == 0) {
      tok = strtok(NULL, " ");
      SERVERPORT += atoi(tok);
    }
    if(strcmp(tok, "-noupdate") == 0) {
      initauto = 0;
    }
    tok = strtok(NULL, " ");
  }

  LOG("JEFF", "serverport = %d", SERVERPORT);
}

JevpViewer::JevpViewer(const TGWindow *p,UInt_t w,UInt_t h, char *args) {
  // Create a main frame
  parseArgs(args);
  
  
  gROOT->SetEditHistograms(kFALSE);
  gROOT->SetBit(kNoContextMenu | kCannotPick);
  gStyle->SetBit(kNoContextMenu | kCannotPick);

  eth.shift.serverTags = NULL;
  //eth.shift.connectToServer("evp.starp.bnl.gov", SERVERPORT);
  eth.shift.connectToServer("evp.starp.bnl.gov", SERVERPORT);
  if(eth.shift.connected()) {
    LOG("JEFF", "Shift server connected!");
  }
  else {
    LOG("JEFF", "Shift server not connected");
  }

  eth.l4.connectToServer("l4evp.starp.bnl.gov", SERVERPORT);
  if(eth.l4.connected()) {
    LOG("JEFF", "l4 server connected!");
  }
  else {
    LOG("JEFF", "l4 server not connected");
  }

  eth.shift.tabs_valid = 0;
  if(eth.shift.connected()) {
    eth.shift.readDisplayFromServer("shift");
    eth.shift.display()->setServerTags("");
    eth.shift.display()->updateDisplayRoot();
    eth.shift.display()->ignoreServerTags = 0;
    //LOG("JEFF", "shift updated");
  }

  eth.l4.tabs_valid = 0;
  if(eth.l4.connected()) {
    //    LOG("JEFF", "abab");
    eth.l4.readDisplayFromServer("l4");
    eth.l4.display()->setServerTags("");
    eth.l4.display()->updateDisplayRoot();
    eth.l4.display()->ignoreServerTags = 0;
  }

  //server.display()->ignoreServerTags = 0;
  //server.display()->dump();
  //printf("\n\n\n---------------------\n");
   
  fMain = new TGMainFrame(p,w,h);
  fMain->Connect("CloseWindow()", "JevpViewer", this, "ExitViewer()");
  fMain->SetBit(kNoContextMenu | kCannotPick);

  menu = new MenuHelper(this);
  menu->buildMenu(fMain);
   
  //LOG("JEFF", "Done with menu");

 
  tabs = new TabHelper(this);

  //LOG("JEFF", "did tabs");
  TGTab2 *mainTabs = new TGTab2(fMain, 1200, 900);
   
  tabs->buildTabs(mainTabs);
  //LOG("JEFF", "built tabs");
  mainTabs->Connect("Selected(Int_t)", "TabHelper", tabs, "tabSelected(Int_t)");
  fMain->AddFrame(mainTabs, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0,0,0,0));

  //LOG("JEFF", "added tabs");

  // Set a name to the main frame
  fMain->SetWindowName("Simple Example");
    
  // Map all subwindows of main frame
  fMain->MapSubwindows();
    
  // Initialize the layout algorithm
  fMain->Resize(fMain->GetDefaultSize());
    
  // Map main frame
  fMain->MapWindow();
    
  //update();
  //return;

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

void JevpViewer::entryPoint(char *args) {
    // Popup the GUI...
    
    rtsLogLevel((char *)WARN);
    rtsLogOutput(RTS_LOG_STDERR);

    //LOG("JEFF", "args: %s", args ? args : "none");

    new JevpViewer(gClient->GetRoot(),1200,900, args);
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
	  LOG("JEFF", "obey server tags");
	  if(eth.shift.connected()) {
	    eth.shift.display()->ignoreServerTags = 0;
	    eth.shift.display()->updateDisplayRoot();
	  }
	  if(eth.l4.connected()) {
	    eth.l4.display()->ignoreServerTags = 0;
	    eth.l4.display()->updateDisplayRoot();
	  }
	}
	else {
	  LOG("JEFF", "ignore server tags now");
	  if(eth.shift.connected()) {
	    eth.shift.display()->ignoreServerTags = 1;
	    eth.shift.display()->updateDisplayRoot();
	  }
	  if(eth.l4.connected()) {
	    eth.l4.display()->ignoreServerTags = 1;
	    eth.l4.display()->updateDisplayRoot();
	  }
	}

	LOG("JEFF", "rebuild tabs");
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
  //LOG("JEFF", "timerFired");

  Int_t id = menu->input->GetEntry("Auto Update")->GetEntryId();
  if(menu->input->IsEntryChecked(id)) {
    update();
  }
}

void JevpViewer::update() {
  static int updating = 0;

  if(updating) {
    LOG("JEFF", "Shouldn't update during update()");
    return;
  }
  updating = 1;

  //LOG("JEFF", "update():   %d %d    %d %d", eth.shift.connected(), eth.shift.tabs_valid, eth.l4.connected(), eth.l4.tabs_valid);
  updateRunStatus();
  //LOG("JEFF", "updated status  %d %d    %d %d", eth.shift.connected(), eth.shift.tabs_valid, eth.l4.connected(), eth.l4.tabs_valid);
  
  updateServerTags();

  //LOG("JEFF", "update tags  %d %d    %d %d", eth.shift.connected(), eth.shift.tabs_valid, eth.l4.connected(), eth.l4.tabs_valid);
  updateCurrentPlot();
  //LOG("JEFF", "update plot  %d %d    %d %d", eth.shift.connected(), eth.shift.tabs_valid, eth.l4.connected(), eth.l4.tabs_valid);

  updating = 0;
}

void JevpViewer::updateRunStatus() {
  //LOG("JEFF", "updateing run status");

  if(!eth.shift.connected()) {
    eth.shift.connectToServer("evp.starp.bnl.gov", SERVERPORT);
    if(eth.shift.connected()) {
      LOG("JEFF", "shift server just connected!");
      eth.shift.readDisplayFromServer("shift");
      eth.shift.display()->setServerTags("");
      eth.shift.display()->updateDisplayRoot();
    }
  }

  if(!eth.l4.connected()) {
    eth.l4.connectToServer("l4evp.starp.bnl.gov", SERVERPORT);
    if(eth.l4.connected()) {
      LOG("JEFF", "l4 server just connected!");
      eth.l4.readDisplayFromServer("l4");
      eth.l4.display()->setServerTags("");
      eth.l4.display()->updateDisplayRoot();
    }
  }


  RunStatus *shift_rs = eth.shift.getRunStatus();
  RunStatus *l4_rs = eth.l4.getRunStatus();

  //  LOG("JEFF", "shift (conn = %d stat = %d %s)   l4 (conn = %d stat = %d %s)",
  //  eth.shift.connected(), eth.shift.tabs_valid, shift_rs->status, eth.l4.connected(), eth.l4.tabs_valid, l4_rs->status);
  
  

  int secs = time(NULL) - shift_rs->timeOfLastChange;
  
  char winlab[120];
  sprintf(winlab, "%s:  Run #%d  (%s for %d seconds)","Live",shift_rs->run, shift_rs->status, secs);
  fMain->SetWindowName(winlab);

  //LOG("JEFF", "a %p", shift_rs);
  delete shift_rs;
  //LOG("JEFF", "b");
  delete l4_rs;
  //LOG("JEFF", "c");
}

void JevpViewer::updateServerTags() {
  // updateServerTags automatically handles unconnected/disconnecting/connecting nodes...
  int update=0;

  update += eth.shift.updateServerTags();
  update += eth.l4.updateServerTags();
  
  if(update) {
    //LOG("JEFF", "rebuildTabs()");
    tabs->rebuildTabs();
  }
} 

void JevpViewer::updateCurrentPlot() {
  //LOG("JEFF", "a");
  TGCompositeFrame *container = tabs->getCurrentContainer();
  
  //  LOG("JEFF", "container = %p", container);
  
  DummyFrame *df = dynamic_cast<DummyFrame *>(container);
  if(df) {
    LOG("JEFF", "updating dummy frame, nothing to do!");
    return;
  }

  CanvasFrame *frame = dynamic_cast<CanvasFrame *>(container);
  if(!frame) {
    LOG("JEFF","updateCurrentPlot(): don't have a current frame");
    return;
  }
  
  frame->canvas->GetCanvas()->SetEditable(kTRUE);
  //  LOG("JEFF", "a frame->id=%d c=%d s=%d pi=%p", frame->plotInfo->combo_index, eth.shift.connected(), eth.shift.tabs_valid, frame->plotInfo);

  if(frame->plotInfo->ethclient->connected()) {
    frame->plotInfo->DrawOnScreen();
    frame->canvas->GetCanvas()->SetEditable(kFALSE);
  }
  else {
    LOG("JEFF", "updateCurrentPlot but current plot not connected");
  }
}

CanvasFrame::CanvasFrame(const TGWindow *p, EthClient *ethclient, int id) : TGCompositeFrame(p, 50, 50) {
    SetBit(kNoContextMenu | kCannotPick);
    
    this->id = id;
    canvas = new TRootEmbeddedCanvas("DrawCanvas", this, 50, 50);
    canvas->SetBit(kNoContextMenu | kCannotPick);

    Disconnect();
    TCanvas *c = canvas->GetCanvas();
    c->SetBit(kNoContextMenu | kCannotPick);
    c->Connect("ProcessedEvent(Int_t,Int_t,Int_t,TObject*)", "CanvasFrame", this, "DoEvent(Int_t,Int_t,Int_t,TObject*");
    
    plotInfo = new JevpPlotInfo(this, ethclient, id);
    
    AddFrame(canvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10,10,10,1) );
}

CanvasFrame::~CanvasFrame() {
  //LOG("JEFF", "Deleting canvas frame");
    if(canvas) delete canvas;
}

void CanvasFrame::DoEvent(Int_t cmd, Int_t x, Int_t y, TObject *o) {
  
  if(cmd == kButton3Down) 
    LOG("JEFF", "CanvasFrame: Doing event cmd=%d x=%d y=%d", cmd, x, y);

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

    //int n = plotInfo->npads;
    int w = source_canvas->GetWw();
    int h = source_canvas->GetWh();

    
    int wide = plotInfo->getDisplay()->getIntParentProperty("wide");
    int deep = plotInfo->getDisplay()->getIntParentProperty("deep");
	
    int wk = plotx / (w/wide);
    int hk = ploty / (h/deep);
	
    int pn = wk + hk * wide + 1;
	
    LOG("JEFF", "pn %d %d [id =%d  pi id = %d", pn, plotInfo->npads, id, plotInfo->combo_index);

    if(pn < 1) pn = 1;
    if(pn > plotInfo->npads) pn = plotInfo->npads;
    JevpPlot *p = plotInfo->getPlotAtIdx(pn);
    if(!p) {
      LOG("JEFF","plot is null");
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



JevpPlotInfo::JevpPlotInfo(CanvasFrame *p, EthClient *ethclient, int combo_index) {
  this->myCanvasFrame = p;
  this->ethclient = ethclient;
  this->combo_index = combo_index;
  jevpPlots = new THashTable();
  plotItems = new TList();
  npads = 0;
  cleanTime = 0;
  
  //displayTab = NULL;
  //server = NULL;
  //canvas = NULL;
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
    TSocket *sock = ethclient->getSocket();
    sock->Send(mess);
    

    TMessage *rmsg;
    int ret = sock->Recv(rmsg);
    
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
    DisplayNode *ctab = ethclient->getTabDisplayLeaf(combo_index);
    //LOG("JEFF", "getTabDisplayLeaf: %s", ctab ? ctab->name : "none");

    while(ctab) {
	nplots++;
       	downloadPlot(ctab->name);
	ctab = ctab->next;
    }

    double t = clock.record_time();
    //LOG("JEFF", "Downloaded all plots[%d].  %d plots in %lf seconds", combo_index, nplots, t);
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

void JevpPlotInfo::DrawOnScreen() {
  TCanvas *canvas = myCanvasFrame->canvas->GetCanvas();
  //LOG("JEFF", "dap");
  downloadAllPlots();
  //LOG("JEFF", "dap done");
  //LOG("JEFF", "DrawOnScreen[%d] winid = %lu",combo_index, winId());
  //tcanvas->setUpdatesEnabled(false);
  //assert(displayTab->leaf);
    
  
  DisplayNode *ctab = ethclient->getTabDisplayLeaf(combo_index);
  //LOG("JEFF", "getTabDisplayLeaf:  %s : %d (%p - %s)", ctab ? ctab->name : "none", combo_index, ethclient, ethclient == gshifteth ? "shift" : "l4");

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

    

  ctab =  ethclient->getTabDisplayLeaf(combo_index);
  //    LOG("JEFF", "getTabDisplayLeaf: %s", ctab ? ctab->name : "none");

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
  DisplayNode *n = ethclient->getTabDisplayBranchOrLeaf(combo_index);
  
  //LOG("JEFF", "getTagDisplayLeaf %d %s leaf=%d", idx, n ? n->name : "none", n ? n->leaf : -1);
  
  DisplayNode *ctab = ethclient->getTabDisplayLeaf(combo_index);
  //LOG("JEFF", "getTabDisplayLeaf: %s", ctab ? ctab->name : "none");
  
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


DummyFrame::DummyFrame(const TGWindow *p, UInt_t w, UInt_t h) : TGCompositeFrame(p, w, h) {
  canvas = new TRootEmbeddedCanvas("DummyCanvas", this, w, h);
  AddFrame(canvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10, 1));
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
