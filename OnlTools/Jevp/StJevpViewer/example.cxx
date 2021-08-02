
// example.C

#include "example.h"
#include <stdio.h>
#include "Jevp/StJevpViewer/TGTab2.h"
#include <TGMenu.h>

#include "EthClient.h"
#include <Jevp/StJevpPlot/JevpPlot.h>
#include <Jevp/StJevpPlot/EvpMessage.h>

ClassImp(MyMainFrame);

MyMainFrame::MyMainFrame(const TGWindow *p,UInt_t w,UInt_t h) {
   // Create a main frame
   fMain = new TGMainFrame(p,w,h);
   tabs = NULL;

   // Add some menu's
   /*
   fileMenu = new TGPopupMenu(gClient->GetRoot());
   fileMenu->AddEntry("Hi", 1);
   fileMenu->AddEntry("boo", 2);
   fileMenu->AddEntry("ccc", 3);
   fileMenu->AddSeparator();
   fileMenu->AddEntry("ddd", 4);
   
   aaaMenu = new TGPopupMenu(gClient->GetRoot());
   aaaMenu->AddEntry("aaa", 11);
   
   TGLayoutHints *lh = new TGLayoutHints(kLHintsTop|kLHintsLeft,0,4,0,0);
   
   TGMenuBar *menubar = new TGMenuBar(fMain, 100,20, kHorizontalFrame);
   menubar->AddPopup("FILE", fileMenu, lh);
   menubar->AddPopup("aaa", aaaMenu, lh);

   fMain->AddFrame(menubar, new TGLayoutHints(kLHintsTop|kLHintsLeft|kLHintsExpandX,0,0,1,1));
   
   fileMenu->Connect("Activated(Int_t)", "MyMainFrame", this, "doMenu(Int_t)");
   aaaMenu->Connect("Activated(Int_t)", "MyMainFrame", this, "doMenu(Int_t)");
   */

   // Add buttons...
   /*
   TGCompositeFrame *buttonframe = new TGCompositeFrame(fMain, 2000, 20, kHorizontalFrame);
   //buttonframe->SetLayoutManager(new TGMatrixLayout(buttonframe, 1, 2));
   update = new TGTextButton(buttonframe, "update", 1);
   //update->Resize(600, update->GetDefaultHeight());
   
   autoupdate = new TGTextButton(buttonframe, "auto update", 2);
   //autoupdate->Resize(600, autoupdate->GetDefaultHeight());
   update->Connect("Clicked()", "MyMainFrame", this, "DoButton()");
   autoupdate->Connect("Clicked()", "MyMainFrame", this, "DoButton()");
   autoupdate->AllowStayDown(kTRUE);
   buttonframe->AddFrame(update, new TGLayoutHints(kLHintsLeft|kLHintsExpandX));
   buttonframe->AddFrame(autoupdate, new TGLayoutHints(kLHintsLeft | kLHintsExpandX));
   fMain->AddFrame(buttonframe, new TGLayoutHints(kLHintsTop|kLHintsLeft));
   */

   buildTabs1();

   // Set a name to the main frame
   fMain->SetWindowName("Simple Example");

   // Map all subwindows of main frame
   fMain->MapSubwindows();

   // Initialize the layout algorithm
   fMain->Resize(fMain->GetDefaultSize());

   // Map main frame
   fMain->MapWindow();

   
   //
   /*
   EthClient server;
   server.connectToServer("evp.starp.bnl.gov", 8499);
  

   server.send("getplot", "daq_h155_time_size_2min");
   
   TObject *o = server.receive();
   JevpPlot *plot = dynamic_cast<JevpPlot *>(o);
   if(plot) {
       printf("Printing plot\n");
       TCanvas *fCanvas = frames[5]->canvas->GetCanvas();
       fCanvas->cd();
       plot->draw();   
       fCanvas->Update();
   }
   else {
       printf("No plot\n");
       EvpMessage *m = dynamic_cast<EvpMessage *>(o);
       printf("%p %p", o, m);
   }
   */
}

void MyMainFrame::buildTabs1() {
    // Add the tabs...   
    
    if(tabs) {
	fMain->RemoveFrame(tabs);
	delete tabs;
    }


    tabs = new TGTab2(fMain, 300, 600);
    tabs->Connect("Selected(Int_t)", "MyMainFrame",this, "DoTab(Int_t)");
    TGCompositeFrame *tf;

    DrawFrame *frames[40];

    for(int i=0;i<40;i++) {
	char name[100];
	sprintf(name, "tab %d", i);
	if(i == 15)
	    sprintf(name, "this is a long tab boo booooooo bob");

	tf = new DrawFrame(tabs, 400, 800, i);     
	tabs->AddTab(name, tf);
	frames[i] = (DrawFrame *)tf;
    }

    fMain->AddFrame(tabs, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0,0,0,0));
   // Map all subwindows of main frame
    fMain->MapSubwindows();

    // Initialize the layout algorithm
    fMain->Resize(fMain->GetDefaultSize());

    // Map main frame
    fMain->MapWindow();

}

void MyMainFrame::buildTabs2()
{
    buildTabs1();
}

MyMainFrame::~MyMainFrame() {
   // Clean up used widgets: frames, buttons, layout hints
   fMain->Cleanup();
   delete fMain;
}

void MyMainFrame::example() {
   // Popup the GUI...
   new MyMainFrame(gClient->GetRoot(),200,200);
}

void MyMainFrame::DoTab(Int_t x) {
    printf("Tab %d selected\n", x);
}

DrawFrame::DrawFrame(const TGWindow *p, UInt_t w, UInt_t h, int id) : TGCompositeFrame(p, w, h) {
    
    this->id = id;
    canvas = new TRootEmbeddedCanvas("DrawCanvas", this, w, h);
    
    AddFrame(canvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10,10,10,1) );

    // Create a horizontal frame widget with buttons
    TGHorizontalFrame *hframe = new TGHorizontalFrame(this,200,40);
    TGTextButton *draw = new TGTextButton(hframe,"&Draw");

    draw->Connect("Clicked()", "DrawFrame", this, "DoDraw()");

    hframe->AddFrame(draw, new TGLayoutHints(kLHintsCenterX,
					     5,5,3,4));
    TGTextButton *exit = new TGTextButton(hframe,"&Exitit",
					  "gApplication->Terminate(0)");
    hframe->AddFrame(exit, new TGLayoutHints(kLHintsCenterX,
					     5,5,3,4));
    AddFrame(hframe, new TGLayoutHints(kLHintsCenterX,
				       2,2,2,2));
    
    f1 = NULL;
    // Mapping done by super class....
}

void DrawFrame::DoDraw() {
    // Draws function graphics in randomly chosen interval
    char z[10];
    sprintf(z, "h_%d",id);

    if(f1) {
	delete f1;
    }
    
 
    TCanvas *fCanvas = canvas->GetCanvas();

    fCanvas->cd();
    f1 = new TF1(z,"sin(x)/x",0,gRandom->Rndm()*10);
    f1->SetLineWidth(3);
    f1->Draw();
    
    fCanvas->Update();
   
   
    printf("id=%d selected\n",id);
}

void MyMainFrame::DoButton() {

    TGTextButton *b = (TGTextButton *)gTQSender;

    printf("Button %s\n",b->GetText()->Data());

    Bool_t x = b->IsDown();
    if(x) {
	printf("On!\n");
    }
    else  {
	printf("Off!\n");
    }

    if(strcmp(b->GetText()->Data(), "update") == 0) {
	printf("Update!");
	tabs->RemoveTab(0);
    }
}

void MyMainFrame::doMenu(Int_t x) {

    TGPopupMenu *p = (TGPopupMenu *)gTQSender;

    printf("Menu %d\n", x);

    if(!p->IsEntryChecked(x))
	p->CheckEntry(x);
    else
	p->UnCheckEntry(x);
}
