#include "ZoomFrame.h"
#include <TF1.h>

ClassImp(ZoomFrame);

ZoomFrame::~ZoomFrame() {
    if(hframe) delete hframe;
    if(exitb) delete exitb;
    if(expandb) delete expandb;
    if(contractb) delete contractb;
    if(canvas) delete canvas;
    if(tgcanvas) delete tgcanvas;
    if(frame) delete frame;
    if(plotclone) delete plotclone;
}

ZoomFrame::ZoomFrame(TGWindow *p,  JevpPlot *plot) : TGMainFrame(p, 1200, 900) {
    plotclone = NULL;

    tgcanvas = new TGCanvas(this, 1200, 900);
    
    frame = new TGCompositeFrame(tgcanvas->GetViewPort(), 1200, 900);
    frame->ChangeOptions(frame->GetOptions()|kFixedSize);

    canvas = new TRootEmbeddedCanvas("boo", frame, 1200, 900);
    frame->AddFrame(canvas, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));

    tgcanvas->SetContainer(frame);     

    canvas->GetCanvas()->cd();
  
    plotclone = (JevpPlot *)plot->Clone();
    plotclone->draw();

    AddFrame(tgcanvas, new TGLayoutHints(kLHintsExpandY | kLHintsExpandX));

    hframe = new TGHorizontalFrame(this, 200, 40);
    exitb = new TGTextButton(hframe, "Exit");
    expandb = new TGTextButton(hframe, "+");
    contractb = new TGTextButton(hframe, "-");
    exitb->Connect("Clicked()", "ZoomFrame", this, "exit()");
    expandb->Connect("Clicked()", "ZoomFrame", this, "expand()");
    contractb->Connect("Clicked()", "ZoomFrame", this, "contract()");
    hframe->AddFrame(contractb, new TGLayoutHints(kLHintsCenterX, 5,5,3,4));
    hframe->AddFrame(expandb, new TGLayoutHints(kLHintsCenterX, 5,5,3,4));
    hframe->AddFrame(exitb, new TGLayoutHints(kLHintsCenterX, 5,5,3,4));
    AddFrame(hframe, new TGLayoutHints(kLHintsCenterX, 2,2,2,2));
        
    Resize();
    MapSubwindows();
    MapWindow();

  
}

void ZoomFrame::exit() {
    CloseWindow();
}

void ZoomFrame::expand() {
    TGDimension dim = frame->GetSize();
      
    dim.fHeight *= 2;
    dim.fWidth *= 2;
    if((dim.fHeight > 8000) || (dim.fWidth > 8000)) {
	dim.fHeight = 8000;
	dim.fWidth = 8000;
    }
 
    frame->SetSize(dim);
    Resize();
}

void ZoomFrame::contract() {
    TGDimension dim = frame->GetSize();

    dim.fHeight /= 2;
    dim.fWidth /= 2;

    frame->SetSize(dim);
    Resize();
}
