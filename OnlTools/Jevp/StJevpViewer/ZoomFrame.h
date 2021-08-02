#pragma once

#include <TROOT.h>
#include <RQ_OBJECT.h>
#include <TGFrame.h>
#include <TGWindow.h>
#include <TGCanvas.h>
#include <TCanvas.h>
#include <TRootEmbeddedCanvas.h>
#include <TGButton.h>

#include <Jevp/StJevpPlot/JevpPlot.h>

class ZoomFrame : public TGMainFrame {
    RQ_OBJECT("ZoomFrame");

 public:
    TGCanvas *tgcanvas;           // the window canvas
    TGCompositeFrame *frame;      // the static sized frame
    TRootEmbeddedCanvas *canvas;  // the inner canvas
    
    TGHorizontalFrame *hframe;
    TGTextButton *exitb;
    TGTextButton *expandb;
    TGTextButton *contractb;

    JevpPlot *plotclone;

    ZoomFrame(TGWindow *p, JevpPlot *plot);
    virtual ~ZoomFrame();

    void exit();
    void expand();
    void contract();

    ClassDef(ZoomFrame, 0);
};
