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
#include "TGTab2.h"


class MyMainFrame : public TObject {
    RQ_OBJECT("MyMainFrame")
    private:
    TGMainFrame         *fMain;
    TRootEmbeddedCanvas *fEcanvas;

    TGPopupMenu *fileMenu;
    TGPopupMenu *aaaMenu;

    TGButton *update;
    TGButton *autoupdate;

    TGTab2 *tabs;

public:
    MyMainFrame(const TGWindow *p,UInt_t w,UInt_t h);
    virtual ~MyMainFrame();

    void buildTabs1();
    void buildTabs2();

    void DoTab(Int_t);
    void doMenu(Int_t);
    void DoButton();

    static void example();
    ClassDef(MyMainFrame, 0);
};


class DrawFrame : public TGCompositeFrame {
    RQ_OBJECT("DrawFrame")

 private:
    int id;
 public:
    TF1 *f1;
    DrawFrame(const TGWindow *p, UInt_t w, UInt_t h, int id);
    void DoDraw();
    TRootEmbeddedCanvas *canvas;
    ClassDef(DrawFrame, 0);
};
