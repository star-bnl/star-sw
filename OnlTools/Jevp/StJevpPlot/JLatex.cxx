#include "JLatex.h"
#include "TPad.h"
#include <rtsLog.h>

ClassImp(JLatex);

JLatex::JLatex() {
    SetBit(kNoContextMenu | kCannotPick);
    ndcY = 1;   // NDC coordinates for Y
    ndcX = 0;   // user coordinates for X
}

JLatex::JLatex(Double_t x, Double_t y, const char *text) : TLatex(x,y,text) {
    SetBit(kNoContextMenu | kCannotPick);
    ndcY = 1;
    ndcX = 0;
    jx = x;
    jy = y;
}

JLatex::JLatex(JLatex &l) : TLatex(l) {
    SetBit(kNoContextMenu | kCannotPick);
    ndcY = l.ndcY;
    ndcX = l.ndcX;
    jx = l.jx;
    jy = l.jy;

    LOG(DBG, "Copy constructor for JLatex");
}

void JLatex::SetNDC_x(int ndc) {
  ndcX = ndc;
}

void JLatex::SetNDC_y(int ndc) {
  ndcY = ndc;
}

void JLatex::SetX(Double_t x) {
  jx = x;
}

void JLatex::SetY(Double_t y) {
  jy = y;
}

void JLatex::Draw(Option_t *option) {
  SetNDC(false);
  //gPad->AbsCoordinates(false);

  Double_t ux = jx;

  if(ndcX) {
    ux = gPad->PixeltoX(gPad->UtoPixel(jx));
  }

  Double_t uy = jy;

  if(ndcY) {
    uy = gPad->PixeltoY(gPad->VtoPixel(jy) - gPad->GetWh());
  }

  TLatex::SetX(ux);
  TLatex::SetY(uy);
  
  TLatex::Draw(option);
}
