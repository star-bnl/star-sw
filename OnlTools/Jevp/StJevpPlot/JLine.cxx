#include "JLine.h"
#include "TPad.h"

ClassImp(JLine);

JLine::JLine() {
  ndcY = 1;   // NDC coordinates for Y
  ndcX = 0;   // user coordinates for X
}

JLine::JLine(Double_t x1, Double_t y1, Double_t x2, Double_t y2) : TLine(x1,y1,x2,y2) {
  ndcY = 1;
  ndcX = 0;
  jx[0] = x1;
  jx[1] = x2;
  jy[0] = y1;
  jy[1] = y2;
}

void JLine::SetNDC_x(int ndc) {
  ndcX = ndc;
}

void JLine::SetNDC_y(int ndc) {
  ndcY = ndc;
}

void JLine::SetX1(Double_t x) {
  jx[0] = x;
}

void JLine::SetX2(Double_t x) {
  jx[1] = x;
}

void JLine::SetY1(Double_t y) {
  jy[0] = y;
}

void JLine::SetY2(Double_t y) {
  jy[1] = y;
}

void JLine::SetXY(Double_t x1, Double_t y1, Double_t x2, Double_t y2)
{
  SetX1(x1);
  SetX2(x2);
  SetY1(y1);
  SetY2(y2);
}

void JLine::Draw(Option_t *option) {
  //  SetNDC(false);
  //gPad->AbsCoordinates(false);

  Double_t ux[2];
  Double_t uy[2];

  for(int i=0;i<2;i++) {
    ux[i] = jx[i];
    uy[i] = jy[i];
  }

  if(ndcX) {
    for(int i=0;i<2;i++) {
      ux[i] = gPad->PixeltoX(gPad->UtoPixel(jx[i]));
    }
  }

  if(ndcY) {
    for(int i=0;i<2;i++) {
      uy[i] = gPad->PixeltoY(gPad->VtoPixel(jy[i]) - gPad->GetWh());
    }
  }

  TLine::SetX1(ux[0]);
  TLine::SetX2(ux[1]);
  TLine::SetY1(uy[0]);
  TLine::SetY2(uy[1]);

  TLine::Draw(option);
}
