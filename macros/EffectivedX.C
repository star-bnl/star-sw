/*
  .L EffectivedX.C+
  .x bfc.C(0)
  .x Run1Ev.C
  StTpcRSMaker *trs = (StTpcRSMaker *) chain->Maker("TpcRS");
  TF1 *itpx = trs->GetChargeFraction(0,1);
  TF1 *otpx = trs->GetChargeFraction(1,1);
  TF1 *itpc = trs->GetChargeFraction(0,20);
  SetIO(itpx,otpx,itpc);
  TF1 *inner = EffectivedX(0); // inner->Integral(-5,5) = 1.1594;
  TF1 *outer = EffectivedX(1); // outer->Integral(-5,5) = 1.9542;
  TF1 *itpcX = EffectivedX(2); // itpcX->Integral(-5,5) = 1.1439;
  
*/

#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TF1.h"
#endif
static TF1 *ChargeFraction[3] = {0};
//________________________________________________________________________________
void SetIO(TF1 *i = 0 , TF1 *o = 0, TF1 *itpc = 0) {
  ChargeFraction[0] = i;
  ChargeFraction[1] = o;
  ChargeFraction[2] = itpc;
}
//________________________________________________________________________________
Double_t dXFunc(Double_t *x, Double_t *p) {
  static Double_t w = 0.4; // anode wire spacing
  Double_t rMidOfPad = p[0];
  Double_t rMinW = p[1];
  Int_t    io = p[2];
  if (io < 0 || io > 2) return 0;
  if (! ChargeFraction[io]) return 0;
  Double_t   dX = rMidOfPad + x[0] - rMinW;
  Double_t iwire = TMath::Nint(dX/w);
  Double_t rOnWire = rMinW + w*iwire;
  Double_t xOff  = rOnWire - rMidOfPad;
  Double_t val = ChargeFraction[io]->Eval(xOff);
  return val;
}
//________________________________________________________________________________
TF1* EffectivedX(Int_t InOut = 0) {
  TF1 *f = 0;
  static Double_t rMinIW =  53.2; // 1-st anode wire radius
  static Double_t rMinOW = 122.795;  
  if (InOut == 0) {
    Double_t rMinIP =  60.0; // the center of the first pad
    f = new TF1("EffdXInner",dXFunc,-5,5,3);
    f->SetParameters(rMinIP,rMinIW,0);
  } else if (InOut == 1) {
    Double_t rMinOP = 127.195; 
    f = new TF1("EffdXOuter",dXFunc,-5,5,3);
    f->SetParameters(rMinOP,rMinOW,1);
  } else if (InOut == 2) {
    Double_t rMinIP =  55.8; // the center of the first pad
    f = new TF1("EffdXiTPC",dXFunc,-5,5,3);
    f->SetParameters(rMinIP,rMinIW,2);
  }
  return f;
}
