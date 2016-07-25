/*
  .L EffectivedX.C+
  .x bfc.C(0)
  .x Run1Ev.C
  StTpcRSMaker *trs = (StTpcRSMaker *) chain->Maker("TpcRS");
  SetIO(trs->GetChargeFraction(),trs->GetChargeFraction(1));
  TF1 *inner = EffectivedX(0);
  TF1 *outer = EffectivedX(1);
*/

#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TF1.h"
#endif
static TF1 *ChargeFraction[2] = {0};
//________________________________________________________________________________
void SetIO(TF1 *i =0 , TF1 *o = 0) {
  ChargeFraction[0] = i;
  ChargeFraction[1] = o;
}
//________________________________________________________________________________
Double_t dXFunc(Double_t *x, Double_t *p) {
  static Double_t w = 0.4; // anode wire spacing
  Double_t rMidOfPad = p[0];
  Double_t rMinW = p[1];
  Int_t    io = p[2];
  if (io < 0 || io > 1) return 0;
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
  static Double_t rMinIP =  60.0; // the center of the first pad
  static Double_t rMinOP = 127.195; 
  static Double_t rMinIW =  53.2; // 1-st anode wire radius
  static Double_t rMinOW = 122.8;  
  if (!InOut) {
    f = new TF1("EffdXInner",dXFunc,-2,2,3);
    f->SetParameters(rMinIP,rMinIW,0);
  } else {
    f = new TF1("EffdXOuter",dXFunc,-2,2,3);
    f->SetParameters(rMinOP,rMinOP,1);
  }
  return f;
}
