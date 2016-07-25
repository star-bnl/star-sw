/*
  root.exe   MudEdx_Sparse_pT100_eta24R0R0.Frac.root lBichsel.C FitBichseldX.C
 */

#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TF2.h"
#include "TH3.h"
#include "TTree.h"
#include "TCanvas.h"
#include "StBichsel/Bichsel.h"
#else
class Bichsel;
#endif
Bichsel *m_Bichsel = 0;
const Int_t NMasses = 5;
const Double_t Masses[NMasses] = {0.93827231,
				  0.493677,
				  0.13956995,
				  0.51099907e-3,
				  1.87561339};
//________________________________________________________________________________
Double_t Rbichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return m_Bichsel->GetI70(TMath::Log10(poverm),par[3])/m_Bichsel->GetI70(TMath::Log10(poverm));
}
//________________________________________________________________________________
Double_t Rfunc(Double_t *x,Double_t *par) {
  Double_t B = 0.5; // T => 5 kG
  Double_t pT = TMath::Abs(x[0]);
  Double_t R  = 100 * pT/(0.3 * 0.1 * B); // cm
  Double_t L  = 2 + par[1]; // cm
  Double_t alpha = 0;
  if (2*R > L) alpha = TMath::ASin(L/(2*R));
  Double_t dxPhi = 2*R*alpha;
  if (dxPhi < 1) dxPhi = 1;
  Double_t eta = x[1];
  Double_t dX = dxPhi*TMath::CosH(par[2]*eta);
  Double_t p = pT*TMath::CosH(eta);
  Double_t poverm = p/Masses[2];
  return par[0]+TMath::Log(m_Bichsel->GetI70(TMath::Log10(poverm),TMath::Log2(dX)))
    -           TMath::Log(m_Bichsel->GetI70(TMath::Log10(poverm)));
}
//________________________________________________________________________________
Double_t Rfunc1(Double_t *x,Double_t *par) {
  Double_t pT = par[3];
  Double_t xx[2] = {pT, x[0]};
  return Rfunc(xx,par);
}
//________________________________________________________________________________
void FitBichseldX() {
  if (!m_Bichsel) m_Bichsel = Bichsel::Instance();
  TTree *FitP = (TTree *) gDirectory->Get("FitP");
  if (! FitP) return;
  enum EBinning {NRefMult = 10, NpT = 100, Neta = 24};
  const Double_t cpTBins[NpT+1] = {
    -50.000,-2.183,-1.631,-1.389,-1.242,-1.136,-1.046,-0.974,-0.919,-0.865,
    -0.811,-0.774,-0.742,-0.710,-0.677,-0.645,-0.613,-0.588,-0.569,-0.550,
    -0.531,-0.512,-0.493,-0.473,-0.454,-0.435,-0.416,-0.398,-0.385,-0.373,
    -0.361,-0.348,-0.336,-0.323,-0.311,-0.298,-0.286,-0.273,-0.261,-0.248,
    -0.236,-0.224,-0.211,-0.197,-0.166,-0.136,-0.105,-0.075,-0.045,-0.014,
    0.016, 0.047, 0.079, 0.110, 0.141, 0.172, 0.201, 0.214, 0.226, 0.239,
    0.251, 0.263, 0.276, 0.288, 0.301, 0.313, 0.326, 0.338, 0.351, 0.363,
    0.375, 0.388, 0.401, 0.419, 0.438, 0.457, 0.476, 0.494, 0.513, 0.532,
    0.550, 0.569, 0.588, 0.611, 0.642, 0.673, 0.704, 0.736, 0.767, 0.798,
    0.848, 0.899, 0.951, 1.003, 1.087, 1.171, 1.288, 1.438, 1.685, 2.238,
    50.000};
  const Double_t etaMax = 1.2;
  TH2D *pTEta    = new TH2D("pTEta","Eta q*pT for all tracks"
					,NpT,cpTBins,Neta,-etaMax,etaMax);
  pTEta->SetStats(0);
  Int_t nFound = FitP->Draw("pT:eta:mu:dmu","(dmu>0&&dmu<2e-3)","goff");
  for (Int_t i = 0; i < nFound; i++) {
    Int_t bin = pTEta->FindBin(FitP->GetV1()[i],FitP->GetV2()[i]);
    pTEta->SetBinContent(bin, FitP->GetV3()[i]);
    pTEta->SetBinError(bin, FitP->GetV4()[i]);
  }
  //  new TCanvas();
  //  pTEta->Draw("colz");
  TF2 *func = new TF2("func",Rfunc,-50,50,-1.2,1.2,3);
  func->SetParName(0,"scale");
  func->FixParameter(0, 0.0);
  func->SetParName(1,"shift");
  func->SetParameter(1, 0.0);
  func->SetParLimits(1, -1, 4);
  func->SetParName(2,"surf");
  func->SetParameter(2, 1.0);
  func->SetParLimits(2, 0.1, 10);
  pTEta->Fit(func,"0");
  new TCanvas();
  pTEta->Draw("colz");
  TF1 *func1 = new TF1("func1",Rfunc1,-1.2,1.2,4);
  func1->SetParameters(func->GetParameters());
  func1->SetParameter(3,0.5);
}
//________________________________________________________________________________
void Print(TH2D *pTEta = 0, TF2 *func = 0) {
  if (! pTEta || ! func) return;
  Int_t NpT  = pTEta->GetXaxis()->GetNbins();
  Int_t Neta = pTEta->GetYaxis()->GetNbins();
  Int_t p = 0;
  cout << Form("#    p      pT    eta      mu        dmu      va             dev") << endl;    
  for (Int_t i = 1; i <= NpT; i++) {
    Double_t pT = pTEta->GetXaxis()->GetBinCenter(i);
    for (Int_t j = 1; j <= Neta; j++) {
      Double_t eta = pTEta->GetYaxis()->GetBinCenter(j);
      Double_t dmu = pTEta->GetBinError(i,j);
      if (! dmu) continue;
      if (dmu < 0.001) dmu = 0.001;
      Double_t mu =  pTEta->GetBinContent(i,j);
      Double_t va = func->Eval(pT, eta);
      Double_t dev = (mu-va)/dmu;
      cout << Form("#%4i %7.3f %7.3f %7.3f+/- %7.3f %7.3f %7.3f",p,pT,eta,mu,dmu,va,dev) << endl;
      p++;
    }
  }
}
/*
 FCN=1.95939e+06 FROM MIGRAD    STATUS=FAILED        299 CALLS         300 TOTAL
                     EDM=7.14791e+08    STRATEGY= 1      ERR MATRIX NOT POS-DEF
  EXT PARAMETER                APPROXIMATE        STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  scale        1.31871e-02   7.85253e-01   0.00000e+00   1.35281e+04
   2  shift        3.29130e-03   1.16429e+00  -0.00000e+00   1.93058e+04
   3  surf         3.48032e-01   2.15828e+00   0.00000e+00   6.23990e+02
*/
