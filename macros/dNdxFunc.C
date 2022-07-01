/*  Compare dE/dx distributions from Bichsel with dN/dx parameerizations
    root.exe lBichsel.C dNdxFunc.C
*/
#include "TMath.h"
#include "TF1.h"
#include "StBichsel/StdEdxModel.h"
#include "StBichsel/Bichsel.h"
#include "TLegend.h"
#include "TCanvas.h"
Bichsel *m_Bichsel = 0;

//________________________________________________________________________________
Double_t zMP(Double_t *x, Double_t *p) {
  Double_t log10bg = x[0];
  Double_t pOverM  = TMath::Power(10., log10bg);
  Double_t log2dx  = p[0];
  Double_t dx      = TMath::Power( 2., log2dx);
  Double_t dNdx = StdEdxModel::instance()->dNdx(pOverM);
  Double_t Np = dNdx*dx;
  Double_t mu = StdEdxModel::instance()->Mu(Np); 
  Double_t dEdx  = dNdx*TMath::Exp(mu);
  Double_t shift = TMath::Log(1e-3) + m_Bichsel->MostProbableZShift() - 7.26742600141722234e-02;
  return shift + TMath::Log(dEdx);// - m_Bichsel->Parameterization()->MostProbableZShift();
}
//________________________________________________________________________________
TF1 *ZMP(Double_t log2dx = 1) {
  TF1 *f = 0;
  if (! f) {
    f = new TF1(Form("N%i",(int)log2dx+2),zMP,-2,5,1);
    f->SetParName(0,"log2dx");
    f->SetLineStyle(2);
    f->SetParameter(0,log2dx);
  }
  return f;
}
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  return m_Bichsel->GetMostProbableZ(x[0],par[0]);
}
//________________________________________________________________________________
TF1 *ZMPB(Double_t log2dx = 1) {
  TF1 *f = 0;
  if (! f) {
    f = new TF1(Form("B%i",(int)log2dx+2),bichselZ,-1,4,1);
    f->SetParName(0,"log2dx");
    f->SetParameter(0,log2dx);
  }
  return f;
}
//________________________________________________________________________________
void dNdxFunc(Double_t log2dx = 1) {
  if (!m_Bichsel) m_Bichsel = Bichsel::Instance();
  TCanvas *c1 = new TCanvas("c1","c1");
  TLegend *l = new TLegend(0.4,0.6,0.8,0.9);
  TH1F *frame = c1->DrawFrame(-1,0.5,4,4.5);
  for (Int_t color = 1; color < 8; color++) {
    Double_t log2dx = color - 2;
    Double_t dx = TMath::Power(2.,log2dx);
#if 1
    TF1 *fn = ZMP(log2dx);
    fn->SetLineColor(color);
    fn->SetMarkerColor(color);
    fn->Draw("same");
    l->AddEntry(fn,Form("%4.1fcm",dx));
#endif
#if 1
    TF1 *bn = ZMPB(log2dx);
    bn->SetLineColor(color);
    bn->SetMarkerColor(color);
    bn->Draw("same");
    l->AddEntry(bn,Form("Bichel %4.1fcm",dx));
#endif
  }
  l->Draw();
}
//________________________________________________________________________________
Double_t checkParameters(Double_t *x, Double_t *p) {
  Double_t X = TMath::Exp(x[0]);
  Int_t k = p[0];
  return StdEdxModel::instance()->Parameter(X, k);
    }
//________________________________________________________________________________
TF1 *CheckParameters(Int_t k = 0) {
  static TF1 *f = 0;
  if (!f) {
    f = new TF1("CheckParamer",checkParameters,3,9,1);
  }
  f->SetParameter(0,k);
  return f;
}
