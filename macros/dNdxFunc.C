/*  Compare dE/dx distributions from Bichsel with dN/dx parameerizations
    root.exe lBichsel.C dNdxFunc.C
*/
#include "TMath.h"
#include "TF1.h"
#include "StBichsel/StdEdxModel.h"
#include "StBichsel/Bichsel.h"
#include "TLegend.h"
#include "TCanvas.h"
#include "TArrayD.h"
#include "TGraph.h"
Bichsel *m_Bichsel = 0;
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
  TH1F *frame = c1->DrawFrame(-1,0.5,4,6);
  frame->SetTitle("The most probable log(dE/dx[keV/cm]) versu log_{10}(#beta #gamma)");
  frame->SetXTitle("log_{10}(#beta #gamma)");
  //  for (Int_t color = 1; color < 8; color++) {
  for (Int_t color = 2; color <= 4; color++) {
    Double_t log2dx = color - 2;
    Double_t dx = TMath::Power(2.,log2dx);
#if 1
    TF1 *fnOld = StdEdxModel::ZMPold(log2dx);
    fnOld->SetLineColor(color);
    fnOld->SetMarkerColor(color);
    fnOld->Draw("same");
    l->AddEntry(fnOld,Form("%4.1fcm Old",dx));
    TF1 *fn = StdEdxModel::ZMP(log2dx);
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
//________________________________________________________________________________
Double_t scanleFCn(Double_t *x, Double_t *p) {
  Double_t pMoMIP = 0.526;
  Double_t bg10pi = TMath::Log10(pMoMIP/0.13956995);// =  5.76193821239086468e-01
  Double_t bg10P = TMath::Log10(pMoMIP/0.93827231); //  = -2.51343155597725409e-01;
  Double_t difMIP = 1.18931 - (-0.03569); // Positive Outer
  StdEdxModel::SetScale(x[0]);
  TF1 *F = StdEdxModel::ZMP(1.0);
  Double_t  diff1 =  F->Eval(bg10P) - F->Eval(bg10pi) - difMIP;
  return diff1;
}
//________________________________________________________________________________
TF1 *Fdif() {
  TF1 *f = new TF1("FDIFF",scanleFCn,0.5,2.,0);
  return f;
}
//________________________________________________________________________________
TGraph *dNdxBeta() {
  TH1D *dNdx = StdEdxModel::instance()->GetdNdx();
  Int_t nx = dNdx->GetNbinsX();
  TArrayD X(nx);
  TArrayD Y(nx);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t bg = dNdx->GetXaxis()->GetBinCenter(i);
    Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
    X[i-1] = beta;
    Y[i-1] = dNdx->GetBinContent(i);
  }
  TGraph *gr = new TGraph(nx,X.GetArray(), Y.GetArray());
  return gr;
}
//________________________________________________________________________________
TGraph *dNdxBetaLogLog() {
  TH1D *dNdx = StdEdxModel::instance()->GetdNdx();
  Int_t nx = dNdx->GetNbinsX();
  TArrayD X(nx);
  TArrayD Y(nx);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t bg = dNdx->GetXaxis()->GetBinCenter(i);
    Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
    X[i-1] = TMath::Log(beta);
    Y[i-1] = TMath::Log(dNdx->GetBinContent(i));
  }
  TGraph *gr = new TGraph(nx,X.GetArray(), Y.GetArray());
  return gr;
}
//________________________________________________________________________________
TGraph *dNdxBetaLogLogHeed() {
  TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdx_Heed.root");
  if (! f ) return 0;
  TH1D *dNdxL10 = (TH1D*) f->Get("dNdxL10");
  Int_t nx = dNdxL10->GetNbinsX();
  TArrayD X(nx);
  TArrayD Y(nx);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t bgL10 = dNdxL10->GetXaxis()->GetBinCenter(i);
    Double_t bg  = TMath::Power(10., bgL10);
    Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
    X[i-1] = TMath::Log(beta);
    Y[i-1] = TMath::Log(1.2*dNdxL10->GetBinContent(i)); // +20% to match Bichsel
  }
  TGraph *gr = new TGraph(nx,X.GetArray(), Y.GetArray());
  gr->SetMarkerColor(2);
  return gr;
}
/*
root.exe [2] TGraph *gb = dNdxBetaLogLog()
root.exe [3] gb->Draw("axp")
root.exe [4] TGraph *gh = dNdxBetaLogLogHeed()
root.exe [5] gh->Draw("p")
root.exe [6] gb->Fit("pol1","er","",-5,-2)

****************************************
Minimizer is Minuit / Migrad
Chi2                      =    0.0588655
NDf                       =           10
Edm                       =   7.9069e-21
NCalls                    =           51
p0                        =      3.82557   +/-   0.0953801   
p1                        =     -1.53436   +/-   0.027871    
(class TFitResultPtr)77739248
root.exe [7] gh->Fit("pol1","er","",-5,-2)

****************************************
Minimizer is Minuit / Migrad
Chi2                      =    0.0456108
NDf                       =           10
Edm                       =  2.02813e-21
NCalls                    =           47
p0                        =      3.80815   +/-   0.0848441   
p1                        =     -1.54108   +/-   0.0252986   
*/
