#include "Names.h"
#include "TLegend.h"
#include "TString.h"
#include "TF1.h"
#include "TMath.h"
#include "TFile.h"
#include "TH1.h"
//________________________________________________________________________________
Double_t tmax(Double_t *x, Double_t *p) {
  static Double_t Tcut = 1e-4; // 100 keV maximum cluster size (~80 keV)
  static const Double_t m_e = .51099907e-3;
  Int_t h = (Int_t) p[0];
  Double_t M = TpcRSPart[h].mass;
  Int_t pdg  = TpcRSPart[h].pdg; 
  Int_t charge = TpcRSPart[h].charge; 
  Double_t bgL10 = x[0];
  Double_t bg = TMath::Power(10.,bgL10);
  Double_t bg2 = bg*bg;
  Double_t gamma = TMath::Sqrt(bg2 + 1);
  Double_t Tmax; 
  if (TMath::Abs(pdg) == 11) {
    if (charge > 0) Tmax =     m_e*(gamma - 1);
    else            Tmax = 0.5*m_e*(gamma - 1);
  } else {
    Double_t r = m_e/M;
    Tmax = 2*m_e*bg2/(1 + 2*gamma*r + r*r); 
  }
  return TMath::Min(Tcut, Tmax);
}
//________________________________________________________________________________
void Tmax() {
  TLegend *l = new TLegend(0.5,0.6,0.8,0.9);
  TString same;
  for (Int_t h = 0; h < NTpcRSParts; h++) {
  //  for (Int_t h = 0; h < 3; h++) {
    cout << "create " << TpcRSPart[h].name << endl;
    TString name(TpcRSPart[h].name);
    name.ReplaceAll("+","P");
    name.ReplaceAll("-","N");
    TF1 *f = new TF1(name,tmax,-2.,0.,1);
    f->SetParName(0,"TpcRSIndex");
    f->SetParameter(0,h );
    f->SetLineColor(h+1);
    f->Draw(same); same = "same";
    l->AddEntry(f,f->GetName());
  }
  l->Draw();
}
//________________________________________________________________________________
Double_t Ec(Double_t *x, Double_t *p) {  // StTpcRSMaker::
  if (x[0] < p[0]/2 || x[0] > 3.064*p[0]) return 0;
  if (x[0] < p[0]) return 1;
  return TMath::Power(p[0]/x[0],4);
}
//________________________________________________________________________________
TF1 *fEc(Double_t w) { // StTpcRSMaker::StTpcRSMaker::
  TF1 *f = new TF1("Ec",Ec,0,3.064*w,1);
  f->SetParameter(0,w);
  return f;
}
//--------------------------------------------------------------------------------
void CheckNdE() {
  TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
  //  TH1D *dNdE = (TH1D* ) f->Get("dNdE");
  TH1D *dNdEL10 = (TH1D*) f->Get("dNdEL10");
  //  TH1D *dNdE1 = new TH1D("dNdE1","dNdE from dNdE",100,0.0, 1e3);
  TH1D *dNdE2 = new TH1D("dNdE2","dNdE from dNdEL10",100,0.0, 1e3);
  static Double_t cLog10 = TMath::Log(10.);
  for (Int_t i = 0; i < 10000; i++) {
    //    Double_t e1 = dNdE->GetRandom();
    //    dNdE1->Fill(e1);
    Double_t e2 = TMath::Exp(cLog10*dNdEL10->GetRandom());
    dNdE2->Fill(e2);
  }
}
//________________________________________________________________________________
Double_t dNdEFunc(Double_t *x, Double_t *p) {
  static TH1D *dNdE = 0;
  static Double_t dEmaxL10 = 5.40502303800167372e+00;
  static Double_t dNmax    = -1;
  static Double_t slope    = -2.40581e+00;
  static Double_t xmin     =  0;
  static Double_t Norm     =  2.95275302029816906e+01; // F->Integral(0,1e9) 
  if (! dNdE) {
    TFile *f = new TFile("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    dNdE = (TH1D* ) f->Get("dNdE");
    Int_t nx = dNdE->GetXaxis()->GetNbins();
    dEmaxL10 = TMath::Log10(dNdE->GetBinCenter(nx));
    dNmax    = dNdE->GetBinContent(nx);
    xmin     = dNdE->GetBinLowEdge(1);
  }
  if (x[0] < xmin) return 0;
  Double_t xL10 = TMath::Log10(x[0]);
  if (xL10 < dEmaxL10) return dNdE->Interpolate(x[0])/Norm;
  else                 return dNmax*TMath::Power(10., slope*(xL10 - dEmaxL10))/Norm; 
}
//--------------------------------------------------------------------------------
TF1 *dNdxF() {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("dNdxF",dNdEFunc,8.69499969482421875e+00, 2.54110750000000000e+05, 0);
    f->SetNpx(951);
  }
  return f;
}
//________________________________________________________________________________
Double_t NpEffFunc(Double_t *x, Double_t *p) {
  static TF1 *F = dNdxF();
  static Double_t W = 26.2; // eV
  Double_t tMax = 1e9*tmax(x, p);
  return F->Integral(W/2, tMax);
}
//________________________________________________________________________________
TF1 *dNdxEff(Int_t h = 0) {
  TString name(TpcRSPart[h].name);
  name.ReplaceAll("+","P");
  name.ReplaceAll("-","N");
  TF1 *f = new TF1(Form("Eff%s",name.Data()),NpEffFunc,-2.,0.,1);
  f->SetParName(0,"TpcRSIndex");
  f->SetParameter(0,h );
  f->SetLineColor(h+1);
  return f;  
}
