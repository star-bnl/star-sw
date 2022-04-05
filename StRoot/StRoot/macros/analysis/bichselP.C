#include "Riostream.h"
class Bichsel;
Bichsel *m_Bichsel = 0;
class BetheBloch;
BetheBloch *m_BetheBloch = 0;
const Int_t NF = 5;
const Char_t *FNames[5] = {"BetheBloch","Girrf","Sirrf","B70M","B70"};
//________________________________________________________________________________
Double_t bfunc(Double_t *x,Double_t *par) {
  if (! m_BetheBloch) m_BetheBloch = new BetheBloch();
  return m_BetheBloch->operator()(x[0]);
}
//________________________________________________________________________________
Double_t gfunc(Double_t *x,Double_t *par) {
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return 1e-6*BetheBloch::Girrf(x[0],par[4],k);
}
//________________________________________________________________________________
Double_t sifunc(Double_t *x,Double_t *par) {
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return 1e-6*BetheBloch::Sirrf(x[0],par[2],k);
}
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return 1e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(x[0]),par[3]));
}
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  //  Double_t Scale = BetheBloch::Sirrf(4.,par[2],k)/
  //    TMath::Exp(m_Bichsel->GetI70(TMath::Log10(4),par[3]));
  return 1e-6*m_Bichsel->GetI70(TMath::Log10(poverm),par[3]);
}
//________________________________________________________________________________
Double_t bichsel70M(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  //  Double_t Scale = BetheBloch::Sirrf(4.,par[2],k)/
  //    TMath::Exp(m_Bichsel->GetI70(TMath::Log10(4),par[3]));
  return 1e-6*m_Bichsel->GetI70M(TMath::Log10(poverm),par[3]);
}
//________________________________________________________________________________
Double_t bichsel60(Double_t *x,Double_t *par) {
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return m_Bichsel->GetI60(TMath::Log10(x[0]),par[3]);
}
void bichselP() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
  }
  if (!m_Bichsel) m_Bichsel = new Bichsel();
  TCanvas *c1 = new TCanvas("c1");
  c1->SetLogx();
  c1->SetLogy();
  c1->SetGrid();
  //  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  //  TH1F *hr = c1->DrawFrame(1.e-2,1,1.e3,1.e2);
  TH1F *hr = c1->DrawFrame(1.e-1,1e-6,1.e3,2.e-4);
  //  hr->SetXTitle("Momentum (GeV/c)");
  hr->SetTitle("dE/dx predictions (GeV/cm) versus #beta#gamma = p/m");
  hr->SetXTitle("#beta#gamma ");
  hr->SetYTitle("dE/dx (GeV/cm)");  
  hr->Draw();
  //                     Mass Type Length  log2(dx)
  Double_t params[5] = {  1.0,  0.,   60., 1., 1e-3};  
  TLegend *leg = new TLegend(0.2,0.7,1,0.9,"");
  for (Int_t f = 0; f < NF; f++) { // Functions
    Int_t dx = 0;
    Char_t *FunName = FNames[f];
    TF1 *func = 0;
    if (TString(FNames[f]) == "BetheBloch") {
      func = new TF1(FunName,bfunc,1.e-2,1.e3,5);
      func->SetLineColor(1);
    }
    if (TString(FNames[f]) == "Sirrf") {
      func = new TF1(FunName,sifunc,1.e-2,1.e3,5);
      func->SetLineColor(2);
    }
    if (TString(FNames[f]) == "Girrf") {
      func = new TF1(FunName,gfunc,1.e-2,1.e3,5);
      params[4] = 1.e-3;
      if (dx == 1) params[4] = 1.e-2;
      if (dx == 2) params[4] = 1.e-3;
      if (dx == 3) params[4] = 1.e-4;
      func->SetLineColor(3);
    }
    else {if (TString(FNames[f]) == "Bz") {
	func = new TF1(FunName,bichselZ,1.e-2,1.e3,5);
	func->SetLineColor(4);
      }
      else {if (TString(FNames[f]) == "B70") {
	  func = new TF1(FunName,bichsel70,1.e-2,1.e3,5);
	  func->SetLineColor(6);
	}
	else {if (TString(FNames[f]) == "B70M") {
	    func = new TF1(FunName,bichsel70M,1.e-2,1.e3,5);
	    func->SetLineColor(7);
	  }}}}
    if (! func) continue;
    func->SetParameters(params); 
    func->Draw("same");
    TString name(FNames[f]);
    if (name == "BetheBloch") name += ": Fitted from Year 0 data (P00hm production only)";
    else if (name == "Sirrf") name += ": Fitted from Year 1 data (for production before P03h)";
    else if (name == "Girrf") name += ": Geant3 prediction (for simulated data only)";
    else if (name == "Bz") name += ": Bichsel most probable";
    else if (name == "B70") name += ": Bichsel, 30 % truncation (for production P03h and after)";
    else if (name == "B70M") name += ": Bichsel, 30 % truncation, with correction for saturation";
    leg->AddEntry(func,name.Data(),"L");
  }
  leg->Draw();
}
