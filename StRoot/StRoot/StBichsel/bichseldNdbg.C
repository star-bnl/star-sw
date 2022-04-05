#include "Riostream.h"
class Bichsel;
Bichsel *m_Bichsel = 0;
const Int_t NMasses = 5;
const Double_t Masses[NMasses] = {0.93827231,
				  0.493677,
				  0.13956995,
				  0.51099907e-3,
				  1.87561339};
const Char_t *Names[NMasses] = {"p", "K","pi","e","d"};
const Int_t NF = 3;
const Char_t *FNames[3] = {"Bz","B70","B60"};
const Int_t Nlog2dx = 3;
const Double_t log2dx[3] = {0,1,2};
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return par[1]*TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),par[3]));
}
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return par[1]*m_Bichsel->GetI70M(TMath::Log10(poverm),par[3]);
}
//________________________________________________________________________________
Double_t bichsel60(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return  par[1]*m_Bichsel->GetI60(TMath::Log10(poverm),par[3]);
}
void bichseldNdbg() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
  }
  if (!m_Bichsel) m_Bichsel = Bichsel::Instance();
  TCanvas *c1 = new TCanvas("c1");
  c1->SetLogx();
  c1->SetLogy();
  c1->SetGrid();
  //  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  //  TH1F *hr = c1->DrawFrame(1.e-2,1,1.e3,1.e2);
  TH1F *hr = c1->DrawFrame(1.e-2,1,1.e4,1.e3);
  //  hr->SetXTitle("Momentum (GeV/c)");
  hr->SetTitle("dE/dx predictions");
  hr->SetXTitle("#beta #gamma");
  hr->SetYTitle("dE/dx (keV/cm)");  
  hr->Draw();
  //                     Mass Scale Length  log2(dx)
  Double_t params[5] = {  1.0,  1.21773e+01,   60., 1., 1e-3};  
  TLegend *leg = new TLegend(0.4,0.7,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  //  for (Int_t h = 0; h < 4; h++) { // Masses
  Int_t h = 2;
  //  for (Int_t f = 0; f < NF; f++) { // Functions
  for (Int_t f = 1; f < 2; f++) { // Functions
    for (Int_t dx = 1; dx < 2; dx++) { 
      params[3] = log2dx[dx];
      Char_t *FunName = Form("%s%s%i",FNames[f],Names[h],(int)log2dx[dx]);
      cout << "Make " << FunName << endl;
      if (TString(FNames[f]) == "Bz") {
	func = new TF1(FunName,bichselZ,1.e-2,1.e4,5);
	func->SetLineColor(4);
      }	else {if (TString(FNames[f]) == "B70") {
	func = new TF1(FunName,bichsel70,1.e-2,1.e4,5);
	func->SetLineColor(6);
      } else {if (TString(FNames[f]) == "B60") {
	func = new TF1(FunName,bichsel60,1.e-2,1.e4,5);
	func->SetLineColor(7);
      }}}
      if (! func) continue;
      //      func->SetParameters(params); 
      for (Int_t j = 0; j < 5; j++) {
	if (j == 1) func->SetParameter(j,params[j]);
	else        func->FixParameter(j,params[j]);
      }
      func->Draw("same");
      TString name(FNames[f]);
      if (name == "Bz") name += ": Bichsel most probable";
      else if (name == "B70") name += ": Bichsel, 30 % truncation";
      else if (name == "B60") name += ": Bichsel, 40 % truncation";
      leg->AddEntry(func,name.Data(),"L");
      }
  }
  leg->Draw();
}
