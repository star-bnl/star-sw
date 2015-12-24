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
const Int_t NF = 4;
const Char_t *FNames[5] = {"Girrf","Sirrf","Bz","B70","B60"};
const Int_t Nlog2dx = 3;
const Double_t log2dx[3] = {0,1,2};
//________________________________________________________________________________
Double_t gfunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  //  Double_t Scale = BetheBloch::Sirrf(4.,par[2],k)/
  //   BetheBloch::Girrf(4.,par[4],k);
  return BetheBloch::Girrf(poverm,par[4],k);
}
//________________________________________________________________________________
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return BetheBloch::Sirrf(poverm,par[2],k);
}
// Double_t bbfunc(Double_t *x,Double_t *par) {
//   Double_t pove   = x[0];
//   Double_t poverm = pove/par[0];
//   BetheBloch BB;
//   Double_t value = 1.e6*BB(poverm);
//   //  printf("x : %f p: %f  val : %f \n",x[0],poverm,value);
//   return value;
// }
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  //  Double_t Scale = BetheBloch::Sirrf(4.,par[2],k)/
  //    TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(4),par[3]));
  return TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),par[3]));
}
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  //  Double_t Scale = BetheBloch::Sirrf(4.,par[2],k)/
  //    TMath::Exp(m_Bichsel->GetI70(TMath::Log10(4),par[3]));
  return m_Bichsel->GetI70(TMath::Log10(poverm),par[3]);
}
Double_t bichsel60(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  //  Double_t Scale = BetheBloch::Sirrf(4.,par[2],k)/
  //    TMath::Exp(m_Bichsel->GetI60(TMath::Log10(4),par[3]));
  return m_Bichsel->GetI60(TMath::Log10(poverm),par[3]);
}
void bichselBG() {
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
  TH1F *hr = c1->DrawFrame(1.e-1,1,1.e4,2.e2);
  //  hr->SetXTitle("Momentum (GeV/c)");
  hr->SetTitle("dE/dx predictions");
  hr->SetXTitle("#beta #gamma");
  hr->SetYTitle("dE/dx (keV/cm)");  
  hr->Draw();
  //                     Mass Type Length  log2(dx)
  Double_t params[5] = {  1.0,  0.,   60., 1., 1e-3};  
  TLegend *leg = new TLegend(0.4,0.7,0.9,0.9,"");//TLegend(0.79,0.91,0.89,0.89,"");
  //  for (Int_t h = 0; h < 4; h++) { // Masses
  for (Int_t h = 2; h < 4; h++) { // Masses
    params[0] = 1.; // Masses[h];
    params[1] = h;
    //    for (Int_t f = 0; f < NF; f++) { // Functions
    for (Int_t f = 3; f < 4; f++) { // Functions
      if (f == 2) continue;
      //      for (Int_t dx = 0; dx < Nlog2dx; dx++) { 
      for (Int_t dx = 1; dx < 2; dx++) { 
	params[3] = log2dx[dx];
	Char_t *FunName = Form("%s%s%i",FNames[f],Names[h],(int)log2dx[dx]);
	//	cout << "Make " << FunName << endl;
	TF1 *func = 0;
	if (TString(FNames[f]) == "Sirrf") {
	  func = new TF1(FunName,sifunc,1.e-2,1.e4,5);
	  func->SetLineColor(2);
	}
	if (TString(FNames[f]) == "Girrf") {
	  func = new TF1(FunName,gfunc,1.e-2,1.e4,5);
	  params[4] = 1.e-5;
	  if (dx == 1) params[4] = 1.e-2;
	  if (dx == 2) params[4] = 1.e-3;
	  if (dx == 3) params[4] = 1.e-4;
	  //	  func->SetLineColor(3);
	  func->SetLineColor(dx+1);
	}
	else {if (TString(FNames[f]) == "Bz") {
	  func = new TF1(FunName,bichselZ,1.e-2,1.e4,5);
	  func->SetLineColor(4);
	}
	else {if (TString(FNames[f]) == "B70") {
	  func = new TF1(FunName,bichsel70,1.e-2,1.e4,5);
	  func->SetLineColor(6);
	}
	else {if (TString(FNames[f]) == "B60") {
	  func = new TF1(FunName,bichsel60,1.e-2,1.e4,5);
	  func->SetLineColor(7);
	}}}}
	if (! func) continue;
	func->SetParameters(params); 
	func->Draw("same");
	if (dx == 0 && h == 2) {
	  TString name(FNames[f]);
	  if (name == "Sirrf") name += ": Fitted from Year 1 data";
	  else if (name == "Girrf") {
	    name += ": Geant3 prediction";
	    name += Form(" T_{max} = %7f keV",1e6*params[4]);
	  }
	  else if (name == "Bz") name += ": Bichsel most probable";
	  else if (name == "B70") name += ": Bichsel, 30 % truncation";
	  else if (name == "B60") name += ": Bichsel, 40 % truncation";
	  leg->AddEntry(func,name.Data(),"L");
	}
      }
    }
  }
  leg->Draw();
}
