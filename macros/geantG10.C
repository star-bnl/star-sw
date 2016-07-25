//#include "Riostream.h"
class Bichsel;
Bichsel *m_Bichsel = 0;
const Int_t NMasses = 9;
const Double_t Masses[NMasses] = {0.93827231,
				  0.493677,
				  0.13956995,
				  0.51099907e-3,
				  1.87561339,
				  0.1056584,
				  2.80925,
				  2.80923, //GEANT3
				  3.727417 //GEANT3
};
const Char_t *Names[NMasses] = {"p", "K","pi","e","d","mu","t","He3","#alpha"};
const Int_t NF = 4;
const Char_t *FNames[5] = {"Girrf","Sirrf","Bz","B70","B60"};
const Int_t Nlog2dx = 3;
const Double_t log2dx[Nlog2dx] = {0,1,2};
Double_t geant70(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t poverm = pove/par[0]; 
  Double_t charge = 1.;
  if (par[1] > 1.0) {
    charge = 2;
    poverm *= charge;
  }
  //  return  charge*charge*TMath::Log10(m_Bichsel->GetI70M(TMath::Log10(poverm),1.));
  return  TMath::Log10(charge*charge*BetheBloch::Girrf(poverm)) - 0.2;
  //  return charge*charge*TMath::Log10(m_Bichsel->GetI70(TMath::Log10(poverm),1.));
}
void geantG10() {
  TLegend *leg = new TLegend(0.65,0.6,0.75,0.9,"");
  for (int h = 0; h < NMasses; h++) { // Masses
    Int_t f = 0;
    Int_t dx = 1;
    Char_t *FunName = Form("%s%s%i",FNames[f],Names[h],(int)log2dx[dx]);
    //	cout << "Make " << FunName << endl;
    TF1 *func = new TF1(FunName,geant70,-1.5., 2.2,2);
    func->SetParameter(0,Masses[h]);
    func->SetParameter(1,1.);
    if (h >= 7) func->SetParameter(1,2.);
    Int_t color = h+1;
    //    if (color > 7) color++;
    func->SetLineColor(color);
    func->Draw("same");
    leg->AddEntry(func,Names[h]);
  }
  leg->Draw();
}
