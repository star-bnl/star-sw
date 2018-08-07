//#include "Riostream.h"
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
const Double_t log2dx[Nlog2dx] = {0,1,2};
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t bgL10 = x[0];
  return m_Bichsel->GetI70M(bgL10,1.);
}
TF1 *bichselG() {
  if (gClassTable->GetID("StBichsel") < 0 || !m_Bichsel) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
  /*
  for (int h = 0; h < 4; h++) { // Masses
    Int_t f = 3;
    Int_t dx = 1;
    Char_t *FunName = Form("%s%s%i",FNames[f],Names[h],(int)log2dx[dx]);
    //	cout << "Make " << FunName << endl;
    TF1 *func = new TF1(FunName,bichsel70,0.1, 2.2,5);
    func->SetParameter(0,Masses[h]);
    func->SetLineColor(h+1);
    func->Draw("same");
  }
  */
  TF1 *func = new TF1("bichBg",bichsel70,-1., 5,0);
  func->Draw();
}
