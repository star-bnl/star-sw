#include "Riostream.h"
#include "Bichsel.h"
ClassImp(Bichsel);
Bichsel::Bichsel(const Char_t *tag) : m_Tag(tag), m_dEdxParameterization(0) {
  if (! m_dEdxParameterization) 
    m_dEdxParameterization = new dEdxParameterization(m_Tag.Data());
}
//________________________________________________________________________________
Double_t  Bichsel::GetI70(Double_t log10bg, Double_t log2dx, Int_t kase)  {
  Double_t Value = m_dEdxParameterization->GetI70(log10bg,log2dx,kase);
#ifdef TOFCORRECTION
  Value *= TMath::Exp(TofCorrection(log10bg));
#endif
  return  Value;
}
//________________________________________________________________________________
Double_t  Bichsel::TofCorrection(Double_t log10bg) {
  static const Double_t par[6] = {// gFunc->Eval(log10(4)) = 3.89675597262326284e-02
    /*   0  Offset          */   1.33408e-01,// 1.72376e-01,
    /*   1  beta2Inv1       */  -4.30665e-01,
    /*   2  beta2Inv2       */   1.29081e-01,
    /*   3  ::log(beta*gamma)1*/  -9.94174e-02,
    /*   4  ::log(beta*gamma)2*/   1.70633e-02,
    /*   5  ::log(beta*gamma)3*/  -1.03019e-03,
  };
  Double_t poverm = TMath::Power(10.,log10bg);
  Double_t beta2Inv  = 1. + 1./(poverm*poverm);
  Double_t value = par[0] + TMath::Log(beta2Inv)*(par[1]+par[2]*TMath::Log(beta2Inv)) + 
    TMath::Log(poverm)*(par[3] + TMath::Log(poverm)*(par[4] + TMath::Log(poverm)*par[5]));
  return value;
}
//________________________________________________________________________________
void Bichsel::Print() {
  cout << "Bichsel:: " << m_Tag << endl;
  if (m_dEdxParameterization) m_dEdxParameterization->Print();
}
