#include "Bichsel.h"
dEdxParameterization *Bichsel::m_dEdxParameterization = 0;
ClassImp(Bichsel);
Bichsel::Bichsel() {
  if (! m_dEdxParameterization) 
    m_dEdxParameterization = new dEdxParameterization("bich",
						      5.07402529167365057e-01, // MostProbableZShift
						      5.07402529167365057e-01, // AverageZShft
						      9.16531837651389347e-01, // I70Shft
						      9.75432754685096048e-01  // I60Shift
						      );
}
//________________________________________________________________________________
Double_t  Bichsel::GetI70(Double_t log10bg, Double_t log2dx, Int_t kase=0)  {
  return m_dEdxParameterization->GetI70(log10bg,log2dx,kase)
#ifndef P03ia
    *TMath::Exp(-TofCorrection(log2dx))
#endif
    ;
}
//________________________________________________________________________________
Double_t  Bichsel::TofCorrection(Double_t log10bg) {
  static const Double_t par[6] = {// gFunc->Eval(log10(4)) = 3.89675597262326284e-02
    /*   0  Offset          */   1.33408e-01,// 1.72376e-01,
    /*   1  beta2Inv1       */  -4.30665e-01,
    /*   2  beta2Inv2       */   1.29081e-01,
    /*   3  log(beta*gamma)1*/  -9.94174e-02,
    /*   4  log(beta*gamma)2*/   1.70633e-02,
    /*   5  log(beta*gamma)3*/  -1.03019e-03,
  };
  Double_t poverm = TMath::Power(10.,log10bg);
  Double_t beta2Inv  = 1. + 1./(poverm*poverm);
  Double_t value = par[0] + TMath::Log(beta2Inv)*(par[1]+par[2]*TMath::Log(beta2Inv)) + 
    TMath::Log(poverm)*(par[3] + TMath::Log(poverm)*(par[4] + TMath::Log(poverm)*par[5]));
  return value;
}
