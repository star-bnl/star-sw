#ifndef St_TpcdEdxModelC_h
#define St_TpcdEdxModelC_h
#include "St_tpcCorrectionC.h"
class St_TpcdEdxModelC : public St_tpcCorrectionC {
 public:
  // Np = no. of primary clusters
  // ne == n = no. of conductining electorns
  static St_TpcdEdxModelC* 	instance();
  static Double_t E(Double_t ne) {return GeVperElectron*ne;} // deposited energy GeV from ne
  static Double_t n(Double_t e) {return e/GeVperElectron;}   // ne  from energy (GeV)
  static Double_t LogE(Double_t Logne) {return LogGeVperElectron  + Logne;} // deposited energy GeV from ne
  static Double_t Logne(Double_t LogE) {return LogE - LogGeVperElectron;}   // ne  from energy (GeV)
  static Double_t Parameter(Double_t Np, Int_t k = 0); 
  static Double_t Derivative(Double_t Np, Int_t k = 0); 
  static Double_t Mu(Double_t Np)    {return  Parameter(Np, 0);} // Most Probable log (ne/Np) versus Np
  static Double_t Sigma(Double_t Np) {return  Parameter(Np, 1);} // RMS
  static Double_t Alpha(Double_t Np) {return  Parameter(Np, 2);} // assymetry
  static Double_t MuDeriv(Double_t Np)    {return  Derivative(Np, 0);} // Most Probable log (ne/Np) versus Np derivateive wrt log(Np)
  static Double_t SigmaDeriv(Double_t Np) {return  Derivative(Np, 1);} // RMS       -"-
  static Double_t AlphaDeriv(Double_t Np) {return  Derivative(Np, 2);} // assymetry -"-
  static Double_t LogdEMPV(Double_t Np)   {return LogE(Mu(Np) + TMath::Log(Np));} // 
 protected:
  St_TpcdEdxModelC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdEdxModelC() {fgInstance = 0;}
 private:
  static Double_t GeVperElectron;
  static Double_t LogGeVperElectron;
  static St_TpcdEdxModelC* fgInstance;
  ClassDef(St_TpcdEdxModelC,1)
};
#endif
