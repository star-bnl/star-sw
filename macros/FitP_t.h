#ifndef __FitP_t_h__
#define __FitP_t_h__
#include "TObject.h"
#include "Names.h"
#include <string.h>
class FitP_t : public TObject {
 public: 
  void Reset() {memset(beg, 0, end-beg);}
  void ResetParams() {memset(beg2, 0, end-beg2);}
  FitP_t() {Reset();}
  virtual ~FitP_t() {}
  Char_t beg[1];
  Int_t c, ipT, jeta;
  Double_t RefMult, RefMult_Min, RefMult_Max;
  Double_t pT, pT_Min, pT_Max;
  Double_t eta, eta_Min, eta_Max;
  Double_t KolmoD[KPidParticles*(KPidParticles+1)/2];
  Char_t beg2[1];
  Int_t iter;
  Double_t Frac[KPidParticles];
  Double_t ErFrac[KPidParticles];
  Double_t CovFrac[KPidParticles*(KPidParticles+1)/2];
  Double_t Chisq;
  Double_t Prob;
  Double_t mu, dmu;
  Double_t sigma, dsigma;
  Double_t ProbMu;
  Char_t end[1];
  ClassDef(FitP_t,1)
};
#endif

