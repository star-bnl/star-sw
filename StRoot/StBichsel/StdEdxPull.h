#ifndef __StdEdxPull_h__
#define  __StdEdxPull_h__
#include "Rtypes.h"
#include "TMath.h"
namespace StdEdxPull {
  Double_t EvalPred(Double_t betagamma, UChar_t fit = 0, Int_t charge=1);
  Double_t EvalDeV(Double_t dEdx, Double_t betagamma, UChar_t fit = 0, Int_t charge=1);
  Double_t Eval(Double_t dEdx, Double_t dEdxError, Double_t betagamma, UChar_t fit = 0, Int_t charge=1);
  Double_t EvalPred2(Double_t betagamma, Double_t dx2, UChar_t fit = 0, Int_t charge=1);
  Double_t EvalDeV2(Double_t dEdx, Double_t betagamma, Double_t dx2, UChar_t fit = 0, Int_t charge=1);
  Double_t Eval2(Double_t dEdx, Double_t dEdxError, Double_t betagamma, Double_t dx2, UChar_t fit = 0, Int_t charge=1);
};
#endif /* __StdEdxPull_h__ */
