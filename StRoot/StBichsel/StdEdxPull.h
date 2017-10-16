#ifndef __StdEdxPull_h__
#define  __StdEdxPull_h__
#include "Rtypes.h"
namespace StdEdxPull {
  Double_t Eval(Double_t dEdx, Double_t dEdxError, Double_t betagamma, UChar_t fit = 0, Double_t log2dX = 1, Int_t charge=1);
};
#endif /* __StdEdxPull_h__ */
