#include "Bichsel.h"
#include "StdEdxModel.h"
#include "StdEdxPull.h"
//________________________________________________________________________________
Double_t StdEdxPull::Eval(Double_t dEdx, Double_t dEdxError, Double_t betagamma, UChar_t fit, Int_t charge) {
  Double_t z = -999.;
  Double_t dedx_expected;
  if (! fit) { // I70
    dedx_expected = 1.e-6*charge*charge*Bichsel::Instance()->GetI70M(TMath::Log10(betagamma)); 
  } else if ( fit == 1) {     // Ifit
    dedx_expected = 1.e-6*charge*charge*TMath::Exp(Bichsel::Instance()->GetMostProbableZ(TMath::Log10(betagamma)));
  } else {     // dNdx
    dedx_expected = StdEdxModel::instance()->dNdx(betagamma,charge);
  }
  if (dEdxError > 0) z = TMath::Log(dEdx/dedx_expected)/dEdxError;
  return z;
}
