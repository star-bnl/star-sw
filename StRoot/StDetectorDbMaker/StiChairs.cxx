#include "TMath.h"
#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
//________________________________________________________________________________
#include "StiHitErrorCalculator.h"
ClassImp(StiHitErrorCalculator);
//________________________________________________________________________________
void StiHitErrorCalculator::calculateError(Double_t _z,  Double_t _eta, Double_t _tanl, Double_t &ecross, Double_t &edip) const {
  static const Double_t tenMicrons = 1e-3;
  static const Double_t min2Err = tenMicrons*tenMicrons;
  static const Double_t max2Err = 1.;
  const Double_t *Coeff = ((StiHitErrorCalculator *) this)->coeff();
#if 0
  Double_t dz = (200.-TMath::Abs(_z+100))/100.; // Local z
#else
  Double_t dz = (200.-TMath::Abs(_z))/100.; // Global z
#endif
  if (dz < 0) dz = 0;
#if 0
  Int_t sec  = TMath::Nint(_eta*TMath::RadToDeg()/30);
  Double_t Phi = _eta - TMath::DegToRad()*30*sec;
#else
  Double_t Phi = _eta;
#endif
  Double_t cosCA = TMath::Cos(Phi);
  Double_t sinCA = TMath::Sin(Phi);
  if (TMath::Abs(cosCA)<0.01) cosCA=0.01;
  Double_t tanCA = sinCA/cosCA;
  ecross=Coeff[0]+Coeff[1]*dz/(cosCA*cosCA) +Coeff[2]*tanCA*tanCA;
  if (ecross< min2Err) ecross = min2Err;
  if (ecross> max2Err) ecross = max2Err;
  Double_t tanDip=_tanl;
  Double_t cosDipInv2=1+tanDip*tanDip;
         edip=Coeff[3]+Coeff[4]*dz*cosDipInv2+Coeff[5]*tanDip*tanDip;
  if (edip< min2Err) edip = min2Err;
  if (edip> max2Err) edip = max2Err;
//	Temporary hack for Gene. Increase prompt hit errors
//  if (fabs(_z) >200) {ecross*=10; edip*=10;}


}
//________________________________________________________________________________
#include "StiTrackingParameters.h"
ClassImp(StiTrackingParameters)
#include "StiDefaultTrackingParameters.h"
MakeChairInstance2(TrackingParameters,StiDefaultTrackingParameters,Calibrations/tracker/DefaultTrackingParameters);
#include "StiLocalTrackSeedFinderParameters.h"
MakeChairInstance2(LocalTrackSeedFinder,StiLocalTrackSeedFinderParameters,Calibrations/tracker/LocalTrackSeedFinder);
#include "StiKalmanTrackFitterParameters.h"
MakeChairInstance2(KalmanTrackFitterParameters,StiKalmanTrackFitterParameters,Calibrations/tracker/KalmanTrackFitterParameters);
#include "StiKalmanTrackFinderParameters.h"
MakeChairInstance2(KalmanTrackFinderParameters,StiKalmanTrackFinderParameters,Calibrations/tracker/KalmanTrackFinderParameters);
