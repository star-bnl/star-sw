#ifndef __StAnneling_h__
#define __StAnneling_h__
// $Id: StAnneling.h,v 2.2 2015/12/20 01:06:39 fisyak Exp $
#include "TMath.h"
#include "TObject.h"
//________________________________________________________________________________
class StAnneling : public TObject {
public:
  static void      SetTemperature(Double_t  Temperature=1)   {fTemperature = Temperature;}
  static Double_t  Temperature() {return fTemperature;}
  static void      SetChi2Cut    (Double_t chi2Cut=32.2       )  {fChi2Cut     = chi2Cut;}
  static void      SetChi2CutUniq(Double_t chi2Cut=13.81551055)  {fChi2CutUniq = chi2Cut;}
  static Double_t  Chi2Cut()     {return fChi2Cut;}
  static Double_t  Chi2CutUniq() {return fChi2CutUniq;}
  static Double_t  Weight(Double_t chi2)   {
    // J.Phys. G: Nucl. Part. Phys. 34 (2007) N343-N356. Eq.(8)
#if 0
    return // TMath::Sqrt(chi2)/ 
      1.0/(1.0 + TMath::Exp(-(Chi2Cut()-chi2)/(2*Temperature())));
#endif
    return 
      (TMath::Exp(-chi2/(2*fTemperature)) + TMath::Exp(-fChi2Cut/(2*fTemperature)))/
      (                                1. + TMath::Exp(-fChi2Cut/(2*fTemperature)));
  }
private:
  static Double_t  fTemperature;
  static Double_t  fChi2Cut;
  static Double_t  fChi2CutUniq;
  ClassDef(StAnneling,1)
};
// $Log: StAnneling.h,v $
// Revision 2.2  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.2  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.3  2012/02/07 19:38:26  fisyak
// Repackage
//
#endif
