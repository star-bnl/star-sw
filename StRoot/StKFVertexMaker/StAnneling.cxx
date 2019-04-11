// $Id: StAnneling.cxx,v 2.2 2018/04/10 11:32:09 smirnovd Exp $
#include "StAnneling.h"
Double_t StAnneling::fChi2CutUniq = 13.81; // Prob = 1e-3
Double_t StAnneling::fChi2Cut        =  200.0;// 32.2;    // Prob = 1e-7
Double_t StAnneling::fTemperature = 1;  
ClassImp(StAnneling);
Double_t  StAnneling::Weight(Double_t chi2)   {
  // J.Phys. G: Nucl. Part. Phys. 34 (2007) N343-N356. Eq.(8)
  Double_t w = 0;
#if 0
  //  return // TMath::Sqrt(chi2)/ 
   w =  1.0/(1.0 + TMath::Exp(-(Chi2Cut()-chi2)/(2*Temperature())));
#endif
  w = 
    (TMath::Exp(-chi2/(2*fTemperature)) + TMath::Exp(-fChi2Cut/(2*fTemperature)))/
    (                                                        1. + TMath::Exp(-fChi2Cut/(2*fTemperature)));
  return w;
}
// $Log: StAnneling.cxx,v $
// Revision 2.2  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.2  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
