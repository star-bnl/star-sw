#ifndef StTrackCuts_HH
#define StTrackCuts_HH

///////////////////////////////////////////////////////////////////////////////
//
// StTrackCuts
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "StTrackForPool.h"

class StTrackCuts {

public:
           StTrackCuts();
           ~StTrackCuts();
  Int_t TrackSatisfiesCuts( StTrackForPool* t);
  
  // set cuts
  void  SetMomentumCuts(Double_t lowerCut, Double_t upperCut);
  void  SetPtCuts(Double_t lowerCut, Double_t upperCut); 
  void  SetTrackCharge(Double_t Cut); 
  void  SetPseudoRapidityCuts(Double_t lowerCut, Double_t upperCut);
  void  SetRChiXYCuts(Double_t lowerCut, Double_t upperCut);
  void  SetRChiZCuts(Double_t lowerCut, Double_t upperCut);
  void  SetNTPCPointsCuts(Double_t lowerCut, Double_t upperCut);
  
 private:
  TString chargeCut;
  Double_t p_lowCut,p_upCut;
  Double_t pt_lowCut,pt_upCut;
  Double_t charge;
  Double_t rap_lowCut,rap_upCut;
  Double_t rchiXY_lowCut,rchiXY_upCut;
  Double_t rchiZ_lowCut,rchiZ_upCut;
  Double_t nPoints_lowCut,nPoints_upCut;
  
  

};

#endif
