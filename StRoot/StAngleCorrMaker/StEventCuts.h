#ifndef StEventCuts_HH
#define StEventCuts_HH

///////////////////////////////////////////////////////////////////////////////
//
// StEventCuts
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

#include "StEvent.h"

class StEventCuts {

public:
           StEventCuts();
           ~StEventCuts();
  Int_t EventSatisfiesCuts(StEvent& ev);
  
  // set cuts
  void  SetMultiplicityCuts(Double_t lowerCut, Double_t upperCut);
    
 private:
  Double_t upperMult,lowerMult;
};

#endif
