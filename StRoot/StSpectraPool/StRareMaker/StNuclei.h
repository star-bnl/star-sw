#ifndef STNuclei_HH
#define STNuclei_HH
#include "StRareTrackCut.h"
#include <Stiostream.h>
class StPrimaryTrack;
class StNuclei : public StRareTrackCut {
 
 public:
  StNuclei(float pminin = 0.2, float pmaxin=10.0,int charge = 0, int mass = 2);
  ~StNuclei(){};
  int  Accept(StPrimaryTrack* track);
  void Report();  
  ClassDef(StNuclei,1)
    private:
  float pcut[2];
  int chargeToAccept;  //<0 = negatives, 0 = all, >0 = positives
  int minMass;
};

#endif
