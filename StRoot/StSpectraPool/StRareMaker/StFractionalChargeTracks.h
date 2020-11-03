#ifndef STFractionalChargeTRACKS_HH
#define STFractionalChargeTRACKS_HH
#include "StRareTrackCut.h"
#include <Stiostream.h>
class StPrimaryTrack;
class StFractionalChargeTracks : public StRareTrackCut {
 
 public:
  StFractionalChargeTracks(float dedxcutin, float pminin);
  ~StFractionalChargeTracks(){};
  int  Accept(StPrimaryTrack* track);
  void Report();  
  ClassDef(StFractionalChargeTracks,1)
    private:
  float pcut;
  float dedxcut;
};

#endif
