#ifndef StNegativeTracks_HH
#define StNegativeTracks_HH
#include "StRareTrackCut.h"
#include <Stiostream.h>
class StPrimaryTrack;
class StNegativeTracks : public StRareTrackCut {
 
 public:
  StNegativeTracks(float pminin = 0.2, float pmaxin=10.0);
  ~StNegativeTracks(){};
  int  Accept(StPrimaryTrack* track);
  void Report();  
  ClassDef(StNegativeTracks,1)
    private:
  float pcut[2];
};

#endif
