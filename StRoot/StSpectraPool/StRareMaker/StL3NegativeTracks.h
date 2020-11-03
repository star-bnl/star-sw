#ifndef STL3NEGATIVETRACKS_HH
#define STL3NEGATIVETRACKS_HH

#include "StL3RareTrackCut.h"
#include <Stiostream.h>

class StGlobalTrack;

class StL3NegativeTracks : public StL3RareTrackCut {
 public:
      StL3NegativeTracks(float pminin = 0.3, float pmaxin=10.0);
      ~StL3NegativeTracks(){};
      int  Accept(StGlobalTrack* track);
      void Report();

 private:
      float pcut[2];

 ClassDef(StL3NegativeTracks,1)

};

#endif
