#ifndef STACCEPTALLTRACKS_HH
#define STACCEPTALLTRACKS_HH
#include "StRareTrackCut.h"
#include <Stiostream.h>
class StPrimaryTrack;
class StAcceptAllTracks : public StRareTrackCut {
 
 public:
  StAcceptAllTracks(){};
  ~StAcceptAllTracks(){};
  int  Accept(StPrimaryTrack* track);
  void Report();  
  ClassDef(StAcceptAllTracks,1)
    };

#endif
