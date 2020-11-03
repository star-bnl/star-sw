#ifndef STACCEPTALLL3TRACKS_HH
#define STACCEPTALLL3TRACKS_HH

#include "StL3RareTrackCut.h"
#include <Stiostream.h>

class StGlobalTrack;

class StAcceptAllL3Tracks : public StL3RareTrackCut {
 public:
  StAcceptAllL3Tracks(){};
  ~StAcceptAllL3Tracks(){};
  int  Accept(StGlobalTrack* track);
  void Report();

  ClassDef(StAcceptAllL3Tracks,1)
};

#endif
