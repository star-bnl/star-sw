#ifndef StiTrackToIntMap_H_INCLUDED
#define StiTrackToIntMap_H_INCLUDED
#include "Sti/StiTrack.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrackContainer.h"
#include <map>
typedef map<StiTrack*, int> TrackToIntMap;

class StiTrackToIntMap: public TrackToIntMap

{
  public:	
  StiTrackToIntMap();
  virtual ~StiTrackToIntMap(); 
  void add(StiTrack* track);
  StiTrack* getBestTrack();
  int getBestTrackHitCount();
  int getSize();
};
 #endif
