#ifndef StiTrackToObjMap_H_INCLUDED
#define StiTrackToObjMap_H_INCLUDED
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackToIntMap.h"
#include "Sti/StiHitToHitMap.h"
#include "Sti/StiHitToTrackMap.h"
#include <map>
typedef map<StiTrack*, StiTrackToIntMap*> TrackToObjMap;

class StiTrackToObjMap: public TrackToObjMap
{
 public:
  StiTrackToObjMap();
  ~StiTrackToObjMap();
  
  void build(StiTrackContainer*,
	     StiHitToHitMap*, 
	     StiHitToTrackMap*);
  void clear();
  void analyze(Filter<StiTrack> * filter, StiHit * vertex);
};

#endif
