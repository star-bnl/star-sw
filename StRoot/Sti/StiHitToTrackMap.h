#ifndef StiHitToTrackMap_H_INCLUDED
#define StiHitToTrackMap_H_INCLUDED
#include "StiHit.h"
#include "StiTrack.h"
#include "StiTrackContainer.h"
typedef map<StiHit*,StiTrack*> HitToTrackMap;

class StiHitToTrackMap : public HitToTrackMap
{
 public:

  StiHitToTrackMap();
  virtual ~StiHitToTrackMap();
  void build(StiTrackContainer * trackContainer);
};


#endif
