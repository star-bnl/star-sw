#include "StiHitContainer.h"
#include "StiHitToTrackMap.h"

StiHitToTrackMap::StiHitToTrackMap()
{}

StiHitToTrackMap::~StiHitToTrackMap()
{}

void StiHitToTrackMap::build(StiTrackContainer & trackContainer)
{ 
  TrackToTrackMap::const_iterator  trackIter;
  vector<StiHit*>::const_iterator hitIter;
  for (trackIter=trackContainer.begin();
       trackIter!=trackContainer.end();
       trackIter++)
    {
      const vector<StiHit*> & hits = trackIter->first->getHits();
      for (hitIter=hits.begin();hitIter!=hits.end();hitIter++)
         insert( HitToTrackMap::value_type(*hitIter, trackIter->first) );
    }
}
