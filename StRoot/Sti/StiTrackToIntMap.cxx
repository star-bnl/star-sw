#include "Sti/StiTrack.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiTrackToIntMap.h"

StiTrackToIntMap::StiTrackToIntMap()
{}

StiTrackToIntMap::~StiTrackToIntMap()
{}

void StiTrackToIntMap::add(StiTrack* track)
{
  TrackToIntMap::iterator iter;
  iter= this->find(track);
  if(iter!=this->end())
    iter->second++;
  else
    insert(pair<StiTrack*, int>(track,1));
}

StiTrack* StiTrackToIntMap::getBestTrack()
{
  StiTrackToIntMap::const_iterator iter;
  int bestHitCount=0, hitCount=0;
  StiTrack* bestTrack=0;
  for(iter=this->begin(); iter!=this->end(); ++iter)
    {
      hitCount=iter->second;
      if(hitCount>bestHitCount)
	{
	  bestHitCount=hitCount;
	  bestTrack=iter->first;
	}
    }
  return bestTrack;
}

int StiTrackToIntMap::getBestTrackHitCount()
{ 
  StiTrackToIntMap::const_iterator iter;
  int bestHitCount=0, hitCount=0;
  for(iter=this->begin(); iter!=this->end(); ++iter)
    {
      hitCount=iter->second;
      if(hitCount>bestHitCount)
	bestHitCount=hitCount;
    }
  return bestHitCount;
}

int StiTrackToIntMap::getSize()
{
  return this->TrackToIntMap::size();
}
      

      


