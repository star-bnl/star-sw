#include "Sti/StiTrackToObjMap.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiHitContainer.h"
#include "Sti/Base/Filter.h"

StiTrackToObjMap::StiTrackToObjMap()
{}

StiTrackToObjMap::~StiTrackToObjMap()
{}

void StiTrackToObjMap::build(StiTrackContainer* trackContainer, 
			     StiHitToHitMap* hitToHitMap,
			     StiHitToTrackMap * hitToTrackMap)

{
  StiTrackContainer::iterator trackIter;
  HitVectorType::const_iterator hitIter;
  for (trackIter = trackContainer->begin(); trackIter != trackContainer->end(); ++trackIter)
    {
      //create a new trackToIntMap obj 
      StiTrackToIntMap * trackToIntMap = new StiTrackToIntMap;
      //loops through tracks
      const HitVectorType & hits = trackIter->first->getHits();
      for( hitIter = hits.begin();hitIter!=hits.end();++hitIter)
	{ 
	  HitToHitMap::const_iterator hitToHitMapIterator;
	  hitToHitMapIterator=hitToHitMap->find(*hitIter);
	  if(hitToHitMapIterator != hitToHitMap->end())
	    { 
	      HitToTrackMap::iterator hitToTrackMapIter;
	      hitToTrackMapIter = hitToTrackMap->find(hitToHitMapIterator->second);
	      if(hitToTrackMapIter != hitToTrackMap->end())
		{
		  trackToIntMap->add(hitToTrackMapIter->second);
		}
          }
	}
       insert(TrackToObjMap::value_type(trackIter->first, trackToIntMap));
    }
}
  
///Clear and delete all entries owned by this map
void StiTrackToObjMap::clear()
{
  for(StiTrackToObjMap::iterator iter=begin();iter!=end(); ++iter)
    {
      StiTrackToIntMap * trackToIntMap = iter->second;
      delete trackToIntMap;
    }
  TrackToObjMap::clear();
}

void StiTrackToObjMap::analyze(Filter<StiTrack> * filter)
{
  double analyzed = 0;
  double accepted = 0;
  double matched  = 0;
  for(StiTrackToObjMap::iterator iter=begin();iter!=end(); ++iter)
    {
      ++analyzed;
      if (filter->filter(iter->first))
	{
	  ++accepted;
	  StiTrackToIntMap * trackToIntMap = iter->second;
	  if (trackToIntMap)
	    {
	      cout << "Number of cnadidates:"<<trackToIntMap->getSize()<<endl;
	      if (trackToIntMap->getBestTrackHitCount()>10)
		++matched;
	    }
	}
    }  
  cout << "Analysis Results:" << endl
       << "   analyzed:" << analyzed << endl
       << "   accepted:" << accepted << endl
       << "    matched:" << matched <<endl;
  if (accepted>0)
    cout << " efficiency:" << matched/accepted << endl;
}
