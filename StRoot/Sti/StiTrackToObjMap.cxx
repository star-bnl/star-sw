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

void StiTrackToObjMap::analyze(Filter<StiTrack> * filter, StiHit * vertex)
{
  cout << "StiTrackToObjMap::analyze() -I- Started"<<endl;
  if (!filter || !vertex)
    {
      cout << "StiTrackToObjMap::analyze() -E- filter :" << filter <<" vertex:" << vertex<<endl;
      return;
    }
  double analyzed = 0;
  double accepted = 0;
  double acceptedPrimary = 0;
  double matched1  = 0;
  double matched2  = 0;
  double matched3  = 0;
  double matchedPrimary  = 0;
  double matchedPrimaryGoodDca  = 0;
  for(StiTrackToObjMap::iterator iter=begin();iter!=end(); ++iter)
    {
      cout << "StiTrackToObjMap::analyze() -I- loop "<<endl;          
      StiTrack * first = iter->first;
      if(!first) continue;
      StiTrack * second; 
      ++analyzed;
      if (filter->filter(first))
	{
	  ++accepted;
	  bool firstPrimary = first->isPrimary();
	  if (firstPrimary)
	    ++acceptedPrimary;
	  
	  StiTrackToIntMap * trackToIntMap = iter->second;
	  if (trackToIntMap)
	    {
	      //cout << "Number of cnadidates:"<<trackToIntMap->getSize()<<endl;
	      if (trackToIntMap->getBestTrackHitCount()>5)
		++matched1;
	      if (trackToIntMap->getBestTrackHitCount()>5 && trackToIntMap->getSize()<2)
		++matched2;
	      if (trackToIntMap->getBestTrackHitCount()>5 && trackToIntMap->getSize()<3)
		++matched3;
	      second = trackToIntMap->getBestTrack();
	      if (firstPrimary)
		{
		  ++matchedPrimary;
		  if (vertex && second->getDca(vertex)<3.)
		    ++matchedPrimaryGoodDca;
		}
	    }
	}
    }  
  cout << "Analysis Results:" << endl
       << "   analyzed:" << analyzed << endl
       << "   accepted:" << accepted << endl
       << "    matched1:" << matched1 <<endl
       << "    matched2:" << matched2 <<endl
       << "    matched3:" << matched3 <<endl;
  if (accepted>0)
    {
      cout << " efficiency1:" << matched1/accepted << endl;
      cout << " efficiency2:" << matched2/accepted << endl;
      cout << " efficiency3:" << matched3/accepted << endl;
    }
  cout << "Primaries Results"<<endl
       << "  accepted primaries" << acceptedPrimary << endl
       << "  matched  primaries" << matchedPrimary << endl
       << "  good dca primaries" << matchedPrimaryGoodDca << endl;
  if (acceptedPrimary>0)
    cout << " matched primary  eff:"<< matchedPrimary/acceptedPrimary << endl
	 << " good dca primary eff:"<< matchedPrimaryGoodDca/acceptedPrimary << endl;
}
