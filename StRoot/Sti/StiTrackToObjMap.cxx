#include "Sti/StiTrackToObjMap.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiHitContainer.h"
#include "Sti/Base/Filter.h"

StiTrackToObjMap::StiTrackToObjMap()
{}

StiTrackToObjMap::~StiTrackToObjMap()
{
  clear();
}

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
  double matchedPrimary1  = 0;
  double matchedPrimary2  = 0;
  double matchedPrimary3  = 0;

  for(StiTrackToObjMap::iterator iter=begin();iter!=end(); ++iter)
    {
      //cout << "StiTrackToObjMap::analyze() -I- loop "<<endl;          
      StiTrack * first = iter->first;
      if(!first) continue;
      StiTrack * second=0; 	if(second){}
      ++analyzed;
      if (filter->filter(first))
	{
	  ++accepted;
	  bool firstPrimary = first->isPrimary();
	  double nPtsFirst = first->getPointCount();
	  double nCommon;
	  double ratio;
	  if (firstPrimary)
	    ++acceptedPrimary;
	  
	  StiTrackToIntMap * trackToIntMap = iter->second;
	  if (trackToIntMap && nPtsFirst>0)
	    {

	      //cout << "Number of cnadidates:"<<trackToIntMap->getSize()<<endl;
	      nCommon = trackToIntMap->getBestTrackHitCount();
	      ratio = nCommon/nPtsFirst;
	      if (nCommon>5) ++matched1;
	      if (nCommon>5 && ratio>0.5) ++matched2;
	      if (nCommon>5 && ratio>0.75) ++matched3;
	      if (firstPrimary)
		{
		  if (nCommon>5) ++matchedPrimary1;
		  if (nCommon>5 && ratio>0.5) ++matchedPrimary2;
		  if (nCommon>5 && ratio>0.75) ++matchedPrimary3;
		}

	    }
	  else cout << " BUG!" <<endl;
	}
    }  

    if (accepted>0)
      cout << "Analysis Results:" << endl
	   << "   analyzed: " << analyzed << endl
	   << "   accepted: " << accepted << endl
	   << "   matched1: " << matched1 <<" efficiency1: " << matched1/accepted <<endl
	   << "   matched2: " << matched2 <<" efficiency2: " << matched2/accepted <<endl
	   << "   matched3: " << matched3 <<" efficiency3: " << matched3/accepted <<endl;
    else
      cout  << "Analysis Results:" << endl
	    << "   analyzed: " << analyzed << endl
	    << "   accepted:" << accepted << endl;
    if (acceptedPrimary>0)
      cout << "Primaries Results"<<endl
	   << "  accepted primaries" << acceptedPrimary << endl
	   << "   matched1: " << matchedPrimary1 <<" efficiency1: " << matchedPrimary1/acceptedPrimary <<endl
	   << "   matched2: " << matchedPrimary2 <<" efficiency2: " << matchedPrimary2/acceptedPrimary <<endl
	   << "   matched3: " << matchedPrimary3 <<" efficiency3: " << matchedPrimary3/acceptedPrimary <<endl;
    else
      cout << "Primaries Results"<<endl
	   << "  accepted primaries" << acceptedPrimary << endl;

}
