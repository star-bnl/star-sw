#include "Stiostream.h"
#include <cmath>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiDetector.h"
#include "StiSsd/StiSsdHitLoader.h"

StiSsdHitLoader::StiSsdHitLoader()
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SsdHitLoader")
{}
    
StiSsdHitLoader::StiSsdHitLoader(StiHitContainer* hitContainer,
				 StiHitContainer* mcHitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder*transform)
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SsdHitLoader",hitContainer,mcHitContainer,hitFactory,transform)
{}

StiSsdHitLoader::~StiSsdHitLoader()
{}

void StiSsdHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter, 
			       Filter<StiHit> * hitFilter)
{
  _messenger <<"StiSsdHitLoader::loadHits(StEvent* source) - INFO - Starting"<<endl;
  double nhit=0;
  double nprint = 1000.;
  StSsdHitCollection* ssdHitCollection = source->ssdHitCollection();

  StiHit* stiHit;
  unsigned int ladder;
  unsigned int wafer;
  StSsdHit* hit;
//VP  vector<StSsdHit*>::const_iterator it;
  const_StSsdHitIterator it;
  StSsdLadderHitCollection* ssdLadderHitCollection;
  StSsdWaferHitCollection* ssdWaferHitCollection;
  for (ladder=1; 
       ladder<=ssdHitCollection->numberOfLadders(); 
       ++ladder) 
    {
      ssdLadderHitCollection = ssdHitCollection->ladder(ladder-1);
      for (wafer=1; 
	   wafer<=ssdLadderHitCollection->numberOfWafers(); 
	   ++wafer) 
	{
	  ssdWaferHitCollection = ssdLadderHitCollection->wafer(wafer-1);
	  const StSPtrVecSsdHit& hits = ssdWaferHitCollection->hits();
	  for (it=hits.begin(); 
	       it!=hits.end(); 
	       ++it) 
	    {
	      hit = static_cast<StSsdHit*>(*it);
	      if (hit) 
		{
		  stiHit = _hitFactory->getInstance();
		  stiHit->reset();
		  operator()(hit, stiHit);
		  //stiHit->setDetector (layer );
		  _hitContainer->add( stiHit );
		  if (fmod(++nhit, nprint)==0.) _messenger <<"Filling SsdHit:\t"<<nhit<<endl;
		}
	    }
	}
    }
}

void StiSsdHitLoader::operator() (const StSsdHit* ssdhit, StiHit* stiHit)
{
  // needs an implementation
}

void StiSsdHitLoader::loadMcHits(StMcEvent* source,
				 bool useMcAsRec,
				 Filter<StiTrack> * trackFilter, 
				 Filter<StiHit> * hitFilter)
{}
