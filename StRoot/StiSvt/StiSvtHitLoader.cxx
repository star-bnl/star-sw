#include <iostream>
#include <cmath>
#include <stdio.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/Base/Factory.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiDetector.h"

StiSvtHitLoader::StiSvtHitLoader()
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SvtHitLoader")
{}
    
StiSvtHitLoader::StiSvtHitLoader(StiHitContainer* hitContainer,
				 StiHitContainer* mcHitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder*detector)
  : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SvtHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiSvtHitLoader::~StiSvtHitLoader()
{}

void StiSvtHitLoader::loadHits(StEvent* source,
			       Filter<StiTrack> * trackFilter, 
			       Filter<StiHit> * hitFilter)
{
  return;
  _messenger <<"StiSvtHitLoader::loadHits() - Started"<<endl;
  if (!source)
    throw runtime_error("StiSvtHitLoader::loadHits() - FATAL - source==0 ");
  StSvtHitCollection* svthits = source->svtHitCollection();
  if (!svthits)
    {
      cout << "StiSvtHitLoader::loadHits(StEvent* source) - WARNING - NO SVT hits"<<endl;
      return;
    }
  StSvtHit* hit;
  StiHit* stiHit;
  StiDetector* detector;
  if (!_hitContainer)
    throw runtime_error("StiSvtHitLoader::loadHits() - FATAL - _hitContainer==0 ");
  for (unsigned int barrel=0; barrel<svthits->numberOfBarrels(); ++barrel) 
    {
      StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
      if (!barrelhits) break;
      for (unsigned int ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder) 
	{
	  StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
	  if (!ladderhits) break;
	  for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) 
	    {
	      _messenger <<"Barrel "<<barrel<<"\tladder "<<ladder<<"\twafer "<<wafer<<endl;
	      StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	      _messenger <<" StSvtWaferHitCollection retrieved" << endl;
	      if (!waferhits) break;
	      const StSPtrVecSvtHit& hits = waferhits->hits(); 
	      for (vector<StSvtHit*>::const_iterator it=hits.begin(); 
		   it!=hits.end(); 
		   ++it) 
		{
		  cout << "fetch hit"<<endl;
		  if (!*it) 
		    {
		      _messenger <<"StiSvtHitLoader::loadHits() - WARNING - *it==0"<<endl;
		      break;
		    }
		  hit = static_cast<StSvtHit*>(*it);
		  if (!hit) 
		    {
		      _messenger <<"StiSvtHitLoader::loadHits() - WARNING - hit==0"<<endl;
		      break;
		    }
		  detector = _detector->getDetector(hit->layer(),hit->ladder());
		  if (hit && detector && hit->flag()<4) 
		    {
		      cout << "StiSvtHitLoader::loadHits() - INFO - set global"<<endl;
		      stiHit = _hitFactory->getInstance();
		      stiHit->setGlobal(detector,hit,
					hit->position().x(),
					hit->position().y(),
					hit->position().z(),
					hit->charge() );
		      cout << "push the hit back"<<endl;
		      _hitContainer->push_back( stiHit );
		      
		    }
		}
	    }
	}
    }
  _messenger <<"StiSvtHitLoader::loadHits() - INFO - Done"<<endl;
}
	

void StiSvtHitLoader::loadMcHits(StMcEvent* source,
				 bool useMcAsRec,
				 Filter<StiTrack> * trackFilter, 
				 Filter<StiHit> * hitFilter)
{
  return;
}
	

