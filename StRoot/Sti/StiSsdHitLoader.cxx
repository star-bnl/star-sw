#include <iostream>
#include <cmath>

// StEvent
#include "StEventTypes.h"

//StDb
#include "StDbUtilities/StGlobalCoordinate.hh"

#include "StiSsdHitLoader.h"
#include "Factory.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiGeometryTransform.h"
#include "StiDetectorFinder.h"
#include "StiDetector.h"
#include "StEvent.h"
#include <stdio.h>

StiSsdHitLoader::StiSsdHitLoader()
  : StiHitLoader<StEvent,StiGeometryTransform>()
{}
    
StiSsdHitLoader::StiSsdHitLoader(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiGeometryTransform*transform)
  : StiHitLoader<StEvent,StiGeometryTransform>(hitContainer,hitFactory,transform)
{}

StiSsdHitLoader::~StiSsdHitLoader()
{}

void StiSsdHitLoader::loadHits(StEvent* source)
{
  _messenger <<"StiSsdHitLoader::loadHits(StEvent* source) - INFO - Starting"<<endl;
  double nhit=0;
  double nprint = 1000.;
  StSsdHitCollection* ssdHitCollection = source->ssdHitCollection();

  StiHit* stiHit;
  unsigned int ladder;
  unsigned int wafer;
  StSsdHit* hit;
  vector<StSsdHit*>::const_iterator it;
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
		  _transform->operator()(hit, stiHit);
		  //stiHit->setDetector (layer );
		  _hitContainer->push_back( stiHit );
		  if (fmod(++nhit, nprint)==0.) _messenger <<"Filling SsdHit:\t"<<nhit<<endl;
		}
	    }
	}
    }
}
