#include <iostream>
#include <cmath>

// StEvent
#include "StEventTypes.h"

//StDb
#include "StDbUtilities/StGlobalCoordinate.hh"

#include "StiSvtHitLoader.h"
#include "Factory.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiGeometryTransform.h"
#include "StiDetectorFinder.h"
#include "StiDetector.h"
#include "StEvent.h"
#include <stdio.h>

StiSvtHitLoader::StiSvtHitLoader()
  : StiHitLoader<StEvent,StiGeometryTransform>()
{}
    
StiSvtHitLoader::StiSvtHitLoader(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiGeometryTransform*transform)
  : StiHitLoader<StEvent,StiGeometryTransform>(hitContainer,hitFactory,transform)
{}

StiSvtHitLoader::~StiSvtHitLoader()
{}

void StiSvtHitLoader::loadHits(StEvent* source)
{
  _messenger <<"StiHitFiller::fillSvtHits()"<<endl;
  double nhit=0;
  double nprint = 1000.;
  StSvtHitCollection* svthits = source->svtHitCollection();
  //loop on barrels
  for (unsigned int barrel=1; barrel<=svthits->numberOfBarrels(); ++barrel) 
    {
      StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel-1);
      //Loop on ladders
      for (unsigned int ladder=1; ladder<=barrelhits->numberOfLadders(); ++ladder) 
	{
	  StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder-1);
	  //Loop on wafers
	  for (unsigned int wafer=1; wafer<=ladderhits->numberOfWafers(); ++wafer) 
	    {
	      //_messenger <<"Barrel "<<barrel<<"\tladder "<<ladder<<"\twafer "<<wafer<<endl;
	      StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer-1);
	      const StSPtrVecSvtHit& hits = waferhits->hits();  //Finally!
	      for (vector<StSvtHit*>::const_iterator it=hits.begin(); it!=hits.end(); ++it) 
		{
		  StSvtHit* hit = dynamic_cast<StSvtHit*>(*it);
		  if (hit) 
		    {
		      if (hit->flag()<4) 
			{
			  //Now we've got the hit, fillerup!
			  StiHit* stihit = _hitFactory->getInstance();
			  stihit->reset();
			  _transform->operator()(hit, stihit);
			  //stihit->setDetector (layer );
			  _hitContainer->push_back( stihit );
			  if (fmod(++nhit, nprint)==0.) 
			    _messenger <<"Filling SvtHit:\t"<<nhit<<endl;
			}
		    }
		}
	    }
	}
    }
}
