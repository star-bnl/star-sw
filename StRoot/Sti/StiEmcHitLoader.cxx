#include <iostream>
#include <cmath>

// StEvent
#include "StEventTypes.h"

//StDb
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StiEmcHitLoader.h"
#include "Factory.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiGeometryTransform.h"
#include "StiDetectorFinder.h"
#include "StiDetector.h"
#include "StEvent.h"
#include <stdio.h>

StiEmcHitLoader::StiEmcHitLoader()
  : StiHitLoader<StEvent,StiGeometryTransform>()
{}
    
StiEmcHitLoader::StiEmcHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      StiGeometryTransform*transform)
  : StiHitLoader<StEvent,StiGeometryTransform>(hitContainer,hitFactory,transform)
{}

StiEmcHitLoader::~StiEmcHitLoader()
{}

void StiEmcHitLoader::loadHits(StEvent* source)
{
  double nhit=0;
  double nprint = 10000.;
  StiHit* stiHit;
  vector<StEmcPoint*>::const_iterator pointIter;

  const StEmcCollection* emcCollection = source->emcCollection();

  // Barrel EMC
  const StSPtrVecEmcPoint & barrelPoints = emcCollection->barrelPoints();
  for (pointIter=barrelPoints.begin();
       pointIter!=barrelPoints.end();
       pointIter++)
    {
      stiHit = _hitFactory->getInstance();
      stiHit->reset(); 
      //_transform->operator()( *iter, stiHit);
      //stiHit->setDetector( layer );
      //_hitContainer->push_back( stihit );
      if (fmod(++nhit, nprint)==0.) 
	_messenger <<"Filling Barrel EMC Hit:\t"<<nhit<<endl;
    }  

  // Endcap EMC
  const StSPtrVecEmcPoint & endcapPoints = emcCollection->endcapPoints();
  for (pointIter=endcapPoints.begin();
       pointIter!=endcapPoints.end();
       pointIter++)
    {
      stiHit = _hitFactory->getInstance();
      stiHit->reset(); 
      //_transform->operator()( *iter, stiHit);
      //stiHit->setDetector( layer );
      //_hitContainer->push_back( stihit );
      if (fmod(++nhit, nprint)==0.) 
	_messenger <<"Filling Barrel EMC Hit:\t"<<nhit<<endl;
    }  
}
