#include <iostream>
#include <cmath>

// StEvent
#include "StEventTypes.h"

//StDb
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StiFtpcHitLoader.h"
#include "Factory.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiGeometryTransform.h"
#include "StiDetectorFinder.h"
#include "StiDetector.h"
#include "StEvent.h"
#include <stdio.h>

StiFtpcHitLoader::StiFtpcHitLoader()
  : StiHitLoader<StEvent,StiGeometryTransform>()
{}
    
StiFtpcHitLoader::StiFtpcHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      StiGeometryTransform*transform)
  : StiHitLoader<StEvent,StiGeometryTransform>(hitContainer,hitFactory,transform)
{}

StiFtpcHitLoader::~StiFtpcHitLoader()
{}

void StiFtpcHitLoader::loadHits(StEvent* source)
{
  double nhit=0;
  double nprint = 10000.;
  unsigned int plane;
  unsigned int sector;
  const StFtpcHitCollection* ftpcHitCollection = source->ftpcHitCollection();
  const StFtpcPlaneHitCollection* ftpcHitPlaneCollection;
  const StFtpcSectorHitCollection*ftpcHitSectorCollection;
  StiHit* stiHit;

  for (plane=0; 
       plane<ftpcHitCollection->numberOfPlanes(); 
       plane++) 
    {
      ftpcHitPlaneCollection = ftpcHitCollection->plane(plane);
      for (sector=0; 
	   sector<ftpcHitPlaneCollection->numberOfSectors(); 
	   sector++) 
	{
	  ftpcHitSectorCollection = ftpcHitPlaneCollection->sector(sector);
	  const StSPtrVecFtpcHit&hitVector = ftpcHitSectorCollection->hits();
	  vector<StFtpcHit*>::const_iterator iter;
	  for (iter=hitVector.begin();
	       iter!=hitVector.end();
	       iter++)
	    {
	      stiHit = _hitFactory->getInstance();
	      stiHit->reset();
	      //_transform->operator()( *iter, stiHit);
	      //stihit->setDetector( layer );
	      //_hitContainer->push_back( stiHit );
	      if (fmod(++nhit, nprint)==0.) 
		_messenger <<"Filling FtpcHit:\t"<<nhit<<endl;
	    }
	}
    }
}
