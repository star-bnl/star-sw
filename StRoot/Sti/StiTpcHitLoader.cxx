#include <iostream>
#include <cmath>

// StEvent
#include "StEventTypes.h"

//StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

#include "StiTpcHitLoader.h"
#include "Factory.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiGeometryTransform.h"
#include "StiDetectorFinder.h"
#include "StiDetector.h"
#include "StEvent.h"
#include <stdio.h>

StiTpcHitLoader::StiTpcHitLoader()
  : StiHitLoader<StEvent,StiGeometryTransform>()
{}
    
StiTpcHitLoader::StiTpcHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      StiGeometryTransform*transform)
  : StiHitLoader<StEvent,StiGeometryTransform>(hitContainer,hitFactory,transform)
{}

StiTpcHitLoader::~StiTpcHitLoader()
{}

void StiTpcHitLoader::loadHits(StEvent* source)
{
  double nhit=0;
  double nprint = 10000.;
  const StTpcHitCollection* tpcHits = source->tpcHitCollection();

  //Loop over sectors
  for (int sector=1; sector<=24; sector++) 
    {
      const StTpcSectorHitCollection* secHits = tpcHits->sector(sector-1);
      //Loop over padrows
      for (int prow=1; prow<=45; prow++) 
	{
	  const StTpcPadrowHitCollection* padrowHits = secHits->padrow(prow-1);
	  const StSPtrVecTpcHit& hitvec = padrowHits->hits();
	  //Find the detector for this set of hits:
	  char szBuf[100];
	  int iIttfSector=sector;
	  if (sector>12) iIttfSector = 12 - (sector-12)%12;
	  sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d",static_cast<int>(prow), static_cast<int>(iIttfSector));
	  StiDetector* layer = StiDetectorFinder::instance()->findDetector(szBuf);
	  if (!layer) 
	    {
	      _messenger <<"StiHitFiller::fillTpcHits(). ERROR:\t";
	      _messenger <<"Detector for (sector,padrow): (";
	      _messenger <<sector<<","<<prow<<") not found.  Abort"<<endl;
	      return;
	    }
	  //Loop over hits   
	  for (vector<StTpcHit*>::const_iterator iter = hitvec.begin();
	       iter != hitvec.end(); iter++) 
	    {
	      //Now we have the hit
	      StiHit* stihit = _hitFactory->getInstance();
	      stihit->reset();
	      _transform->operator()( *iter, stihit);
	      stihit->setDetector( layer );
	      //Now Fill the Hit Container!
	      //Check the hit (temp)
	      _hitContainer->push_back( stihit );
	      ++nhit;
	      if (fmod(nhit, nprint)==0.) 
		_messenger <<"Filling TpcHit:\t"<<nhit<<endl;
	    }
	}
    }
}
