#include <iostream>
#include <stdexcept>
#include <cmath>
#include <stdio.h>
#include "StEvent.h"
#include "StEventTypes.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "StiTpcHitLoader.h"

StiTpcHitLoader::StiTpcHitLoader()
  : StiHitLoader<StEvent,StiDetectorBuilder>("TpcHitLoader")
{}
    
StiTpcHitLoader::StiTpcHitLoader(StiHitContainer* hitContainer,
				 Factory<StiHit>*hitFactory,
				 StiDetectorBuilder * detector)
  : StiHitLoader<StEvent,StiDetectorBuilder>("TpcHitLoader",hitContainer,hitFactory,detector)
{}

StiTpcHitLoader::~StiTpcHitLoader()
{}

void StiTpcHitLoader::loadHits(StEvent* source)
{
  //_messenger << "StiTpcHitLoader::loadHits(StEvent*) - INFO - Started" << endl;
  if (!_detector)
    throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
  if(!_hitContainer)
    throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");

  StiDetector * detector;
  StiHit* stiHit;
  const StTpcHitCollection* tpcHits = source->tpcHitCollection();
  unsigned int stiSector;
  for (unsigned int sector=0; sector<24; sector++) 
    {
      if (sector<12)
	stiSector = sector;
      else
	stiSector = 11 - (sector-11)%12;
      const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
      if (!secHits) 
	{
	  _messenger << "StiTpcHitLoader::loadHits(StEvent* source) -W- no hits for sector:"<<sector<<endl;
	  break;
	}
      for (unsigned int row=0; row<45; row++) 
	{
	  //_messenger << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
	  const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
	  if (!padrowHits)
	    {
	      //_messenger << "StiTpcHitLoader:loadHits() - row=="<<row<<" has padrowHits==0"<<endl;
	      break;
	    }
	  const StSPtrVecTpcHit& hitvec = padrowHits->hits();
	  detector = _detector->getDetector(row,stiSector);
	  
	  if (!detector)
	    {
	      cout << "StiTpcHitLoader::loadHits(StEvent*) -E- Detector not found for pad row:" << row
		   << " sector:"<<sector<<endl;
	      throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- Detector element not found");
	    }
	  for (vector<StTpcHit*>::const_iterator iter = hitvec.begin();
	       iter != hitvec.end(); 
	       iter++) 
	    {
	      StTpcHit*hit=*iter;
	      if(!_hitFactory)
		throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- _hitFactory==0");
	      stiHit = _hitFactory->getInstance();
	      if(!stiHit)
		throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- stiHit==0");
	      stiHit->setGlobal(detector, 
				hit, 
				hit->position().x(),
				hit->position().y(),
				hit->position().z(),
				hit->charge());
	      _hitContainer->push_back( stiHit );
	    }
	}
    }
}

