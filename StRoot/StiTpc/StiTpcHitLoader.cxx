#include "Stiostream.h"
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
#include "Sti/StiHitTest.h"

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

void StiTpcHitLoader::loadHits(StEvent* source,
                               Filter<StiTrack> * trackFilter,
                               Filter<StiHit> * hitFilter)
{
  cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Started" << endl;
  if (!_detector)
    throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
  if(!_hitContainer)
    throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");

  StiDetector * detector;
  StiHit* stiHit;
  const StTpcHitCollection* tpcHits = source->tpcHitCollection();
  if (!tpcHits) return;
  unsigned int stiSector;
  for (unsigned int sector=0; sector<24; sector++)
    {
#if 0
      stiSector = sector;
#else
    if (sector<12)
      stiSector = sector;
    else
      stiSector = 11 - (sector-11)%12;
#endif
    const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
    if (!secHits)
      {
      cout << "StiTpcHitLoader::loadHits(StEvent* source) -W- no hits for sector:"<<sector<<endl;
      break;
      }
    for (unsigned int row=0; row<45; row++)
      {
      //cout << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
      if (!padrowHits) break;
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      detector = _detector->getDetector(row,stiSector);

      if (!detector) throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- Detector element not found");
      const_StTpcHitIterator iter;
      StiHitTest hitTest;
      for (iter = hitvec.begin();iter != hitvec.end();++iter)
        {
        StTpcHit*hit=*iter;
        if(!_hitFactory) throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- _hitFactory==0");
        stiHit = _hitFactory->getInstance();
        if(!stiHit)   throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- stiHit==0");
        stiHit->reset();
        stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(), hit->position().z(),hit->charge());
        hitTest.add(hit->position().x(),hit->position().y(), hit->position().z());
        _hitContainer->add( stiHit );
        }
      if (hitTest.width()>0.1) {
  	  printf("**** TPC hits too wide (%g) sector=%d row%d\n"
	        ,hitTest.width(),sector,row);
      }
  
      }
    }
  cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Done" << endl;
}
