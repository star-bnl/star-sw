#include "Stiostream.h"
#include <stdexcept>
#include <cmath>
#include <stdio.h>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
#include "Sti/Base/Factory.h"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcTrack.hh"
#include "Sti/StiHit.h"
#include "Sti/StiMcTrack.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "StiTpcHitLoader.h"

StiTpcHitLoader::StiTpcHitLoader()
: StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("TpcHitLoader")
{}

StiTpcHitLoader::StiTpcHitLoader(StiHitContainer* hitContainer,
                                 StiHitContainer* mcHitContainer,
                                 Factory<StiHit>*hitFactory,
                                 StiDetectorBuilder * detector)
: StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("TpcHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
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
    if (sector<12)
      stiSector = sector;
    else
      stiSector = 11 - (sector-11)%12;
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
      for (iter = hitvec.begin();iter != hitvec.end();++iter)
        {
        StTpcHit*hit=*iter;
        if(!_hitFactory) throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- _hitFactory==0");
        stiHit = _hitFactory->getInstance();
        if(!stiHit)   throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- stiHit==0");
        stiHit->reset();
        stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(), hit->position().z(),hit->charge());
        _hitContainer->add( stiHit );
        }
      }
    }
  cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Done" << endl;
}

void StiTpcHitLoader::loadMcHits(StMcEvent* source,
                                 bool useMcAsRec,
                                 Filter<StiTrack> * trackFilter,
                                 Filter<StiHit> * hitFilter,
                                 StMcTrack & stMcTrack,
                                 StiMcTrack & stiMcTrack)
{
  if (!_detector)         throw runtime_error("StiTpcHitLoader::loadMcHits(StEvent*) -F- _detector==0");
  if(!_mcHitContainer)    throw runtime_error("StiTpcHitLoader::loadMcHits(StEvent*) -F- _mcHitContainer==0");
  if(!_mcTrackFactory)    throw runtime_error("StiTpcHitLoader::loadMcHits() -F- _mcTrackFactory==0");
  if (!_mcTrackContainer) throw runtime_error("StiTpcHitLoader::loadMcHitss() -F- _mcTrackContainer==0");
  if(!_hitFactory)        throw runtime_error("StiTpcHitLoader::loadMcHits(StMcEvent*) -F- _hitFactory==0");
  const StPtrVecMcTpcHit& hits = stMcTrack.tpcHits();
  int nPts = hits.size();					if( nPts){}
  for (vector<StMcTpcHit*>::const_iterator iterHit = hits.begin();
       iterHit != hits.end();
       iterHit++)
    {
    StMcTpcHit*hit=*iterHit;
    if (!hit) 	throw runtime_error("StiTpcHitLoader::loadMcHits(...) -E- hit==0");
    unsigned int row = hit->padrow()-1;
    unsigned int sector = hit->sector()-1;
    unsigned int stiSector;
    if (sector<12)stiSector = sector;
    else stiSector = 11 - (sector-11)%12;
    //cout << "sector = " << sector <<" stiSector = "<<stiSector<<endl;
    StiDetector * detector = _detector->getDetector(row,stiSector);
    if (!detector)  throw runtime_error("StiTpcHitLoader::loadMcHits(StMcEvent*) -E- Detector element not found");
    StiHit * stiHit = _hitFactory->getInstance();
    if(!stiHit) throw runtime_error("StiTpcHitLoader::loadMcHits(StMcEvent*) -E- stiHit==0");
    stiHit->reset();
    stiHit->setGlobal(detector, 0, hit->position().x(),hit->position().y(),hit->position().z(),hit->dE());
    _mcHitContainer->add( stiHit );
    stiMcTrack.addHit(stiHit);
    if (useMcAsRec) _hitContainer->add( stiHit );
    }
}


