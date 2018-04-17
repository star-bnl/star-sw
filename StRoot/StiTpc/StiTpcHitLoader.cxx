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
#include "Sti/StiKalmanTrackNode.h"
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
//________________________________________________________________________________
StiTpcHitLoader::StiTpcHitLoader(): StiHitLoader<StEvent,StiDetectorBuilder>("TpcHitLoader"), 
				    _minRow(1), _maxRow(100), _minSector(1), _maxSector(24) {}
//________________________________________________________________________________
StiTpcHitLoader::StiTpcHitLoader(StiHitContainer* hitContainer,
                                 Factory<StiHit>*hitFactory,
                                 StiDetectorBuilder * detector)
: StiHitLoader<StEvent,StiDetectorBuilder>("TpcHitLoader",hitContainer,hitFactory,detector), 
				    _minRow(1), _maxRow(100), _minSector(1), _maxSector(24) {}
//________________________________________________________________________________
void StiTpcHitLoader::loadHits(StEvent* source,
                               Filter<StiTrack> * trackFilter,
                               Filter<StiHit> * hitFilter)
{
  static Int_t debug = 0;
  _maxRow = St_tpcPadPlanesC::instance()->padRows();
//  cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Started" << endl;
  assert(_detector);
  assert(_hitContainer);
  StiDetector * detector;
  StiHit* stiHit;
  const StTpcHitCollection* tpcHits = source->tpcHitCollection();
  if (!tpcHits) return;
  UInt_t stiSector;
  UInt_t noHitsLoaded = 0;
  for (UInt_t sector=_minSector-1; sector<_maxSector; sector++)    {
#if 0
    stiSector = sector;
#else
    if (sector<12)      stiSector = sector;
    else                stiSector = 11 - (sector-11)%12;
#endif
    const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
    if (!secHits) {
      cout << "StiTpcHitLoader::loadHits(StEvent* source) -W- no hits for sector:"<<sector<<endl;
      break;
    }
    Float_t driftvel = 1e-6*gStTpcDb->DriftVelocity(sector+1); // cm/mkmsec
    for (UInt_t row=_minRow-1; row<_maxRow; row++) {
      //cout << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
      if (!padrowHits) break;
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      detector = _detector->getDetector(row,stiSector);

      if (!detector) throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- Detector element not found");
      const_StTpcHitIterator iter;
      StiHitTest hitTest;
      for (iter = hitvec.begin();iter != hitvec.end();++iter)        {
        StTpcHit*hit=*iter;
        if (hit->detector() == kiTpcId) continue;
	if (StiKalmanTrackNode::IsLaser() && hit->flag()) continue;
	if (hit->flag() & FCF_CHOPPED || hit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	if (hit->pad() > 182 || hit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
        assert(_hitFactory);
        stiHit = _hitFactory->getInstance();
        assert(stiHit);
        stiHit->reset();
        stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(), hit->position().z(),hit->charge());
        hitTest.add(hit->position().x(),hit->position().y(), hit->position().z());
	if (hit->sector() <= 12) stiHit->setVz( driftvel);
	else                     stiHit->setVz(-driftvel);
        _hitContainer->add( stiHit );
	noHitsLoaded++;
	if (debug) {
	  cout << "add hit S/R =" << sector << "/" << row << " to detector " << *detector << endl;
	}
      }
      if (hitTest.width()>0.1) {
	printf("**** TPC hits too wide (%g) sector=%d row%d\n"
	       ,hitTest.width(),sector,row);
      }
      
    }
  }
//    cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Done with " << noHitsLoaded << " hits" <<  endl;
}
//________________________________________________________________________________
