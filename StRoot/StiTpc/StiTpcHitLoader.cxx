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
#include "StiTpc/StiTpcDetectorBuilder.h"
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
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
//  cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Started" << endl;
  assert(_detector);
  assert(_hitContainer);
  const StTpcHitCollection* tpcHits = source->tpcHitCollection();
  if (!tpcHits) return;
  UInt_t noHitsLoaded = 0;
  for (UInt_t sector=_minSector-1; sector<_maxSector; sector++)    {
    const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
    if (!secHits) {
      cout << "StiTpcHitLoader::loadHits(StEvent* source) -W- no hits for sector:"<<sector<<endl;
      break;
    }
    Float_t driftvel = 1e-6*gStTpcDb->DriftVelocity(sector+1); // cm/mkmsec

    _maxRow = St_tpcPadConfigC::instance()->numberOfRows(sector+1);

    for (UInt_t row=_minRow-1; row<_maxRow; row++) {
      //cout << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;

      // Negative Sti indices indicate a problem with Sti TPC geometry layout
      int stiSector, stiRow;
      std::tie(stiSector, stiRow) = StiTpcDetectorBuilder::toStiLayer(sector+1, row+1);

      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
      if (!padrowHits) break;
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      StiDetector * detector = _detector->getDetector(stiRow,stiSector);
      if (!detector) throw runtime_error("StiTpcHitLoader::loadHits(StEvent*) -E- Detector element not found");
      StiHitTest hitTest;
      for (const StTpcHit* hit : hitvec)
      {
	if (StiKalmanTrackNode::IsLaser() && hit->flag()) continue;
	if (hit->flag() & FCF_CHOPPED || hit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	if (hit->pad() > 182 || hit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
        assert(_hitFactory);
        StiHit* stiHit = _hitFactory->getInstance();
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
