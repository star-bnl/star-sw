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
#include "StiTpcDetectorBuilder.h"
#include "Sti/StiHitTest.h"
#include "Sti/StiKalmanTrackNode.h"
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
//________________________________________________________________________________
StiTpcHitLoader::StiTpcHitLoader(): StiHitLoader<StEvent,StiDetectorBuilder>("TpcHitLoader"),
  _minRow(1), _maxRow(72), _minSector(1), _maxSector(24), _maxZ(1000)   { }

//________________________________________________________________________________
StiTpcHitLoader::StiTpcHitLoader(StiHitContainer* hitContainer,
                                 Factory<StiHit>*hitFactory,
                                 StiDetectorBuilder * detector)
: StiHitLoader<StEvent,StiDetectorBuilder>("TpcHitLoader",hitContainer,hitFactory,detector), 
  _minRow(1), _maxRow(72), _minSector(1), _maxSector(24), _maxZ(1000) {}
//________________________________________________________________________________
void StiTpcHitLoader::loadHits(StEvent* source,
                               Filter<StiTrack> * trackFilter,
                               Filter<StiHit> * hitFilter)
{
  static Int_t debug = 0;
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
    const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
    if (! secHits->numberOfHits()) continue;
    if (_detector->getNSectors() == 24) {
      stiSector = sector;
    } else {
      if (sector<12)      stiSector = sector;
      else                stiSector = 11 - (sector-11)%12;
    }
    _maxRow = St_tpcPadConfigC::instance()->padRows(sector+1);
    for (UInt_t row=_minRow-1; row<_maxRow; row++) {
      //cout << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
      if (!padrowHits) break;
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      if (! hitvec.size()) continue;
      const_StTpcHitIterator iter;
      StiHitTest hitTest;
      Int_t StiRow = StiTpcDetectorBuilder::StiRow(sector+1,row+1)-1;
      detector = _detector->getDetector(StiRow,stiSector);
      assert(detector);
      
      for (iter = hitvec.begin();iter != hitvec.end();++iter)        {
        StTpcHit*hit=*iter;
	if (StiKalmanTrackNode::IsLaser() && hit->flag()) continue;
	if (hit->flag() & FCF_CHOPPED || hit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	if (hit->pad() > 182 || hit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
	if (TMath::Abs( hit->position().z() ) > _maxZ) continue;
#if 0 /*VP*/
{        
        double x = hit->position().x();
        double y = hit->position().y();
//      double z = hit->position().z();
        double ang = atan2(y,x);
        int sec = StiTpcDetectorBuilder::sector(ang,hit->sector()>12);
        sec -= (int)hit->sector();
	if (sec> 6) sec-=12;
	if (sec<-6) sec+=12;
        assert(abs(sec)<=1);

}
#endif
	assert(_hitFactory);
        stiHit = _hitFactory->getInstance();
        assert(stiHit);
        stiHit->reset();
        stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(), hit->position().z(),hit->charge());
        hitTest.add(hit->position().x(),hit->position().y(), hit->position().z());
	Float_t driftvel = 1e-6*gStTpcDb->DriftVelocity(sector+1,row+1); // cm/mkmsec
	if (hit->sector() <= 12) stiHit->setVz( driftvel);
	else                     stiHit->setVz(-driftvel);
        _hitContainer->add( stiHit );
	noHitsLoaded++;
	if (debug) {
	  cout << "add hit S/R =" << sector + 1 << "/" << row + 1 << " to detector " << *detector << endl;
	}
      }
      if (hitTest.width()>0.1) {
	printf("**** TPC hits too wide (%g) sector=%d row%d\n"
	       ,hitTest.width(),sector,row);
      }
      
    }
  }
}
//________________________________________________________________________________
