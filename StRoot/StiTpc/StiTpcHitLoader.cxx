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
  StiDetector * detector;
  StiHit* stiHit;
  const StTpcHitCollection* tpcHits = source->tpcHitCollection();
  if (!tpcHits) return;
  UInt_t stiSector;
  UInt_t noHitsLoaded = 0;
  for (UInt_t sector=_minSector-1; sector<_maxSector; sector++)    {
    const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
    if (! secHits->numberOfHits())	continue;
    stiSector = toStiSect(sector);
    _maxRow = St_tpcPadConfigC::instance()->padRows(sector+1);
    Float_t driftvel = 1e-6*gStTpcDb->DriftVelocity(sector+1); // cm/mkmsec
    for (UInt_t row=_minRow-1; row<_maxRow; row++) {
      //cout << "StiTpcHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
      if (!padrowHits) 			continue;
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      if (! hitvec.size()) 		continue;
      const_StTpcHitIterator iter;
      StiHitTest hitTest;
      detector = _detector->getDetector(row,sector);
      assert(detector);
      if (!detector->isActive())	continue;

// printf("HitLoad %s sect=%d row=%d\n",detector->getName().c_str(),sector+1,row+1);
      
      for (iter = hitvec.begin();iter != hitvec.end();++iter)        {
        StTpcHit *hit=*iter;
	if (StiKalmanTrackNode::IsLaser() && hit->flag()) continue;
	if (hit->flag() & FCF_CHOPPED || hit->flag() & FCF_SANITY)     continue; // ignore hits marked by AfterBurner as chopped or bad sanity
	if (hit->pad() > 182 || hit->timeBucket() > 511) continue; // some garbadge  for y2001 daq
{        
        double x = hit->position().x();
        double y = hit->position().y();
//      double z = hit->position().y();
        double ang = atan2(y,x);
        int sec = StiTpcDetectorBuilder::sector(ang,hit->sector()>12);
        assert((int)hit->sector()==sec);

}
	assert(_hitFactory);
        stiHit = _hitFactory->getInstance();
	assert(stiHit);
        stiHit->reset();
        stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(), hit->position().z(),hit->charge());
        hitTest.add(hit->position().x(),hit->position().y(), hit->position().z());
	if (hit->sector() <= 12) stiHit->setVz( driftvel);
	else                     stiHit->setVz(-driftvel);
       _hitContainer->add( stiHit );
///     giveOut(detector,stiSector,row,stiHit);
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
//  _hitContainer->print(); 
//    cout << "StiTpcHitLoader::loadHits(StEvent*) -I- Done with " << noHitsLoaded << " hits" <<  endl;
}
//________________________________________________________________________________
//________________________________________________________________________________
int StiTpcHitLoader::nextSideSect(int s) 
{
static int side[] = {
  23,22,21,20,19,18,17,16,15,14,13,24,
  11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1,12};
  if (iTPCvers()==0) return s;
  return side[s]-1;
  
}
//________________________________________________________________________________
int StiTpcHitLoader::nextStiSect(int sect,int add) 
{
  int s = sect+add;
  if (sect<=12) {
    s = (s+12)%12;
  } else {
    s = (s-12+24)%12+12;
  }
  return s;
}
//________________________________________________________________________________
int StiTpcHitLoader::toStiSect(int sector) 
{
  int stiSector = sector;
    if (_detector->getNSectors()==12 && sector>=12) {stiSector = 11 - (sector-11)%12;}
  return stiSector;
}
//________________________________________________________________________________
int StiTpcHitLoader::giveOut(const StiDetector *stiDetector,int stiSector
                            ,int stiRow,StiHit *stiHit)
{
 std::multimap<double, StiDetector*> myMap;
#if 1
    _hitContainer->add( stiHit);
    return 1;
#endif
   _hitContainer->add( stiHit);

  int nGive = 0;
  double hiPos[3] = {stiHit->x_g(),stiHit->y_g(),stiHit->z_g()};
  double hiRxy    = stiHit->rxy();
  int list[6];
  list[0] = nextStiSect(stiSector,-1);
  list[1] = stiSector;
  list[2] = nextStiSect(stiSector,+1);
  list[3] = -1;
  if (iTPCvers()) {for (int i=0;i<3;i++) {list[i+3] = nextSideSect(list[i]);}};
  for (int mySector: list) 
  { 
    if (mySector<0) break;
    int nRows = St_tpcPadConfigC::instance()->padRows(mySector+1);
    int rowDow = 0, rowUpp = nRows-1;
    auto  *detDow = _detector->getDetector(rowDow,mySector);
    auto  *detUpp = _detector->getDetector(rowUpp,mySector);
    double rxyDow = detDow->getPlacement()->getNormalRadius();
    double rxyUpp = detUpp->getPlacement()->getNormalRadius();
    if (hiRxy>rxyUpp) hiRxy=rxyUpp-1e-4;
    if (hiRxy<rxyDow) hiRxy=rxyDow+1e-4;
    while (rowUpp-rowDow>1) {
      int rowMed = 0.5+rowDow+(rowUpp-rowDow)/(rxyUpp-rxyDow)*(hiRxy-rxyDow);
      if (rowMed<=rowDow || rowMed>=rowUpp ) rowMed=(rowDow+rowUpp)/2;
      auto *detMed = _detector->getDetector(rowMed,mySector);
      double rxyMed = detMed->getPlacement()->getNormalRadius();
      if (rxyMed>=hiRxy) {
        rxyUpp = rxyMed; rowUpp = rowMed; detUpp = detMed;
      } else {
        rxyDow = rxyMed; rowDow = rowMed; detDow = detMed;
      }
    }// end row while

    assert( rxyDow <= hiRxy && hiRxy <= rxyUpp);
    for (auto *det :{detDow,detUpp}) {
      det->insideG(hiPos,15);
      myMap.insert(std::pair<double,StiDetector*>(StiDetector::mgValue[2],det));
    }

  }//end sector for   

  double dis = -1;
  for (auto it = myMap.begin(); it != myMap.end();++it) {
    double mydis = (*it).first;
    if (dis<0) dis = mydis;
    if (mydis>5*dis) break;
    auto *myDetector = (*it).second;
    if (myDetector==stiDetector) continue;
    nGive++;
#if 0
    _hitContainer->add( stiHit,myDetector);
#endif
#if 1
    auto *myStiHit = _hitFactory->getInstance();
    *myStiHit = *stiHit;
    myStiHit->setDetector(myDetector);
    _hitContainer->add( myStiHit);
#endif
  }
  return nGive;
}
//________________________________________________________________________________
int StiTpcHitLoader::iTPCvers()
{
 if (_iTPC>-1) return _iTPC;
 _iTPC = 0;
 if (_detector->getNSectors() == 12) return _iTPC;

 if (St_tpcPadConfigC::instance()->padRows(20)<=13) return _iTPC;
 _iTPC = 1;
 if (St_tpcPadConfigC::instance()->padRows(21)<=13) return _iTPC;
 _iTPC = 2;
 return _iTPC;
}
 
