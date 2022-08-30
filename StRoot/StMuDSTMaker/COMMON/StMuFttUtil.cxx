#include "StEvent/StFttCollection.h"
#include "StEvent/StFttCluster.h"

#include "StMuDSTMaker/COMMON/StMuFttCluster.h"
#include "StMuDSTMaker/COMMON/StMuFttRawHit.h"
#include "StMuDSTMaker/COMMON/StMuFttPoint.h"
// #include "StMuDSTMaker/COMMON/StMuFttInfo.h"
#include "StMuDSTMaker/COMMON/StMuFttUtil.h"
#include "StMuDSTMaker/COMMON/StMuFttCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StEvent.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StContainers.h"

#include <algorithm>  // For std::find
#include <iterator>  // For std::distance

#include "TCollection.h"  // For TIter
#include "TRefArray.h"
#include "TVector3.h"

ClassImp(StMuFttUtil)

StMuFttUtil::StMuFttUtil()
{
}

StMuFttUtil::~StMuFttUtil()
{
}

StMuFttCollection* StMuFttUtil::getMuFtt(StFttCollection *fttcol)
{
  LOG_DEBUG << "StMuFttUtil::getMuFtt" << endm;
  if(!fttcol) return NULL;
  StMuFttCollection* muFtt=new StMuFttCollection();
  fillMuFtt(muFtt,fttcol);
  return muFtt;
} // getMuFtt

StFttCollection* StMuFttUtil::getFtt(StMuFttCollection* muFtt)
{
  if(!muFtt) return NULL;
  
  StFttCollection *ftt=new StFttCollection();
  fillFtt(ftt,muFtt);
  return ftt;
} //getFtt

void StMuFttUtil::fillMuFtt(StMuFttCollection *muFtt,StFttCollection *fttcol)
{
    LOG_INFO << "fillMuFtt" << endm;
  if(!fttcol) return;
  if(!muFtt) return;
  
  fillMuFttRawHits(muFtt, fttcol);
  fillMuFttPoints(muFtt, fttcol);
  fillMuFttClusters(muFtt, fttcol);

  // rebuild the pointers that give cluster <-> hit, cluster <-> cluster,
  // and cluster <-> point relationships
  rebuildRelationships( fttcol, muFtt );
} // fillMuFtt

void StMuFttUtil::fillFtt(StFttCollection* fttcol,StMuFttCollection* muFtt)
{
  if(!muFtt) return;
  if(!fttcol) return;
  fillFttRawHits(fttcol, muFtt);
} // fillFtt

void StMuFttUtil::fillMuFttRawHits(StMuFttCollection* muFtt,
                                   StFttCollection* fttcol) {
    LOG_INFO << "fillMuFttRawHits" << endm;
    const StSPtrVecFttRawHit& vecHit = fttcol->rawHits();
    for(unsigned int i=0; i<fttcol->numberOfRawHits(); i++){

        StMuFttRawHit* muFttHit = muFtt->addRawHit();
        muFttHit->set( vecHit[i] );
        // store in memory map between StEvent and StMuDst version for associations
        mMapHits[ fttcol->rawHits()[i] ] = muFttHit;
    } // for i
} // fillMuFttRawHits

void StMuFttUtil::rebuildRelationships(StFttCollection* fttcol, 
                                       StMuFttCollection* muFtt ) {

    
} // rebuildRelationships(...)


void StMuFttUtil::fillMuFttClusters(StMuFttCollection* muFtt,
                                    StFttCollection* fttcol) {
    LOG_INFO << "fillMuFttClusters" << endm;
    const StSPtrVecFttCluster& vecClu = fttcol->clusters();
    for(unsigned int i=0; i<fttcol->numberOfClusters(); i++){

        StMuFttCluster* muFttCluster = muFtt->addCluster();
        muFttCluster->set( vecClu[i] );
        // store in memory map between StEvent and StMuDst version for associations
        mMapClusters[ fttcol->clusters()[i] ] = muFttCluster;
    } // for i
} //fillMuFttClusters

void StMuFttUtil::fillMuFttPoints(StMuFttCollection* muFtt,
                                  StFttCollection* fttcol) {
    LOG_INFO << "fillMuFttPoints" << endm;
    const StSPtrVecFttPoint& vecPoint = fttcol->points();
    for(unsigned int i=0; i<fttcol->numberOfPoints(); i++){

        StMuFttPoint* muFttPoint = muFtt->addPoint();
        muFttPoint->set( vecPoint[i] );
        // store in memory map between StEvent and StMuDst version for associations
        mMapPoints[ fttcol->points()[i] ] = muFttPoint;
    } // for i
} // fillMuFttPoints

void StMuFttUtil::fillFttRawHits(StFttCollection* fttcol,
                              StMuFttCollection* muFtt) {
  // Using TIter to iterate is safe in the case of hits being NULL
  TIter next(muFtt->getRawHitArray());
  StMuFttRawHit* muHit(NULL);
  while ((muHit = static_cast<StMuFttRawHit*>(next()))) {
    fttcol->addRawHit(new StFttRawHit);
    StFttRawHit* hit = fttcol->rawHits().back();
    hit->setRaw(
        muHit->sector(), muHit->rdo(), muHit->feb(),
        muHit->vmm(), muHit->channel(), muHit->adc(),
        muHit->bcid(), muHit->tb(), muHit->dbcid() );
  }  // while
} // fillFttHits
