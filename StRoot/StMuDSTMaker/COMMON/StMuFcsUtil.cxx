#include "StEvent/StFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"
#include "StMuDSTMaker/COMMON/StMuFcsUtil.h"
#include "StMuDSTMaker/COMMON/StMuFcsInfo.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StEvent.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerData.h"

#include <algorithm>  // For std::find
#include <iterator>  // For std::distance

#include "TCollection.h"  // For TIter
#include "TRefArray.h"
#include "TVector3.h"

ClassImp(StMuFcsUtil)

StMuFcsUtil::StMuFcsUtil()
{
}
StMuFcsUtil::~StMuFcsUtil()
{
}

StMuFcsCollection* StMuFcsUtil::getMuFcs(StFcsCollection *fcscol)
{
  LOG_DEBUG << "StMuFcsUtil::getMuFcs" << endm;
  if(!fcscol) return NULL;
  StMuFcsCollection* muFcs=new StMuFcsCollection();
  fillMuFcs(muFcs,fcscol);
  return muFcs;
} // getMuFcs

StFcsCollection* StMuFcsUtil::getFcs(StMuFcsCollection* muFcs)
{
  if(!muFcs) return NULL;
  
  StFcsCollection *fcs=new StFcsCollection();
  fillFcs(fcs,muFcs);
  return fcs;
} //getFcs

void StMuFcsUtil::fillMuFcs(StMuFcsCollection *muFcs,StFcsCollection *fcscol)
{
  if(!fcscol) return;
  if(!muFcs) return;
  
  fillMuFcsHits(muFcs, fcscol);
  fillMuFcsPoints(muFcs, fcscol);
  fillMuFcsClusters(muFcs, fcscol);
  fillMuFcsInfo(muFcs, fcscol);

  // rebuild the pointers that give cluster <-> hit, cluster <-> cluster,
  // and cluster <-> point relationships
  rebuildRelationships( fcscol, muFcs );
} // fillMuFcs

void StMuFcsUtil::fillFcs(StFcsCollection* fcscol,StMuFcsCollection* muFcs)
{
  if(!muFcs) return;
  if(!fcscol) return;
  fillFcsHits(fcscol, muFcs);
} // fillFcs

void StMuFcsUtil::fillMuFcsHits(StMuFcsCollection* muFcs,
                                StFcsCollection* fcscol) {
  
    for ( unsigned int idet = 0; idet < kFcsNDet+1; idet++ ){
        StSPtrVecFcsHit vecHit = fcscol->hits(idet);
        for(unsigned int i=0; i<fcscol->numberOfHits(idet); i++){

            StMuFcsHit* muFcsHit = muFcs->addHit();


            unsigned int l = vecHit[i]->nTimeBin();
            if ( vecHit[i]->zs() ) l *= 2; // size of data, x2 if zero suppressed
            unsigned short data[ l ];
            for (unsigned int j = 0; j < l; j++ ){
                data[ j ] = vecHit[i]->data( j );
            }

            muFcsHit->setFcsHit( 
                vecHit[i]->zs(), vecHit[i]->detectorId(), vecHit[i]->id(),
                vecHit[i]->ns(), vecHit[i]->ehp(), vecHit[i]->dep(), vecHit[i]->channel(), 
                l, data
            );

            // set the corrected energy too
            muFcsHit->setEnergy( vecHit[i]->energy() );

            // store in memory map between StEvent and StMuDst version
            mMapHits[ fcscol->hits(idet)[i] ] = muFcsHit;
        } // for i
    } // for idet
} // fillMuFcsHits

void StMuFcsUtil::rebuildRelationships(StFcsCollection* fcscol, 
                                StMuFcsCollection* muFcs
                                ) {

    // First take care of the hits
    for (unsigned int idet = 0; idet < kFcsNDet+1; idet++){
        for(unsigned int i=0; i<fcscol->numberOfHits(idet); i++){

            if ( mMapHits.count( fcscol->hits(idet)[i] ) > 0 ) {
                StMuFcsHit * muHit = static_cast<StMuFcsHit*>(mMapHits[ fcscol->hits(idet)[i] ]);
                if ( nullptr == fcscol->hits(idet)[i]->cluster() ) continue;
                if ( mMapClusters.count( fcscol->hits(idet)[i]->cluster() ) == 0 
                    || mMapClusters[ fcscol->hits(idet)[i]->cluster() ] == nullptr )
                    continue;

                StMuFcsCluster* muCluster = mMapClusters[ fcscol->hits(idet)[i]->cluster() ];
                muHit->setCluster( muCluster );
            } else {
                // By construction the StFcs objectts should always exist in the map, so this is an error
                LOG_ERROR << "Cannot find StMuFcsHit for StFcsHit=" << fcscol->hits(idet)[i] << endm;
            } // if / else
        } // for i
    } // for idet

    // Take care of the clusters
    for (unsigned int idet = 0; idet < kFcsNDet; idet++){
        StSPtrVecFcsCluster &vecClu = fcscol->clusters(idet);
        for(unsigned int i=0; i<vecClu.size(); i++){

            assert( mMapClusters.count( vecClu[i] ) > 0 );

            StMuFcsCluster * clu             = static_cast<StMuFcsCluster*>( mMapClusters[ vecClu[i]] );
            StPtrVecFcsHit& vecHits          = vecClu[i]->hits();
            StPtrVecFcsCluster& vecNeighbors = vecClu[i]->neighbor();
            StPtrVecFcsPoint& vecPoints      = vecClu[i]->points();

            for (unsigned int j = 0; j < vecHits.size(); j++ ){
                if ( vecHits[j] && mMapHits.count( vecHits[j] ) > 0 && mMapHits[vecHits[j]] != nullptr ) {
                    StMuFcsHit * muHit = static_cast<StMuFcsHit*>( mMapHits[ vecHits[j]] );
                    clu->hits()->Add( muHit );
                } else {
                    LOG_ERROR << "Cannot find StMuFcsHit for StFcsHit=" << vecHits[j] << endm;
                } // if / else
            } // for j

            for (unsigned int j = 0; j < vecNeighbors.size(); j++ ){
                if ( vecNeighbors[j] && mMapClusters.count( vecNeighbors[j] ) > 0 && mMapClusters[vecNeighbors[j]] != nullptr) {
                    StMuFcsCluster * muCluster = static_cast<StMuFcsCluster*>(  mMapClusters[vecNeighbors[j]]);
                    clu->addNeighbor( muCluster );
                } else {
                    LOG_ERROR << "Cannot find StMuFcsCluster for StFcsCluster=" << vecNeighbors[j] << endm;
                } // if / else
            } // for j

            for (unsigned int j = 0; j < vecPoints.size(); j++ ){
                if ( vecPoints[j] && mMapPoints.count( vecPoints[j] ) > 0 && mMapPoints[vecPoints[j]] != nullptr ) {
                    StMuFcsPoint * muPoint = static_cast<StMuFcsPoint*>( mMapPoints[vecPoints[j]]);
                    clu->addPoint( muPoint );
                } else {
                    LOG_ERROR << "Cannot find StMuFcsPoint for StFcsPoint=" << vecPoints[j] << endm;
                }
            } // for j
        } // for i
    } // for idet

    // // Take care of the Points
    // for (unsigned int idet = 0; idet < kFcsNDet; idet++){
    //     // StSPtrVecFcsHit vecHit = fcscol->hits(idet);
    //     for(unsigned int i=0; i<fcscol->numberOfPoints(idet); i++){
    //         if ( mMapPoints.count( fcscol->points(idet)[i] ) > 0 ) {
    //             StMuFcsPoint * muPoint = static_cast<StMuFcsPoint*>(mMapPoints[ fcscol->points(idet)[i] ]);
    //             muPoint->setCluster( static_cast<StMuFcsCluster*>(mMapClusters[ fcscol->points(idet)[i]->cluster() ]) );
    //         } else {
    //             LOG_ERROR << "Cannot find StMuFcsPoint for StFcsPoint=" << fcscol->points(idet)[i] << endm;
    //         } // if / else
    //     } // for i
    // } // for idet
} // rebuildRelationships(...)


void StMuFcsUtil::fillMuFcsClusters(StMuFcsCollection* muFcs,
                                    StFcsCollection* fcscol) {
  // Fill clusters
    for ( unsigned int idet = 0; idet < kFcsNDet; idet++ ){
        for (unsigned i(0); i < fcscol->numberOfClusters(idet); ++i) {
            const StFcsCluster* cluster = fcscol->clusters(idet)[i];
            StMuFcsCluster* muCluster = muFcs->addCluster();  // Expand StMuFcsCollection cluster array by 1

            muCluster->setId( cluster->id() );
            muCluster->setDetectorId( cluster->detectorId() );
            muCluster->setCategory( cluster->category() );
            muCluster->setNTowers( cluster->nTowers() );
            muCluster->setEnergy( cluster->energy() );
            muCluster->setX( cluster->x() );
            muCluster->setY( cluster->y() );
            muCluster->setSigmaMin( cluster->sigmaMin() );
            muCluster->setSigmaMax( cluster->sigmaMax() );
            muCluster->setTheta( cluster->theta() );
            muCluster->setChi2Ndf1Photon( cluster->chi2Ndf1Photon() );
            muCluster->setChi2Ndf2Photon( cluster->chi2Ndf2Photon() );
            TLorentzVector lv;
            lv.SetPxPyPzE( cluster->fourMomentum().px(), cluster->fourMomentum().py(), cluster->fourMomentum().pz(), cluster->fourMomentum().e() );
            muCluster->setFourMomentum( lv );

            mMapClusters[ cluster ] = muCluster;
        } // for i
    }  // for idet
} //fillMuFcsClusters

void StMuFcsUtil::fillMuFcsPoints(StMuFcsCollection* muFcs,
                                  StFcsCollection* fcscol) {
    
    for ( unsigned int idet = 0; idet < kFcsNDet; idet++ ){
        for (unsigned i(0); i < fcscol->numberOfPoints(idet); ++i) {
            const StFcsPoint* point = fcscol->points(idet)[i]; 
            StMuFcsPoint* muPoint = muFcs->addPoint();
            if (point && muPoint) {
                muPoint->setDetectorId( point->detectorId() );
                muPoint->setEnergy( point->energy() );
                muPoint->setX( point->x() );
                muPoint->setY( point->y() );
                muPoint->setNParentClusterPhotons( point->nParentClusterPhotons() );
                muPoint->setXYZ( TVector3(point->xyz().x(), point->xyz().y(), point->xyz().z()) );
                TLorentzVector lv;
                lv.SetPxPyPzE( point->fourMomentum().px(), point->fourMomentum().py(), point->fourMomentum().pz(), point->fourMomentum().e() );
                muPoint->setFourMomentum( lv );

                mMapPoints[ point ] = muPoint;
            }  // if
        }  // for i
    } // for idet
} // fillMuFcsPoints

void StMuFcsUtil::fillMuFcsInfo(StMuFcsCollection* muFcs,
                                  StFcsCollection* fcscol) {
    muFcs->addInfo();
    muFcs->setFcsReconstructionFlag(fcscol->fcsReconstructionFlag());

    // Now store the index for the first and last hit/cluster/point
    // for each detector
    size_t hitCount = 0;
    size_t clusterCount = 0;
    size_t pointCount = 0;
    for ( unsigned int idet = 0; idet < kFcsNDet + 1; idet++ ){
        

        muFcs->getInfo()->setHitIndex( idet, hitCount );
        hitCount     += fcscol->numberOfHits(idet);

        if ( idet == kFcsNDet )
            break;
        muFcs->getInfo()->setClusterIndex( idet, clusterCount );
        clusterCount += fcscol->numberOfClusters(idet);

        muFcs->getInfo()->setPointIndex( idet, pointCount );
        pointCount   += fcscol->numberOfPoints(idet);
    }

    // fill the final index with the total number
    // so that it is easy to compute the # for the final detector
    muFcs->getInfo()->setHitIndex( kFcsNDet + 1, hitCount );
    muFcs->getInfo()->setClusterIndex( kFcsNDet, clusterCount );
    muFcs->getInfo()->setPointIndex( kFcsNDet, pointCount );

}

void StMuFcsUtil::fillFcsHits(StFcsCollection* fcscol,
                              StMuFcsCollection* muFcs) {
  // Using TIter to iterate is safe in the case of hits being NULL
  TIter next(muFcs->getHitArray());
  StMuFcsHit* muHit(NULL);
  while ((muHit = static_cast<StMuFcsHit*>(next()))) {
    unsigned int idet = static_cast<unsigned int>(muHit->detectorId());
    fcscol->addHit(idet, new StFcsHit);
    StFcsHit* hit = fcscol->hits(idet).back();

    unsigned int l = muHit->nTimeBin();
    if ( muHit->zs() ) l *= 2;
    hit->setFcsHit( 
            muHit->zs(), muHit->detectorId(), muHit->id(),
            muHit->ns(), muHit->ehp(), muHit->dep(), muHit->channel(), 
            l, muHit->data()
        );

    hit->setEnergy( muHit->energy() );
  }  // while
} // fillFcsHits
