#include "StMuFwdTrackUtil.h"
#include "StMuFwdTrackCollection.h"

#include "StEvent/StFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"
#include "StMuDSTMaker/COMMON/StMuFcsUtil.h"
#include "StMuDSTMaker/COMMON/StMuFcsInfo.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"


#include "StEvent/StFwdTrackCollection.h"
#include "StEvent/StFwdTrack.h"
#include "St_base/StMessMgr.h"

#include "StMuFwdTrack.h"
#include "StMuFwdTrackCollection.h"

StMuFwdTrackCollection* StMuFwdTrackUtil::getMuFwdTrack(StFwdTrackCollection* evc){

    if ( evc == nullptr ){
        LOG_WARN << "NULL StFwdTrackCollection, cannot fill MuDST" << endm;
    }

    StMuFwdTrackCollection *muc = new StMuFwdTrackCollection();
    return muc;
}
StFwdTrackCollection*   StMuFwdTrackUtil::getFwdTrack(StMuFwdTrackCollection*){
    return new StFwdTrackCollection();
}
void StMuFwdTrackUtil::fillMuFwdTrack(StMuFwdTrackCollection* muc,StFwdTrackCollection* evc, StMuFcsUtil *fcsu){
    
    if ( evc == nullptr ){
        LOG_WARN << "NULL StFwdTrackCollection, cannot fill FwdTracks in MuDST" << endm;
        return;
    }

    if ( muc == nullptr ){
        LOG_WARN << "NULL StMuFwdTrackCollection, cannot fill FwdTracks in MuDST" << endm;
        return;
    }

    if ( fcsu == nullptr ){
        LOG_WARN << "NULL StMuFcsUtil, cannot fill FwdTracks in MuDST" << endm;
        return;
    }

    auto fcsClusterMapEvToMu = fcsu->getClusterMap();

    const StSPtrVecFwdTrack& evTracks = evc->tracks();
    LOG_INFO << "Adding " << evc->numberOfTracks() << " StMuFwdTracks to MuDSt" << endm; 
    for ( size_t i = 0; i < evc->numberOfTracks(); i++ ){
        StFwdTrack * evTrack = evTracks[i];
        StMuFwdTrack * muFwdTrack = muc->addFwdTrack();
        muFwdTrack->set( evTrack );

        // Fix ECAL, HCAL associations
        const StPtrVecFcsCluster& ecalClus = evTrack->ecalClusters();
        const StPtrVecFcsCluster& hcalClus = evTrack->hcalClusters();

        for (auto eClu : ecalClus ){
            
            if ( fcsClusterMapEvToMu.count( eClu ) == 0){
                LOG_WARN << "Cannot find ECAL Cluster in map" << endm;
                continue;
            }
            StMuFcsCluster * muClu = fcsClusterMapEvToMu[ eClu ];
            muFwdTrack->addEcalCluster( muClu );
        }

        for (auto hClu : hcalClus ){
            
            if ( fcsClusterMapEvToMu.count( hClu ) == 0){
                LOG_WARN << "Cannot find HCAL Cluster in map" << endm;
                continue;
            }
            StMuFcsCluster * muClu = fcsClusterMapEvToMu[ hClu ];
            muFwdTrack->addHcalCluster( muClu );
        }
    }

    return;
}
void StMuFwdTrackUtil::fillFwdTrack(StFwdTrackCollection*,StMuFwdTrackCollection*){
    return;
}