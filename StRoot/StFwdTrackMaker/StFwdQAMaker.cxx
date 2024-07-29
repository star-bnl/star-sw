#include "StFwdTrackMaker/StFwdQAMaker.h"
#include "StFwdQAMaker.h"
#include "St_base/StMessMgr.h"
#include "StBFChain/StBFChain.h"
#include "StFwdTrackMaker/StFwdTrackMaker.h"

#include "StFwdTrackMaker/include/Tracker/FwdTracker.h"
#include "StFwdTrackMaker/include/Tracker/ObjExporter.h"
// StEvent includes
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttCollection.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StEvent/StFwdTrackCollection.h"
#include "StEvent/StFwdTrack.h"


#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuFstCollection.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrack.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrackCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFttCluster.h"
#include "StMuDSTMaker/COMMON/StMuFttPoint.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"

// ClassImp(FcsClusterWithStarXYZ);

/** Clear the FwdQATreeData from one event to next */
void FwdQATreeData::clear(){
    header.clear();
    mcTracks.reset();
    fttPoints.reset();
    fttClusters.reset();
    fstPoints.reset();
    reco.reset();
    seeds.reset();
    wcal.reset();
    hcal.reset();
    wcalHits.reset();
    hcalHits.reset();
    epdHits.reset();
}

FcsClusterWithStarXYZ::FcsClusterWithStarXYZ( StMuFcsCluster *clu, StFcsDb *fcsDb ) {
    if ( nullptr == clu ) return;
    StThreeVectorD xyz = fcsDb->getStarXYZfromColumnRow(clu->detectorId(),clu->x(),clu->y());
    float detOffset = 0.0;
    if ( clu->detectorId() == kFcsEcalNorthDetId || clu->detectorId() == kFcsEcalSouthDetId ){
        detOffset = 715.0; // cm from IP
    } else if ( clu->detectorId() == kFcsHcalNorthDetId || clu->detectorId() == kFcsHcalSouthDetId ){
        detOffset = 807.0; // cm from IP
    }
    mXYZ.SetXYZ( xyz.x(), xyz.y(), xyz.z() + detOffset );
    mClu = clu;
}

FcsHitWithStarXYZ::FcsHitWithStarXYZ( StMuFcsHit *hit, StFcsDb *fcsDb ) {
    if ( nullptr == hit ) return;
    StThreeVectorD xyz = fcsDb->getStarXYZ(hit->detectorId(),hit->id());
    float detOffset = 0.0;
    if ( hit->detectorId() == kFcsEcalNorthDetId || hit->detectorId() == kFcsEcalSouthDetId ){
        detOffset = 715.0; // cm from IP
    } else if ( hit->detectorId() == kFcsHcalNorthDetId || hit->detectorId() == kFcsHcalSouthDetId ){
        detOffset = 807.0; // cm from IP
    } else if ( hit->detectorId() == kFcsPresNorthDetId || hit->detectorId() == kFcsPresSouthDetId ){
        StEpdGeom epdgeo;
        double zepd=375.0;
        int pp,tt,n;
        double x[5],y[5];
        fcsDb->getEPDfromId(hit->detectorId(),hit->id(),pp,tt);
        epdgeo.GetCorners(100*pp+tt,&n,x,y);
        double x0 = (x[0] + x[1] + x[2] + x[3]) / 4.0;
        double y0 = (y[0] + y[1] + y[2] + y[3]) / 4.0;
        xyz.setX(x0);
        xyz.setY(y0);
        xyz.setZ(zepd);
    }
    mXYZ.SetXYZ( xyz.x(), xyz.y(), xyz.z() + detOffset );
    mHit = hit;
}

StFwdQAMaker::StFwdQAMaker() : StMaker("fwdQAMaker"), mTreeFile(nullptr), mTree(nullptr) {

}

int StFwdQAMaker::Init() {

    mTreeFile = new TFile("fwdtree.root", "RECREATE");
    mTree = new TTree("fwd", "fwd tracking tree");

    mTree->Branch("header",           &mTreeData. header, 3200, 99 );
    mTreeData.mcTracks.createBranch(mTree, "mcTracks");
    mTree->Branch("nSeedTracks",      &mTreeData.nSeedTracks, "nSeedTracks/I");
    mTreeData.fstPoints.createBranch(mTree, "fstHits");
    mTreeData.fttPoints.createBranch(mTree, "fttPoints");
    mTreeData.fttClusters.createBranch(mTree, "fttClusters");
    mTreeData.fstPoints.createBranch(mTree, "fstPoints");
    mTreeData.wcal.createBranch(mTree, "wcalClusters");
    mTreeData.hcal.createBranch(mTree, "hcalClusters");

    mTreeData.wcalHits.createBranch(mTree, "wcalHits");
    mTreeData.hcalHits.createBranch(mTree, "hcalHits");
    mTreeData.epdHits.createBranch(mTree, "epdHits");

    mTreeData.reco.createBranch(mTree, "reco");
    mTreeData.seeds.createBranch(mTree, "seeds");
    return kStOk;
}
int StFwdQAMaker::Finish() {

    if ( mTreeFile && mTree ){
        mTreeFile->cd();
        mTree->Write();
        mTreeFile->Write();
        LOG_DEBUG << "StFwdQA File written" << endm;
    }
    return kStOk;
}
int StFwdQAMaker::Make() {
    LOG_INFO << "FWD Report:" << endm;
    StEvent *mStEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if ( mStEvent ){
        // report number of fwd tracks
        auto fwdTracks = mStEvent->fwdTrackCollection();
        LOG_INFO << "Number of FwdTracks (StFwdTrackCollection): " << fwdTracks->tracks().size() << endm;
        LOG_INFO << "Number of Ftt Points (StEvent)" << mStEvent->fttCollection()->points().size() << endm;
    }
    LOG_INFO << "SETUP START" << endm;
    // setup the datasets / makers
    mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(mMuDstMaker) {
        mMuDst = mMuDstMaker->muDst();
        mMuForwardTrackCollection = mMuDst->muFwdTrackCollection();
        mMuFcsCollection = mMuDst->muFcsCollection();
        if (mMuForwardTrackCollection){
            LOG_DEBUG << "Number of StMuFwdTracks: " << mMuForwardTrackCollection->numberOfFwdTracks() << endm;
        }
    } else {
        LOG_DEBUG << "No StMuDstMaker found: " << mMuDstMaker << endm;
    }
    mFcsDb = static_cast<StFcsDb *>(GetDataSet("fcsDb"));

    mFwdTrackMaker = (StFwdTrackMaker*) GetMaker( "fwdTrack" );
    if (!mFwdTrackMaker) {
        LOG_WARN << "No StFwdTrackMaker found, skipping StFwdQAMaker" << endm;
        // return kStOk;
    }

    mTreeData.header.run = mMuDst->event()->runNumber();
    LOG_DEBUG << "SETUP COMPLETE" << endm;

    auto muFstCollection = mMuDst->muFstCollection();
    if ( muFstCollection ){
        LOG_DEBUG << "MuDst has #fst hits: " << muFstCollection->numberOfHits() << endm;
        for ( size_t i = 0; i < muFstCollection->numberOfHits(); i++ ){
            StMuFstHit * h = muFstCollection->getHit(i);
            mTreeData.fstPoints.add( h );
        }
    }
    FillMcTracks();
    FillTracks();
    FillFstPoints();
    FillFttClusters();
    FillFcsStMuDst();
    mTree->Fill();
    return kStOk;
}
void StFwdQAMaker::Clear(const Option_t *opts) {
    mTreeData.clear();
    return;
}

void StFwdQAMaker::FillFstPoints(){
    StMuFstCollection * fst = mMuDst->muFstCollection();
    if (!fst) {
        LOG_WARN << "No StMuFstCollection ... bye-bye" << endm;
        return;
    }

    // size_t numFwdHitsPrior = mFwdHitsFst.size();
    LOG_INFO << "Loading " << fst->numberOfHits() << " StMuFstHits" << endm;
    // TMatrixDSym hitCov3(3);
    for ( unsigned int index = 0; index < fst->numberOfHits(); index++){
        StMuFstHit * muFstHit = fst->getHit( index );
        mTreeData.fstPoints.add( muFstHit );


        // float vR = muFstHit->localPosition(0);
        // float vPhi = muFstHit->localPosition(1);
        // float vZ = muFstHit->localPosition(2);

        // const float dz0 = fabs( vZ - mFstZFromGeom[0] );
        // const float dz1 = fabs( vZ - mFstZFromGeom[1] );
        // const float dz2 = fabs( vZ - mFstZFromGeom[2] );
        // static const float fstThickness = 2.0; // thickness in cm between inner and outer on sigle plane

        // // assign disk according to which z value the hit has, within the z-plane thickness
        // int d = 0 * ( dz0 < fstThickness ) + 1 * ( dz1 < fstThickness ) + 2 * ( dz2 < fstThickness );

        // float x0 = vR * cos( vPhi );
        // float y0 = vR * sin( vPhi );
        // hitCov3 = makeSiCovMat( TVector3( x0, y0, vZ ), mFwdConfig );

        // LOG_DEBUG << "FST HIT: d = " << d << ", x=" << x0 << ", y=" << y0 << ", z=" << vZ << endm;
        // mFstHits.push_back( TVector3( x0, y0, vZ)  );

        // // we use d+4 so that both FTT and FST start at 4
        // mFwdHitsFst.push_back(FwdHit(count++, x0, y0, vZ, d+4, 0, hitCov3, nullptr));
        // count++;
    } // index
}

void StFwdQAMaker::FillTracks() {
    mTreeData.nSeedTracks = 0;
    if ( mMuForwardTrackCollection ){
        LOG_DEBUG << "Adding " << mMuForwardTrackCollection->numberOfFwdTracks() << " FwdTracks (MuDst)" << endm;
        for ( size_t iTrack = 0; iTrack < mMuForwardTrackCollection->numberOfFwdTracks(); iTrack++ ){
            auto muTrack = mMuForwardTrackCollection->getFwdTrack(iTrack);
            mTreeData.reco.add( muTrack );

            for (auto fsth : muTrack->mFSTPoints){
                mTreeData.seeds.add( fsth );
                mTreeData.nSeedTracks++;
            }
            for (auto ftth : muTrack->mFTTPoints){
                mTreeData.seeds.add( ftth );
                mTreeData.nSeedTracks++;
            }
            if ( iTrack > 5000 ) {
                LOG_WARN << "Truncating to 5000 tracks" << endm;
                break;
            }
        }
    }
    LOG_DEBUG << "TRACKS COMPLETE" << endm;
}

void StFwdQAMaker::FillFcsStMuDst( ) {

    if ( !mMuDst ){
        LOG_DEBUG << "No mMuDst found, skipping StFwdQAMaker::FillFcsStEvent" << endm;
        return;
    }
    StMuFcsCollection* fcs = mMuDst->muFcsCollection();
    if ( !fcs ){
        LOG_DEBUG << "No muFcsCollection found, skipping StFwdQAMaker::FillFcsStEvent" << endm;
        return;
    }

    StFcsDb* fcsDb=static_cast<StFcsDb*>(GetDataSet("fcsDb"));

    // LOAD ECAL / HCAL CLUSTERS
    LOG_INFO << "MuDst has #fcs clusters: " << fcs->numberOfClusters() << endm;
    for( size_t i = 0; i < fcs->numberOfClusters(); i++){
        StMuFcsCluster * clu = fcs->getCluster(i);
        FcsClusterWithStarXYZ *cluSTAR = new FcsClusterWithStarXYZ(clu, fcsDb);
        if ( clu->detectorId() == kFcsEcalNorthDetId || clu->detectorId() == kFcsEcalSouthDetId ){
            LOG_INFO << "Adding WCAL Cluster to FwdTree" << endm;
            mTreeData.wcal.add( cluSTAR );
        } else if ( clu->detectorId() == kFcsHcalNorthDetId || clu->detectorId() == kFcsHcalSouthDetId ){
            LOG_INFO << "Adding HCAL Cluster to FwdTree" << endm;
            mTreeData.hcal.add( cluSTAR );
        } 

        delete cluSTAR;
    }

    // LOAD ECAL / HCAL CLUSTERS
    LOG_INFO << "MuDst has #fcs hits: " << fcs->numberOfHits() << endm;
    for( size_t i = 0; i < fcs->numberOfHits(); i++){
        StMuFcsHit * hit = fcs->getHit(i);
        FcsHitWithStarXYZ *hitSTAR = new FcsHitWithStarXYZ(hit, fcsDb);
        if ( hit->detectorId() == kFcsEcalNorthDetId || hit->detectorId() == kFcsEcalSouthDetId ){
            LOG_DEBUG << "Adding WCAL Cluster to FwdTree" << endm;
            mTreeData.wcalHits.add( hitSTAR );
        } else if ( hit->detectorId() == kFcsHcalNorthDetId || hit->detectorId() == kFcsHcalSouthDetId ){
            LOG_DEBUG << "Adding HCAL Cluster to FwdTree" << endm;
            mTreeData.hcalHits.add( hitSTAR );
        } else if ( hit->detectorId() == kFcsPresNorthDetId || hit->detectorId() == kFcsPresSouthDetId ){
            LOG_DEBUG << "Adding PRES hit to FwdTree" << endm;
            mTreeData.epdHits.add( hitSTAR );
        }
        delete hitSTAR;
    }

    // TODO, cleanup?
}

void StFwdQAMaker::FillMcTracks(){
    // Retrieve pointer to MC tracks
    TClonesArray *mcTracks = mMuDst->mcArray(1);
    LOG_INFO << "MuDst has #mc tracks: " << mcTracks->GetEntriesFast() << endm;
    // Loop over MC vertices
    for (Int_t iVtx=0; iVtx<mcTracks->GetEntriesFast(); iVtx++) {
        // Retrieve i-th MC vertex from MuDst
        StMuMcTrack *mcTrack = (StMuMcTrack*)mcTracks->UncheckedAt(iVtx);
        if ( !mcTrack ) continue;

        // Add MC track to the tree
        mTreeData.mcTracks.add( mcTrack );
    }
}


void StFwdQAMaker::FillFttClusters(){

    auto muFttCollection = mMuDst->muFttCollection();
    if ( muFttCollection ){
        LOG_DEBUG << "MuDst has #ftt clusters: " << muFttCollection->numberOfClusters() << endm;
        for ( size_t i = 0; i < muFttCollection->numberOfClusters(); i++ ){
            StMuFttCluster * c = muFttCollection->getCluster(i);
            mTreeData.fttClusters.add( c );
        }

        for ( size_t i = 0; i < muFttCollection->numberOfPoints(); i++ ){
            StMuFttPoint * c = muFttCollection->getPoint(i);
            mTreeData.fttPoints.add( c );
        }
    }
}
