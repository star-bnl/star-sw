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
    setLocalOutputFile( "./fwdHists.root" ); // default off
}

int StFwdQAMaker::Init() {

    mTreeFile = new TFile( mTreeFilename.Data(), "RECREATE");
    mTree = new TTree("fwd", "fwd tracking tree");

    mTree->Branch("header",           &mTreeData. header, 3200, 99 );
    mTreeData.mcTracks.createBranch(mTree, "mcTracks");
    mTree->Branch("nSeedTracks",      &mTreeData.nSeedTracks, "nSeedTracks/I");
    mTreeData.fstPoints.createBranch(mTree, "fstHits");
    mTreeData.fttPoints.createBranch(mTree, "fttPoints");
    mTreeData.fttClusters.createBranch(mTree, "fttClusters");
    mTreeData.wcal.createBranch(mTree, "wcalClusters");
    mTreeData.hcal.createBranch(mTree, "hcalClusters");

    mTreeData.wcalHits.createBranch(mTree, "wcalHits");
    mTreeData.hcalHits.createBranch(mTree, "hcalHits");
    mTreeData.epdHits.createBranch(mTree, "epdHits");

    mTreeData.reco.createBranch(mTree, "reco");
    mTreeData.seeds.createBranch(mTree, "seeds");



    //========================================================================================================= adding histograms (new)
    AddHist( mHists["fwdMultFailed"] =      new TH1F("fwdMultFailed", ";N_{ch}^{FWD}; counts", 100, 0, 100) );
    AddHist( mHists["fwdMultAll"] =         new TH1F("fwdMultAll", ";N_{ch}^{FWD}; counts", 100, 0, 100) );
    AddHist( mHists["fwdMultGood"] =        new TH1F("fwdMultGood", ";N_{ch}^{FWD}; counts", 100, 0, 100) );
    AddHist( mHists["fwdMultFST"] =         new TH1F("fwdMultFST", ";N_{ch}^{FWD}; counts", 100, 0, 100) );
    AddHist( mHists["nHitsFit"] =           new TH1F("nHitsFit", ";nHitsFit; counts", 10, 0, 10) );
    AddHist( mHists["fwdMultEcalMatch"] =   new TH1F("fwdMultEcalMatch", ";N_{ch}^{FWD}; counts", 100, 0, 100) );
    AddHist( mHists["fwdMultHcalMatch"] =   new TH1F("fwdMultHcalMatch", ";N_{ch}^{FWD}; counts", 100, 0, 100) );
    AddHist( mHists["fwdMultEcalClusters"] = new TH1F("fwdMultEcalClusters", ";N_{Clu}^{ECAL}; counts", 100, 0, 100) );
    AddHist( mHists["fwdMultHcalClusters"] = new TH1F("fwdMultHcalClusters", ";N_{Clu}^{HCAL}; counts", 100, 0, 100) );
    AddHist( mHists["eta"] =                new TH1F("eta", ";#eta; counts", 100, 0, 5) );
    AddHist( mHists["phi"] =                new TH1F("phi", ";#phi; counts", 100, -3.1415926, 3.1415926) );
    AddHist( mHists["pt"] =                 new TH1F("pt", "; pT; counts", 500, 0, 10) );
    AddHist( mHists["charge"] =             new TH1F("charge", "; charge; counts", 4, -2, 2) );
    AddHist( mHists["ecalMatchPerTrack"] =  new TH1F("ecalMatchPerTrack", ";N_{match} / track; counts", 5, 0, 5) );
    AddHist( mHists["hcalMatchPerTrack"] =  new TH1F("hcalMatchPerTrack", ";N_{match} / track; counts", 5, 0, 5) );
    AddHist( mHists["matchedEcalEnergy"] =  new TH1F("matchedEcalEnergy", ";Energy; counts", 100, 0, 15) );
    AddHist( mHists["matchedHcalEnergy"] =  new TH1F("matchedHcalEnergy", ";Energy; counts", 100, 0, 15) );
    AddHist( mHists["ecalEnergy"] =         new TH1F("ecalEnergy", ";Energy; counts", 100, 0, 15) );
    AddHist( mHists["hcalEnergy"] =         new TH1F("hcalEnergy", ";Energy; counts", 100, 0, 15) );
    AddHist( mHists["ecalXY"] =             new TH2F( "ecalXY", ";ecalX;ecalY", 200, -200, 200, 200, -200, 200 ) );
    AddHist( mHists["hcalXY"] =             new TH2F( "hcalXY", ";hcalX;hcalY", 200, 0, 50, 200, 0, 50 ) );
    AddHist( mHists["ecaldX"] =             new TH1F( "ecaldX", ";dx (trk - ecal); counts", 400, -200, 200 ) );
    AddHist( mHists["matchedEcaldX"] =      new TH1F( "matchedEcaldX", ";dx (trk - ecal); counts", 400, -200, 200 ) );
    AddHist( mHists["ecaldY"] =             new TH1F( "ecaldY", ";dy (trk - ecal); counts", 400, -200, 200 ) );
    AddHist( mHists["matchedEcaldY"] =      new TH1F( "matchedEcaldY", ";dy (trk - ecal); counts", 400, -200, 200 ) );
    AddHist( mHists["ecaldR"] =             new TH1F( "ecaldR", ";dr (trk - ecal); counts", 400, 0, 400 ) );
    AddHist( mHists["ecalMindR"] =          new TH1F( "ecalMindR", ";dr (trk - ecal); counts", 400, 0, 400 ) );
    AddHist( mHists["matchedEcaldR"] =      new TH1F( "matchedEcaldR", ";dr (trk - ecal); counts", 400, 0, 400 ) );
    AddHist( mHists["hcaldX"] =             new TH1F( "hcaldX", ";dx (trk - hcal); counts", 400, -200, 200 ) );
    AddHist( mHists["hcaldXdNFit"] =        new TH2F( "hcaldXdNFit", ";dx (trk - hcal); nFit", 400, -200, 200, 10, 0, 10 ) );
    AddHist( mHists["matchedHcaldX"] =      new TH1F( "matchedHcaldX", ";dx (trk - hcal); counts", 400, -200, 200 ) );
    AddHist( mHists["hcaldY"] =             new TH1F( "hcaldY", ";dy (trk - hcal); counts", 400, -200, 200 ) );
    AddHist( mHists["hcaldYdNFit"] =        new TH2F( "hcaldYdNFit", ";dy (trk - hcal); nFit", 400, -200, 200, 10, 0, 10 ) );
    AddHist( mHists["matchedHcaldY"] =      new TH1F( "matchedHcaldY", ";dy (trk - hcal); counts", 400, -200, 200 ) );
    AddHist( mHists["hcaldR"] =             new TH1F( "hcaldR", ";dr (trk - hcal); counts", 400, 0, 400 ) );
    AddHist( mHists["hcalMindR"] =          new TH1F( "hcalMindR", ";dr (trk - hcal); counts", 400, 0, 400 ) );
    AddHist( mHists["matchedHcaldR"] =      new TH1F( "matchedHcaldR", ";dr (trk - hcal); counts", 400, 0, 400 ) );
    AddHist( mHists["trkEcalX"] =           new TH2F( "trkEcalX", ";trkX;ecalX", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkEcalY"] =           new TH2F( "trkEcalY", ";trkY;ecalY", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkEcalMinX"] =        new TH2F( "trkEcalMinX", ";trkX;ecalX", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkEcalMinY"] =        new TH2F( "trkEcalMinY", ";trkY;ecalY", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkHcalX"] =           new TH2F( "trkHcalX", ";trkX;hcalX", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkHcalY"] =           new TH2F( "trkHcalY", ";trkY;hcalY", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkHcalMinX"] =        new TH2F( "trkHcalMinX", ";trkX;hcalX", 300, -150, 150, 300, -150, 150 ) );
    AddHist( mHists["trkHcalMinY"] =        new TH2F( "trkHcalMinY", ";trkY;hcalY", 300, -150, 150, 300, -150, 150 ) );

//===================================================================================================================================

    return kStOk;
}



int StFwdQAMaker::Finish() {

    if ( mTreeFile && mTree ){
        mTreeFile->cd();
        mTree->Write();
        mTreeFile->Write();
        LOG_DEBUG << "StFwdQA File written" << endm;
    }

    //beginning new
    if ( mLocalOutputFile != "" ){
        auto prevDir = gDirectory;

        // output file name
        TFile *fOutput = new TFile(mLocalOutputFile, "RECREATE");
        fOutput->cd();
        for (auto nh : mHists) {
            nh.second->SetDirectory(gDirectory);
            nh.second->Write();
        }

        // restore previous directory
        gDirectory = prevDir;

        LOG_INFO << "Done writing StFwdQAMaker output to local file : " << mLocalOutputFile << endm;
    }//end new

    return kStOk;
}


int StFwdQAMaker::Make() {
    LOG_INFO << "FWD Report:" << endm;
    StEvent *mStEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if ( mStEvent ){
        // report number of fwd tracks
        auto fwdTracks = mStEvent->fwdTrackCollection();
        LOG_INFO << "Number of FwdTracks (StFwdTrackCollection): " << fwdTracks->tracks().size() << endm;
        if ( mStEvent->fttCollection() ){
            LOG_INFO << "Number of Ftt Points (StEvent)" << mStEvent->fttCollection()->points().size() << endm;
        }
    }
    LOG_INFO << "SETUP START" << endm;
    // setup the datasets / makers


    mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(mMuDstMaker) {
        mMuDst = mMuDstMaker->muDst();
        mMuForwardTrackCollection = mMuDst->muFwdTrackCollection();
        mMuFcsCollection = mMuDst->muFcsCollection();
        if (mMuDst->event())
            mTreeData.header.run = mMuDst->event()->runNumber();
        if (mMuForwardTrackCollection){
            LOG_DEBUG << "Number of StMuFwdTracks: " << mMuForwardTrackCollection->numberOfFwdTracks() << endm;
        }
        else{
            LOG_DEBUG << "No muFwdTrackCollection " << endm;
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


    LOG_DEBUG << "SETUP COMPLETE" << endm;
    ProcessFwdTracks();
    ProcessFwdMuTracks();

    FillMcTracks();
    FillTracks(); //maybe not running
    FillFstPoints(); //no fst
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

    LOG_INFO << "Loading " << fst->numberOfHits() << " StMuFstHits" << endm;
    for ( unsigned int index = 0; index < fst->numberOfHits(); index++){
        StMuFstHit * muFstHit = fst->getHit( index );
        mTreeData.fstPoints.add( muFstHit );
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
    } else {
        LOG_WARN << "No StMuFwdTrackCollection found" << endm;
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
    else{
        LOG_INFO << "no muFttCollection " << endm;
    }
}

//==================================================adding new function

void StFwdQAMaker::ProcessFwdTracks(  ){
    // This is an example of how to process fwd track collection
    LOG_DEBUG << "StFwdAnalysisMaker::ProcessFwdTracks" << endm;
    StEvent *stEvent = static_cast<StEvent *>(GetInputDS("StEvent"));
    if (!stEvent)
        return;

    if (stEvent){
        StFttCollection *fttCol = stEvent->fttCollection();
        if (fttCol){
            LOG_DEBUG << "The Ftt Collection has " << fttCol->numberOfPoints() << " points" << endm;
        }
    }
    StFwdTrackCollection * ftc = stEvent->fwdTrackCollection();
    if (!ftc) {
        LOG_DEBUG << "Forward Track Collection is not present" << endm;
        return;
    }

    LOG_DEBUG << "Checking FcsCollection" << endm;
    StFcsCollection *fcs = stEvent->fcsCollection();
    if (!fcs) return;

    StFcsDb *mFcsDb = static_cast<StFcsDb *>(GetDataSet("fcsDb"));

    size_t fwdMultEcalMatch = 0;
    size_t fwdMultHcalMatch = 0;
    size_t fwdMultFST = 0;

    LOG_INFO << "FwdTrackCollection has: " << ftc->tracks().size() << " tracks" << endm;

    getHist( "fwdMultAll" )->Fill( ftc->tracks().size() );

    // Cluster info (independen t of tracks)
    size_t fwdMultEcalClusters = 0;
    size_t fwdMultHcalClusters = 0;
    for ( int iDet = 0; iDet < 4; iDet++ ){
        for( size_t i = 0; i < fcs->clusters(iDet).size(); i++){
            StFcsCluster * clu = fcs->clusters(iDet)[i];

            if ( iDet < 2 ){
                fwdMultEcalClusters++;
                getHist( "ecalEnergy" )->Fill( clu->energy() );
            } else if ( iDet < 4 ){
                fwdMultHcalClusters++;
                getHist( "hcalEnergy" )->Fill( clu->energy() );
            }
        }
    }

    getHist( "fwdMultEcalClusters" )->Fill( fwdMultEcalClusters );
    getHist( "fwdMultHcalClusters" )->Fill( fwdMultHcalClusters );


    size_t nGood = 0;
    size_t nFailed = 0;
    for ( auto fwdTrack : ftc->tracks() ){
        if ( !fwdTrack->didFitConvergeFully() ) {
            nFailed++;
            continue;
        }
        nGood++;
        LOG_DEBUG << TString::Format("StFwdTrack[ nProjections=%lu, nFTTSeeds=%lu, nFSTSeeds=%lu, mPt=%f ]", fwdTrack->mProjections.size(), fwdTrack->mFTTPoints.size(), fwdTrack->mFSTPoints.size(), fwdTrack->momentum().perp()) << endm;
        LOG_DEBUG << "track fit momentum " << TString::Format( "(pt=%f, eta=%f, phi=%f)", fwdTrack->momentum().perp(), fwdTrack->momentum().pseudoRapidity(), fwdTrack->momentum().phi() ) << endm;
        LOG_DEBUG << "StFwdTrack has " << fwdTrack->ecalClusters().size() << " ecal matches" << endm;
        LOG_DEBUG << "StFwdTrack has " << fwdTrack->hcalClusters().size() << " hcal matches" << endm;

        getHist("ecalMatchPerTrack")->Fill( fwdTrack->ecalClusters().size() );
        getHist("hcalMatchPerTrack")->Fill( fwdTrack->hcalClusters().size() );

        getHist( "nHitsFit" )->Fill( fwdTrack->numberOfFitPoints() );

        if (fwdTrack->mFSTPoints.size() > 0){
            fwdMultFST ++;
        }

        getHist("eta")->Fill( fwdTrack->momentum().pseudoRapidity() );
        getHist("phi")->Fill( fwdTrack->momentum().phi() );
        getHist("pt")->Fill( fwdTrack->momentum().perp() );

        getHist("charge")->Fill( fwdTrack->charge() );

        // ecal proj
        int detId = kFcsWcalId;
        TVector3 ecalXYZ;
        TVector3 ecapP;

        StFwdTrackProjection ecalProj = fwdTrack->getProjectionFor( detId, 0 );
        StFwdTrackProjection hcalProj = fwdTrack->getProjectionFor( kFcsHcalId, 0 );
        LOG_DEBUG << "EcalProj z= " << ecalProj.mXYZ.z() << endm;
        LOG_DEBUG << "HcalProj z= " << hcalProj.mXYZ.z() << endm;
        LOG_DEBUG << "EcalProj Mom" << TString::Format( "(pt=%f, eta=%f, phi=%f)", ecalProj.mMom.perp(), ecalProj.mMom.pseudoRapidity(), ecalProj.mMom.phi() ) << endm;

        for ( size_t iEcal = 0; iEcal < fwdTrack->ecalClusters().size(); iEcal++ ){
            StFcsCluster *clu = fwdTrack->ecalClusters()[iEcal];
            LOG_DEBUG << "Ecal clu detId = " << clu->detectorId() << endm;
            getHist("matchedEcalEnergy")->Fill( clu->energy() );

            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(clu->detectorId(), clu->x(), clu->y());
            float dx = ecalProj.mXYZ.x() - xyz.x();
            float dy = ecalProj.mXYZ.y() - xyz.y();
            float dr = sqrt(dx*dx + dy*dy);
            getHist("matchedEcaldX")->Fill( dx );
            getHist("matchedEcaldY")->Fill( dy );
            getHist("matchedEcaldR")->Fill( dr );
        }

        if (ecalProj.mXYZ.z() > 500){
            double mindR = 999;
            StFcsCluster * cclu = nullptr; // closet cluster
            for ( int iDet = 0; iDet < 2; iDet++ ){
                for( size_t i = 0; i < fcs->clusters(iDet).size(); i++){
                    StFcsCluster * clu = fcs->clusters(iDet)[i];

                    StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(clu->detectorId(), clu->x(), clu->y());
                    getHist("ecalXY")->Fill( xyz.x(), xyz.y() );

                    float dx = ecalProj.mXYZ.x() - xyz.x();
                    float dy = ecalProj.mXYZ.y() - xyz.y();
                    float dr = sqrt(dx*dx + dy*dy);

                    if ( fabs(dy) < 25 )
                        getHist( "ecaldX" )->Fill( dx );
                    if ( fabs(dx) < 25 )
                        getHist( "ecaldY" )->Fill( dy );
                    getHist( "ecaldR" )->Fill( dr );
                    if ( dr < mindR ){
                        mindR = dr;
                        cclu = clu;
                    }

                    getHist( "trkEcalX" ) -> Fill( ecalProj.mXYZ.x(), xyz.x() );
                    getHist( "trkEcalY" ) -> Fill( ecalProj.mXYZ.y(), xyz.y() );

                }
            }
            getHist( "ecalMindR" )->Fill( mindR );
            if (cclu){
                StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(cclu->detectorId(), cclu->x(), cclu->y());
                getHist( "trkEcalMinX" ) -> Fill( ecalProj.mXYZ.x(), xyz.x() );
                getHist( "trkEcalMinY" ) -> Fill( ecalProj.mXYZ.y(), xyz.y() );
            }
        }

        if (hcalProj.mXYZ.z() > 500){

            double mindR = 999;
            StFcsCluster * cclu = nullptr;
            for ( int iDet = 2; iDet < 4; iDet++ ){
                for( size_t i = 0; i < fcs->clusters(iDet).size(); i++){
                    StFcsCluster * clu = fcs->clusters(iDet)[i];
                    if (!clu) continue;
                    StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(clu->detectorId(), clu->x(), clu->y());
                    getHist("hcalXY")->Fill( xyz.x(), xyz.y() );

                    float dx = hcalProj.mXYZ.x() - xyz.x();
                    float dy = hcalProj.mXYZ.y() - xyz.y();
                    float dr = sqrt(dx*dx + dy*dy);

                    if ( fabs(dy) < 25 ){
                        getHist( "hcaldX" )->Fill( dx );
                        getHist( "hcaldXdNFit" )->Fill( dx, fwdTrack->numberOfFitPoints() );

                    }
                    if ( fabs(dx) < 25 ){
                        getHist( "hcaldY" )->Fill( dy );
                        getHist( "hcaldYdNFit" )->Fill( dy, fwdTrack->numberOfFitPoints() );
                    }
                    getHist( "hcaldR" )->Fill( dr );

                    if ( dr < mindR ){
                        mindR = dr;
                        cclu = clu;
                    }

                    getHist( "trkHcalX" ) -> Fill( hcalProj.mXYZ.x(), xyz.x() );
                    getHist( "trkHcalY" ) -> Fill( hcalProj.mXYZ.y(), xyz.y() );
                }
            }
            getHist( "hcalMindR" )->Fill( mindR );
            if (cclu){
                StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(cclu->detectorId(), cclu->x(), cclu->y());
                getHist( "trkHcalMinX" ) -> Fill( hcalProj.mXYZ.x(), xyz.x() );
                getHist( "trkHcalMinY" ) -> Fill( hcalProj.mXYZ.y(), xyz.y() );
            }
        }

        if (fwdTrack->ecalClusters().size() > 0)
            fwdMultEcalMatch++;
        if (fwdTrack->hcalClusters().size() > 0)
            fwdMultHcalMatch++;

    } // Loop ftc->tracks()

    getHist( "fwdMultGood" )->Fill( nGood );
    getHist( "fwdMultFailed" )->Fill( nFailed );
    getHist("fwdMultFST")->Fill( fwdMultFST );
    getHist("fwdMultHcalMatch")->Fill( fwdMultHcalMatch );
    getHist("fwdMultEcalMatch")->Fill( fwdMultEcalMatch );

    LOG_INFO << "Found " << nFailed << " failed track fits out of " << ftc->tracks().size()  << endm;
} // ProcessFwdTracks
//========================================================end of added function



void StFwdQAMaker::ProcessFwdMuTracks(  ){
    // This is an example of how to process fwd track collection
    LOG_DEBUG << "StFwdAnalysisMaker::ProcessFwdMuTracks" << endm;
    StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(!mMuDstMaker) {
        LOG_WARN << " No MuDstMaker ... bye-bye" << endm;
        return;
    }
    StMuDst *mMuDst = mMuDstMaker->muDst();
    if(!mMuDst) {
        LOG_WARN << " No MuDst ... bye-bye" << endm;
        return;
    }
    StMuFwdTrackCollection * ftc = mMuDst->muFwdTrackCollection();
    if (!ftc) return;

    StMuFcsCollection *fcs = mMuDst->muFcsCollection();
    if (!fcs) return;

    LOG_INFO << "Number of StMuFwdTracks: " << ftc->numberOfFwdTracks() << endl;

    StFcsDb *mFcsDb = static_cast<StFcsDb *>(GetDataSet("fcsDb"));

    size_t fwdMultFST = 0;
    size_t fwdMultEcalMatch = 0;
    size_t fwdMultHcalMatch = 0;

    for ( size_t iTrack = 0; iTrack < ftc->numberOfFwdTracks(); iTrack++ ){
        StMuFwdTrack * muFwdTrack = ftc->getFwdTrack( iTrack );
        // LOG_DEBUG << TString::Format("StMuFwdTrack[ nProjections=%lu, nFTTSeeds=%lu, nFSTSeeds=%lu, mPt=%f ]", muFwdTrack->mProjections.size(), muFwdTrack->mFTTPoints.size(), muFwdTrack->mFSTPoints.size(), muFwdTrack->momentum().Pt()) << endm;

        LOG_DEBUG << "StMuFwdTrack has " << muFwdTrack->mEcalClusters.GetEntries() << " Ecal matched" << endm;
        LOG_DEBUG << "StMuFwdTrack has " << muFwdTrack->mHcalClusters.GetEntries() << " Hcal matched" << endm;

        getHist("eta")->Fill( muFwdTrack->momentum().Eta() );
        getHist("phi")->Fill( muFwdTrack->momentum().Phi() );

        if (muFwdTrack->mFSTPoints.size() > 0){
            fwdMultFST ++;
        }

        if (muFwdTrack->mEcalClusters.GetEntries() > 0)
            fwdMultEcalMatch++;
        if (muFwdTrack->mHcalClusters.GetEntries() > 0)
            fwdMultHcalMatch++;


        // ecal proj
        int detId = kFcsWcalId;
        TVector3 ecalXYZ;
        TVector3 ecapP;

        StMuFwdTrackProjection ecalProj;
        bool foundEcalProj = muFwdTrack->getProjectionFor( detId, ecalProj, 0 );

        if (foundEcalProj){
            for( size_t i = 0; i < fcs->numberOfClusters(); i++){
                StMuFcsCluster * clu = fcs->getCluster(i);

                if ( clu->detectorId() > 1 ) continue;

                if ( clu->energy() < 1 ) continue;
                StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(clu->detectorId(), clu->x(), clu->y());

                float dx = ecalProj.mXYZ.X() - xyz.x();
                float dy = ecalProj.mXYZ.Y() - xyz.y();
                float dr = sqrt(dx*dx + dy*dy);

                getHist( "ecaldX" )->Fill( dx );
                getHist( "ecaldY" )->Fill( dy );
                getHist( "ecaldR" )->Fill( dr );

                getHist( "trkEcalX" ) -> Fill( ecalProj.mXYZ.X(), xyz.x() );

            } // i
        } // foundEcalProj


        for ( int i = 0; i < muFwdTrack->mEcalClusters.GetEntries(); i++ ){
            auto c = (StMuFcsCluster*) muFwdTrack->mEcalClusters.At(i);
            if (!c) continue;
            getHist("ecalEnergy")->Fill( c->energy() );

            LOG_DEBUG << "eCal Cluster detId = " << c->detectorId() << endm;
            StThreeVectorD xyz = mFcsDb->getStarXYZfromColumnRow(c->detectorId(), c->x(), c->y());
            getHist("ecalXY")->Fill( xyz.x(), xyz.y() );

            if (foundEcalProj){
                getHist("matchedEcaldX")->Fill( ecalProj.mXYZ.X() - xyz.x() );
            }
        } // i

        getHist("ecalMatchPerTrack")->Fill( muFwdTrack->mEcalClusters.GetEntries() );
        getHist("hcalMatchPerTrack")->Fill( muFwdTrack->mHcalClusters.GetEntries() );

        for ( int i = 0; i < muFwdTrack->mHcalClusters.GetEntries(); i++ ){
            auto c = (StMuFcsCluster*) muFwdTrack->mHcalClusters.At(i);
            if (!c) continue;
            getHist("hcalEnergy")->Fill( c->energy() );

            getHist("hcalXY")->Fill( c->x(), c->y() );
        } // i
    } // iTrack

    getHist("fwdMult")->Fill( ftc->numberOfFwdTracks() );
    getHist("fwdMultFST")->Fill( fwdMultFST );
    getHist("fwdMultHcalMatch")->Fill( fwdMultHcalMatch );
    getHist("fwdMultEcalMatch")->Fill( fwdMultEcalMatch );
}
