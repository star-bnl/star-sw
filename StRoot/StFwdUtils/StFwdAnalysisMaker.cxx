#include "StFwdUtils/StFwdAnalysisMaker.h"

#include "TVector3.h"

#include "StEvent/StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFttCollection.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFwdTrackCollection.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrack.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrackCollection.h"

#include "St_base/StMessMgr.h"

#include "StEvent/StFwdTrack.h"
#include "StFcsDbMaker/StFcsDb.h"

//________________________________________________________________________
StFwdAnalysisMaker::StFwdAnalysisMaker() : StMaker("fwdAna"){
    setLocalOutputFile( "" ); // default off
};
int StFwdAnalysisMaker::Finish() { 
    
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

        LOG_INFO << "Done writing StFwdAnalysisMaker output to local file : " << mLocalOutputFile << endm;
    }

    return kStOk; 
}

//________________________________________________________________________
int StFwdAnalysisMaker::Init() { 
    LOG_DEBUG << "StFwdAnalysisMaker::Init" << endm; 

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

    return kStOK;
}
//________________________________________________________________________
int StFwdAnalysisMaker::Make() {
    LOG_DEBUG << "StFwdAnalysisMaker::Make" << endm;
    long long itStart = FwdTrackerUtils::nowNanoSecond();
    if (!mAnalyzeMuDst)
        ProcessFwdTracks();
    else 
        ProcessFwdMuTracks();
    LOG_DEBUG << "Processing Fwd Tracks took: " << (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e6 << " ms" << endm;
    return kStOK;
} // Make
//________________________________________________________________________
void StFwdAnalysisMaker::Clear(const Option_t *opts) { LOG_DEBUG << "StFwdAnalysisMaker::CLEAR" << endm; }
//________________________________________________________________________
void StFwdAnalysisMaker::ProcessFwdTracks(  ){
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

//________________________________________________________________________
void StFwdAnalysisMaker::ProcessFwdMuTracks(  ){
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
