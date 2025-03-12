//usr/bin/env root4star -l -b -q  $0'("'"$1"'")'; exit $?

#include "TTree.h"
#include "TClonesArray.h"
#include <map>
// #include <__config>

// TClonesArrays to read from the tree
TClonesArray *mcTracks = NULL;
TClonesArray *fttPoints = NULL;
TClonesArray *fttClusters = NULL;
TClonesArray *fstPoints = NULL;
TClonesArray *wcal = NULL;
TClonesArray *wcalHits = NULL;
TClonesArray *hcal = NULL;
TClonesArray *hcalHits = NULL;
TClonesArray *epdHits = NULL;
TClonesArray *fwdTracks = NULL;
TClonesArray *seeds = NULL;
TTree *t = NULL;

std::map<std::string, TH1*> histograms;
TH1* addH1( string name, string title, int nx, float x1, float x2 ){
    histograms[name] = new TH1F( name.c_str(), title.c_str(), nx, x1, x2 );
    return histograms[name];
}
TH2* addH2( string name, string title, int nx, float x1, float x2, int ny, float y1, float y2 ){
    histograms[name] = new TH2F( name.c_str(), title.c_str(), nx, x1, x2, ny, y1, y2 );
    return (TH2*)histograms[name];
}
inline TH1 *getH1( string name ){
    // printf( "Looking for histogram name=[%s]", name.c_str() );
    assert( histograms.count( name ) && "Histogram cannot be found" && name.c_str() );
    assert( histograms[name] && TString::Format( "Histogram %s is NULL", name.c_str() ) );
    return histograms[name];
}
inline TH2 *getH2( string name ){
    return (TH2*) getH1( name );
}

void setupRead( TString filename = "fwdtree.root" ){
    // setup and make sure libraries are loaded
    gSystem->Load( "libStarRoot.so" );
	gSystem->Load("libStarClassLibrary.so");
    gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
	gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
	loadSharedLibraries();

	gSystem->Load("libgenfit2.so");
	gSystem->Load("libKiTrack.so");
	gSystem->Load( "libStFwdTrackMaker.so" );

    // now open our data file
    TFile *f = new TFile(filename);
    t = (TTree*)f->Get("fwd");

    // create the readers for each branch
	mcTracks = new TClonesArray("StMuMcTrack");
   	t->GetBranch("mcTracks")->SetAutoDelete(kFALSE);
   	t->SetBranchAddress("mcTracks",&mcTracks);

	fttPoints = new TClonesArray("StMuFttPoint");
	t->GetBranch("fttPoints")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("fttPoints",&fttPoints);

	fttClusters = new TClonesArray("StMuFttCluster");
	t->GetBranch("fttClusters")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("fttClusters",&fttClusters);

	fstPoints = new TClonesArray("StMuFstHit");
	t->GetBranch("fstHits")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("fstHits",&fstPoints);

	wcal = new TClonesArray("FcsClusterWithStarXYZ");
	t->GetBranch("wcalClusters")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("wcalClusters",&wcal);

	wcalHits = new TClonesArray("FcsHitWithStarXYZ");
	t->GetBranch("wcalHits")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("wcalHits",&wcalHits);

	hcal = new TClonesArray("FcsClusterWithStarXYZ");
	t->GetBranch("hcalClusters")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("hcalClusters",&hcal);

	hcalHits = new TClonesArray("FcsHitWithStarXYZ");
	t->GetBranch("hcalHits")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("hcalHits",&hcalHits);

	epdHits = new TClonesArray("FcsHitWithStarXYZ");
	t->GetBranch("epdHits")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("epdHits",&epdHits);

	fwdTracks = new TClonesArray("StMuFwdTrack");
	t->GetBranch("reco")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("reco",&fwdTracks);

	seeds = new TClonesArray("StMuFwdTrackSeedPoint");
	t->GetBranch("seeds")->SetAutoDelete(kFALSE);
	t->SetBranchAddress("seeds",&seeds);
}

void qaMomentumResolution(){

    if ( histograms.count( "curveRcVsMc" ) == 0 ) {
        printf( "Creating Momentum Resolution Histograms\n" );
        addH2( "curveRcVsMc", "Track curvature; MC; RC", 200, -10, 10, 200, -10, 10 );
        addH2( "curveMcVsPtMc", ";MC Pt; MC Curve ", 200, 0, 10, 100, 0, 10 );
        addH2( "ptRcVsMc", "Track Pt; MC; RC", 200, 0, 10, 200, 0, 10 );
        addH1( "curveRes", "Curvature Resolution; (C^{MC}-C^{RC})/C^{MC}", 200, -2, 2 );
        addH1( "transMomRes", "Pt Resolution; (Pt^{MC} - Pt^{RC}) / Pt^{MC}", 200, -2, 2 );
        addH1( "deltaCharge", "deltaCharge; |q_{MC}-q_{RC}|;counts;", 5, 0, 5 );
    }

    const TH2 * hCurveRcVsMc = getH2( "curveRcVsMc" );
    const TH2 * hCurveMcVsPtMc = getH2( "curveMcVsPtMc" );
    const TH2 * hPtRcVsMc = getH2( "ptRcVsMc" );

    const TH1 * hCurveRes = getH1( "curveRes" );
    const TH1 * hTransMomRes = getH1( "transMomRes" );
    const TH1 * hDeltaCharge = getH1( "deltaCharge" );

    for ( int j = 0; j < fwdTracks->GetEntries(); j++ ){
        
        StMuFwdTrack *fwt = fwdTracks->At(j);
        UShort_t indexToMatchedMC = fwt->idTruth() - 1;
    //     // cout << "Processing track " << j << ", mcid = " << indexToMatchedMC << endl;
        if (indexToMatchedMC >= mcTracks->GetEntries()) continue;
    //     // get corresponding MC track
        StMuMcTrack *mct = mcTracks->At(indexToMatchedMC);

        float curveMc = fabs(mct->Charge() / mct->pT());
        float curveRc = fabs(fwt->charge() / fwt->momentum().Pt());
    //     // cout << "mct->pT() = " << mct->pT() << endl;
        if ( mct->pT() > 0.1 && fwt->pval() > 0.01){
            hDeltaCharge->Fill( abs( mct->Charge() - fwt->charge() ) );
            hCurveRes->Fill( (curveMc - curveRc) / curveMc );
            hTransMomRes->Fill( (mct->pT() - fwt->momentum().Pt()) / mct->pT() );

            hCurveRcVsMc->Fill( curveMc, curveRc );
            hCurveMcVsPtMc->Fill( curveMc, mct->pT() );
            hPtRcVsMc->Fill( mct->pT(), fwt->momentum().Pt() );
        }
    }
}

void qaFCSTrackMatch(){

    if (histograms.count("dxWCAL") == 0){
        addH1( "dxWCAL", "WCAL; dx", 500, -100, 100 );
        addH1( "dxWCAL2", "WCAL; dx", 500, -100, 100 );
        addH1( "dyWCAL", "WCAL; dy", 500, -100, 100 );
        addH1( "dxHCAL", "HCAL; dx", 500, -100, 100 );
        addH1( "drWCAL", "WCAL; dr", 500, 0, 100 );
        addH1( "drHCAL", "HCAL; dr", 500, 0, 100 );
        addH2( "wcalTrackX", "; WCAL x; Track x", 500, -50, 50, 500, -50, 50 );
        addH2( "wcalTrackY", "; WCAL y; Track y", 500, -50, 50, 500, -50, 50 );
    }

    const TH1 * hdxWCAL = getH1("dxWCAL");
    const TH1 * hdxWCAL2 = getH1("dxWCAL2");
    const TH1 * hdyWCAL = getH1("dyWCAL");
    const TH1 * hdxHCAL = getH1("dxHCAL");

    const TH1 * hdrWCAL = getH1("drWCAL");
    const TH1 * hdrHCAL = getH1("drHCAL");

    const TH2 * hWCALX = getH1("wcalTrackX");
    const TH2 * hWCALY = getH1("wcalTrackY");

    for ( int j = 0; j < fwdTracks->GetEntries(); j++ ){
            StMuFwdTrack *track = (StMuFwdTrack *)fwdTracks->At(j);
            
            // printf("Track %d: pt=%f, eta=%f, phi=%f\n", j, track->momentum().Pt(), track->momentum().Eta(), track->momentum().Phi());
            if ( track->pval() < 0.01 ) continue;
			StMuFwdTrackProjection projWCAL;
			track->getProjectionFor(kFcsWcalId, projWCAL);
			// printf("Projection @ WCAL: det=%d, x=%f, y=%f, z=%f\n", projWCAL.mDetId, projWCAL.mXYZ.X(), projWCAL.mXYZ.Y(), projWCAL.mXYZ.Z());

			StMuFwdTrackProjection projHCAL;
			track->getProjectionFor(kFcsHcalId, projHCAL);
			// printf("Projection @ HCAL: det=%d, x=%f, y=%f, z=%f\n", projHCAL.mDetId, projHCAL.mXYZ.X(), projHCAL.mXYZ.Y(), projHCAL.mXYZ.Z());

			// loop over WCAL clusters
			for ( int k = 0; k < wcal->GetEntries(); k++ ){
				FcsClusterWithStarXYZ *cluster = (FcsClusterWithStarXYZ*)wcal->At(k);
                double dx = projWCAL.mXYZ.X() - cluster->mXYZ.X();
                double dy = projWCAL.mXYZ.Y() - cluster->mXYZ.Y();
                double dr = sqrt( dx*dx + dy*dy );

                hdxWCAL2->Fill( dx );
                if ( projWCAL.mXYZ.X() < 0 && cluster->mClu->detectorId() == 1 ) continue;
                if ( projWCAL.mXYZ.X() > 0 && cluster->mClu->detectorId() == 0 ) continue;

                hdxWCAL->Fill( dx );
                hdyWCAL->Fill( dy );
				// printf("WCAL Cluster %d: x=%f, y=%f, z=%f\n", k, cluster->mXYZ.X(), cluster->mXYZ.Y(), cluster->mXYZ.Z());
                hdrWCAL->Fill( dr );

                hWCALX->Fill( cluster->mXYZ.X(), projWCAL.mXYZ.X() );
                hWCALY->Fill( cluster->mXYZ.Y(), projWCAL.mXYZ.Y() );
			}

			// loop over WCAL clusters
			for ( int k = 0; k < hcal->GetEntries(); k++ ){
				FcsClusterWithStarXYZ *cluster = (FcsClusterWithStarXYZ*)hcal->At(k);
                double dx = projHCAL.mXYZ.X() - cluster->mXYZ.X();
                double dy = projHCAL.mXYZ.Y() - cluster->mXYZ.Y();
                double dr = sqrt( dx*dx + dy*dy );
                
                hdxHCAL->Fill( dx );
                hdrHCAL->Fill( dr );
			}

		}
}


// Loop on events
void eventLoop( int numEventsLimit = -1, int reportEveryNthEvent = -1 ){
    int lastEventIndex = (numEventsLimit > 0 ? numEventsLimit : t->GetEntries() );
    for ( int i = 0; i < lastEventIndex; i++ ){
        t->GetEntry(i);
        if ( reportEveryNthEvent > 0 && i % reportEveryNthEvent == 0){
            printf( "Processing Event %d...\n", i );
        }
        // run qa subroutines here
        // qaMomentumResolution();
        qaFCSTrackMatch();
    }
}



void qa( TString filename = "fwdtree.root" ){
    setupRead( filename );
    TFile * fOut = new TFile( "QuickQA.root", "RECREATE" );
    fOut->cd();
    eventLoop(5000, 100);
    fOut->Write();
    // writeHistograms();
    return;
    //loop over the events
    int nEvents = t->GetEntries();
    if (nEvents > 1000) nEvents = 1000;
    for ( int i = 0; i < nEvents; i++ ){
        t->GetEntry(i);
        for ( int j = 0; j < fwdTracks->GetEntries(); j++ ){
            StMuFwdTrack *track = fwdTracks->At(j);
            printf("Track %d: pt=%f, eta=%f, phi=%f\n", j, track->momentum().Pt(), track->momentum().Eta(), track->momentum().Phi());

			StMuFwdTrackProjection projWCAL;
			track->getProjectionFor(kFcsWcalId, projWCAL);
			printf("Projection @ WCAL: det=%d, x=%f, y=%f, z=%f\n", projWCAL.mDetId, projWCAL.mXYZ.X(), projWCAL.mXYZ.Y(), projWCAL.mXYZ.Z());


			StMuFwdTrackProjection projHCAL;
			track->getProjectionFor(kFcsHcalId, projHCAL);
			printf("Projection @ HCAL: det=%d, x=%f, y=%f, z=%f\n", projHCAL.mDetId, projHCAL.mXYZ.X(), projHCAL.mXYZ.Y(), projHCAL.mXYZ.Z());

			// loop over WCAL clusters
			for ( int k = 0; k < wcal->GetEntries(); k++ ){
				FcsClusterWithStarXYZ *cluster = (FcsClusterWithStarXYZ*)wcal->At(k);

				printf("WCAL Cluster %d: x=%f, y=%f, z=%f\n", k, cluster->mXYZ.X(), cluster->mXYZ.Y(), cluster->mXYZ.Z());

			}


			// loop over WCAL clusters
			for ( int k = 0; k < wcal->GetEntries(); k++ ){
				FcsClusterWithStarXYZ *cluster = (FcsClusterWithStarXYZ*)wcal->At(k);

				printf("WCAL Cluster %d: x=%f, y=%f, z=%f\n", k, cluster->mXYZ.X(), cluster->mXYZ.Y(), cluster->mXYZ.Z());

			}

		}
    }
	cout << "Processed: " << t->GetEntries() << " entries" << endl;
}
