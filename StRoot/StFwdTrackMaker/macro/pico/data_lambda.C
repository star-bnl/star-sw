#include "StPicoMcTrack.hpp"
#include "StPicoMcVertex.hpp"
#include "StPicoFwdTrack.hpp"
#include "StPicoFwdVertex.hpp"
#include "StPicoFcsHit.hpp"
#include "StPicoFcsCluster.hpp"
#include "StPicoEvent.hpp"

bool inAcc(StPicoMcTrack *track) {
    // Example acceptance criteria
    return (track->nHitsFts() > 2 );
}

int testTrackType = 2; // 0=Global, 1=BLC, 2=Primary, 3=FwdVertex

// make a TClonesArray for McTrack
TClonesArray *mcTracks    = new TClonesArray("StPicoMcTrack", 1000);
TClonesArray *mcVertices  = new TClonesArray("StPicoMcVertex", 1000);
TClonesArray *fwdTracks   = new TClonesArray("StPicoFwdTrack", 1000);
TClonesArray *fwdVertices = new TClonesArray("StPicoFwdVertex", 1000);
TClonesArray *fcsHits     = new TClonesArray("StPicoFcsHit", 1000);
TClonesArray *fcsClusters = new TClonesArray("StPicoFcsCluster", 1000);
TClonesArray *picoEvent   = new TClonesArray("StPicoEvent", 1);
// StPicoEvent *picoEvent = new StPicoEvent();

void data_lambda( TString glob = "/Users/Nick/STAR/fwd_qa/*picoDst.root", int tt = 0 ){

    testTrackType = tt;
    cout << "Analyzing Track Type: " << testTrackType << endl;

    cout << "Loading from: \n " << glob << endl;


    TFile *fOutput = new TFile("lambda_analysis.root", "RECREATE");
    TH2F * hNumFstStgc = new TH2F("hNumFstStgc", "Number of FST and STGC hits; nFST; nSTGC", 15, 0, 15, 15, 0, 15);
    TH1F * hNumFST = new TH1F("hNumFST", "Number of FST hits", 100, 0, 100);
    TH1F * hNumHCAL = new TH1F("hNumHCAL", "Number of HCAL hits", 100, 0, 100);
    TH1F * hNumWCAL = new TH1F("hNumWCAL", "Number of WCAL hits", 100, 0, 100);
    TH1F * hNumSTGC = new TH1F("hNumSTGC", "Number of STGC hits", 100, 0, 100);
    TH1F * hM0 = new TH1F("hM0", "Mass of all tracks", 100, 1, 2);
    // TH1F * hFwdM0 = new TH1F("hFwdM0", "Mass of all tracks", 100, 1, 2);
    TH1F * hFwdM0Correct = new TH1F("hFwdM0Correct", "Mass of all tracks", 100, 1, 2);
    TH1F * hFwdM0Incorrect = new TH1F("hFwdM0Incorrect", "Mass of all tracks", 100, 1, 2);
    TH1F * hEta = new TH1F("hEta", "Pseudorapidity of tracks", 100, -5, 5);
    TH1F * hEtaAcc = new TH1F("hEtaAcc", "Pseudorapidity of tracks that hit FWD", 100, -5, 5);
    TH1F * hIdTruthEtaAcc = new TH1F("hIdTruthEtaAcc", "Pseudorapidity of tracks that hit FWD", 100, -5, 5);

    TH1F * hPt = new TH1F("hPt", "Transverse momentum of tracks", 100, 0, 10);
    TH1F * hPtAcc = new TH1F("hPtAcc", "Transverse momentum of tracks that hit FWD", 100, 0, 10);
    TH1F * hIdTruthPtAcc = new TH1F("hIdTruthPtAcc", "Transverse momentum of tracks that hit FWD", 100, 0, 10);

    TH1F * hFwdM0 = new TH1F("hFwdM0", "Mass of all tracks", 1000, 1, 2);
    TH1F * hFwdM1 = new TH1F("hFwdM1", "Mass of all tracks", 1000, 1, 2);
    TH1F * hFwdM2 = new TH1F("hFwdM2", "Mass of all tracks", 1000, 1, 2);
    TH1F * hFwdM3 = new TH1F("hFwdM3", "Mass of all tracks", 1000, 1, 2);
    TH1F * hFwdM3ls = new TH1F("hFwdM3ls", "Mass of all tracks", 1000, 1, 2);

    TChain *chain = new TChain("PicoDst");
    chain->Add(glob);

    chain->SetBranchAddress("McTrack", &mcTracks);
    chain->SetBranchAddress("McVertex", &mcVertices);
    chain->SetBranchAddress("FwdTracks", &fwdTracks);
    chain->SetBranchAddress("FwdVertices", &fwdVertices);
    chain->SetBranchAddress("FcsHits", &fcsHits);
    chain->SetBranchAddress("FcsClusters", &fcsClusters);
    chain->SetBranchAddress("Event", &picoEvent);

    size_t nEntries = chain->GetEntries();
    cout << "Number of Files: " << chain->GetListOfFiles()->GetEntries() << endl;
    cout << "Number of events: " << nEntries << endl;
    if (nEntries > 50000 )
        nEntries = 50000;

    size_t nLambda = 0;
    for (size_t i = 0; i < nEntries; ++i) {
        mcTracks->Clear(); // Clear the TClonesArray for each entry
        mcVertices->Clear(); // Clear the TClonesArray for each entry
        fwdTracks->Clear(); // Clear the TClonesArray for each entry
        fwdVertices->Clear(); // Clear the TClonesArray for each entry
        fcsHits->Clear(); // Clear the TClonesArray for each entry
        fcsClusters->Clear(); // Clear the TClonesArray for each entry

        chain->GetEntry(i);
        if (i % 1000 == 0) {
            cout << "Processing entry: " << i << endl;
            cout << "Event has " << fwdTracks->GetEntriesFast() << " FwdTracks" << endl;
        }

        StPicoEvent *event = static_cast<StPicoEvent*>(picoEvent->At(0));
        if (!event) {
            cout << "No event found for entry " << i << endl;
            continue;
        }

        // printf("Event %zu: RunId=%d, EventId=%d, BunchCrossId=%d, RefMult=%d\n",
        //        i, event->runId(), event->eventId(), event->bunchId(), event->refMult());

        if (event->refMult() < 3) continue;
        if (event->nBTOFMatch() < 2) continue;
        if (fabs(event->primaryVertex().Z()) > 50 ) continue;

        // Loop over the FwdTracks
        for (int j = 0; j < fwdTracks->GetEntriesFast(); ++j) {
            StPicoFwdTrack *fwdTrack = static_cast<StPicoFwdTrack*>(fwdTracks->At(j));
            if ( fwdTrack->trackType() != testTrackType ) continue; // Only consider global tracks
            if (!fwdTrack) continue;

            for (int k = j; k < fwdTracks->GetEntriesFast(); k++) {
                if ( j == k ) continue; // Skip self-comparison
                StPicoFwdTrack *fwdTrack2 = static_cast<StPicoFwdTrack*>(fwdTracks->At(k));
                if (!fwdTrack2) continue;
                if ( fwdTrack2->trackType() != testTrackType ) continue; // Only consider global tracks

                TLorentzVector fwdlv1 = fwdTrack->fourMomentum( 0.13957); // Assuming pion mass for lv calculation);
                TLorentzVector fwdlv2 = fwdTrack2->fourMomentum( 0.93827); // Assuming proton mass for lv calculation);
                TLorentzVector fwdlvA = fwdlv1 + fwdlv2;
                hFwdM0->Fill(fwdlvA.M());
            
                TLorentzVector fwdlv1B = fwdTrack2->fourMomentum( 0.13957); // Assuming pion mass for lv calculation);
                TLorentzVector fwdlv2B = fwdTrack->fourMomentum( 0.93827); // Assuming proton mass for lv calculation);
                TLorentzVector fwdlvB = fwdlv1B + fwdlv2B;
                hFwdM0->Fill(fwdlvB.M());
                nLambda++;

                if ( fwdlv1.Eta() > 2.5 && fwdlv1.Eta() < 4.0 && fwdlv2.Eta() > 2.5 && fwdlv2.Eta() < 4.0 ) {} 
                else continue;

                if (fwdTrack->status() < 2 || fwdTrack2->status() < 2) continue; // Check if fit converged

                if ( fwdTrack->charge() != fwdTrack2->charge() ) {
                    if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM1->Fill(fwdlvA.M());}
                    if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM1->Fill(fwdlvB.M());}

                    if ( fwdTrack->chi2() > 0.001 && fwdTrack->chi2() < 200 && fwdTrack2->chi2() > 0.001 && fwdTrack2->chi2() < 200 ) {
                        if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM2->Fill(fwdlvA.M());}
                        if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM2->Fill(fwdlvB.M());}

                        if ( fwdTrack->momentum().Perp() > 0.1 && fwdTrack2->momentum().Perp() > 0.1 ) {
                            if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM3->Fill(fwdlvA.M());}
                            if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM3->Fill(fwdlvB.M());}
                        }
                    }
                } else {
                    if ( fwdTrack->chi2() > 0.001 && fwdTrack->chi2() < 200 && fwdTrack2->chi2() > 0.001 && fwdTrack2->chi2() < 200 ) {
                        

                        if ( fwdTrack->momentum().Perp() > 0.01 && fwdTrack2->momentum().Perp() > 0.01 ) {
                            if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM3ls->Fill(fwdlvA.M());}
                            if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM3ls->Fill(fwdlvB.M());}
                        }
                    }
                } // like sign
            }
        }
    } // end of loop over entries
    cout << "Number of Lambda candidates found: " << nLambda << endl;
    fOutput->cd();
    fOutput->Write();
    fOutput->Close();
}