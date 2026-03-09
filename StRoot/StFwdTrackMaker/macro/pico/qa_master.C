#include "StPicoMcTrack.hpp"
#include "StPicoMcVertex.hpp"
#include "StPicoFwdTrack.hpp"
#include "StPicoFwdVertex.hpp"
#include "StPicoFcsHit.hpp"
#include "StPicoFcsCluster.hpp"
#include "StPicoEvent.hpp"

#include "TFile.h"
#include "TChain.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TClonesArray.h"
#include "TLorentzVector.h"
#include "TSystem.h"
#include <iostream>
#include <fstream>

using namespace std;

// Allocate memory globally for ROOT 6 safety
TClonesArray *mcTracks    = NULL;
TClonesArray *mcVertices  = NULL;
TClonesArray *fwdTracks   = NULL;
TClonesArray *fwdVertices = NULL;
TClonesArray *fcsHits     = NULL;
TClonesArray *fcsClusters = NULL;
TClonesArray *picoEvent   = NULL;

void qa_master(int targetType = 2, TString listFile = "pico.list") {
    
    // 1. Setup Environment
    gSystem->Load("libStarClassLibrary.so");
    gSystem->Load("St_base");
    gSystem->Load("StPicoEvent");

    mcTracks    = new TClonesArray("StPicoMcTrack", 1000);
    mcVertices  = new TClonesArray("StPicoMcVertex", 1000);
    fwdTracks   = new TClonesArray("StPicoFwdTrack", 1000);
    fwdVertices = new TClonesArray("StPicoFwdVertex", 1000);
    fcsHits     = new TClonesArray("StPicoFcsHit", 1000);
    fcsClusters = new TClonesArray("StPicoFcsCluster", 1000);
    picoEvent   = new TClonesArray("StPicoEvent", 1);

    ifstream inList(listFile.Data());
    if (!inList.is_open()) { cout << "Error opening " << listFile << endl; return; }

    TString typeLabel = (targetType == 0) ? "Global" : (targetType == 1) ? "BLC" : (targetType == 2) ? "Primary" : "Other";

    const int nVars = 23;
    TString vars[nVars] = {
        "mId", "mNumberOfSeedPoints", "mNumberOfFitPoints", "mChi2",
        "mMomentumX", "mMomentumY", "mMomentumZ", "mStatus", "mDCAXY", "mDCAZ",
        "mVtxIndex", "mGlobalTrackIndex", "mEcalMatchIndex", "mHcalMatchIndex",
        "mIdTruth", "mQATruth", "mPVal", "mECalX", "mECalY", "mECalZ",
        "mHCalX", "mHCalY", "mHCalZ"
    };

    string line;
    while (getline(inList, line)) {
        TString inFile = line.c_str();
        if (inFile.IsWhitespace()) continue;

        TChain *chain = new TChain("PicoDst");
        chain->Add(inFile);
        
        TString base = gSystem->BaseName(inFile.Data());
        TString outName = Form("fwdQA_%s_%s", typeLabel.Data(), base.Data());
        TFile *fOut = new TFile(outName, "RECREATE");

        // --- 2. Initialize Custom Analysis & QA Histograms ---
        TH1F *hMult = new TH1F("hMult", Form("%s Multiplicity; nTracks; Events", typeLabel.Data()), 50, 0, 50);
	    TH2F *hMult_vs_nBTOFMatch = new TH2F("hMult_vs_nBTOFMatch", Form("%s Multiplicity vs nBTOFMatch; nBTOFMatch; nTracks", typeLabel.Data()), 100, 0, 100, 50, 0, 50);
        TH2F *hMult_vs_refMult = new TH2F("hMult_vs_refMult", Form("%s Multiplicity vs refMult; refMult; nTracks", typeLabel.Data()), 100, 0, 100, 50, 0, 50);
        TH1F *hFwdM0 = new TH1F("hFwdM0", Form("%s Invariant Mass (All); Mass (GeV/c^2)", typeLabel.Data()), 500, 1.0, 5.0);
        TH1F * hFwdM1 = new TH1F("hFwdM1", "Mass of all tracks", 1000, 1, 5);
        TH1F * hFwdM2 = new TH1F("hFwdM2", "Mass of all tracks", 1000, 1, 5);
        TH1F * hFwdM3 = new TH1F("hFwdM3", "Mass of all tracks", 1000, 1, 5);
        TH1F * hFwdM3ls = new TH1F("hFwdM3ls", "Mass of all tracks", 1000, 1, 5);
        //TH1F *hFwdM3 = new TH1F("hFwdM3", Form("%s Lambda Signal (Opposite Sign); Mass (GeV/c^2)", typeLabel.Data()), 500, 1.0, 2.0);
        TH1F *hPt = new TH1F("hPt", Form("%s p_{T}; p_{T} (GeV/c)", typeLabel.Data()), 500, 0, 10);
        TH1F *hEta = new TH1F("hEta", Form("%s #eta; #eta", typeLabel.Data()), 500, 0.0, 10.0);
        TH1F *hPhi = new TH1F("hPhi", Form("%s #phi; #phi (rad)", typeLabel.Data()), 100, -3.2, 3.2);
        TH2F *hEtaPhi = new TH2F("hEtaPhi", Form("%s #eta vs #phi Map; #eta; #phi", typeLabel.Data()), 50, 2.5, 4.5, 50, -3.2, 3.2);


	    TH1F *hPt_pos = new TH1F("hPt_pos", Form("%s p_{T} (Pos); p_{T} [GeV/c]", typeLabel.Data()), 500, 0, 10);
        TH1F *hPt_neg = new TH1F("hPt_neg", Form("%s p_{T} (Neg); p_{T} [GeV/c]", typeLabel.Data()), 500, 0, 10);
        TH1F *hEta_pos = new TH1F("hEta_pos", Form("%s #eta (Pos); #eta", typeLabel.Data()), 500, 0.0, 10.0);
        TH1F *hEta_neg = new TH1F("hEta_neg", Form("%s #eta (Neg); #eta", typeLabel.Data()), 500, 0.0, 10.0);
        TH1F *hPhi_pos = new TH1F("hPhi_pos", Form("%s #phi (Pos); #phi [rad]", typeLabel.Data()), 100, -3.2, 3.2);
        TH1F *hPhi_neg = new TH1F("hPhi_neg", Form("%s #phi (Neg); #phi [rad]", typeLabel.Data()), 100, -3.2, 3.2);
        TH2F *hMult_vs_nEcal = new TH2F("hMult_vs_nEcal", Form("%s Multiplicity vs nECal Clusters; nECal Clusters; nTracks", typeLabel.Data()), 50, 0, 50, 50, 0, 50);
        TH2F *hMult_vs_nHcal = new TH2F("hMult_vs_nHcal", Form("%s Multiplicity vs nHCal Clusters; nHCal Clusters; nTracks", typeLabel.Data()), 50, 0, 50, 50, 0, 50);
        // --- 3. Event Loop ---
        chain->SetBranchAddress("FwdTracks", &fwdTracks);
        chain->SetBranchAddress("Event", &picoEvent);
	    chain->SetBranchAddress("FcsClusters", &fcsClusters);

        for (int i = 0; i < chain->GetEntries(); i++) {
            fwdTracks->Clear(); picoEvent->Clear();
            chain->GetEntry(i);
	    std::cout << "looping through entry " << i << std::endl;
            
            StPicoEvent *ev = (StPicoEvent*)picoEvent->At(0);
            if (!ev || ev->nBTOFMatch() < 2 || fabs(ev->primaryVertex().Z()) > 50) continue;

	    int nEcalClusters = 0;
            int nHcalClusters = 0;
            for (int c = 0; c < fcsClusters->GetEntriesFast(); c++) {
                StPicoFcsCluster *cluster = (StPicoFcsCluster*)fcsClusters->At(c);
                if (!cluster) continue;
                
                int detId = cluster->detectorId();
                
                if (detId == 1 || detId == 2) {
                    nEcalClusters++;
                } else if (detId == 3 || detId == 4) {
                    nHcalClusters++;
                }
            } 

            int nGoodTracks = 0;
            for (int j = 0; j < fwdTracks->GetEntriesFast(); j++) {
		//std::cout << "looping through fwdTrack " << j << std::endl;
                StPicoFwdTrack *trk1 = (StPicoFwdTrack*)fwdTracks->At(j);
                if (!trk1 || trk1->trackType() != targetType || trk1->status() <= 0) continue;
                nGoodTracks++;

                hPt->Fill(trk1->momentum().Perp());
                hEta->Fill(trk1->momentum().Eta());
                hPhi->Fill(trk1->momentum().Phi());
                hEtaPhi->Fill(trk1->momentum().Eta(), trk1->momentum().Phi());
                
	        if (trk1->charge() > 0) {
                    hPt_pos->Fill(trk1->momentum().Perp());
                    hEta_pos->Fill(trk1->momentum().Eta());
                    hPhi_pos->Fill(trk1->momentum().Phi());
            } else if (trk1->charge() < 0) {
                    hPt_neg->Fill(trk1->momentum().Perp());
                    hEta_neg->Fill(trk1->momentum().Eta());
                    hPhi_neg->Fill(trk1->momentum().Phi());
            }
            
	        for (int k = j + 1; k < fwdTracks->GetEntriesFast(); k++) {
                    StPicoFwdTrack *trk2 = (StPicoFwdTrack*)fwdTracks->At(k);
                    if (!trk2 || trk2->trackType() != targetType) continue;

                    TLorentzVector fwdlv1 = trk1->fourMomentum(0.13957); 
                    TLorentzVector fwdlv2 = trk2->fourMomentum(0.93827); 
                    TLorentzVector fwdlvA = fwdlv1 + fwdlv2;
                    hFwdM0->Fill(fwdlvA.M());

		            TLorentzVector fwdlv1B = trk2->fourMomentum( 0.13957); // Assuming pion mass for lv calculation);
                    TLorentzVector fwdlv2B = trk1->fourMomentum( 0.93827); // Assuming proton mass for lv calculation);
                    TLorentzVector fwdlvB = fwdlv1B + fwdlv2B;
                    hFwdM0->Fill(fwdlvB.M());


		        if ( fwdlv1.Eta() > 2.5 && fwdlv1.Eta() < 4.0 && fwdlv2.Eta() > 2.5 && fwdlv2.Eta() < 4.0 ) {} 
                    else continue;

                    if (trk1->status() < 2 || trk2->status() < 2) continue;

                    if ( trk1->charge() != trk2->charge() ) {
                        if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM1->Fill(fwdlvA.M());}
                        if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM1->Fill(fwdlvB.M());}

                        if ( trk1->chi2() > 0.001 && trk1->chi2() < 200 && trk2->chi2() > 0.001 && trk2->chi2() < 200 ) {
                            if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM2->Fill(fwdlvA.M());}
                            if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM2->Fill(fwdlvB.M());}

                            if ( trk1->momentum().Perp() > 0.1 && trk2->momentum().Perp() > 0.1 ) {
                                if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM3->Fill(fwdlvA.M());}
                                if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM3->Fill(fwdlvB.M());}
                            }
                        }
                    } else {
                        if ( trk1->chi2() > 0.001 && trk1->chi2() < 200 && trk2->chi2() > 0.001 && trk2->chi2() < 200 ) {
                        

                            if ( trk1->momentum().Perp() > 0.01 && trk2->momentum().Perp() > 0.01 ) {
                                if ( fwdlvA.Eta() > 2.5 && fwdlvA.Eta() < 4.0 ) {hFwdM3ls->Fill(fwdlvA.M());}
                                if ( fwdlvB.Eta() > 2.5 && fwdlvB.Eta() < 4.0 ) {hFwdM3ls->Fill(fwdlvB.M());}
                            }
                        }
                    } // like sign
                }
            }
            hMult->Fill(nGoodTracks);
	        hMult_vs_nBTOFMatch->Fill(ev->nBTOFMatch(), nGoodTracks);
            hMult_vs_refMult->Fill(ev->refMult(), nGoodTracks);
	        hMult_vs_nEcal->Fill(nEcalClusters, nGoodTracks);
            hMult_vs_nHcal->Fill(nHcalClusters, nGoodTracks);
        }


	
	for (int v = 0; v < nVars; v++) {
            TString varName = vars[v];
            TString histName = Form("h_%s", varName.Data());
            
            // Default initialization
            int nBins = 1000; double xMin = -1000, xMax = 1000;
            double scale = 1.0; 
            TString unitLabel = "";
	    if (varName.Contains("mHCal") || varName.Contains("mECal")) { 
                if (varName.Contains("Z")) {
                    scale = 10.0;
                    nBins = 300; xMin = 600; xMax = 900; // Face is ~710 or ~800 cm
                } else {
                    scale = 100.0;
                    nBins = 700; xMin = -350; xMax = 350; // Physical bounding box
                }
                unitLabel = " [cm]";
            }

	    else if (varName.Contains("Momentum")) { 
                scale = 1000.0;
                if (varName.Contains("Z")) {
                    nBins = 250; xMin = 0; xMax = 250; // Max beam energy is 250 GeV
                } else {
                    nBins = 200; xMin = -5; xMax = 5;  // Transverse momentum is small
                }
                unitLabel = " [GeV/c]";
            }

            else if (varName == "mPVal") { 
                scale = 10000.0;
                nBins = 100; xMin = 0; xMax = 1; 
            }
	    else if (varName == "mDCAZ") { nBins = 5000; xMin = -250; xMax = 250; unitLabel = " [cm]"; }
            else if (varName == "mDCAXY") { nBins = 200; xMin = 0; xMax = 20; unitLabel = " [cm]"; }
            else if (varName.Contains("Points")) { nBins = 15; xMin = 0; xMax = 15; }
            else if (varName == "mChi2") { nBins = 250; xMin = 0; xMax = 50; }
            else if (varName.Contains("mId") || varName.Contains("Index") || varName.Contains("Truth")) { nBins = 250; xMin = -1; xMax = 249; }
            else if (varName == "mStatus") { nBins = 20; xMin = -5; xMax = 15; }

            TString fullTitle = Form("%s: %s; %s%s; Counts", typeLabel.Data(), varName.Data(), varName.Data(), unitLabel.Data());
            TH1F *hRaw = new TH1F(histName, fullTitle, nBins, xMin, xMax);

            TString drawCmd = Form("FwdTracks.%s / %f >> %s", varName.Data(), scale, histName.Data());
            TString cutCmd = Form("FwdTracks.trackType() == %d && FwdTracks.status() > 0", targetType);

            chain->Draw(drawCmd, cutCmd, "goff");
            hRaw->Write();
            delete hRaw;
        }

        // --- 5. Save Custom Analysis Histograms ---
        fOut->cd();
        hMult->Write(); hFwdM0->Write(); hFwdM1->Write(); hFwdM2->Write(); hFwdM3->Write(); hFwdM3ls->Write(); hPt->Write(); hEta->Write(); hPhi->Write(); hEtaPhi->Write(); hMult_vs_nBTOFMatch->Write(); hMult_vs_refMult->Write();
        hPt_pos->Write(); hPt_neg->Write(); hEta_pos->Write(); hEta_neg->Write(); hPhi_pos->Write(); hPhi_neg->Write(); hMult_vs_nEcal->Write(); hMult_vs_nHcal->Write();
        fOut->Close();
        chain->ResetBranchAddresses(); 
        delete chain;
        delete fOut;
        cout << "--> Finished " << outName << " (Track Type: " << targetType << ")" << endl;
    }
}
