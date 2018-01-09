/*
   root.exe 'lMuDst.C(0,"*.MuDst.root")' readPrimaries.C+
 */
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TStopwatch.h"
#include "Riostream.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"

const double C_C_LIGHT = 29.9792458; // [cm/ns]

void readPrimaries(Long64_t nevent=1000) {
  
  TH1::SetDefaultSumw2(1);
  
  TH1D* hRcPt  = new TH1D("hRcPt", "", 500, 0, 5);
  TH1D* hRcEta = new TH1D("hRcEta", "", 400, -2, 2);
  TH2D* hOneOverBetaP = new TH2D("hOneOverBetaP", "1/#beta-1", 300, 0, 3, 300, -0.1, 2.9);
  TH2D* hMass2P = new TH2D("hMass2P", "M^{2} versus P", 300, 0, 3, 300, -0.1, 1.9);
  TH2D* hBTofHitLTTray = new TH2D("hBTofHitLTTray", "", 150, 0, 150, 100, 0, 100);
  TH1D* hBTofBeta = new TH1D("hBTofBeta", "", 200, 0, 1);
  
  TH2D* hBTofHitLETvsTof = new TH2D("hBTofHitLETvsTof", "", 100, 0, 100, 100, 0, 100);
  
  TH1D* hMcPt  = new TH1D("hMcPt", "", 500, 0, 5);
  TH1D* hMcEta = new TH1D("hMcEta", "", 400, -2, 2);
  
  TH1D* hEffPt = new TH1D("hEffPt", "", 500, 0, 5);
  TH1D* hEffEta = new TH1D("hEffEta", "", 400, -2, 2);
  
  TStopwatch time_all;
  // Chain
  StMuDstMaker* maker = StMuDstMaker::instance();
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {"MuEvent"
				    ,"PrimaryVertices"
				    ,"PrimaryTracks"
				    ,"GlobalTracks"
				    ,"CovGlobTrack"
				    ,"BTofHit"
#if 1
				    ,"StStMuMcVertex"
				    ,"StStMuMcTrack"
#endif
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  //  StMuDebug::setLevel(0);  
  TChain *tree = StMuDstMaker::instance()->chain();
  if (! tree) return;
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  if (nentries < 100) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
  for (Long64_t i_evt = 0; i_evt < nevent; i_evt++) {
    maker->Clear();
    if (maker->Make()) break;
    StMuDst *mudst = StMuDst::instance();
    if (! mudst) {
      cout << "No event" << endl;
      continue;
    }
#if 1
    if (! i_evt%1000) {
      cout << "Event " << mudst->event()->eventId() << ": "
	   << mudst->numberOfPrimaryTracks() << " primary tracks, "
	   << mudst->numberOfGlobalTracks() << " global tracks" << endl;
    }
#endif    
    for (UInt_t iPTrk = 0; iPTrk < mudst->numberOfPrimaryTracks(); ++iPTrk) {
      StMuTrack *pTrk = mudst->primaryTracks(iPTrk);
      if (!pTrk) continue;
      if (! pTrk->idTruth()) continue;
      hRcPt->Fill(pTrk->pt());
      hRcEta->Fill(pTrk->eta());
      const StMuBTofHit* tofHit = pTrk->tofHit();
      // int idxTofHit = pTrk->index2BTofHit();
      // StMuBTofHit* tofHit = mudst->btofHit(idxTofHit);
      if (tofHit) {
	// hOneOverBetaP->Fill(pTrk->p().mag(), tofHit->leadingEdgeTime()/pTrk->btofPidTraits().pathLength());
	//Hongwei	double tofBeta = pTrk->btofPidTraits().pathLength() / ((tofHit->leadingEdgeTime()-19) * C_C_LIGHT);
#if 1
#if 1
	double tofBeta = -1.;
	if (pTrk->btofPidTraits().beta() > 0) tofBeta = pTrk->btofPidTraits().beta();
#else
	double tofBeta = pTrk->btofPidTraits().pathLength() / ((pTrk->btofPidTraits().timeOfFlight()) * C_C_LIGHT);
#endif
#else
	double tofBeta = pTrk->btofPidTraits().pathLength() / ((pTrk->btofPidTraits().timeOfFlight()-19) * C_C_LIGHT);
#endif
	hBTofBeta->Fill(tofBeta);
	hOneOverBetaP->Fill(pTrk->p().mag(), 1.0/tofBeta - 1.0);
	Double_t mass2 = (1.0/(tofBeta*tofBeta) - 1)*pTrk->p().mag2();
	hMass2P->Fill(pTrk->p().mag(), mass2);
	hBTofHitLTTray->Fill(tofHit->tray(), tofHit->leadingEdgeTime());
	hBTofHitLETvsTof->Fill(tofHit->leadingEdgeTime()-19, pTrk->btofPidTraits().timeOfFlight());
      }
    }
#if 0    
    TClonesArray* MCTracks = StMuDst::mcArray(1);
    for (int iMCTrk = 0; iMCTrk < MCTracks->GetSize(); ++iMCTrk) {
      StMuMcTrack* mcTrk = (StMuMcTrack*)MCTracks->UncheckedAt(iMCTrk);
      // StMuMcTrack* mcTrk = mudst-><MCtrack(iMCTrk);
      if (!mcTrk) continue;
      // cout << Form("%15.6f%15.6f%15.6f", mcTrk.pT(), mcTrk.Pxyz().x(), mcTrk.Pxyz().y()) << endl;
      Int_t IdVx = mcTrk->IdVx();
      if (IdVx != 1) continue; // original vertex 
      if (TMath::Abs(mcTrk->Eta()) > 1) continue;
      if (!mcTrk->Charge()) continue;
      hMcPt->Fill(mcTrk->pT());
    }
#endif    
  } // event loop end
  
  cout << hRcPt->GetEntries() << "\t" << hMcPt->GetEntries() << endl;
  
  hEffPt->Divide(hRcPt, hMcPt);
  
  TCanvas* c1 = new TCanvas("c1", "", 700, 600);
  c1->DrawFrame(0, 0, 5, 1);
  hEffPt->Draw("same");
  
  hOneOverBetaP->Draw("colz");
}
