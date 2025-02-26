// ROOT headers
#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"

// C++ headers
#include <iostream>

// Macro name
void checkStRefMultCorrOO(const char *inFileName,
                          const char *oFileName = "oTest.root") {
  const bool bUseRefMult6 = false; // use refMult6 for centrality definition, otherwise use totnMIP
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StPicoEvent");
  gSystem->Load("StRefMultCorr");

  std::cout << "Hi! Lets do some physics, Master!" << std::endl;
  
  StPicoDstReader* picoReader = new StPicoDstReader(inFileName);
  picoReader->Init();

  //Long64_t events2read = picoReader->chain()->GetEntries();
  
  // This is a way if you want to spead up IO
  std::cout << "Explicit read status for some branches" << std::endl;
  picoReader->SetStatus("*",0);
  picoReader->SetStatus("Event",1);
  picoReader->SetStatus("Track*",1);
  picoReader->SetStatus("BTofPidTraits",1);
  picoReader->SetStatus("EpdHit*",1);
  std::cout << "Status has been set" << std::endl;

  std::cout << "Now I know what to read, Master!" << std::endl;

  if( !picoReader->chain() ) {
    std::cout << "No chain has been found." << std::endl;
    std::cout << "Terminating..." << std::endl;
    exit(0);
  }
  Long64_t eventsInTree = picoReader->tree()->GetEntries();
  std::cout << "eventsInTree: "  << eventsInTree << std::endl;
  Long64_t events2read = picoReader->chain()->GetEntries();

  std::cout << "Number of events to read: " << events2read << std::endl;

  // Histogramming
  // Event
  TH1F *hRefMult = new TH1F(Form("h%s",(bUseRefMult6)?"RefMult6":"TotnMIP"), Form("Reference multiplicity;%s",(bUseRefMult6)?"refMult6":"totnMIP"), 1000, -0.5, 999.5);
  TH2F *hTofMatchVsRefMult6BeforeCut = new TH2F("hTofMatchVsRefMult6BeforeCut", ";bTofMatched;refMult6",
                                               500, -0.5, 499.5, 500, -0.5, 499.5);
  TH2F *hTofMatchVsRefMult6AfterCut = new TH2F("hTofMatchVsRefMult6AfterCut", ";bTofMatched;refMult6",
                                               500, -0.5, 499.5, 500, -0.5, 499.5);
  TH2F *hTofMatchVsTotnMIPBeforeCut = new TH2F("hTofMatchVsTotnMIPBeforeCut", ";bTofMatched;totnMIP",
                                               500, -0.5, 499.5, 1000, -0.5, 999.5);
  TH2F *hTofMatchVsTotnMIPAfterCut = new TH2F("hTofMatchVsTotnMIPAfterCut", ";bTofMatched;totnMIP",
                                               500, -0.5, 499.5, 1000, -0.5, 999.5);
  TH2F *hWeightVsRefMultCorr = new TH2F(Form("hWeightVs%sCorr",(bUseRefMult6)?"RefMult6":"TotnMIP"),Form(";%s;weight",(bUseRefMult6)?"refMult6":"totnMIP"),
                                        1000, -0.5, 999.5, 125, 0.5, 3.);
  TH1F *hRefMultCorr = new TH1F(Form("h%sCorr",(bUseRefMult6)?"RefMult6":"TotnMIP"), Form("Corrected reference multiplicity;%s",(bUseRefMult6)?"refMult6":"totnMIP"), 1000, -0.5, 999.5);
  TH1F *hCent16 = new TH1F("hCent16","Centrality bins;Centrality bin", 16, -0.5, 15.5);
  TH2F *hVtxXvsY = new TH2F("hVtxXvsY", "hVtxXvsY", 200,-10.,10.,200,-10.,10.);
  TH1F *hVtxZ = new TH1F("hVtxZ","hVtxZ", 210, -210., 210.);


  // Initialize StRefMultCorr
  StRefMultCorr *mRefMultCorrUtil = NULL;
  if (bUseRefMult6) mRefMultCorrUtil = new StRefMultCorr("refmult6"); // refmult6-based centrality
  else              mRefMultCorrUtil = new StRefMultCorr("totnmip"); //  totnMIP-based centrality
  mRefMultCorrUtil->setVerbose(kFALSE);

  Int_t counter = 0;
  Int_t loopSize = 1000;
  Bool_t verbose = kTRUE;

  // Loop over events
  for(Long64_t iEvent=0; iEvent<events2read; iEvent++) {

    counter++;
    if ( counter >= loopSize ) {
      std::cout << "Working on event #[" << (iEvent+1) << "/" << events2read << "]" << std::endl;
      counter = 0;
    }
    

    Bool_t readEvent = picoReader->readPicoEvent(iEvent);
    if (!readEvent) {
      std::cout << "No input was provided" << std::endl;
      break;
    }

    // Retrieve picoDst
    StPicoDst *dst = picoReader->picoDst();

    // Retrieve event information
    StPicoEvent *event = dst->event();
    if( !event ) {
      std::cout << "No event was found" << std::endl;
      break;
    }
    
    // Event vertex selection
    TVector3 pVtx = event->primaryVertex();
    if (fabs(pVtx.Z()) > 30.) continue;
    if (sqrt(pow(pVtx.X(),2.)+pow(pVtx.Y(),2.)) > 2.) continue;
    Int_t runId = event->runId();
    mRefMultCorrUtil->init(runId);

    if (verbose) {
      std::cout << "Checking bad run";
    }
    
    if ( mRefMultCorrUtil->isBadRun( runId ) ) {
      if (verbose) {
        std::cout << "\t[failed]" << std::endl;
      }
      continue;
    }
    if (verbose) {
      std::cout << "\t[passed]" << std::endl;
    }
    
    if (verbose) {
      std::cout << "Checking MB triggers";
    }
    
    // Check trigger (production_OO_200GeV_2021)
    if ( !event->isTrigger(860001) && !event->isTrigger(860002)) { 
      if (verbose) {
        std::cout << "\t[failed]" << std::endl;
      }
      continue;
    }
    if (verbose) {
      std::cout << "\t[passed]" << std::endl;
    }

    // NOTE: the refMult6 and totnMIP are defined as follows
    Int_t refMult6 = 0;
    for ( UInt_t iTrk=0; iTrk<dst->numberOfTracks(); iTrk++ ) {
      StPicoTrack* track = dst->track( iTrk );
      if ( !track ) continue;
      if ( !track->isPrimary() ) continue;
      if ( TMath::Abs( track->pMom().Eta() ) > 1.5 ) continue;
      if ( track->pPt() >= 2.0 ) continue;
      if ( track->pPt() <= 0.2 ) continue;
      if ( track->gDCA(event->primaryVertex()).Mag() >= 3. ) continue;
      if ( track->nHitsFit() <= 15 ) continue;
      if ( (Double_t)track->nHitsFit() / track->nHitsPoss() <= 0.52 ) continue;
      refMult6++;
    } // for ( UInt_t iTrk=0; iTrk<dst->numberOfTracks(); iTrk++ )
    Double_t totnMIP = 0.;
    Int_t nEpdHits = dst->numberOfEpdHits();
    for(Int_t iHit=0; iHit<nEpdHits; iHit++) {
      StPicoEpdHit *epdHit = dst->epdHit(iHit);
      if( !epdHit ) continue;
      if (epdHit->nMIP() > 6.) totnMIP += 6.;
      else if (epdHit->nMIP() < 0.3) continue;
      else totnMIP += epdHit->nMIP();
    } // for(Int_t iHit=0; iHit<nEpdHits; iHit++)

    Int_t nBTofMatched = event->nBTOFMatch();
    hTofMatchVsRefMult6BeforeCut->Fill( nBTofMatched, refMult6  );
    hTofMatchVsTotnMIPBeforeCut->Fill( nBTofMatched, totnMIP  );

    // IMPORTANT: both refmult6 and totnMIP are needed for d+Au and O+O Run21 pileup rejection
    if (mRefMultCorrUtil->isPileUpEvent( refMult6, nBTofMatched, pVtx.Z(), totnMIP ) ) continue;

    if (bUseRefMult6) mRefMultCorrUtil->initEvent(refMult6, pVtx.Z()); // refmult6-based centrality
    else              mRefMultCorrUtil->initEvent(totnMIP,  pVtx.Z()); //  totnMIP-based centrality

    // In case centrality calculation is failed for the given event it will
    if (mRefMultCorrUtil->getCentralityBin16() < 0 ||
        mRefMultCorrUtil->getCentralityBin9() < 0) {
      if (verbose) {
        std::cout << "\tBad centrality < 0" << std::endl;
      }
      continue;
    }

    Int_t cent9 = mRefMultCorrUtil->getCentralityBin9();       // 0: 70-80%, 1: 60-70%, ..., 6: 10-20%, 7: 5-10%, 8: 0-5%
    Int_t cent16 = mRefMultCorrUtil->getCentralityBin16();     // 0: 75-80%, 1: 70-75%, ..., 8: 0-5%
    Double_t refMultCorr = mRefMultCorrUtil->getRefMultCorr(); // Retrieve corrected refmult value
    Double_t weight = mRefMultCorrUtil->getWeight();           // Retrieve weight

    if (verbose) {
      std::cout << "refMult: " << Form("%d",(bUseRefMult6) ? refMult6 : totnMIP) << " refMultCorr: " << refMultCorr
                << " cent16: " << cent16 << " cent9: " << cent9
                << " Total weight: " << weight << " trigger efficiency: " 
                << mRefMultCorrUtil->triggerWeight()
                << " z: " << pVtx.Z()
                << " shape weight: " << mRefMultCorrUtil->getShapeWeight_SubVz2Center()
                << std::endl;
    }

    hRefMult->Fill( (bUseRefMult6) ? refMult6 : totnMIP );
    hRefMultCorr->Fill( refMultCorr );
    hCent16->Fill( cent16 );
    hVtxXvsY->Fill( pVtx.X(), pVtx.Y() );
    hVtxZ->Fill( pVtx.Z() );
    hTofMatchVsRefMult6AfterCut->Fill( nBTofMatched, refMult6 );
    hTofMatchVsTotnMIPAfterCut->Fill( nBTofMatched, totnMIP );
    hWeightVsRefMultCorr->Fill(refMultCorr, weight);

  } //for(Long64_t iEvent=0; iEvent<events2read; iEvent++)
  picoReader->Finish();

  TFile *oFile = new TFile(oFileName, "recreate");
  hRefMult->Write();
  hRefMultCorr->Write();
  hWeightVsRefMultCorr->Write();
  hCent16->Write();
  hVtxXvsY->Write();
  hVtxZ->Write();
  hTofMatchVsRefMult6BeforeCut->Write();
  hTofMatchVsRefMult6AfterCut->Write();
  hTofMatchVsTotnMIPBeforeCut->Write();
  hTofMatchVsTotnMIPAfterCut->Write();
  oFile->Write();
  oFile->Close();

  std::cout << "Analysis was finished" << std::endl;
}
