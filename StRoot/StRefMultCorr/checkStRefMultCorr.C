// ROOT headers
#include "TH1.h"
#include "TChain.h"
#include "TSystem.h"
#include "TFile.h"

// C++ headers
#include <iostream>

// Macro name
void checkStRefMultCorr(const char *inFileName = "/star/u/gnigmat/soft/u/prithwish/data/st_physics_adc_16064082_raw_5000007.MuDst.root",
                        const char *oFileName = "oFileForPrithwish.root") {

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
  picoReader->SetStatus("BTofPidTraits",1);
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
  TH1F *hRefMult = new TH1F("hRefMult", "Reference multiplicity;refMult", 500, -0.5, 499.5);
  TH2F *hTofMatchVsRefMultBeforeCut = new TH2F("hTofMatchVsRefMultBeforeCut", ";bTofMatched;refMult",
                                               500, -0.5, 499.5, 500, -0.5, 499.5);
  TH2F *hTofMatchVsRefMultAfterCut = new TH2F("hTofMatchVsRefMultAfterCut", ";bTofMatched;refMult",
                                               500, -0.5, 499.5, 500, -0.5, 499.5);
  TH2F *hWeightVsRefMultCorr = new TH2F("hWeightVsRefMultCorr",";refMult;weight",
                                        500, -0.5, 499.5, 125, 0.5, 3.);
  TH1F *hRefMultCorr = new TH1F("hRefMultCorr", "Corrected reference multiplicity;refMult", 500, -0.5, 499.5);
  TH1F *hCent16 = new TH1F("hCent16","Centrality bins;Centrality bin", 16, -0.5, 15.5);
  TH2F *hVtxXvsY = new TH2F("hVtxXvsY", "hVtxXvsY", 200,-10.,10.,200,-10.,10.);
  TH1F *hVtxZ = new TH1F("hVtxZ","hVtxZ", 210, -210., 210.);


  // Initialize StRefMultCorr
  StRefMultCorr *mRefMultCorrUtilFxt = new StRefMultCorr("fxtmult");
  mRefMultCorrUtilFxt->setVerbose(kFALSE);

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

    Int_t runId = event->runId();
    mRefMultCorrUtilFxt->init(runId);

    if (verbose) {
      std::cout << "Checking bad run";
    }
    
    if ( mRefMultCorrUtilFxt->isBadRun( runId ) ) {
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

    // Check trigger (3.2 GeV)
    if ( !event->isTrigger(680001) ) { 
      if (verbose) {
        std::cout << "\t[failed]" << std::endl;
      }
      continue;
    }
    if (verbose) {
      std::cout << "\t[passed]" << std::endl;
    }

    // NOTE: Lets utilize refMult variable instead of fxtMult but will call the proper one from picoEvent
    Int_t refMult = event->fxtMult();
    TVector3 pVtx = event->primaryVertex();
    Int_t nBTofMatched = event->nBTOFMatch();
    hTofMatchVsRefMultBeforeCut->Fill( nBTofMatched, refMult  );

    // IMPORTANT: vertex position is needed for Au+Au 19.6 GeV 2019
    if (mRefMultCorrUtilFxt->isPileUpEvent( refMult, nBTofMatched, pVtx.Z() ) ) continue;

    mRefMultCorrUtilFxt->initEvent(refMult, pVtx.Z(), event->ZDCx());

    // In case centrality calculation is failed for the given event it will
    if (mRefMultCorrUtilFxt->getCentralityBin16() < 0 ||
        mRefMultCorrUtilFxt->getCentralityBin9() < 0) {
      if (verbose) {
        std::cout << "\tBad centrality < 0" << std::endl;
      }
      continue;
    }

    Int_t cent9 = mRefMultCorrUtilFxt->getCentralityBin9();       // 0: 70-80%, 1: 60-70%, ..., 6: 10-20%, 7: 5-10%, 8: 0-5%
    Int_t cent16 = mRefMultCorrUtilFxt->getCentralityBin16();     // 0: 75-80%, 1: 70-75%, ..., 8: 0-5%
    Double_t refMultCorr = mRefMultCorrUtilFxt->getRefMultCorr(); // Retrieve corrected refMult value
    Double_t weight = mRefMultCorrUtilFxt->getWeight();           // Retrieve weight

    if (verbose) {
      std::cout << "refMult: " << refMult << " refMultCorr: " << refMultCorr
                << " cent16: " << cent16 << " cent9: " << cent9
                << " Total weight: " << weight << " trigger efficiency: " 
                << mRefMultCorrUtilFxt->triggerWeight()
                << " z: " << pVtx.Z()
                << " shape weight: " << mRefMultCorrUtilFxt->getShapeWeight_SubVz2Center()
                << std::endl;
    }

    hRefMult->Fill( refMult );
    hRefMultCorr->Fill( refMultCorr );
    hCent16->Fill( cent16 );
    hVtxXvsY->Fill( pVtx.X(), pVtx.Y() );
    hVtxZ->Fill( pVtx.Z() );
    hTofMatchVsRefMultAfterCut->Fill( nBTofMatched, refMult );
    hWeightVsRefMultCorr->Fill(refMultCorr, weight);

  } //for(Long64_t iEvent=0; iEvent<events2read; iEvent++)
  picoReader->Finish();

  TFile *oFile = new TFile(oFileName, "recreate");
  hRefMult->Write();
  hRefMultCorr->Write();
  hCent16->Write();
  hVtxXvsY->Write();
  hVtxZ->Write();
  hTofMatchVsRefMultBeforeCut->Write();
  hTofMatchVsRefMultAfterCut->Write();
  hWeightVsRefMultCorr->Write();
  oFile->Write();
  oFile->Close();

  std::cout << "Analysis was finished" << std::endl;
}
