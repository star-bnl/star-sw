/**
 * \brief Example of how to read a file (list of files) using StPicoEvent class in a standalone mode
 *
 * picoAnalyzer.C is an example of reading STAR picoDst format in a standalone mode
 * on your laptop or local computer farm.
 * Prerequisites:
 * - StPicoEvent directory with the library (libStPicoDst.so) compiled
 * - CERN ROOT package
 * - g++ >= 4.8
 * - Makefile
 *
 * First, the program must be compiled with the Makefile, with simple command in the bash shell:
 * make
 *
 * Then the executable file picoAnalyzerStandalone will be created. The current version of the program
 * expects 3 arguments: ./picoAnalyzerStandalone inputFile outputFile
 * The first one is the program name, the second one is the name of the inputfile that 
 * maybe either the picoDst file itself, in a format dummyname.picoDst.root or a list of
 * such files called dummyname.list or dummyname.lis. The outputFile assumes the some_output_name.root.
 *
 * \author Grigory Nigmatkulov
 * \date August 6, 2019
 * \email nigmatkulov@gmail.com
 */

// C++ headers
#include <iostream>

// ROOT headers
#include "TROOT.h"
#include "TFile.h"
#include "TChain.h"
#include "TTree.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"

// PicoDst headers
#include "StPicoDstReader.h"
#include "StPicoDst.h"
#include "StPicoEvent.h"
#include "StPicoTrack.h"
#include "StPicoBTofHit.h"
#include "StPicoBTowHit.h"
#include "StPicoEmcTrigger.h"
#include "StPicoBTofPidTraits.h"
#include "StPicoTrackCovMatrix.h"
#include "StPicoFmsHit.h"
#include "StPicoETofHit.h"
#include "StPicoEpdHit.h"

//_________________
int main(int argc, char* argv[]) {

#if ROOT_VERSION_CODE >= ROOT_VERSION(6,0,0)
  R__LOAD_LIBRARY(libStPicoDst);
#else
  gSystem->Load("../libs/libStPicoDst.so");
#endif

  std::cout << "Hi! Lets do some physics, Master!" << std::endl;

  const char* fileName;
  const char* oFileName;

  switch (argc) {
  case 3:
    fileName = argv[1];
    oFileName = argv[2];
    break;
  default:
    std::cout << "Usage: picoAnalyzerStandalone inputFileName outputFileName.root" << std::endl;
    return -1;
  }
  std::cout << " inputFileName : " << fileName << std::endl;
  std::cout << " outputFileName: " << oFileName << std::endl;
  
  StPicoDstReader* picoReader = new StPicoDstReader(fileName);
  picoReader->Init();

  // This is a way if you want to spead up I/O
  std::cout << "Explicit read status for some branches" << std::endl;
  picoReader->SetStatus("*",0);
  picoReader->SetStatus("Event*", 1);
  picoReader->SetStatus("Track*", 1);
  picoReader->SetStatus("BTofHit*", 1);
  picoReader->SetStatus("BTofPidTraits*", 1);
  picoReader->SetStatus("BTowHit*", 1);
  picoReader->SetStatus("ETofHit*", 1);
  picoReader->SetStatus("EpdHit*", 1);
  //picoReader->SetStatus("EmcTrigger",0);
  //picoReader->SetStatus("TrackCovMatrix",1);
  std::cout << "Status has been set" << std::endl;

  std::cout << "Now I know what to read, Master!" << std::endl;

  if( !picoReader->chain() ) {
    std::cout << "No chain has been found." << std::endl;
  }
  Long64_t eventsInTree = picoReader->tree()->GetEntries();
  std::cout << "eventsInTree: "  << eventsInTree << std::endl;
  Long64_t events2read = picoReader->chain()->GetEntries();

  std::cout << "Number of events to read: " << events2read
	    << std::endl;


  TFile *oFile = new TFile(oFileName, "recreate");
  
  // Histogramming
  // Event
  TH1F *hRefMult = new TH1F("hRefMult",
			    "Reference multiplicity;refMult",
			    500, -0.5, 499.5);
  TH2F *hVtxXvsY = new TH2F("hVtxXvsY",
			    "hVtxXvsY",
			    200,-10.,10.,200,-10.,10.);
  TH1F *hVtxZ = new TH1F("hVtxZ","hVtxZ",
			 140, -70., 70.);

  // Track
  TH1F *hGlobalPtot = new TH1F("hGlobalPtot",
			       "Global track momentum;p (GeV/c)",
			       100, 0., 1. );
  TH1F *hGlobalPtotCut = new TH1F("hGlobalPtotCut",
				  "Global track momentum after cut;p (GeV/c)",
				  100, 0., 1. );
  TH1F *hPrimaryPtot = new TH1F("hPrimaryPtot",
				"Primary track momentum;p (GeV/c)",
			       100, 0., 1. );
  TH1F *hPrimaryPtotCut = new TH1F("hPrimaryPtotCut",
				   "Primary track momentum after cut;p (GeV/c)",
				  100, 0., 1. );
  TH1F *hTransvMomentum = new TH1F("hTransvMomentum",
				   "Track transverse momentum;p_{T} (GeV/c)",
				   200, 0., 2.);
  TH2F *hGlobalPhiVsPt[2];
  for(int i=0; i<2; i++) {
    hGlobalPhiVsPt[i] = new TH2F(Form("hGlobalPhiVsPt_%d",i),
				 Form("#phi vs. p_{T} for charge: %d;p_{T} (GeV/c);#phi (rad)", (i==0) ? 1 : -1),
				 300, 0., 3.,
				 630, -3.15, 3.15);
  }
  TH1F *hNSigmaPion = new TH1F("hNSigmaPion",
			       "n#sigma(#pi);n#sigma(#pi)",
			       400, -10., 10.);
  TH1F *hNSigmaElectron = new TH1F("hNSigmaElectron",
				   "n#sigma(e);n#sigma(e)",
				   400,-10.,10.);
  TH1F *hNSigmaKaon = new TH1F("hNSigmaKaon",
			       "n#sigma(K);n#sigma(K)",
			       400, -10., 10.);
  TH1F *hNSigmaProton = new TH1F("hNSigmaProton",
				 "n#sigma(p);n#sigma(p)",
				 400, -10., 10.);
    
  // BTof pid traits
  TH1F *hTofBeta = new TH1F("hTofBeta", "BTofPidTraits #beta;#beta",
			    2000, 0., 2.);

  // BTOF hit
  TH1F *hBTofTrayHit = new TH1F("hBTofTrayHit","BTof tray number with the hit",
				120, -0.5, 119.5);

  // BTOW hit
  TH1F *hBTowAdc = new TH1F("hBTowAdc","Barrel tower ADC;ADC",500,0.,500);

  // FMS hit
  TH1F *hFmsAdc = new TH1F("hFmsAdc","ADC in FMS modules;ADC",1000, 0.,5000);

  // ETOF hit
  TH1F *hETofToT = new TH1F("hETofToT","eTOF TOT;Time over threshold (ns)",300, 0.,150);

  // EPD hit
  TH1F *hEpdAdc = new TH1F("hEpdAdc","ADC in EPD;ADC",4095, 0., 4095);


  // Loop over events
  for(Long64_t iEvent=0; iEvent<events2read; iEvent++) {

    std::cout << "Working on event #[" << (iEvent+1)
	      << "/" << events2read << "]" << std::endl;

    Bool_t readEvent = picoReader->readPicoEvent(iEvent);
    if( !readEvent ) {
      std::cout << "Something went wrong, Master! Nothing to analyze..."
		<< std::endl;
      break;
    }

    // Retrieve picoDst
    StPicoDst *dst = picoReader->picoDst();

    // Retrieve event information
    StPicoEvent *event = dst->event();
    if( !event ) {
      std::cout << "Something went wrong, Master! Event is hiding from me..."
		<< std::endl;
      break;
    }
    hRefMult->Fill( event->refMult() );

    TVector3 pVtx = event->primaryVertex();
    hVtxXvsY->Fill( event->primaryVertex().X(), event->primaryVertex().Y() );
    hVtxZ->Fill( event->primaryVertex().Z() );

    // Track analysis
    Int_t nTracks = dst->numberOfTracks();
    Int_t nMatrices = dst->numberOfTrackCovMatrices();
    if(nTracks != nMatrices) {
      //std::cout << "Number of tracks and matrices do not match!" << std::endl;
    }
    //std::cout << "Number of tracks in event: " << nTracks << std::endl;
    
    // Track loop
    for(Int_t iTrk=0; iTrk<nTracks; iTrk++) {

      // Retrieve i-th pico track
      StPicoTrack *picoTrack = dst->track(iTrk);
      
      if(!picoTrack) continue;
      //std::cout << "Track #[" << (iTrk+1) << "/" << nTracks << "]"  << std::endl;

      hGlobalPtot->Fill( picoTrack->gMom().Mag() );
      if( picoTrack->isPrimary() ) {
	hPrimaryPtot->Fill( picoTrack->pMom().Mag() );
      }
      
      // Simple single-track cut
      if( picoTrack->gMom().Mag() < 0.1 ||
	  picoTrack->gDCA(pVtx).Mag()>50. ) {
	continue;
      } 

      hGlobalPtotCut->Fill( picoTrack->gMom().Mag() );
      if( picoTrack->isPrimary() ) {
	hPrimaryPtotCut->Fill( picoTrack->pMom().Mag() );
      }
      if( picoTrack->charge() > 0 ) {
	hGlobalPhiVsPt[0]->Fill( picoTrack->gMom().Pt(),
				 picoTrack->gMom().Phi() );
      }
      else {
	hGlobalPhiVsPt[1]->Fill( picoTrack->gMom().Pt(),
				 picoTrack->gMom().Phi() );	
      }
      hNSigmaElectron->Fill( picoTrack->nSigmaElectron() );
      hNSigmaPion->Fill( picoTrack->nSigmaPion() );
      hNSigmaKaon->Fill( picoTrack->nSigmaKaon() );
      hNSigmaProton->Fill( picoTrack->nSigmaProton() );
      
      hTransvMomentum->Fill( picoTrack->gMom().Pt() );

      // Check if track has TOF signal
      if( picoTrack->isTofTrack() ) {
	// Retrieve corresponding trait
	StPicoBTofPidTraits *trait = dst->btofPidTraits( picoTrack->bTofPidTraitsIndex() );
	if( !trait ) {
	  std::cout << "O-oh... No BTofPidTrait # " << picoTrack->bTofPidTraitsIndex()
		    << " for track # " << iTrk << std::endl;
	  std::cout << "Check that you turned on the branch!" << std::endl;
	  continue;
	}
	// Fill beta
	hTofBeta->Fill( trait->btofBeta() );
      } //if( isTofTrack() )
      
    } //for(Int_t iTrk=0; iTrk<nTracks; iTrk++)

    //////////////////
    // Hit analysis //
    //////////////////

    // BTOF hits
    Int_t nBTofHits = dst->numberOfBTofHits();
    //std::cout << "Number of btofHits in event: " << nBTofHits << std::endl;
    for(Int_t iHit=0; iHit<nBTofHits; iHit++) {
      StPicoBTofHit *btofHit = dst->btofHit(iHit);
      if( !btofHit ) continue;
      //std::cout << "BTofHit #[" << (iHit+1) << "/" << nBTofHits << "]"  << std::endl;
      hBTofTrayHit->Fill( btofHit->tray() );
    } //for(Int_t iHit=0; iHit<nBTofHits; iHit++)

    // BTOW hits
    Int_t nBTowHits = dst->numberOfBTowHits();
    for(Int_t iHit=0; iHit<nBTowHits; iHit++) {
      StPicoBTowHit *btowHit = dst->btowHit(iHit);
      if( !btowHit ) continue;
      //std::cout << "BTowHit #[" << (iHit+1) << "/" << nBTowHits << "]"  << std::endl;
      hBTowAdc->Fill( btowHit->adc() );
    }

    // FMS hits
    Int_t nFmsHits = dst->numberOfFmsHits();
    for(Int_t iHit=0; iHit<nFmsHits; iHit++) {
      StPicoFmsHit *fmsHit = dst->fmsHit(iHit);
      if( !fmsHit ) continue;
      //std::cout << "FmsHit #[" << (iHit+1) << "/" << nFmsHits << "]"  << std::endl;
      hFmsAdc->Fill( fmsHit->adc() );
    }

    // ETOF hits
    Int_t nETofHits = dst->numberOfETofHits();
    for(Int_t iHit=0; iHit<nETofHits; iHit++) {
      StPicoETofHit *etofHit = dst->etofHit(iHit);
      if( !etofHit ) continue;
      //std::cout << "ETofHit #[" << (iHit+1) << "/" << nETofHits << "]"  << std::endl;
      hETofToT->Fill( etofHit->timeOverThreshold() );
    }
    
    // EPD hits
    Int_t nEpdHits = dst->numberOfEpdHits();
    for(Int_t iHit=0; iHit<nEpdHits; iHit++) {
      StPicoEpdHit *epdHit = dst->epdHit(iHit);
      if( !epdHit ) continue;
      //std::cout << "EpdHit #[" << (iHit+1) << "/" << nEpdHits << "]"  << std::endl;
      hEpdAdc->Fill( epdHit->adc() );
    }

  } //for(Long64_t iEvent=0; iEvent<events2read; iEvent++)

  picoReader->Finish();
  oFile->Write();
  oFile->Close();

  std::cout << "I'm done with analysis. We'll have a Nobel Prize, Master!"
	    << std::endl;
}
