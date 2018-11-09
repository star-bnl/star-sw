/**
 * \brief Example of how to read a file (list of files) using StPicoEvent classes
 *
 * RunPicoDstAnalyzer.C is an example of reading STAR picoDst format.
 * One can use either picoDst file or a list of picoDst files (inFile.lis or
 * inFile.list) as an input, and preform physics analysis
 *
 * \author Grigory Nigmatkulov
 * \date May 29, 2018
 */

// This is needed for calling standalone classes (not needed on RACF)
#define _VANILLA_ROOT_

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
#include "../StPicoDstReader.h"
#include "../StPicoDst.h"
#include "../StPicoEvent.h"
#include "../StPicoTrack.h"
#include "../StPicoBTofHit.h"
#include "../StPicoBTowHit.h"
#include "../StPicoEmcTrigger.h"
#include "../StPicoBTofPidTraits.h"
#include "../StPicoTrackCovMatrix.h"

// Load libraries (for ROOT_VERSTION_CODE >= 393215)
#if ROOT_VERSION_CODE >= ROOT_VERSION(6,0,0) 
R__LOAD_LIBRARY(../libStPicoDst)
#endif

// inFile - is a name of name.picoDst.root file or a name
//          of a name.lis(t) files that contains a list of
//          name1.picoDst.root files

//_________________
void PicoDstAnalyzer(const Char_t *inFile = "../files/st_physics_12126101_raw_3040006.picoDst.root") {

  std::cout << "Hi! Lets do some physics, Master!" << std::endl;
  
  StPicoDstReader* picoReader = new StPicoDstReader(inFile);
  picoReader->Init();

  //Long64_t events2read = picoReader->chain()->GetEntries();
  
  // This is a way if you want to spead up IO
  std::cout << "Explicit read status for some branches" << std::endl;
  picoReader->SetStatus("*",0);
  picoReader->SetStatus("Event",1);
  picoReader->SetStatus("Track",1);
  picoReader->SetStatus("BTofHit",1);
  picoReader->SetStatus("BTofPidTraits",1);
  //picoReader->SetStatus("BTowHit",0);
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
    
  // TofPidTrait
  TH1F *hTofBeta = new TH1F("hTofBeta",
			    "BTofPidTraits #beta;#beta",
			    2000, 0., 2.);

  // TofHit
  TH1F *hBTofTrayHit = new TH1F("hBTofTrayHit","BTof tray number with the hit",
				120, -0.5, 119.5);


  // Loop over events
  for(Long64_t iEvent=0; iEvent<events2read; iEvent++) {

    std::cout << "Working on event #[" << (iEvent+1)
	      << "/" << events2read << "]" << std::endl;

    Bool_t readEvent = picoReader->ReadPicoEvent(iEvent);
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

    // Hit analysis
    Int_t nBTofHits = dst->numberOfBTofHits();
    //std::cout << "Number of btofHits in event: " << nBTofHits << std::endl;

    // Hit loop
    for(Int_t iHit=0; iHit<nBTofHits; iHit++) {

      StPicoBTofHit *btofHit = dst->btofHit(iHit);
      if( !btofHit ) continue;
      hBTofTrayHit->Fill( btofHit->tray() );
    } //for(Int_t iHit=0; iHit<nBTofHits; iHit++)



  } //for(Long64_t iEvent=0; iEvent<events2read; iEvent++)

  picoReader->Finish();

  std::cout << "I'm done with analysis. We'll have a Nobel Prize, Master!"
	    << std::endl;
}
