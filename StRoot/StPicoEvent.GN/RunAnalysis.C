/*
 * Author: Grigory Nigmatkulov
 * Date: May 29, 2018
 *
 * RunAnalysis.C is an example of reading STAR picoDst format.
 * One can use either picoDst file or a list of picoDst files 
 * as an input, and preform physics analysis.
 *
 **/

#define _VANILLA_ROOT_

/// C++ headers
#include <iostream>

/// PicoDst headers
#include "StPicoDstReader.h"
#include "StPicoDst.h"
#include "StPicoEvent.h"
#include "StPicoTrack.h"
#include "StPicoTrackCovMatrix.h"

/// ROOT headers
#include "TFile.h"
#include "TChain.h"
#include "TSystem.h"
#include "TH1.h"
#include "TMath.h"

R__LOAD_LIBRARY(libStPicoDst)

//_________________
void RunAnalysis(const Char_t *inFile = "st_physics_12126101_raw_3040006.picoDst.root") {

  //gSystem->Load("libStPicoDst.so");

  std::cout << "Hi! Lets do some physics, Master!" << std::endl;

  StPicoDstReader* picoReader = new StPicoDstReader(inFile);
  picoReader->Init();
  
  /// This is a way if you want to spead up IO
  std::cout << "Explicit read status for some branches" << std::endl;
  picoReader->SetStatus("*",0);
  picoReader->SetStatus("Event",1);
  picoReader->SetStatus("Track",1);
  picoReader->SetStatus("BTofHit",1);
  picoReader->SetStatus("TrackCovMatrix",1);
  std::cout << "Status has been set" << std::endl;

  std::cout << "Now I know what to read, Master!" << std::endl;
  
  //Long64_t events2read = 10;
  Long64_t events2read = picoReader->chain()->GetEntriesFast();

  std::cout << "Number of events to read: " << events2read
	    << std::endl;

  /// Histogramming
  TH1F *hRefMult = new TH1F("hRefMult","Reference multiplicity;refMult",
			    500, -0.5, 499.5);
  TH1F *hTransvMomentum = new TH1F("hTransvMomentum",
				   "Track transverse momentum;p_{T} (GeV/c)",
				   200, 0., 2.);
  TH1F *hBTofTrayHit = new TH1F("hBTofTrayHit","BTof tray number with the hit",
				120, -0.5, 119.5);
  TH1F *hTrackCovMtxZ = new TH1F("hTrackCovMtxZ","z of covariance matrix",
				 200, -10., 10.);
  
  /// Loop over events
  for(Long64_t iEvent=0; iEvent<events2read; iEvent++) {

    std::cout << "Working on event #[" << (iEvent+1)
	      << "/" << events2read << "]" << std::endl;

    Bool_t readEvent = picoReader->ReadPicoEvent(iEvent);
    if( !readEvent ) {
      std::cout << "Something went wrong, Master! Nothing to analyze..."
		<< std::endl;
      break;
    }

    /// Retrieve picoDst
    StPicoDst *dst = picoReader->picoDst();

    /// Retrieve event information
    StPicoEvent *event = dst->event();
    if( !event ) {
      std::cout << "Something went wrong, Master! Event is hiding from me..."
		<< std::endl;
      break;
    }
    hRefMult->Fill( event->refMult() );

    /// Track analysis
    Int_t nTracks = dst->numberOfTracks();
    //std::cout << "Number of tracks in event: " << nTracks << std::endl;
    
    /// Track loop
    for(Int_t iTrk=0; iTrk<nTracks; iTrk++) {
      
      StPicoTrack *picoTrack = dst->track(iTrk);
      if(!picoTrack) continue;
      //std::cout << "Track #[" << (iTrk+1) << "/" << nTracks << "]"  << std::endl;

      /// Single-track cut example
      if( !picoTrack->isPrimary() ||
	  picoTrack->nHits() < 15 ||
	  TMath::Abs( picoTrack->gMom().pseudoRapidity() ) > 0.5 ) {
	continue;
      } //for(Int_t iTrk=0; iTrk<nTracks; iTrk++)
      
      hTransvMomentum->Fill( picoTrack->gPt() );
    }

    /// Hit analysis
    Int_t nBTofHits = dst->numberOfBTofHits();
    
    //std::cout << "Number of btofHits in event: " << nBTofHits << std::endl;

    /// Hit loop
    for(Int_t iHit=0; iHit<nBTofHits; iHit++) {

      StPicoBTofHit *btofHit = dst->btofHit(iHit);
      if( !btofHit ) continue;
      hBTofTrayHit->Fill( btofHit->tray() );
    }

    /// Covariance matrix loop
    Int_t nCovMtx = dst->numberOfTrackCovMatrices();
    std::cout << "Number of covariance matrices in event: " << nCovMtx << std::endl;
    for(Int_t iMtx=0; iMtx<nCovMtx; iMtx++) {
      StPicoTrackCovMatrix *covMtx = dst->trackCovMatrix(iMtx);
      if( !covMtx ) continue;
      hTrackCovMtxZ->Fill( covMtx->z() );
    }
  } //for(Long64_t iEvent=0; iEvent<events2read; iEvent++)

  picoReader->Finish();

  std::cout << "I'm done with analysis. We'll have a Nobel Prize, Master!" << std::endl;
}
