/**
 * Author: Grigory Nigmatkulov
 * Date: May 29, 2018
 *
 * RunPicoDstAnalyzer.C is an example of reading STAR picoDst format.
 * One can use either picoDst file or a list of picoDst files (inFile.lis or
 * inFile.list) as an input, and preform physics analysis.
 *
 **/

/// This is needed for calling standalone classes (not needed on RACF)
#define _VANILLA_ROOT_

/// C++ headers
#include <iostream>

/// ROOT headers
#include <TROOT.h>
#include <TFile.h>
#include <TChain.h>
#include <TTree.h>
#include <TSystem.h>
#include <TH1.h>
#include <TH2.h>
#include <TMath.h>

/// PicoDst headers
#include "../StPicoDstReader.h"
#include "../StPicoDst.h"
#include "../StPicoEvent.h"
#include "../StPicoTrack.h"
#include "../StPicoBTofHit.h"
#include "../StPicoBTowHit.h"
#include "../StPicoTrackCovMatrix.h"

/// Load libraries (for ROOT_VERSTION_CODE >= 393215)
#if ROOT_VERSION_CODE >= ROOT_VERSION(6,0,0) 
R__LOAD_LIBRARY(../libStPicoDst)
#endif

/// inFile - is a name of name.picoDst.root file or a name
///          of a name.lis(t) files that contains a list of
///          name1.picoDst.root files

//_________________
void PicoDstAnalyzer(const Char_t *inFile = "../files/st_physics_12126101_raw_3040006.picoDst.root") {

  std::cout << "Hi! Lets do some physics, Master!" << std::endl;
  
  StPicoDstReader* picoReader = new StPicoDstReader(inFile);
  picoReader->Init();

  //Long64_t events2read = picoReader->chain()->GetEntries();
  
  /// This is a way if you want to spead up IO
  std::cout << "Explicit read status for some branches" << std::endl;
  picoReader->SetStatus("*",0);
  picoReader->SetStatus("Event",1);
  picoReader->SetStatus("Track",1);
  picoReader->SetStatus("BTofHit",1);
  //picoReader->SetStatus("BTowHit",1);
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

  /// Histogramming
  TH1F *hRefMult = new TH1F("hRefMult","Reference multiplicity;refMult",
			    500, -0.5, 499.5);
  TH1F *hTransvMomentum = new TH1F("hTransvMomentum",
				   "Track transverse momentum;p_{T} (GeV/c)",
				   200, 0., 2.);
  TH1F *hBTofTrayHit = new TH1F("hBTofTrayHit","BTof tray number with the hit",
				120, -0.5, 119.5);
  
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
    Int_t nMatrices = dst->numberOfTrackCovMatrices();
    if(nTracks != nMatrices) {
      std::cout << "Number of tracks and matrices do not match!" << std::endl;
    }
    //std::cout << "Number of tracks in event: " << nTracks << std::endl;
    
    /// Track loop
    for(Int_t iTrk=0; iTrk<nTracks; iTrk++) {

      /// Retrieve i-th pico track
      StPicoTrack *picoTrack = dst->track(iTrk);
      
      if(!picoTrack) continue;
      //std::cout << "Track #[" << (iTrk+1) << "/" << nTracks << "]"  << std::endl;

      /// Single-track cut example
      if( !picoTrack->isPrimary() ||
	  picoTrack->nHits() < 15 ||
	  TMath::Abs( picoTrack->gMom().PseudoRapidity() ) > 0.5 ) {
	continue;
      } 
      
      hTransvMomentum->Fill( picoTrack->gMom().Pt() );
    } //for(Int_t iTrk=0; iTrk<nTracks; iTrk++)

    /// Hit analysis
    Int_t nBTofHits = dst->numberOfBTofHits();
    
    //std::cout << "Number of btofHits in event: " << nBTofHits << std::endl;

    /// Hit loop
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
