// $Id: StRareMaker.cxx,v 1.5 2001/10/16 01:26:14 struck Exp $
// $Log: StRareMaker.cxx,v $
// Revision 1.5  2001/10/16 01:26:14  struck
// added filename parameter for tree file to constructors
//
// Revision 1.4  2001/10/15 20:20:27  struck
// first version with L3 included
//
// Revision 1.3  2001/09/06 20:51:23  hardtke
// Update
//
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StRareMaker
//
// Description: 
//  Make uDST for Rare Particle Search
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  David Hardtke, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StRareMaker.h"
#include "StRareEvent.h"
#include "StRareEventCut.h"
#include "StRareTrackCut.h"
#include "StL3RareTrackCut.h"
#include "StAcceptAllEvents.h"
#include "StAcceptAllTracks.h"
#include "StAcceptAllL3Tracks.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StarClassLibrary/StThreeVector.hh"

ClassImp(StRareEventCut)
ClassImp(StRareTrackCut)
ClassImp(StL3RareTrackCut)

static const char rcsid[] = "$Id: StRareMaker.cxx,v 1.5 2001/10/16 01:26:14 struck Exp $";

double dEdx_formula(double momentum, double mass);

ClassImp(StRareMaker)

StRareMaker::StRareMaker(const Char_t *name, Char_t* fileName) : StMaker(name) {
  mRareEvent = new StRareEvent();
  out = new TFile(fileName, "RECREATE");
  out->SetCompressionLevel(2);
  m_Tree = new TTree("RareTree", "RareTree");
  //  m_Tree->SetBranchStyle(0); 
  m_Tree->AutoSave();
  m_Tree->SetAutoSave(10000000);
  m_Tree->Branch("StRareEvent", "StRareEvent", &mRareEvent, 64000, 1);
  mEventCut = new StAcceptAllEvents(); 
  mTrackCut = new StAcceptAllTracks();
  mL3TrackCut = new StAcceptAllL3Tracks();
}

StRareMaker::StRareMaker(const Char_t *name, Char_t* fileName,
			 StRareEventCut* cut, StRareTrackCut* track) : StMaker(name) {
  out = new TFile(fileName, "RECREATE");
  m_Tree = new TTree("RareTree", "RareTree", 1000000);
  m_Tree->AutoSave();
  m_Tree->SetAutoSave(10000000);
  mRareEvent = new StRareEvent();  
  m_Tree->Branch("StRareEvent", "StRareEvent", &mRareEvent, 64000, 1);
  mEventCut = cut; 
  mTrackCut = track;
  mL3TrackCut = 0;
}

StRareMaker::StRareMaker(const Char_t *name, Char_t* fileName,
			 StRareEventCut* cut,
			 StRareTrackCut* trackCut,
			 StL3RareTrackCut* l3trackCut) : StMaker(name) {
  //out = new TFile("/direct/star+data01/pwg/spectra/struck/2001/RareEvent.root","RECREATE");
  out = new TFile(fileName, "RECREATE");
  m_Tree = new TTree("RareTree", "RareTree", 1000000);
  m_Tree->AutoSave();
  m_Tree->SetAutoSave(10000000);
  mRareEvent = new StRareEvent();
  m_Tree->Branch("StRareEvent", "StRareEvent", &mRareEvent, 64000, 1);
  mEventCut = cut; 
  mTrackCut = trackCut;
  mL3TrackCut = l3trackCut;
}

Int_t StRareMaker::Make() {
    //
    //	This method is called every event. That's the
    //  right place to plug in your analysis to be
    //  done every event.
    //
    StEvent* mEvent;
    mEvent = (StEvent *) GetInputDS("StEvent");
    if (!mEvent) return kStOK; // If no event, we're done

    // test
    // get event number
    cout << " event ID = " << mEvent->id() << endl;  

    mRareEvent->clear();

    if (mEventCut->Accept(mEvent)) {
          mRareEvent->fillRareEvent(mEvent);
	  StPrimaryTrackIterator itr;
	  StPrimaryTrack *trk;
	  if (mEvent->primaryVertex()) {
	        const StSPtrVecPrimaryTrack& tracks = mEvent->primaryVertex()->daughters();
		for (itr=tracks.begin(); itr != tracks.end(); itr++){
		      trk = *itr;
		      if (mTrackCut->Accept(trk)) mRareEvent->addTrack(trk);
		}
	  }

	  // now look for L3
	  StL3Trigger* l3Event;
	  l3Event = (StL3Trigger*) mEvent->l3Trigger();
	  if (mL3TrackCut && l3Event) {
	        mRareEvent->fillL3Info(l3Event);
		// Loop over tracks
		StGlobalTrack *l3trk;
		StSPtrVecTrackNode& mtracknodes = (StSPtrVecTrackNode&) l3Event->trackNodes();
		for (Int_t i=0; i<mtracknodes.size(); i++) {
		      l3trk = (StGlobalTrack* )mtracknodes[i]->track(0);
		      if (mL3TrackCut->Accept(l3trk)) mRareEvent->addL3Track(l3trk);
		}
	  }

	  m_Tree->Fill();
	  //m_Tree->Print();
	  mRareEvent->Clear();
    }
    return kStOK;
}

Int_t StRareMaker::Init() {
  number_of_events_processed = 0;
  Report();
  return StMaker::Init();
}

void StRareMaker::Report(){
  mEventCut->Report();
  mTrackCut->Report();
}


void StRareMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StRareMaker.cxx,v 1.5 2001/10/16 01:26:14 struck Exp $\n");
  printf("**************************************************************\n");
}

void StRareMaker::Clear(Option_t *opt) {
}


Int_t StRareMaker::Finish() {
  out->Write();
  out->Close();
  return kStOK;
}










