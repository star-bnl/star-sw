/***************************************************************************
 *
 * $Id: StFlowMaker.cxx,v 1.1 1999/11/04 19:02:12 snelling Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
 * Description:  Maker to fill the FlowEvent from StEvent
 *
 ***************************************************************************
 *
 * $Log: StFlowMaker.cxx,v $
 * Revision 1.1  1999/11/04 19:02:12  snelling
 * First check in of StFlowMaker. It contains the common code from
 * StFlowTagMaker and StFlowAnalysisMaker.
 *
 **************************************************************************/
#include "StFlowMaker.hh"
#include "FlowEvent.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t *name): 
  StMaker(name),
  flowTag(0),
  mEvent(0),
  MakerName(name) {

}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
  delete flowTag; //clean up
}

Int_t StFlowMaker::Make() {

  // Create a new tag
  flowTag = new FlowTag_st;

  // Get a pointer to the DST
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  
  // fill and return FlowEvent
  fillFlowEvent();

  // print pointer to flowtag 
  cout << "pointer to flow tag: " << tag() << endl;

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintInfo() {
  cout << "$Id: StFlowMaker.cxx,v 1.1 1999/11/04 19:02:12 snelling Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

FlowTag_st* StFlowMaker::tag() {

  return flowTag;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {

  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {

  return StMaker::Init();
}

//-----------------------------------------------------------------------

FlowEvent* StFlowMaker::fillFlowEvent() {
  
  const double bField = 0.5*tesla;
  
  // instantiate a new FlowEvent
  FlowEvent* flowEvent = new FlowEvent;
  
  // Initialize Iterator, loop variables
  StTrackCollection* tracks = mEvent->trackCollection();
  StTrackIterator    itr;
  // track loop
  int initialMultiplicity = tracks->size();
  flowEvent->SetNumberOfTracks(initialMultiplicity);
  
  int goodTracks;
 
    for (itr = tracks->begin(), goodTracks = 0; itr != tracks->end(); itr++) {
      StGlobalTrack* gtrk = *itr;
      StTrackFitTraits& fitTraits = gtrk->fitTraits();
      int nFitPoints = fitTraits.numberOfFitPoints();
      if (nFitPoints > 10) {
	StThreeVectorD p = gtrk->helix().momentum(bField); 
	//instantiate new FlowTrack
	FlowTrack* flowTrack = new FlowTrack;
	flowTrack->SetPhi(p.phi());
	flowTrack->SetEta(p.pseudoRapidity());
	flowTrack->SetPt(p.perp());
	flowEvent->TrackCollection()->push_back(flowTrack);
	goodTracks++;
      }
    }
  
  flowEvent->SetNumberOfGoodTracks(goodTracks);
  
  return flowEvent;
}

