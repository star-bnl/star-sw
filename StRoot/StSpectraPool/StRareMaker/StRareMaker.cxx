// $Id: StRareMaker.cxx,v 1.1 2001/01/31 18:05:52 hardtke Exp $
// $Log: StRareMaker.cxx,v $
// Revision 1.1  2001/01/31 18:05:52  hardtke
// Add Rare Particle mini-DST maker to repository
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
#include "StAcceptAllEvents.h"
#include "StAcceptAllTracks.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StarClassLibrary/StThreeVector.hh"
ClassImp(StRareEventCut)
ClassImp(StRareTrackCut)

static const char rcsid[] = "$Id: StRareMaker.cxx,v 1.1 2001/01/31 18:05:52 hardtke Exp $";

double dEdx_formula(double momentum, double mass);

ClassImp(StRareMaker)

StRareMaker::StRareMaker(const Char_t *name) : StMaker(name) {
  out = new TFile("RareEvent.root","RECREATE");
  out->SetCompressionLevel(2);
  m_Tree = new TTree("RareTree","RareTree");
  m_Tree->AutoSave();
  m_Tree->SetAutoSave(10000000);
  EventCut = new StAcceptAllEvents(); 
  TrackCut = new StAcceptAllTracks(); 
}

StRareMaker::StRareMaker(const Char_t *name,StRareEventCut* cut, StRareTrackCut* track) : StMaker(name) {
  out = new TFile("RareEvent.root","RECREATE");
  m_Tree = new TTree("RareTree","RareTree",1000000);
  m_Tree->AutoSave();
  m_Tree->SetAutoSave(10000000);
  revt = new StRareEvent();  
  m_Tree->Branch("StRareEvent","StRareEvent",&revt,64000,1);
  EventCut = cut; 
  TrackCut = track; 
}

Int_t StRareMaker::Make() {
    //
    //	This method is called every event. That's the
    //  right place to plug in your analysis to be
    //  done every event.
    //
    StEvent* mEvent;
    mEvent = (StEvent *) GetInputDS("StEvent");
    if (! mEvent) return kStOK; // If no event, we're done
    StEvent& ev = *mEvent;
    if (EventCut->Accept(mEvent)){
     revt->FillRareEvent(mEvent);
     StPrimaryTrackIterator itr;
     StPrimaryTrack *trk;
     if (ev.primaryVertex()){
      const StSPtrVecPrimaryTrack& tracks = ev.primaryVertex()->daughters();
      for (itr=tracks.begin();itr != tracks.end(); itr++){
        trk = *itr;
	if (TrackCut->Accept(trk)) revt->AddTrack(trk);
      }
     }
      m_Tree->Fill();
      revt->Clear();
     //     m_Tree->Print();
    }
  return kStOK;
}

Int_t StRareMaker::Init() {
  number_of_events_processed = 0;
  Report();
  return StMaker::Init();
}

void StRareMaker::Report(){
  EventCut->Report();
  TrackCut->Report();
}


void StRareMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StRareMaker.cxx,v 1.1 2001/01/31 18:05:52 hardtke Exp $\n");
  printf("**************************************************************\n");
}

void StRareMaker::Clear(Option_t *opt) {
}


Int_t StRareMaker::Finish() {
  out->Write();
  out->Close();
  return kStOK;
}










