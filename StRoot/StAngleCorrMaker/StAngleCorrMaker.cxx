//
///////////////////////////////////////////////////////////////////////////////
//
// StAngleCorrMaker
//
// Description: 
//  Calculates high-pt angular correlations from StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List:
//  Craig Ogilvie MIT 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StAngleCorrMaker.h"
#include "StRoot/StEventReaderMaker/StEventReaderMaker.h"
#include "StChain/StChain.h"
#include "StEvent/StRun.hh"
#include "StEvent/StEvent.hh"

#include <TOrdCollection.h>
#include <TH1.h>

#define TRACKSMAX 100000 

static const char rcsid[] = "$Id: StAngleCorrMaker.cxx,v 1.1 1999/04/28 01:08:01 didenko Exp $";

void summarizeEvent(StEvent& event);

long countPrimaryPairs(StEvent& event, int eventNumber, TOrdCollection* trackpool);

Int_t StAngleCorrMaker::Init() {
  // set up a TOrdCollection as a pool of tracks
  // StTrackForPool has momentum and event number information
  //
  mCollectionOfTracks= new TOrdCollection(TRACKSMAX);
  // book an histogram for the numerator of the angular correlation
  mHistPhiNumerator = new TH1F("phiNumerator","number of pairs in real events",
			       180,0.,180.);
  return StMaker::Init();
}

Int_t StAngleCorrMaker::Make() {
  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! evMaker->event()) return kStOK; // If no event, we're done
  StEvent& ev = *(evMaker->event());
  StRun& run = *(evMaker->run());
  int eventNumber = evMaker->GetNumber();
  cout <<" event number " <<  eventNumber << endl;
  // OK, we've got the event. Pass it and process it.
  summarizeEvent(ev);

  long npair = countPrimaryPairs(ev,eventNumber,mCollectionOfTracks);
  cout << "Primary pairs: " << npair << endl;

  return kStOK;
}


StAngleCorrMaker::StAngleCorrMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
  drawinit = kFALSE;
  
}

StAngleCorrMaker::~StAngleCorrMaker() {
}


void StAngleCorrMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StAngleCorrMaker.cxx,v 1.1 1999/04/28 01:08:01 didenko Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

void StAngleCorrMaker::Clear(Option_t *opt) {

  SafeDelete(m_DataSet);
}

Int_t StAngleCorrMaker::Finish() {
  return kStOK;
}

ClassImp(StAngleCorrMaker)
