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

Int_t StAngleCorrMaker::Init() {

  // set up a TOrdCollection as a pool of tracks
  // StTrackForPool has momentum and event number information

  mCollectionOfTracks= new TOrdCollection(TRACKSMAX);

  // book an histogram for the numerator of the angular correlation
  int nbin = 180;
  float lbin =0. ;
  float ubin = 180.;
  mHistPhiNumerator = new TH1F("phiNumerator",
			       "relative phi in real events",
			       nbin,lbin,ubin);
  mHistPhiDenominator = new TH1F("phiDenominator",
				 "relative phi in real events",
			         nbin,lbin,ubin);

  return StMaker::Init();
}

Int_t StAngleCorrMaker::Make() {

  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! evMaker->event()) return kStOK; // If no event, we're done
  StEvent& ev = *(evMaker->event());

  int eventNumber = evMaker->GetNumber();
  // process all posssible pairs of primary tracks
  // fill histograms, and add tracks to pool of tracks
  //
  analyseRealPairs(ev,eventNumber);

  return kStOK;
}


StAngleCorrMaker::StAngleCorrMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
  drawinit = kFALSE;  
}

StAngleCorrMaker::~StAngleCorrMaker() {
}


void StAngleCorrMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StAngleCorrMaker.cxx,v 1.2 1999/04/28 15:14:49 ogilvie Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

void StAngleCorrMaker::Clear(Option_t *opt) {

  SafeDelete(m_DataSet);
}

Int_t StAngleCorrMaker::Finish() {
  // draw histograms
  mHistPhiNumerator->Draw();
  return kStOK;
}

ClassImp(StAngleCorrMaker)
