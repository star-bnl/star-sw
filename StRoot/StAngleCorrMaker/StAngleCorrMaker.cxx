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
#include "StTrackForPool.hh"
#include "StRoot/StEventReaderMaker/StEventReaderMaker.h"
#include "StChain/StChain.h"
#include "StEvent/StRun.hh"
#include "StEvent/StEvent.hh"

#include <TOrdCollection.h>
#include <TH1.h>
#include <TCanvas.h>
#include <TFile.h>

#define TRACKSMAX 100000 

Int_t StAngleCorrMaker::Init() {

  // set up a TOrdCollection as a pool of tracks
  // StTrackForPool has momentum and event number information

  mCollectionOfTracks= new TOrdCollection(TRACKSMAX);
  mNumberEventsInPool= 0;
  // output file
  mOutput = new TFile("corr.root","RECREATE");

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
  mNumberEventsInPool++;
  //
  // check how many tracks in pool
  //
  StTrackForPool *trackfrompool;
  trackfrompool = (StTrackForPool* ) mCollectionOfTracks->Last();
  Int_t poolCounter = mCollectionOfTracks->IndexOf(trackfrompool);
  double aveTracksPerEvent = 
    float(( poolCounter + 1)) / float( mNumberEventsInPool);
  //
  // include in this decision some estimate of average tracks per event
  //
  double numPossiblePairs = pow((float(poolCounter) - aveTracksPerEvent),2);
  if (numPossiblePairs > 1000000 ) {
    analyseMixedPairs();
    //  empty collection 
    mCollectionOfTracks->Delete();
    mNumberEventsInPool = 0;       
  };
  return kStOK;
}


StAngleCorrMaker::StAngleCorrMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {  
}

StAngleCorrMaker::~StAngleCorrMaker() {
}


void StAngleCorrMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StAngleCorrMaker.cxx,v 1.3 1999/04/29 16:53:26 ogilvie Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

void StAngleCorrMaker::Clear(Option_t *opt) {

  SafeDelete(m_DataSet);
}

Int_t StAngleCorrMaker::Finish() {

  // write out histograms

  mOutput->Write("MyKey",kSingleKey);
  mOutput->Close();

  return kStOK;
}

ClassImp(StAngleCorrMaker)
