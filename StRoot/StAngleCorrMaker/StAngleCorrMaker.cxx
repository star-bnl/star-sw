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

#include "StChain/StChain.h"
#include "StRun.h"
#include "StEvent.h"

#include <TOrdCollection.h>
#include <TH1.h>
#include <TCanvas.h>
#include <TFile.h>

#define TRACKSMAX 100000 

Int_t StAngleCorrMaker::Init() {

  // set up a TOrdCollection as a pool of tracks
  // StTrackForPool has momentum and event number information

  mCollectionOfTracksA= new TOrdCollection(TRACKSMAX);
  mCollectionOfTracksB= new TOrdCollection(TRACKSMAX);
  mCollectionOfHighestPt= new TOrdCollection(TRACKSMAX);
  mNumberEventsInPool= 0;
  mTotalEvents = 0;

  // output file
  mOutput = new TFile("corr.root","RECREATE");

  // book an histogram for the numerator of the angular correlation
  int nbin = 60;
  float lbin =0. ;
  float ubin = 180.;
  mPhiNumeratorPtThresh = new TH1F("phiNumerator",
			       "relative phi in real events",
			       nbin,lbin,ubin);
  mPhiNumeratorPtThresh->Sumw2();
  mPhiDenominatorPtThresh = new TH1F("phiDenominator",
				 "relative phi in real events",
			         nbin,lbin,ubin);
  mPhiDenominatorPtThresh->Sumw2();

  mPhiNumeratorPtHigh = new TH1F("phiNumeratorPtHigh",
			       "relative phi in real events",
			       nbin,lbin,ubin);
  mPhiNumeratorPtHigh->Sumw2();
  mPhiDenominatorPtHigh = new TH1F("phiDenominatorPtHigh",
				 "relative phi in real events",
			         nbin,lbin,ubin);
  mPhiDenominatorPtHigh->Sumw2();
  cout << "end of init" << endl;
  return StMaker::Init();
}

Int_t StAngleCorrMaker::Make() {

  //#if 0
  //StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  //if (! event()) return kStOK; // If no event, we're done
  //StEvent& ev = *(evMaker->event());
  //#endif
  cout << "in Make" << endl;
  StEvent* mEvent;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (! mEvent) return kStOK; // If no event, we're done
  StEvent& ev = *mEvent;

  // <<<<<<< StAngleCorrMaker.cxx
  // int eventNumber = evMaker->GetNumber();
  // 
  // following line deleted by someone, reinstate
  mTotalEvents++;

  int eventNumber = GetNumber();

  // process all posssible pairs of primary tracks
  // fill histograms, and add tracks to pool of tracks
  //
  analyseRealPairs(ev,eventNumber);
  mNumberEventsInPool++;
  //
  // check how many tracks in pools
  //
  StTrackForPool *trackfrompool;
  trackfrompool = (StTrackForPool* ) mCollectionOfTracksA->Last();
  Int_t poolCounterA = mCollectionOfTracksA->IndexOf(trackfrompool);
  double aveTracksAPerEvent = 
    float(( poolCounterA + 1)) / float( mNumberEventsInPool);

  trackfrompool = (StTrackForPool* ) mCollectionOfTracksB->Last();
  Int_t poolCounterB = mCollectionOfTracksB->IndexOf(trackfrompool);
  double aveTracksBPerEvent = 
    float(( poolCounterB + 1)) / float( mNumberEventsInPool);

  //
  // include in this decision some estimate of average tracks per event
  //
  double numPossiblePairs = (float(poolCounterA) - aveTracksAPerEvent)*
    (float(poolCounterB) - aveTracksBPerEvent);
  if (numPossiblePairs > 2000000 ) {
    analyseMixedPairs();
    mCollectionOfTracksA->Delete();
    mCollectionOfTracksB->Delete();
    mNumberEventsInPool = 0;
    mCollectionOfHighestPt->Delete();
  
    trackfrompool = (StTrackForPool* ) mCollectionOfTracksA->Last();
    poolCounterA = mCollectionOfTracksA->IndexOf(trackfrompool);
    cout << "empty pool has " << poolCounterA << " tracks" <<endl;     
  };
  return kStOK;
}


StAngleCorrMaker::StAngleCorrMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {  
}

StAngleCorrMaker::~StAngleCorrMaker() {
}

Int_t StAngleCorrMaker::Finish() {

  analyseMixedPairs();
  //  empty collection 
  mCollectionOfTracksA->Delete(); 
  mCollectionOfTracksB->Delete();
  mNumberEventsInPool = 0; 
  mCollectionOfHighestPt->Delete();
  // write out histograms
  cout << "writing out histograms" << endl;
  mOutput->Write("MyKey",kSingleKey);
  mOutput->Close();
  cout <<   mTotalEvents << "  events analysed" << endl;
  return kStOK;
}

ClassImp(StAngleCorrMaker)



