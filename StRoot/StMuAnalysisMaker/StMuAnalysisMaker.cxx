/*!
 * \class  StMuAnalysisMaker
 * \brief  A typical Analysis Class for MuDst
 * \author Wei-Ming Zhang, KSU, Mar 2004 
 *
 * This is an example of a maker to perform analysis using MuDst.
 *
 * $Id: StMuAnalysisMaker.cxx,v 1.1 2004/08/10 16:09:11 perev Exp $
 * -------------------------------------------------------------------------
 * $Log: StMuAnalysisMaker.cxx,v $
 * Revision 1.1  2004/08/10 16:09:11  perev
 * new GridCollector stuff
 *
 * -------------------------------------------------------------------------
 */
//
//  Include header files. 
#include "TFile.h"
#include "StMessMgr.h"
#include "TH1.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuAnalysisMaker.h"
//
//  Prototype 
void muEventInfo(StMuEvent&, const int&);

ClassImp(StMuAnalysisMaker)

// The constructor. Initialize data members here.
StMuAnalysisMaker::StMuAnalysisMaker(const Char_t *name) : StMaker(name)
{ mEventCounter = 0; mFile = 0; }

StMuAnalysisMaker::~StMuAnalysisMaker() { /* noop */ }
//
//  Called once at the beginning.
Int_t StMuAnalysisMaker::Init()
{
    //  Output file and histogram booking
    mFileName = "muAnalysis.root";

    mGlobalPt = new TH1D("globalPt","globalPt",100,0.,3.);
    mPrimaryPt = new TH1D("primaryPt","primaryPt",100,0.,3.);
    mL3Pt = new TH1D("l3Pt","l3Pt",100,0.,3.);
    mRefMult = new TH1D("refMult","refMult",100,0.,100.);
    
    return StMaker::Init();
}
//
//  Called every event after Make(). 
void StMuAnalysisMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}
//
//  Called once at the end.
Int_t StMuAnalysisMaker::Finish()
{
//  Summarize the run.
    cout << "StMuAnalysisMaker::Finish()\n";
    cout << "\tProcessed " << mEventCounter << " events." << endl;
//
//  Output histograms
    mFile =  new TFile(mFileName.c_str(), "RECREATE");
    cout << "\tHistograms will be stored in file '"
	 <<  mFileName.c_str() << "'" << endl;

    mGlobalPt->Write();
    mPrimaryPt->Write();
    mL3Pt->Write();
    mRefMult->Write();
//
//  Write histos to file and close it.
    if( mFile){
      mFile->Write();  
      mFile->Close();
    }

    return kStOK;
}
//
//  This method is called every event.
Int_t StMuAnalysisMaker::Make()
{
    mEventCounter++;  // increase counter

    DEBUGVALUE2(mEventCounter);
//  Get MuDst
    StMuDst* mu; 
    mu =  (StMuDst*) GetInputDS("MuDst"); 
    DEBUGVALUE2(mu);

    if (!mu){
	  gMessMgr->Warning() << "StMuAnalysisMaker::Make : No MuDst" << endm;
          return kStOK;        // if no event, we're done
    }
//
//  Check StMuEvent branch
    StMuEvent* muEvent;
    muEvent = (StMuEvent*) mu->event();
    if(muEvent) {
      int refMult = muEvent->refMult();
      mRefMult->Fill(refMult);
    }
// 
//  Check track branches
    StMuTrack* muTrack; 
    int nTracks;
    nTracks= mu->globalTracks()->GetEntries();
    printf("Global track # = %d\n",nTracks);
    for (int l=0; l<nTracks; l++) { 
      muTrack =  (StMuTrack*) mu->globalTracks(l); 
      if(muTrack) if (accept(muTrack)) mGlobalPt->Fill(muTrack->pt());
    }
    nTracks= mu->primaryTracks()->GetEntries();
    printf("Primary track # = %d\n",nTracks);
    for (int l=0; l<nTracks; l++) { 
      muTrack =  (StMuTrack*) mu->primaryTracks(l); 
      if(muTrack) if (accept(muTrack)) mPrimaryPt->Fill(muTrack->pt());
    }
    nTracks= mu->l3Tracks()->GetEntries();
    printf("L3 track # = %d\n",nTracks);
    for (int l=0; l<nTracks; l++) { 
      muTrack =  (StMuTrack*) mu->l3Tracks(l); 
      if(muTrack) if (accept(muTrack)) mL3Pt->Fill(muTrack->pt());
    }
//
//  Printout information of StMuEvent 
    if(muEvent)  muEventInfo(*muEvent, mEventCounter); 

    return kStOK;
}
//
//  A simple track filter
bool StMuAnalysisMaker::accept(StMuTrack* track)
{
//  check for positive flags.
    return track && track->flag() >= 0;
}
