//////////////////////////////////////////////////////////////////////
// $Id: StFlowMaker.cxx,v 1.2 1999/11/11 23:08:57 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
// Description:  Maker to fill StFlowEvent from StEvent and
//      base class for StFlowTagMaker and StFlowAnalysisMaker
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.2  1999/11/11 23:08:57  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:12  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////

#include "StFlowMaker.hh"
#include "StFlowEvent.hh"
#include "TFile.h"
#include "StFlowCutEvent.hh"
#include "StFlowCutTrack.hh"
//#include "StGlobalTrack.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
//#include "StLorentzVector.hh"
#define PR(x) cout << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

const Double_t bField = 0.5*tesla;

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t *name): 
  StMaker(name),
  mEvent(0),
  MakerName(name) {
}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
  //delete mFlowTag; //clean up
}

Int_t StFlowMaker::Make() {

  // Create a new tag
  mFlowTag = 0;
  mFlowTag = new FlowTag_st;

  // print pointer to flowtag 
  cout << "FlowMaker pointer to TagPointer: " << TagPointer() << endl;

  // Get a pointer to the DST
  mEvent = (StEvent*)GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
    
  // Check the event cuts
  mFlowEvent = 0;
  if (StFlowCutEvent::CheckEvent(mEvent)) {
    // fill and return StFlowEvent
    mFlowEvent = fillFlowEvent();
  }
  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintInfo() {
  cout << "$Id: StFlowMaker.cxx,v 1.2 1999/11/11 23:08:57 posk Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {

  // Open PhiWgt file
  TDirectory* dirSave = gDirectory;
  TFile* phiWgtFile = new TFile("flowPhiWgt.hist.root", "READ");
  if (!phiWgtFile->IsOpen()) {
    cout << "### Can't open PhiWgt file" << endl;
  }
  gDirectory = dirSave;

  // Fill mPhiWgt
  // for full events for each harmonic
  for (int k = 0; k < nSubs/2; k++) {
    char countSubs[2];
    sprintf(countSubs,"%d",k+1);
    for (int j = 0; j < nHars; j++) {
      float harN  = (float)(j+1);
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      TString* histTitle = new TString("Flow_Phi_Weight_Event");
      histTitle->Append(*countSubs);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      if (phiWgtFile->IsOpen()) {
	TH1* phiWgtHist = (TH1*)phiWgtFile->Get(histTitle->Data());
	for (int n = 0; n < nPhiBins; n++) {
	  mPhiWgt[k][j][n] = (phiWgtHist) ? phiWgtHist->GetBinContent(n+1) : 1.;
	}
      } else {
	for (int n = 0; n < nPhiBins; n++) {
	  mPhiWgt[k][j][n] = 1.;
	}
      }
      delete histTitle;
    }
  }

  // Close PhiWgt file
  if (phiWgtFile->IsOpen()) phiWgtFile->Close();

  // Set the event cuts
//   StFlowCutEvent::SetMult(20, 2000);
//   StFlowCutEvent::SetVertexX(0., 0.);
//   StFlowCutEvent::SetVertexY(0., 0.);
//   StFlowCutEvent::SetVertexZ(-50., 50.);

  return StMaker::Init();
}

//-----------------------------------------------------------------------

StFlowEvent* StFlowMaker::fillFlowEvent() {
  // Make StFlowEvent from StEvent
  
  // Instantiate a new StFlowEvent
  StFlowEvent* flowEvent = new StFlowEvent;
  
  // Initialize Iterator, loop variables
  StTrackCollection* tracks = mEvent->trackCollection();
  StTrackIterator    itr;
  // track loop
  Int_t goodTracks;
  for (itr = tracks->begin(), goodTracks = 0; itr != tracks->end(); itr++) {
    StGlobalTrack* mTrack = *itr;
    if (StFlowCutTrack::CheckTrack(mTrack)) {
      StThreeVectorD p = mTrack->helix().momentum(bField); 
      // Instantiate new StFlowTrack
      StFlowTrack* flowTrack = new StFlowTrack;
      flowTrack->SetPhi(p.phi());
      flowTrack->SetEta(p.pseudoRapidity());
      flowTrack->SetPt(p.perp());
      // SetSubEvent( );
      // SetHarmonic( );
      flowEvent->TrackCollection()->push_back(flowTrack);
      goodTracks++;
    }
  }
  flowEvent->SetNumberOfTracks(goodTracks);
  
  StFlowCutTrack::CheckEvent(); // if kFALSE undo this event

  return flowEvent;
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Tags() {
  // Get the flow tags and calculate the full event quantities
  
  // Get a pointer to the flow tags
  if (!mFlowTag) {
    cout << "$$$$$ null FlowTag pointer" << endl;
    return kStErr;
  }

  for (int j = 0; j < nHars; j++) {
    float harN  = (float)(j+1);
    // sub-event quantities
    mQSub[0][j].Set( mFlowTag->qxa[j], mFlowTag->qya[j] );
    mQSub[1][j].Set( mFlowTag->qxb[j], mFlowTag->qyb[j] );
    mQSub[2][j].Set( mFlowTag->qxc[j], mFlowTag->qyc[j] );
    mQSub[3][j].Set( mFlowTag->qxd[j], mFlowTag->qyd[j] );
    mMulSub[0][j]   = mFlowTag->na[j];
    mMulSub[1][j]   = mFlowTag->nb[j];
    mMulSub[2][j]   = mFlowTag->nc[j];
    mMulSub[3][j]   = mFlowTag->nd[j];
    mSumPtSub[0][j] = mFlowTag->spta[j];
    mSumPtSub[1][j] = mFlowTag->sptb[j];
    mSumPtSub[2][j] = mFlowTag->sptc[j];
    mSumPtSub[3][j] = mFlowTag->sptd[j];

    // calculate Psi
    for (int i = 0; i < nSubs; i++) {
      mPsiSub[i][j] = mQSub[i][j].Phi() / harN;
    }

    // full event quantities
    for (int k = 0; k < nSubs/2; k++) {
      mQ[k][j]    = mQSub[2*k][j] + mQSub[2*k+1][j];
      mPsi[k][j]  = mQ[k][j].Phi() / harN;
      mQMod[k][j] = mQ[k][j].Mod();
      m_q[k][j]   = (mMul[k][j] > 0.) ? mQMod[k][j]/sqrt(mMul[k][j]) : 0.;
      mMul[k][j]  = mMulSub[2*k][j] + mMulSub[2*k+1][j];
      mSumPt[k][j]= mSumPtSub[2*k][j] + mSumPtSub[2*k+1][j];
    }
  }
  return kStOK;
}

//-------------------

Double_t StFlowMaker::PhiWeight(Float_t mPhi, Int_t eventN, Int_t harN) const {
  Int_t n = (mPhi/twopi)*nPhiBins;
  return mPhiWgt[eventN][harN][n];
}
