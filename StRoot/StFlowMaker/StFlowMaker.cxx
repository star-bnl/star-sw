//////////////////////////////////////////////////////////////////////
// $Id: StFlowMaker.cxx,v 1.5 1999/12/04 00:10:34 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
// Description:  Maker to fill StFlowEvent from StEvent and
//      base class for StFlowTagMaker and StFlowAnalysisMaker
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.5  1999/12/04 00:10:34  posk
// Works with the new StEvent
//
// Revision 1.4  1999/11/30 18:52:52  snelling
// First modification for the new StEvent
//
// Revision 1.3  1999/11/24 18:17:15  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
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
#include "StEvent.h"
#include "StEventTypes.h"
#include "StFlowCutEvent.hh"
#include "StFlowCutTrack.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "TFile.h"
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t *name): 
  StMaker(name),
  pEvent(0),
  MakerName(name) {
}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
}

Int_t StFlowMaker::Make() {

  // Get a pointer to the DST
  pEvent = (StEvent*) GetInputDS("StEvent");
  if (!pEvent) return kStOK; // If no event, we're done
    
  // Check the event cuts
  pFlowEvent = 0;
  if (StFlowCutEvent::CheckEvent(pEvent)) {
    // fill and return StFlowEvent
    pFlowEvent = fillFlowEvent();
  }
  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintInfo() {
  cout << "$Id: StFlowMaker.cxx,v 1.5 1999/12/04 00:10:34 posk Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Finish() {
  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Init() {

  // Open PhiWgt file
  readPhiWgtFile();

  // Set the event cuts
//   StFlowCutEvent::SetMult(20, 2000);
//   StFlowCutEvent::SetVertexX(0., 0.);
//   StFlowCutEvent::SetVertexY(0., 0.);
//   StFlowCutEvent::SetVertexZ(-50., 50.);

  return StMaker::Init();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::readPhiWgtFile() {
  // Read the PhiWgt root file

  TDirectory* dirSave = gDirectory;
  TFile* pPhiWgtFile = new TFile("flowPhiWgt.hist.root", "READ");
  if (!pPhiWgtFile->IsOpen()) {
    cout << "### Can't open PhiWgt file" << endl;
  }
  gDirectory = dirSave;

  // Fill mPhiWgt
  // for full events for each harmonic
  for (int k = 0; k < nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    for (int j = 0; j < nHars; j++) {
      float order  = (float)(j+1);
      char countHars[2];
      sprintf(countHars,"%d",j+1);
      TString* histTitle = new TString("Flow_Phi_Weight_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      if (pPhiWgtFile->IsOpen()) {
	TH1* phiWgtHist = (TH1*)pPhiWgtFile->Get(histTitle->Data());
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
  if (pPhiWgtFile->IsOpen()) pPhiWgtFile->Close();
  PR(mPhiWgt[0][0][0]);
  PR(mPhiWgt[1][0][30]);
  PR(mPhiWgt[1][3][59]);

  return kStOK;
}

//-----------------------------------------------------------------------

StFlowEvent* StFlowMaker::fillFlowEvent() {// Make StFlowEvent from StEvent
  const Double_t bField = 0.5*tesla;  

  // Instantiate a new StFlowEvent
  pFlowEvent = new StFlowEvent;

  // Fill PhiWgt array
  Double_t* pPhiWgt = &mPhiWgt[0][0][0];
  pFlowEvent->SetPhiWeight(pPhiWgt);

  // Get Initial multiplicity before TrackCuts 
  UInt_t origTracks = pEvent->primaryVertex(0)->numberOfDaughters(); 
  pFlowEvent->SetOrigTrackN(origTracks);

  // loop over tracks in StEvent
  Int_t goodTracks = 0;
  const StSPtrVecPrimaryTrack& tracks = pEvent->primaryVertex(0)->daughters();
  StSPtrVecPrimaryTrackIterator itr = 0;

  for (itr = tracks.begin(); itr != tracks.end(); itr++) {
    StPrimaryTrack* pTrack = *itr;
    if (StFlowCutTrack::CheckTrack(pTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      StThreeVectorD p = pTrack->geometry()->helix().momentum(bField);
      pFlowTrack->SetPhi(p.phi());
      pFlowTrack->SetEta(p.pseudoRapidity());
      pFlowTrack->SetPt(p.perp());
      for (int k = 0; k < nSels; k++) {
	for (int j = 0; j < nHars; j++) {
	  //cout << j << " " << k << " " << p.pseudoRapidity() << endl;
	  if (StFlowCutTrack::SelectTrack(pFlowTrack, k, j)) {
	    pFlowTrack->SetSelect(j, k); 
	  } 
	}
      }
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  PR(goodTracks);
  
  pFlowEvent->MakeSubEvents();

//   for (int j = 0; j < nHars; j++) {
//     for (int k = 0; k < nSels; k++) {
//       for (int n = 0; n < nSubs; n++) {
// 	cout << "j,k,n= " << j << k << n << " : " << 
// 	  pFlowEvent->Mult(j, k, n+1) << endl;
//       }
//     }
//   }

  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry()) {    // if kFALSE delete this event
    delete pFlowEvent;
    return 0;
  }

  return pFlowEvent;
}
