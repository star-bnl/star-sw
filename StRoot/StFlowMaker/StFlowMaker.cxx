//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowMaker.cxx,v 1.11 2000/01/13 22:19:19 posk Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
//
//////////////////////////////////////////////////////////////////////
//
// Description: Maker to fill StFlowEvent from StEvent
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowMaker.cxx,v $
// Revision 1.11  2000/01/13 22:19:19  posk
// Updates and corrections.
//
// Revision 1.10  1999/12/21 21:30:53  posk
// Updated the README file.
//
// Revision 1.9  1999/12/21 01:11:00  posk
// Added more quantities to StFlowEvent.
//
// Revision 1.8  1999/12/16 18:05:23  posk
// Fixed Linux compatability again.
//
// Revision 1.7  1999/12/15 22:01:27  posk
// Added StFlowConstants.hh
//
// Revision 1.6  1999/12/07 23:30:53  snelling
// Fixed Linux warnings
//
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

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StFlowMaker.hh"
#include "StFlowEvent.hh"
#include "StEvent.h"
#include "StRun.h"
#include "StEventTypes.h"
#include "StFlowCutEvent.hh"
#include "StFlowCutTrack.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "TFile.h"
#include "StFlowConstants.hh"
#define PR(x) cout << "##### FlowMaker: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowMaker)

//-----------------------------------------------------------------------

StFlowMaker::StFlowMaker(const Char_t *name): 
  StMaker(name),
  pEvent(0) {
}

//-----------------------------------------------------------------------

StFlowMaker::~StFlowMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::Make() {

  // Get a pointer to StEvent
  pEvent = (StEvent*)GetInputDS("StEvent");
  if (!pEvent) return kStOK; // If no event, we're done

  // Check the event cuts and fill StFlowEvent
  pFlowEvent = 0;
  if (StFlowCutEvent::CheckEvent(pEvent)) fillFlowEvent();

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::PrintInfo() {
  cout << "$Id: StFlowMaker.cxx,v 1.11 2000/01/13 22:19:19 posk Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();

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

Int_t StFlowMaker::Finish() {
  // Print the cut lists
  StFlowCutEvent::PrintCutList();
  StFlowCutTrack::PrintCutList();
  pFlowEvent->PrintSelectionList();
  
  return StMaker::Finish();
}

//-----------------------------------------------------------------------

Int_t StFlowMaker::readPhiWgtFile() {
  // Read the PhiWgt root file

  static const int& nHars    = Flow::nHars;
  static const int& nSels    = Flow::nSels;
  static const int& nPhiBins = Flow::nPhiBins;

  TDirectory* dirSave = gDirectory;
  TFile* pPhiWgtFile = new TFile("flowPhiWgt.hist.root", "READ");
  if (!pPhiWgtFile->IsOpen()) {
    cout << "##### Can't open PhiWgt file" << endl;
  }
  gDirectory = dirSave;

  // Fill mPhiWgt
  // for each selection and each harmonic
  for (int k = 0; k < nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);
    for (int j = 0; j < nHars; j++) {
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
	for (int n = 0; n < nPhiBins; n++) mPhiWgt[k][j][n] = 1.;
      }
      delete histTitle;
    }
  }

  // Close PhiWgt file
  if (pPhiWgtFile->IsOpen()) pPhiWgtFile->Close();

  return kStOK;
}

//-----------------------------------------------------------------------

void StFlowMaker::fillFlowEvent() {
  // Make StFlowEvent from StEvent

  // Instantiate a new StFlowEvent
  pFlowEvent = new StFlowEvent;

  // Fill PhiWgt array
  pFlowEvent->SetPhiWeight(mPhiWgt);

  // Get event id 
  Long_t eventID = pEvent->id(); 
  pFlowEvent->SetEventNumber(eventID);
  PR(eventID);

  // Get initial multiplicity before TrackCuts 
  UInt_t origMult = pEvent->primaryVertex(0)->numberOfDaughters(); 
  pFlowEvent->SetOrigMult(origMult);
  PR(origMult);

  // Get primary vertex position
  const StThreeVectorF& vertex = pEvent->primaryVertex(0)->position();
  pFlowEvent->SetVertexPos(vertex);

  // loop over tracks in StEvent
  int goodTracks = 0;
  const StSPtrVecPrimaryTrack& tracks = pEvent->primaryVertex(0)->daughters();
  StSPtrVecPrimaryTrackIterator itr = 0;

  for (itr = tracks.begin(); itr != tracks.end(); itr++) {
    StPrimaryTrack* pTrack = *itr;
    if (StFlowCutTrack::CheckTrack(pTrack)) {
      // Instantiate new StFlowTrack
      StFlowTrack* pFlowTrack = new StFlowTrack;
      StThreeVectorD p = pTrack->geometry()->momentum();
      pFlowTrack->SetPhi(p.phi());
      pFlowTrack->SetEta(p.pseudoRapidity());
      pFlowTrack->SetPt(p.perp());
      pFlowTrack->SetCharge(pTrack->geometry()->charge());
//       float b = pTrack->impactParameter();
//       PR(b);
      pFlowTrack->SetImpactPar(pTrack->impactParameter());
      pFlowTrack->SetChi2(pTrack->fitTraits().chi2());
      pFlowTrack->SetFitPts(pTrack->fitTraits().numberOfFitPoints());
      pFlowTrack->SetMaxPts(pTrack->numberOfPossiblePoints());
      pFlowEvent->TrackCollection()->push_back(pFlowTrack);
      goodTracks++;
    }
  }
  PR(goodTracks);
  
  // Check Eta Symmetry
  if (!StFlowCutEvent::CheckEtaSymmetry()) {  // if kFALSE delete this event
    delete pFlowEvent;
    pFlowEvent = 0;

    return;
  }

  pFlowEvent->SetSelections();
  pFlowEvent->MakeSubEvents();

  // Print multiplicities
//   static const int& nHars    = Flow::nHars;
//   static const int& nSels    = Flow::nSels;
//   static const int& nSubs    = Flow::nSubs;
//   int j, k, n;

//   for (j = 0; j < nHars; j++) {
//     for (k = 0; k < nSels; k++) {
//       cout << "j,k= " << j << k << " : " << pFlowEvent->Mult(j, k) << endl;
//     }
//   }

//   for (j = 0; j < nHars; j++) {
//     for (k = 0; k < nSels; k++) {
//       for (n = 0; n < nSubs+1; n++) {
// 	cout << "j,k,n= " << j << k << n << " : " << 
// 	  pFlowEvent->Mult(j, k, n) << endl;
//       }
//     }
//   }

}
