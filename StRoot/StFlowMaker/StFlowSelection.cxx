////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowSelection.cxx,v 1.6 2000/08/31 18:58:26 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Mar 2000
//
// Description:  Class for making selections
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowSelection.cxx,v $
// Revision 1.6  2000/08/31 18:58:26  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.5  2000/08/12 20:22:21  posk
// Recalculate centrality in read from pico.
//
// Revision 1.4  2000/05/26 21:29:32  posk
// Protected Track data members from overflow.
//
// Revision 1.3  2000/05/11 20:00:38  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.2  2000/03/28 23:21:04  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.1  2000/03/15 23:28:53  posk
// Added StFlowSelection.
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StFlowSelection.h"
#include "StFlowEvent.h"
#include "StFlowTrack.h"
#define PR(x) cout << "##### FlowSelection: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowSelection)

//-----------------------------------------------------------------------


StFlowSelection::StFlowSelection() : mCentrality(0), mSubevent(-1) {
  // To make selections
  mNumber[0] = '\0';
  mPid[0] = '\0';
  mPidPart[0] = '\0';
  mPtPart[0] = 0.;
  mPtPart[1] = 0.;
  mEtaPart[0] = 0.;
  mEtaPart[1] = 0.;
}

//-----------------------------------------------------------------------

StFlowSelection::~StFlowSelection() {
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::Select(StFlowEvent* pFlowEvent) {
  // Returns kTRUE if the event is selected

  // Centrality
  if (mCentrality) {
    UInt_t cent = pFlowEvent->Centrality();
    if (mCentrality != cent) return kFALSE;
  }

  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::Select(StFlowTrack* pFlowTrack) {
  // Selects particles for event plane determination
  // Returns kTRUE if the track is selected

  // PID
  if (mPid[0] != '\0') {
    const Char_t* pid = pFlowTrack->Pid();
    if (strstr(pid, mPid)==0) return kFALSE;
  }
  
  // Selected for event plane
  if (!pFlowTrack->Select(mHarmonic, mSelection, mSubevent)) return kFALSE;
  
  return kTRUE;
}

//-----------------------------------------------------------------------

Bool_t StFlowSelection::SelectPart(StFlowTrack* pFlowTrack) {
  // Selects particles for correlation with the event plane
  // Returns kTRUE if the track is selected

  // PID
  if (mPidPart[0] != '\0') {
    const Char_t* pid = pFlowTrack->Pid();
    if (strstr(pid, mPidPart)==0) return kFALSE;
  }
  
  // Pt
  float pt = pFlowTrack->Pt();
  if (mPtPart[1] > mPtPart[0] && 
      (pt < mPtPart[0] || pt >= mPtPart[1])) return kFALSE;
  
  // Eta
  float eta = pFlowTrack->Eta();
  if (mEtaPart[1] > mEtaPart[0] && 
      (eta < mEtaPart[0] || eta >= mEtaPart[1])) return kFALSE;

  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowSelection::PrintList() const {
  
  cout << "#################################################################"
       << endl;
  cout << "# Selection List:" << endl;
  cout << "# Number = " << mNumber << endl;
  cout << "# Centrality = " << mCentrality << endl;
  cout << "# Particles used for the event plane: " << mPid << endl;
  cout << "# Particles correlated with the event plane: " << mPidPart << endl;
  cout << "# Pt for particles correlated with the event plane: " << 
    mPtPart[0] << " to " << mPtPart[1] << " GeV/c" <<endl;
  cout << "# Eta for particles correlated with the event plane: " << 
    mEtaPart[0] << " to " << mEtaPart[1] <<endl;
  cout << "#################################################################"
       << endl;

}

//-----------------------------------------------------------------------
