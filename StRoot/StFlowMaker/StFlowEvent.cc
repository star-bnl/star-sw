//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.cc,v 1.7 1999/12/16 18:05:22 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.cc,v $
// Revision 1.7  1999/12/16 18:05:22  posk
// Fixed Linux compatability again.
//
// Revision 1.6  1999/12/15 22:01:25  posk
// Added StFlowConstants.hh
//
// Revision 1.5  1999/12/07 23:30:52  snelling
// Fixed Linux warnings
//
// Revision 1.4  1999/12/04 00:10:32  posk
// Works with the new StEvent
//
// Revision 1.3  1999/11/30 18:52:51  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:13  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/11 23:08:54  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:04  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include <algorithm>
#include "StFlowEvent.hh"
#include "StFlowTrackCollection.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#define PR(x) cout << "##### FlowEvent: " << (#x) << " = " << (x) << endl;

Float_t  StFlowEvent::mEtaCuts[2][Flow::nHars][Flow::nSels] = {{{0.,0.5},
								{0.,0.},
								{0.,0.5},
								{0.,0.},
								{0.,0.5},
								{0.,0.}},
							       {{2.,2.},
								{2.,1.},
								{2.,2.},
								{2.,1.},
								{2.,2.},
								{2.,1.}}};
Float_t  StFlowEvent::mPtCuts[2][Flow::nHars][Flow::nSels] =  {{{0.05,0.05},
								{0.05,0.05},
								{0.05,0.05},
								{0.05,0.05},
								{0.05,0.05},
								{0.05,0.05}},
							       {{2.,2.},
								{2.,2.},
								{2.,2.},
								{2.,2.},
								{2.,2.},
								{2.,2.}}};

//-----------------------------------------------------------

StFlowEvent::StFlowEvent() {
  // Make a new track collection
  pTrackCollection = new StFlowTrackCollection;

}

//-----------------------------------------------------------

StFlowEvent::~StFlowEvent() {
  StFlowTrackIterator iter;
  for (iter= pTrackCollection->begin(); iter!= pTrackCollection->end(); iter++){
    delete *iter;
  }
  delete pTrackCollection;

}

//-------------------------------------------------------------

Int_t StFlowEvent::checkInput(Int_t harN, Int_t selN, Int_t subN) const {
  // checks arguments for valid range
  
  if (harN < 0 || harN >= Flow::nHars) {
    cout << "### Harmonic " << harN << " not valid" << endl;
    return kFALSE;
  }
  if (selN < 0 || selN >= Flow::nSels) {
    cout << "### Selection " << selN << " not valid" << endl;
    return kFALSE;
  }
  if (subN < -1 || subN > Flow::nSubs) {
    cout << "### Subevent " << subN << " not valid" << endl;
    return kFALSE;
  }

  return kTRUE;
}

//-------------------------------------------------------------

void StFlowEvent::SetPhiWeight(const Flow::PhiWgt_t &pPhiWgt) {
  // Transfers PhiWgt array from StFlowMaker

  static const int& nHars    = Flow::nHars;
  static const int& nSels    = Flow::nSels;
  static const int& nPhiBins = Flow::nPhiBins;

  for (int k = 0; k < nSels; k++) {
    for (int j = 0; j < nHars; j++) {
      for (int n = 0; n < nPhiBins; n++) {
	mPhiWgt[k][j][n] = pPhiWgt[k][j][n];
      }
    }
  }

}

//-------------------------------------------------------------

Double_t StFlowEvent::PhiWeight(Float_t mPhi, Int_t selN, Int_t harN) const {
  if (!checkInput(harN, selN, 0)) return 0.;

  if (mPhi < 0.) mPhi += twopi;
  int n = (int)(mPhi/twopi)*Flow::nPhiBins;

  return mPhiWgt[selN][harN][n];
}

//-------------------------------------------------------------

UInt_t StFlowEvent::Mult(Int_t harN, Int_t selN, Int_t subN) {
  UInt_t mMult = 0;
  if (!checkInput(harN, selN, subN)) return mMult;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowTrack->Select(harN, selN, subN))  mMult++;
  }
  
  return mMult;
}

//-------------------------------------------------------------

Float_t StFlowEvent::MeanPt(Int_t harN, Int_t selN, Int_t subN) {
  Float_t mSumPt = 0.;
  UInt_t  mMult  = 0;
  if (!checkInput(harN, selN, subN)) return 0.;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowTrack->Select(harN, selN, subN)) {
      mSumPt += pFlowTrack->Pt();
      mMult++;
    }
  }

  return (mMult) ? mSumPt/(float)mMult : 0.;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::Q(Int_t harN, Int_t selN, Int_t subN) { 
  TVector2 mQ;
  mQ.Set(0., 0.);
  if (!checkInput(harN, selN, subN)) return mQ;
  double mQx=0., mQy=0.;
  float order = (float)(harN + 1);

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowTrack->Select(harN, selN, subN)) {
      float mPhi = pFlowTrack->Phi();
      double phiWgt = PhiWeight(mPhi, selN, harN);
      if (pFlowTrack->Eta() < 0 && (harN+1) % 2 == 1) phiWgt *= -1.;
      mQx += phiWgt * cos(mPhi * order);
      mQy += phiWgt * sin(mPhi * order);
    }
  }
  mQ.Set(mQx, mQy);

  return mQ;
}

//-------------------------------------------------------------

Float_t StFlowEvent::Psi(Int_t harN, Int_t selN, Int_t subN) {
  Float_t mPsi = 0.;
  if (!checkInput(harN, selN, subN)) return mPsi;
  float order = (float)(harN + 1);

  TVector2 mQ = Q(harN, selN, subN);
  mPsi= mQ.Phi() / order;
  if (mPsi < 0.) {mPsi += twopi / order;}
  
  return mPsi;
}

//-------------------------------------------------------------

Float_t StFlowEvent::q(Int_t harN, Int_t selN, Int_t subN) { 
  Float_t mq = 0.;
  if (!checkInput(harN, selN, subN)) return mq;

  TVector2 mQ  = Q(harN, selN, subN);
  UInt_t mMult = Mult(harN, selN, subN);
  
  return (mMult) ? mQ.Mod() / sqrt(mMult) : 0.;
}

//-----------------------------------------------------------------------

void StFlowEvent::SetSelections() {

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;
  
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Float_t mEta = pFlowTrack->Eta();
    Float_t mPt  = pFlowTrack->Pt();
    for (int selN = 0; selN < nSels; selN++) {
      for (int harN = 0; harN < nHars; harN++) {

	// Eta
	if (mEtaCuts[1][harN][selN] > mEtaCuts[0][harN][selN] && 
	    (fabs(mEta) < mEtaCuts[0][harN][selN] || 
	     fabs(mEta) >= mEtaCuts[1][harN][selN])) goto Skip;
	
	// Pt
	if (mPtCuts[1][harN][selN] > mPtCuts[0][harN][selN] && 
	    (mPt < mPtCuts[0][harN][selN] ||
	     mPt >= mPtCuts[1][harN][selN])) goto Skip;

      	pFlowTrack->SetSelect(harN, selN);

      Skip:  ;  // must have a statement after a label

      }
    }
  }

}

//-------------------------------------------------------------

void StFlowEvent::MakeSubEvents() {

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;
  static const int& nSubs = Flow::nSubs;

  StFlowTrackIterator itr;
  int eventMult[Flow::nHars][Flow::nSels] = {{0}};
  int harN, selN, subN = 0;

  //random_shuffle(TrackCollection()->begin(), TrackCollection()->end());

  // loop to count the total number of tracks for each selection
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    for (selN = 0; selN < nSels; selN++) {
      for (harN = 0; harN < nHars; harN++) {
	if (pFlowTrack->Select(harN, selN)) {
	    eventMult[harN][selN]++;
	}
      }
    }
  }

  // loop to set the SubEvent member variable
  for (selN = 0; selN < nSels; selN++) {
    for (harN = 0; harN < nHars; harN++) {
      int subEventMult = eventMult[harN][selN] / nSubs;
      if (subEventMult) {
	subN = 0;
	int countN = 0;
	for (itr = TrackCollection()->begin(); itr != TrackCollection()->end();
	     itr++) {
	  StFlowTrack* pFlowTrack = *itr;
	  if (pFlowTrack->Select(harN, selN)) {
	    pFlowTrack->SetSubevent(harN, selN, subN);
	    countN++;
	    if (countN % subEventMult == 0.) subN++;
	  }
	}
      }
    }
  }
  
}

//-----------------------------------------------------------------------

void StFlowEvent::PrintSelectionList() {
  // Prints the list of selection cuts
  // Call in Finish

  static const int& nHars    = Flow::nHars;
  static const int& nSels    = Flow::nSels;

  cout << "#######################################################" << endl;
  cout << "# Track Selections List:" << endl; 
  for (int k = 0; k < nSels; k++) {
    for (int j = 0; j < nHars; j++) {
      cout << "#  selection= " << k+1 << " harmonic= " 
	   << j+1 << endl;
      cout << "#    abs(Eta) cuts= " << mEtaCuts[0][j][k] << ", " 
	   << mEtaCuts[1][j][k] << endl;
      cout << "#    Pt cuts= " << mPtCuts[0][j][k] << ", "
	   << mPtCuts[1][j][k] << endl;
    }
  }
  cout << "#######################################################" << endl;

}

