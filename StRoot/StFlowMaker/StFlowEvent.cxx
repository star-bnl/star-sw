//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.cxx,v 1.4 2000/05/12 22:42:04 snelling Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.cxx,v $
// Revision 1.4  2000/05/12 22:42:04  snelling
// Additions for persistency and minor fix
//
// Revision 1.3  2000/05/11 20:00:33  posk
// Preparation for micro and nano DSTs.
//
// Revision 1.2  2000/03/15 23:28:50  posk
// Added StFlowSelection.
//
// Revision 1.1  2000/03/02 23:02:48  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.16  2000/02/29 22:00:53  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.15  2000/02/29 01:26:11  snelling
// removed static const int& nxxx = Flow::nxxx;
//
// Revision 1.14  2000/02/18 22:49:54  posk
// Added PID and centrality.
//
// Revision 1.13  2000/02/11 20:53:09  posk
// Commented out random_shuffle and cout formatting so as to work under CC5.
//
// Revision 1.12  2000/01/31 22:16:58  posk
// CC5 compliant.
//
// Revision 1.9  1999/12/21 17:31:50  posk
// Fixed random_shuffle in making the sub events.
//
// Revision 1.8  1999/12/21 01:10:58  posk
// Added more quantities to StFlowEvent.
//
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
//#include <algorithm>
//#if !defined(ST_NO_NAMESPACES)
//using std::random_shuffle;
//#endif
#include "StFlowEvent.h"
#include "StFlowTrackCollection.h"
#include "StFlowSelection.h"
#include "StFlowConstants.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#define PR(x) cout << "##### FlowEvent: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowEvent)

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

Float_t StFlowEvent::mPiPlusCuts[2]  = {-2., 1.};
Float_t StFlowEvent::mPiMinusCuts[2] = {-2., 2.};
Float_t StFlowEvent::mProtonCuts[2]  = {-1., 2.};

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

Double_t StFlowEvent::PhiWeight(Float_t mPhi, Int_t selN, Int_t harN) const {

  if (mPhi < 0.) mPhi += twopi;
  int n = (int)((mPhi/twopi)*Flow::nPhiBins);

  return mPhiWgt[selN][harN][n];
}

//-------------------------------------------------------------

UInt_t StFlowEvent::Mult(StFlowSelection* pFlowSelect) {
  UInt_t mult = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack))  mult++;
  }

  return mult;
}

//-------------------------------------------------------------

Float_t StFlowEvent::MeanPt(StFlowSelection* pFlowSelect) {
  Float_t sumPt = 0.;
  UInt_t  mult  = 0;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      sumPt += pFlowTrack->Pt();
      mult++;
    }
  }

  return (mult) ? sumPt/(float)mult : 0.;
}

//-------------------------------------------------------------

TVector2 StFlowEvent::Q(StFlowSelection* pFlowSelect) { 
  TVector2 mQ;
  Int_t  selN  = pFlowSelect->Sel();
  Int_t  harN  = pFlowSelect->Har();
  double order = (double)(harN + 1);
  double mQx=0., mQy=0.;

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    if (pFlowSelect->Select(pFlowTrack)) {
      Float_t mPhi = pFlowTrack->Phi();
      double phiWgt = PhiWeight(mPhi, selN, harN);
      if (pFlowTrack->Eta() < 0. && (harN+1) % 2 == 1) phiWgt *= -1.;
      mQx += phiWgt * cos(mPhi * order);
      mQy += phiWgt * sin(mPhi * order);
    }
  }
  mQ.Set(mQx, mQy);

  return mQ;
}

//-------------------------------------------------------------

Float_t StFlowEvent::Psi(StFlowSelection* pFlowSelect) {
  Int_t  harN = pFlowSelect->Har();
  float order = (float)(harN + 1);

  TVector2 mQ = Q(pFlowSelect);
  Float_t psi= mQ.Phi() / order;
  if (psi < 0.) {psi += twopi / order;}
  
  return psi;
}

//-------------------------------------------------------------

Float_t StFlowEvent::q(StFlowSelection* pFlowSelect) { 
  TVector2 mQ  = Q(pFlowSelect);
  UInt_t mult  = Mult(pFlowSelect);
  
  return (mult) ? mQ.Mod() / sqrt((double)mult) : 0.;
}

//-----------------------------------------------------------------------

void StFlowEvent::SetSelections() {

  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Double_t eta = (double)(pFlowTrack->Eta());
    Float_t  Pt  = pFlowTrack->Pt();
    for (int selN = 0; selN < Flow::nSels; selN++) {
      for (int harN = 0; harN < Flow::nHars; harN++) {

	// Eta
	if (mEtaCuts[1][harN][selN] > mEtaCuts[0][harN][selN] && 
	    (fabs(eta) < mEtaCuts[0][harN][selN] || 
	     fabs(eta) >= mEtaCuts[1][harN][selN])) continue;
	
	// Pt
	if (mPtCuts[1][harN][selN] > mPtCuts[0][harN][selN] && 
	    (Pt < mPtCuts[0][harN][selN] ||
	     Pt >= mPtCuts[1][harN][selN])) continue;

      	pFlowTrack->SetSelect(harN, selN);

      }
    }
  }

}

//-------------------------------------------------------------

void StFlowEvent::MakeSubEvents() {

  StFlowTrackIterator itr;
  int eventMult[Flow::nHars][Flow::nSels] = {{0}};
  int harN, selN, subN = 0;

  //  random_shuffle(TrackCollection()->begin(), TrackCollection()->end());

  // loop to count the total number of tracks for each selection
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    for (selN = 0; selN < Flow::nSels; selN++) {
      for (harN = 0; harN < Flow::nHars; harN++) {
	if (pFlowTrack->Select(harN, selN)) {
	    eventMult[harN][selN]++;
	}
      }
    }
  }

  // loop to set the SubEvent member variable
  for (selN = 0; selN < Flow::nSels; selN++) {
    for (harN = 0; harN < Flow::nHars; harN++) {
      int subEventMult = eventMult[harN][selN] / Flow::nSubs;
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

void StFlowEvent::SetPids() {
  
  StFlowTrackIterator itr;
  for (itr = TrackCollection()->begin(); itr != TrackCollection()->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    Char_t pid[10] = "none";
    Short_t charge  = pFlowTrack->Charge();

    if (charge == 1) {
      Float_t piPlus  = pFlowTrack->PidPiPlus();
      Float_t proton  = pFlowTrack->PidProton();
      if (piPlus > mPiPlusCuts[0] && piPlus < mPiPlusCuts[1]) {
	strcpy(pid, "pi+");
      } else if ( proton > mProtonCuts[0] && proton < mProtonCuts[1]) {
	strcpy(pid, "proton");
      }
    } else if (charge == -1) {
      Float_t piMinus = pFlowTrack->PidPiMinus();
      if (piMinus > mPiMinusCuts[0] && piMinus < mPiMinusCuts[1]) {
	strcpy(pid, "pi-");
      }
    }

    pFlowTrack->SetPid(pid);

  }
}

//-----------------------------------------------------------------------

void StFlowEvent::PrintSelectionList() {
  // Prints the list of selection cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Pid Cuts:" << endl; 
  cout << "#    PiPlus cuts=  " << mPiPlusCuts[0] << ", " 
       << mPiPlusCuts[1] << endl;
  cout << "#    PiMinus cuts= " << mPiMinusCuts[0] << ", " 
       << mPiMinusCuts[1] << endl;
  cout << "#    Proton cuts=  " << mProtonCuts[0] << ", " 
       << mProtonCuts[1] << endl;
  cout << "#######################################################" << endl;
  cout << "# Track Selections List:" << endl; 
  for (int k = 0; k < Flow::nSels; k++) {
    for (int j = 0; j < Flow::nHars; j++) {
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

