////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.2 1999/11/24 18:17:11 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.2  1999/11/24 18:17:11  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/11 23:08:51  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/05 00:06:44  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include "StFlowCutTrack.hh"
#include "StGlobalTrack.h"
#include "StFlowTrack.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#define PR(x) cout << "##### FlowCutTrack: " << (#x) << " = " << (x) << endl;

//ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

enum {nSels = 2, nHars = 4};

Int_t    StFlowCutTrack::mFitPtsCuts[2]     = {10, 200};
Float_t  StFlowCutTrack::mFitOverMaxCuts[2] = {0.6, 1.};

Float_t  StFlowCutTrack::mEtaCuts[2][nSels][nHars] = {{{0.,0.,0.,0.},
							 {0.5,0.,0.5,0.}},
							{{2.,2.,2.,2.},
							 {2.,1.,2.,1.}}};
Float_t  StFlowCutTrack::mPtCuts[2][nSels][nHars] = {{{0.05,0.05,0.05,0.05},
							{0.05,0.05,0.05,0.05}},
						       {{2.,2.,2.,2.},
							{2.,2.,2.,2.}}};
UInt_t   StFlowCutTrack::mTrackN          = 0;     
UInt_t   StFlowCutTrack::mGoodTrackN      = 0;
UInt_t   StFlowCutTrack::mEtaSymPosN      = 0;     
UInt_t   StFlowCutTrack::mEtaSymNegN      = 0;     
UInt_t   StFlowCutTrack::mFitPtsCutN      = 0;
UInt_t   StFlowCutTrack::mFitOverMaxCutN  = 0;
UInt_t   StFlowCutTrack::mEtaCutN         = 0;
UInt_t   StFlowCutTrack::mPtCutN          = 0;

const Double_t bField = 0.5*tesla;

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StGlobalTrack* pTrack) {
  // Returns kTRUE if the track survives all the cuts
  mTrackN++;
  
  // Fit Points
  StTrackFitTraits& fitTraits = pTrack->fitTraits();
  Int_t nFitPoints = fitTraits.numberOfFitPoints();
  if (mFitPtsCuts[1] > mFitPtsCuts[0] && 
      (nFitPoints < mFitPtsCuts[0] || nFitPoints >= mFitPtsCuts[1])) {
    mFitPtsCutN++;
    return kFALSE;
  }
  
  // Fit points / max points
  Int_t nMaxPoints = fitTraits.numberOfPossiblePoints();
  //PR(nMaxPoints);
  //Float_t fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.;
  Float_t fitOverMax = (nMaxPoints) ? (float)nFitPoints/(float)nMaxPoints : 0.8;
  if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
      (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
    mFitOverMaxCutN++;
    return kFALSE;
  }

  // Increment counters for Eta symmetry cut
  StThreeVectorD p = pTrack->helix().momentum(bField); 
  if (p.pseudoRapidity() > 0.) {
    mEtaSymPosN++;
  } else {
    mEtaSymNegN++;
  }

  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::SelectTrack(StFlowTrack* pFlowTrack, Int_t selN, Int_t harN) {
  // Returns kTRUE if the track meets all the selection criteria
  
  // Eta
  Float_t mEta =  pFlowTrack->Eta();
  if (mEtaCuts[1][selN][harN] > mEtaCuts[0][selN][harN] && 
      (fabs(mEta) < mEtaCuts[0][selN][harN] || 
       fabs(mEta) >= mEtaCuts[1][selN][harN])) {
    mEtaCutN++;
    return kFALSE;
  }

  // Pt
  Float_t mPt  =  pFlowTrack->Pt();
  if (mPtCuts[1][selN][harN] > mPtCuts[0][selN][harN] && 
      (mPt < mPtCuts[0][selN][harN] || mPt >= mPtCuts[1][selN][harN])) {
    mPtCutN++;
    return kFALSE;
  }
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish
  cout << "#######################################################" << endl;
  cout << "# Track Selections List:" << endl; 
  for (int k = 0; k < nSels; k++) {
    for (int j = 0; j < nHars; j++) {
      cout << "#  selection= " << k+1 << " harmonic= " 
	   << j+1 << endl;
      cout << "#    abs(Eta) cuts= " << mEtaCuts[0][k][j] << ", " 
	   << mEtaCuts[1][k][j] << endl;
      cout << "#    Pt cuts= " << mPtCuts[0][k][j] << ", "
	   << mPtCuts[1][k][j] << endl;
    }
  }
  cout << "# Track Cut List:" << endl;
  cout << "#   FitPts cuts= " << mFitPtsCuts[0] << ", " << mFitPtsCuts[1] 
       << " :\t " << setprecision(4) << (float)mFitPtsCutN/(float)mTrackN/perCent 
       << "% cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
       << mFitOverMaxCuts[1] << " :\t " << setprecision(4)
       << (float)mFitOverMaxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;
}
