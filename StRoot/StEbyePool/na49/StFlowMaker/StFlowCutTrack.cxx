////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.cxx,v 1.2 2001/05/14 23:04:23 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
// Description:  Class for applying track cuts
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.cxx,v $
// Revision 1.2  2001/05/14 23:04:23  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.17  2000/10/12 22:46:33  snelling
//
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <iomanip.h>
#include <stdlib.h>
#include "StEbyeEvent.h"
#include "StFlowCutTrack.h"
#include "StFlowMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#define PR(x) cout << "##### FlowCutTrack: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCutTrack)

//-----------------------------------------------------------------------

Int_t   StFlowCutTrack::mFitPtsCutsV1[2]   = {0, 0};
Int_t   StFlowCutTrack::mFitPtsCutsV2[2]   = {0, 0};
Int_t   StFlowCutTrack::mFitPtsCutsM[2]    = {0, 0};
Int_t   StFlowCutTrack::mMaxPtsCutsV1[2]   = {20, 200};
Int_t   StFlowCutTrack::mMaxPtsCutsV2[2]   = {20, 200};
Int_t   StFlowCutTrack::mMaxPtsCutsM[2]    = {30, 200};
Float_t StFlowCutTrack::mFitOverMaxCuts[2] = {0.55, 1.1};
Float_t StFlowCutTrack::mChiSqCuts[2]      = {0., 10.};
Float_t StFlowCutTrack::mPtCuts[2]         = {0., 2.};
Float_t StFlowCutTrack::mEtaCuts[2]        = {0., 0.};
Float_t StFlowCutTrack::mBxCuts[2]         = {-3.0, 3.0};
Float_t StFlowCutTrack::mByCuts[2]         = {-0.5, 0.5};

UInt_t  StFlowCutTrack::mTrackN            = 0;     
UInt_t  StFlowCutTrack::mGoodTrackN        = 0;
UInt_t  StFlowCutTrack::mEtaSymPosN        = 0;     
UInt_t  StFlowCutTrack::mEtaSymNegN        = 0;     
UInt_t  StFlowCutTrack::mFitPtsCutN        = 0;
UInt_t  StFlowCutTrack::mMaxPtsCutN        = 0;
UInt_t  StFlowCutTrack::mFitOverMaxCutN    = 0;
UInt_t  StFlowCutTrack::mChiSqCutN         = 0;
UInt_t  StFlowCutTrack::mPtCutN            = 0;
UInt_t  StFlowCutTrack::mEtaCutN           = 0;
UInt_t  StFlowCutTrack::mBxCutN            = 0;
UInt_t  StFlowCutTrack::mByCutN            = 0;
UInt_t  StFlowCutTrack::mFlagCutN          = 0;

//-----------------------------------------------------------------------

StFlowCutTrack::StFlowCutTrack() {
  // To apply track cuts
}

//-----------------------------------------------------------------------

StFlowCutTrack::~StFlowCutTrack() {
}

//-----------------------------------------------------------------------

Int_t StFlowCutTrack::CheckTrack(StEbyeTrack* pMicroTrack) {
  // Returns kTRUE if the track survives all the cuts

  mTrackN++;
  
  // flag
  if(pMicroTrack->Flag() != 0) {
    mFlagCutN++;
    return kFALSE;
  }

  // pt
  float pt = pMicroTrack->Pt();
  if (mPtCuts[1] > mPtCuts[0] && 
      (pt < mPtCuts[0] || pt >= mPtCuts[1])) {
    mPtCutN++;
    return kFALSE;
  }

  // eta
  float eta = pMicroTrack->Eta();
  if (mEtaCuts[1] > mEtaCuts[0] && 
      (eta < mEtaCuts[0] || eta >= mEtaCuts[1])) {
    mEtaCutN++;
    return kFALSE;
  }

  // fit points
  int fitPtsV1 = pMicroTrack->NFitPointsV1();
  int fitPtsV2 = pMicroTrack->NFitPointsV2();
  int fitPtsM = pMicroTrack->NFitPointsM();
  if (mFitPtsCutsV1[1] > mFitPtsCutsV1[0] &&
      mFitPtsCutsV2[1] > mFitPtsCutsV2[0] &&
      mFitPtsCutsM[1] > mFitPtsCutsM[0] && 
      (fitPtsV1 < mFitPtsCutsV1[0] || fitPtsV1 > mFitPtsCutsV1[1]) &&
      (fitPtsV2 < mFitPtsCutsV2[0] || fitPtsV2 > mFitPtsCutsV2[1]) &&
      (fitPtsM < mFitPtsCutsM[0] || fitPtsM > mFitPtsCutsM[1])) {
    mFitPtsCutN++;
    return kFALSE;
  }

  // max points
  int maxPtsV1 = pMicroTrack->NMaxPointsV1();
  int maxPtsV2 = pMicroTrack->NMaxPointsV2();
  int maxPtsM = pMicroTrack->NMaxPointsM();
  if (mMaxPtsCutsV1[1] > mMaxPtsCutsV1[0] &&
      mMaxPtsCutsV2[1] > mMaxPtsCutsV2[0] &&
      mMaxPtsCutsM[1] > mMaxPtsCutsM[0] && 
      (maxPtsV1 < mMaxPtsCutsV1[0] || maxPtsV1 > mMaxPtsCutsV1[1]) &&
      (maxPtsV2 < mMaxPtsCutsV2[0] || maxPtsV2 > mMaxPtsCutsV2[1]) &&
      (maxPtsM < mMaxPtsCutsM[0] || maxPtsM > mMaxPtsCutsM[1])) {
    mMaxPtsCutN++;
    return kFALSE;
  }

  // Fit points / max points
  Int_t fitPts = pMicroTrack->NFitPoints();
  Int_t maxPts = pMicroTrack->NMaxPoints();
  float fitOverMax = (maxPts) ? (float)fitPts/(float)maxPts : 0.0;
  if (mFitOverMaxCuts[1] > mFitOverMaxCuts[0] && 
      (fitOverMax < mFitOverMaxCuts[0] || fitOverMax >= mFitOverMaxCuts[1])) {
    mFitOverMaxCutN++;
    return kFALSE;
  }

  // ChiSq
  float chiSq = pMicroTrack->Chi2();
  if (mChiSqCuts[1] > mChiSqCuts[0] && 
      (chiSq < mChiSqCuts[0] || chiSq >= mChiSqCuts[1])) {
    mChiSqCutN++;
    return kFALSE;
  }

  // bx
  float bx = pMicroTrack->Bx();
  if (mBxCuts[1] > mBxCuts[0] &&
      (bx < mBxCuts[0] || bx > mBxCuts[1])) {
    mBxCutN++;
    return kFALSE;
  }

  // by
  float by = pMicroTrack->By();
  if (mByCuts[1] > mByCuts[0] &&
      (by < mByCuts[0] || by > mByCuts[1])) {
    mByCutN++;
    return kFALSE;
  }

  // Increment counters for Eta symmetry cut
  if (eta > Flow::yCM) { mEtaSymPosN++;
  } else { mEtaSymNegN++; }
  mGoodTrackN++;
  return kTRUE;
}

//-----------------------------------------------------------------------

void StFlowCutTrack::PrintCutList() {
  // Prints the list of cuts
  // Call in Finish

  cout << "#######################################################" << endl;
  cout << "# Track Cut List:" << endl;
  cout << "#   Flag=0 cut :\t\t " << setprecision(3)
       << (float)mFlagCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Pt cuts= " << mPtCuts[0] << ", " 
       << mPtCuts[1] << " :\t\t " << setprecision(3)
       << (float)mPtCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Eta cuts= " << mEtaCuts[0] << ", " 
       << mEtaCuts[1] << " :\t\t " << setprecision(3)
       << (float)mEtaCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   FitPtsV1 cuts= " << mFitPtsCutsV1[0] << ", " << mFitPtsCutsV1[1] << endl;
  cout << "#    FitPtsV2 cuts= " << mFitPtsCutsV2[0] << ", " << mFitPtsCutsV2[1] << endl;
  cout << "#    FitPtsM  cuts= " << mFitPtsCutsM[0] << ", " << mFitPtsCutsM[1] 
       << " :\t " << setprecision(3) << (float)mFitPtsCutN/(float)mTrackN/perCent 
       << "% cut" << endl;
  cout << "#   MaxPtsV1 cuts= " << mMaxPtsCutsV1[0] << ", " << mMaxPtsCutsV1[1] << endl;
  cout << "#    MaxPtsV2 cuts= " << mMaxPtsCutsV2[0] << ", " << mMaxPtsCutsV2[1] << endl;
  cout << "#    MaxPtsM  cuts= " << mMaxPtsCutsM[0] << ", " << mMaxPtsCutsM[1] 
       << " :\t " << setprecision(3) << (float)mMaxPtsCutN/(float)mTrackN/perCent 
       << "% cut" << endl;
  cout << "#   FitOverMax cuts= " << mFitOverMaxCuts[0] << ", " 
       << mFitOverMaxCuts[1] << " :\t " << setprecision(3)
       << (float)mFitOverMaxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   ChiSq cuts= " << mChiSqCuts[0] << ", " 
       << mChiSqCuts[1] << " :\t\t " << setprecision(3)
       << (float)mChiSqCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   Bx cuts= " << mBxCuts[0] << ", " 
       << mBxCuts[1] << " :\t\t " << setprecision(3)
       << (float)mBxCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "#   By cuts= " << mByCuts[0] << ", " 
       << mByCuts[1] << " :\t " << setprecision(3)
       << (float)mByCutN/(float)mTrackN/perCent << "% cut" << endl;
  cout << "# Good Tracks = " << (float)mGoodTrackN/(float)mTrackN/perCent
       << "%" << endl;
  cout << "#######################################################" << endl;

}
