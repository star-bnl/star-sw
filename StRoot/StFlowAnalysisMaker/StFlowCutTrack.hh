////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.hh,v 1.1 1999/11/05 00:06:45 posk Exp $
//
// Author: Art Poskanzer, LBNL, Nov 1999
//
// Description:  Class for applying flow track cuts
//               If lo >= hi no cut is applied
//               All functions and data members are static
//               Therefor, no need to instantiate
//               Just use StFlowCutTrack::func();
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.hh,v $
// Revision 1.1  1999/11/05 00:06:45  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////
#ifndef _StFlowCutTrack_INCLUDED_
#define _StFlowCutTrack_INCLUDED_

#include <iostream.h>
#include <stdlib.h>
#include "StFlowAnalysisMaker.h"

class StFlowCutTrack {

 public:

               StFlowCutTrack();
               ~StFlowCutTrack();
  static void  SetFitPts(Int_t lo, Int_t hi, Int_t eventN, Int_t harN);
  static void  SetFitOverMaxPts(Float_t lo, Float_t hi, Int_t eventN, Int_t harN);
  static void  SetEta(Float_t lo, Float_t hi, Int_t eventN, Int_t harN);
  static void  SetPt(Float_t lo, Float_t hi, Int_t eventN, Int_t harN);
  static void  SetRL(Float_t lo, Float_t hi);
  static Int_t CheckTrack(StGlobalTrack* mTrack, Int_t eventN, Int_t harN);
  static Int_t CheckEvent();
  static void  PrintCutList();
  static void  PrintCutSurvival();
  
 private:

  enum {nEvents = 2, nHars = 4};

  static UInt_t  mTrackN;                        // number of tracks
  static UInt_t  mGoodTrackN;                    // number of accepted tracks   
  static UInt_t  mTrackRN;                       // number of right tracks
  static UInt_t  mTrackLN;                       // number of left tracks
						
  static UInt_t  mFitPtsCutN;                    // number not accepted
  static Int_t   mFitPtsCuts[2][nEvents][nHars]; // range

  static UInt_t  mFitOverMaxCutN;                // number not accepted
  static Float_t mFitOverMaxCuts[2][nEvents][nHars]; // range

  static UInt_t  mEtaCutN;                       // number not accepted
  static Float_t mEtaCuts[2][nEvents][nHars];    // range

  static UInt_t  mPtCutN;                        // number not accepted
  static Float_t mPtCuts[2][nEvents][nHars];     // range

  static UInt_t  mRLCutN;                        // number not accepted
  static Float_t mRLCuts[2];                     // range

  ClassDef(StFlowCutTrack,1)                     // macro for rootcint
}; 

inline void StFlowCutTrack::SetFitPts(Int_t lo, Int_t hi,Int_t eventN,Int_t harN) 
  {mFitPtsCuts[0][eventN][harN] = lo; mFitPtsCuts[1][eventN][harN] = hi;}

inline void StFlowCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi, Int_t eventN,
  Int_t harN) {mFitOverMaxCuts[0][eventN][harN] = lo; 
  mFitOverMaxCuts[1][eventN][harN] = hi;}

inline void StFlowCutTrack::SetEta(Float_t lo, Float_t hi, Int_t eventN,
  Int_t harN) {mEtaCuts[0][eventN][harN] = lo; mEtaCuts[1][eventN][harN] = hi;}

inline void StFlowCutTrack::SetPt(Float_t lo, Float_t hi, Int_t eventN,
  Int_t harN) {mPtCuts[0][eventN][harN] = lo; mPtCuts[1][eventN][harN] = hi;}

inline void StFlowCutTrack::SetRL(Float_t lo, Float_t hi) 
  {mRLCuts[0] = lo; mRLCuts[1] = hi;}

#endif
