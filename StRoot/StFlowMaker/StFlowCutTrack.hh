////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.hh,v 1.1 1999/11/11 23:08:52 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Nov 1999
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
// Revision 1.1  1999/11/11 23:08:52  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/05 00:06:45  posk
// First versions of Flow cut classes.
//
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowCutTrack_INCLUDED_
#define _StFlowCutTrack_INCLUDED_
#include <iostream.h>
#include <stdlib.h>
#include "Rtypes.h"
//#include "StFlowMaker.hh"
class StGlobalTrack;
class StFlowTrack;

class StFlowCutTrack {

 public:

               StFlowCutTrack();
  virtual      ~StFlowCutTrack();
  static void  SetFitPts(Int_t lo, Int_t hi);
  static void  SetFitOverMaxPts(Float_t lo, Float_t hi);
  static void  SetEta(Float_t lo, Float_t hi, Int_t eventN, Int_t harN);
  static void  SetPt(Float_t lo, Float_t hi, Int_t eventN, Int_t harN);
  static void  SetRL(Float_t lo, Float_t hi);
  static Int_t CheckTrack(StGlobalTrack* mTrack);
  static Int_t SelectTrack(StFlowTrack* mTrack, Int_t eventN, Int_t harN);
  static Int_t CheckEvent();
  static void  PrintCutList();
  
 private:

  enum {nEvents = 2, nHars = 4};

  static UInt_t  mTrackN;                        // number of tracks
  static UInt_t  mGoodTrackN;                    // number of accepted tracks   
  static UInt_t  mTrackRN;                       // number of right tracks
  static UInt_t  mTrackLN;                       // number of left tracks
						
  static UInt_t  mFitPtsCutN;                    // number not accepted
  static Int_t   mFitPtsCuts[2];                 // range

  static UInt_t  mFitOverMaxCutN;                // number not accepted
  static Float_t mFitOverMaxCuts[2];             // range

  static UInt_t  mRLCutN;                        // number not accepted
  static Float_t mRLCuts[2];                     // range

  static UInt_t  mEtaCutN;                       // number not accepted
  static Float_t mEtaCuts[2][nEvents][nHars];    // range

  static UInt_t  mPtCutN;                        // number not accepted
  static Float_t mPtCuts[2][nEvents][nHars];     // range

  //ClassDef(StFlowCutTrack,1)                     // macro for rootcint
}; 

inline void StFlowCutTrack::SetFitPts(Int_t lo, Int_t hi) 
  {mFitPtsCuts[0] = lo; mFitPtsCuts[1] = hi;}

inline void StFlowCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi) 
  {mFitOverMaxCuts[0] = lo; mFitOverMaxCuts[1] = hi;}

inline void StFlowCutTrack::SetEta(Float_t lo, Float_t hi, Int_t eventN,
  Int_t harN) {mEtaCuts[0][eventN][harN] = lo; mEtaCuts[1][eventN][harN] = hi;}

inline void StFlowCutTrack::SetPt(Float_t lo, Float_t hi, Int_t eventN,
  Int_t harN) {mPtCuts[0][eventN][harN] = lo; mPtCuts[1][eventN][harN] = hi;}

inline void StFlowCutTrack::SetRL(Float_t lo, Float_t hi) 
  {mRLCuts[0] = lo; mRLCuts[1] = hi;}

#endif
