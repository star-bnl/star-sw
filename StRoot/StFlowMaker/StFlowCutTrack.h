////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.h,v 1.3 2000/08/10 23:00:21 posk Exp $
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
// $Log: StFlowCutTrack.h,v $
// Revision 1.3  2000/08/10 23:00:21  posk
// New centralities. pt and eta cuts.
//
// Revision 1.2  2000/07/12 17:54:36  posk
// Added chi2 and dca cuts. Multiplied EtaSym by sqrt(mult).
// Apply cuts when reading picoevent file.
//
// Revision 1.1  2000/03/02 23:02:43  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.5  1999/12/21 01:10:57  posk
// Added more quantities to StFlowEvent.
//
// Revision 1.4  1999/12/15 22:01:24  posk
// Added StFlowConstants.hh
//
// Revision 1.3  1999/11/30 18:52:50  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:11  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
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
class StPrimaryTrack;
class StFlowPicoTrack;

class StFlowCutTrack {

 public:

                StFlowCutTrack();
  virtual       ~StFlowCutTrack();

  static Int_t  CheckTrack(StPrimaryTrack* pTrack);
  static Int_t  CheckTrack(StFlowPicoTrack* pPicoTrack);
  static void   PrintCutList();
  static UInt_t EtaSymPos();
  static UInt_t EtaSymNeg();
  static void   EtaSymClear();
  static void   SetFitPts(Int_t lo, Int_t hi);
  static void   SetFitOverMaxPts(Float_t lo, Float_t hi);
  static void   SetChiSq(Float_t lo, Float_t hi);
  static void   SetDca(Float_t lo, Float_t hi);
  static void   SetPt(Float_t lo, Float_t hi);
  static void   SetEta(Float_t lo, Float_t hi);
  
 private:

  static UInt_t  mTrackN;                    // number of tracks
  static UInt_t  mGoodTrackN;                // number of accepted tracks   
  static UInt_t  mEtaSymPosN;                // number of positive Eta tracks
  static UInt_t  mEtaSymNegN;                // number of negative Eta tracks
						
  static UInt_t  mFitPtsCutN;                // number not accepted
  static Int_t   mFitPtsCuts[2];             // range

  static UInt_t  mFitOverMaxCutN;            // number not accepted
  static Float_t mFitOverMaxCuts[2];         // range

  static UInt_t  mChiSqCutN;                 // number not accepted
  static Float_t mChiSqCuts[2];              // range

  static UInt_t  mDcaCutN;                   // number not accepted
  static Float_t mDcaCuts[2];                // range

  static UInt_t  mPtCutN;                   // number not accepted
  static Float_t mPtCuts[2];                // range

  static UInt_t  mEtaCutN;                   // number not accepted
  static Float_t mEtaCuts[2];                // range

  ClassDef(StFlowCutTrack,1)                 // macro for rootcint
}; 

inline UInt_t StFlowCutTrack::EtaSymPos() { return mEtaSymPosN; }

inline UInt_t StFlowCutTrack::EtaSymNeg() { return mEtaSymNegN; }

inline void StFlowCutTrack::EtaSymClear() { mEtaSymPosN = 0; mEtaSymNegN = 0; }

inline void StFlowCutTrack::SetFitPts(Int_t lo, Int_t hi) {
  mFitPtsCuts[0] = lo; mFitPtsCuts[1] = hi; }

inline void StFlowCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi) {
  mFitOverMaxCuts[0] = lo; mFitOverMaxCuts[1] = hi; }

inline void StFlowCutTrack::SetChiSq(Float_t lo, Float_t hi) {
  mChiSqCuts[0] = lo; mChiSqCuts[1] = hi; }

inline void StFlowCutTrack::SetDca(Float_t lo, Float_t hi) {
  mDcaCuts[0] = lo; mDcaCuts[1] = hi; }

inline void StFlowCutTrack::SetPt(Float_t lo, Float_t hi) {
  mPtCuts[0] = lo; mPtCuts[1] = hi; }

inline void StFlowCutTrack::SetEta(Float_t lo, Float_t hi) {
  mEtaCuts[0] = lo; mEtaCuts[1] = hi; }

#endif
