////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.h,v 1.1 2001/02/23 00:50:52 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
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
// Revision 1.1  2001/02/23 00:50:52  posk
// NA49 version of STAR software.
//
// Revision 1.5  2000/10/12 22:46:35  snelling
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowCutTrack_INCLUDED_
#define _StFlowCutTrack_INCLUDED_
#include <iostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StEbyeTrack;

class StFlowCutTrack {

 public:

                 StFlowCutTrack();
  virtual        ~StFlowCutTrack();

  static Int_t   CheckTrack(StEbyeTrack* pMicroTrack);
  static void    PrintCutList();
  static UInt_t  EtaSymPos();
  static UInt_t  EtaSymNeg();
  static void    EtaSymClear();
  static void    SetFitPtsV1(Int_t lo, Int_t hi);
  static void    SetFitPtsV2(Int_t lo, Int_t hi);
  static void    SetFitPtsM(Int_t lo, Int_t hi);
  static void    SetMaxPtsV1(Int_t lo, Int_t hi);
  static void    SetMaxPtsV2(Int_t lo, Int_t hi);
  static void    SetMaxPtsM(Int_t lo, Int_t hi);
  static void    SetFitOverMaxPts(Float_t lo, Float_t hi);
  static void    SetChiSq(Float_t lo, Float_t hi);
  static void    SetPt(Float_t lo, Float_t hi);
  static void    SetEta(Float_t lo, Float_t hi);
  static void    SetBx(Float_t lo, Float_t hi);
  static void    SetBy(Float_t lo, Float_t hi);

 private:

  static UInt_t  mTrackN;                    // number of tracks
  static UInt_t  mGoodTrackN;                // number of accepted tracks   
  static UInt_t  mEtaSymPosN;                // number of positive Eta tracks
  static UInt_t  mEtaSymNegN;                // number of negative Eta tracks
						
  static UInt_t  mFitPtsCutN;                // number not accepted
  static Int_t   mFitPtsCutsV1[2];           // range
  static Int_t   mFitPtsCutsV2[2];           // range
  static Int_t   mFitPtsCutsM[2];            // range

  static UInt_t  mMaxPtsCutN;                // number not accepted
  static Int_t   mMaxPtsCutsV1[2];           // range
  static Int_t   mMaxPtsCutsV2[2];           // range
  static Int_t   mMaxPtsCutsM[2];            // range

  static UInt_t  mFitOverMaxCutN;            // number not accepted
  static Float_t mFitOverMaxCuts[2];         // range

  static UInt_t  mChiSqCutN;                 // number not accepted
  static Float_t mChiSqCuts[2];              // range

  static UInt_t  mPtCutN;                    // number not accepted
  static Float_t mPtCuts[2];                 // range

  static UInt_t  mEtaCutN;                   // number not accepted
  static Float_t mEtaCuts[2];                // range

  static UInt_t  mBxCutN;                    // number not accepted
  static Float_t mBxCuts[2];                 // range

  static UInt_t  mByCutN;                    // number not accepted
  static Float_t mByCuts[2];                 // range

  static UInt_t  mFlagCutN;                  // number not accepted

  ClassDef(StFlowCutTrack,1)                 // macro for rootcint
}; 

inline UInt_t StFlowCutTrack::EtaSymPos() { return mEtaSymPosN; }

inline UInt_t StFlowCutTrack::EtaSymNeg() { return mEtaSymNegN; }

inline void StFlowCutTrack::EtaSymClear() { mEtaSymPosN = 0; mEtaSymNegN = 0; }

inline void StFlowCutTrack::SetFitPtsV1(Int_t lo, Int_t hi) {
  mFitPtsCutsV1[0] = lo; mFitPtsCutsV1[1] = hi; }

inline void StFlowCutTrack::SetFitPtsV2(Int_t lo, Int_t hi) {
  mFitPtsCutsV2[0] = lo; mFitPtsCutsV2[1] = hi; }

inline void StFlowCutTrack::SetFitPtsM(Int_t lo, Int_t hi) {
  mFitPtsCutsM[0] = lo; mFitPtsCutsM[1] = hi; }

inline void StFlowCutTrack::SetMaxPtsV1(Int_t lo, Int_t hi) {
  mMaxPtsCutsV1[0] = lo; mMaxPtsCutsV1[1] = hi; }

inline void StFlowCutTrack::SetMaxPtsV2(Int_t lo, Int_t hi) {
  mMaxPtsCutsV2[0] = lo; mMaxPtsCutsV2[1] = hi; }

inline void StFlowCutTrack::SetMaxPtsM(Int_t lo, Int_t hi) {
  mMaxPtsCutsM[0] = lo; mMaxPtsCutsM[1] = hi; }

inline void StFlowCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi) {
  mFitOverMaxCuts[0] = lo; mFitOverMaxCuts[1] = hi; }

inline void StFlowCutTrack::SetChiSq(Float_t lo, Float_t hi) {
  mChiSqCuts[0] = lo; mChiSqCuts[1] = hi; }

inline void StFlowCutTrack::SetPt(Float_t lo, Float_t hi) {
  mPtCuts[0] = lo; mPtCuts[1] = hi; }

inline void StFlowCutTrack::SetEta(Float_t lo, Float_t hi) {
  mEtaCuts[0] = lo; mEtaCuts[1] = hi; }

inline void StFlowCutTrack::SetBx(Float_t lo, Float_t hi) {
  mBxCuts[0] = lo; mBxCuts[1] = hi; }

inline void StFlowCutTrack::SetBy(Float_t lo, Float_t hi) {
  mByCuts[0] = lo; mByCuts[1] = hi; }

#endif
