////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.hh,v 1.2 1999/11/24 18:17:11 posk Exp $
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
class StGlobalTrack;
class StFlowTrack;
class StFlowCutTrack {

 public:

               StFlowCutTrack();
  virtual      ~StFlowCutTrack();
  static void  SetFitPts(Int_t lo, Int_t hi);
  static void  SetFitOverMaxPts(Float_t lo, Float_t hi);
  static void  SetEta(Float_t lo, Float_t hi, Int_t selN, Int_t harN);
  static void  SetPt(Float_t lo, Float_t hi, Int_t selN, Int_t harN);
  static Int_t CheckTrack(StGlobalTrack* pTrack);
  static Int_t SelectTrack(StFlowTrack* pFlowTrack, Int_t selN, Int_t harN);
  static void  PrintCutList();
  static Int_t EtaSymPos();
  static Int_t EtaSymNeg();
  static void  EtaSymClear();
  
 private:

  enum {nSels = 2, nHars = 4};

  static UInt_t  mTrackN;                    // number of tracks
  static UInt_t  mGoodTrackN;                // number of accepted tracks   
  static UInt_t  mEtaSymPosN;                // number of positive Eta tracks
  static UInt_t  mEtaSymNegN;                // number of negative Eta tracks
						
  static UInt_t  mFitPtsCutN;                // number not accepted
  static Int_t   mFitPtsCuts[2];             // range

  static UInt_t  mFitOverMaxCutN;            // number not accepted
  static Float_t mFitOverMaxCuts[2];         // range

  static UInt_t  mEtaCutN;                   // number not accepted
  static Float_t mEtaCuts[2][nSels][nHars];  // absolute values of range

  static UInt_t  mPtCutN;                    // number not accepted
  static Float_t mPtCuts[2][nSels][nHars];   // range

  //ClassDef(StFlowCutTrack,1)               // macro for rootcint
}; 

inline void StFlowCutTrack::SetFitPts(Int_t lo, Int_t hi) 
  {mFitPtsCuts[0] = lo; mFitPtsCuts[1] = hi;}

inline void StFlowCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi) 
  {mFitOverMaxCuts[0] = lo; mFitOverMaxCuts[1] = hi;}

inline void StFlowCutTrack::SetEta(Float_t lo, Float_t hi, Int_t selN,
  Int_t harN) {mEtaCuts[0][selN][harN] = lo; mEtaCuts[1][selN][harN] = hi;}

inline void StFlowCutTrack::SetPt(Float_t lo, Float_t hi, Int_t selN,
  Int_t harN) {mPtCuts[0][selN][harN] = lo; mPtCuts[1][selN][harN] = hi;}

inline Int_t StFlowCutTrack::EtaSymPos() {return mEtaSymPosN;}

inline Int_t StFlowCutTrack::EtaSymNeg() {return mEtaSymNegN;}

inline void StFlowCutTrack::EtaSymClear() {mEtaSymPosN = 0; mEtaSymNegN = 0;}

#endif
