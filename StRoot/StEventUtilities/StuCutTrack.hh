////////////////////////////////////////////////////////////////////////////
//
// $Id: StuCutTrack.hh,v 1.3 2003/09/02 17:58:09 perev Exp $
//
// Author: Art Poskanzer, LBNL, Dec 1999
//
// Description:  Class for applying track cuts.
//               If lo >= hi no cut is applied.
//               All functions and data members are static.
//               Therefor, no need to instantiate.
//               Just use StuCutTrack::func();
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StuCutTrack.hh,v $
// Revision 1.3  2003/09/02 17:58:09  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1999/12/21 16:12:26  posk
// Updates.
//
// Revision 1.1  1999/12/17 00:07:09  posk
// Classes for StEvent cuts.
//
// Revision 1.0  posk
// First versions of StEvent cut classes.
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StuCutTrack_INCLUDED_
#define _StuCutTrack_INCLUDED_
#include <Stiostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StPrimaryTrack;

class StuCutTrack {

 public:

                StuCutTrack();
  virtual       ~StuCutTrack();

  static Int_t  CheckTrack(StPrimaryTrack* pTrack);
  static void   PrintCutList();
  static UInt_t EtaSymPos();
  static UInt_t EtaSymNeg();
  static void   EtaSymClear();
  static void   SetFitPts(Int_t lo, Int_t hi);
  static void   SetFitOverMaxPts(Float_t lo, Float_t hi);
  
 private:

  static UInt_t  mTrackN;                    // number of tracks
  static UInt_t  mGoodTrackN;                // number of accepted tracks   
  static UInt_t  mEtaSymPosN;                // number of positive Eta tracks
  static UInt_t  mEtaSymNegN;                // number of negative Eta tracks
						
  static UInt_t  mFitPtsCutN;                // number not accepted
  static Int_t   mFitPtsCuts[2];             // range

  static UInt_t  mFitOverMaxCutN;            // number not accepted
  static Float_t mFitOverMaxCuts[2];         // range

  ClassDef(StuCutTrack,1)               // macro for rootcint
}; 

inline UInt_t StuCutTrack::EtaSymPos() { return mEtaSymPosN; }

inline UInt_t StuCutTrack::EtaSymNeg() { return mEtaSymNegN; }

inline void StuCutTrack::EtaSymClear() { mEtaSymPosN = 0; mEtaSymNegN = 0; }

inline void StuCutTrack::SetFitPts(Int_t lo, Int_t hi) {
  mFitPtsCuts[0] = lo; mFitPtsCuts[1] = hi; }

inline void StuCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi) {
  mFitOverMaxCuts[0] = lo; mFitOverMaxCuts[1] = hi; }

#endif
