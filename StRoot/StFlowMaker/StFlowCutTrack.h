////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.h,v 1.15 2002/02/13 22:29:17 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Nov 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//
// Description:  Class for applying flow track cuts
//               If lo >= hi no cut is applied
//               All functions and data members are static
//               Therefor, no need to instantiate
//               Just use StFlowCutTrack::func();
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowCutTrack_INCLUDED_
#define _StFlowCutTrack_INCLUDED_
#include <iostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StTrack;
class StFlowPicoTrack;

class StFlowCutTrack {

 public:

                 StFlowCutTrack();
  virtual        ~StFlowCutTrack();

  static Int_t   CheckTrack(StTrack* pTrack);
  static Int_t   CheckTrack(StFlowPicoTrack* pPicoTrack);
  static void    PrintCutList();
  static UInt_t  EtaSymPos();
  static UInt_t  EtaSymNeg();
  static UInt_t  EtaSymTpcPos();
  static UInt_t  EtaSymTpcNeg();
  static UInt_t  EtaSymFtpcPos();
  static UInt_t  EtaSymFtpcNeg();
  static void    EtaSymClear();
  static void    SetFitPtsTpc(Int_t lo, Int_t hi);
  static void    SetFitPtsFtpc(Int_t lo, Int_t hi);
  static void    SetFitOverMaxPts(Float_t lo, Float_t hi);
  static void    SetChiSqTpc(Float_t lo, Float_t hi);
  static void    SetChiSqFtpc(Float_t lo, Float_t hi);
  static void    SetDcaFtpc(Float_t lo, Float_t hi);
  static void    SetPtTpc(Float_t lo, Float_t hi);
  static void    SetPtFtpc(Float_t lo, Float_t hi);
  static void    SetEtaTpc(Float_t lo, Float_t hi);
  static void    SetChgTpc(Int_t lo, Int_t hi);
  static void    SetEtaFtpc(Float_t lo_neg, Float_t hi_neg, Float_t lo_pos, Float_t hi_pos);

 private:

  static UInt_t  mTrackN;                    // number of tracks
  static UInt_t  mTpcTrackN;                 // number of Tpc tracks
  static UInt_t  mFtpcTrackN;                // number of Ftpc tracks
  static UInt_t  mFtpcEastTrackN;            // number of Ftpc east tracks
  static UInt_t  mFtpcWestTrackN;            // number of Ftpc west tracks

  static UInt_t  mGoodTrackN;                // number of accepted tracks   
  static UInt_t  mGoodTpcTrackN;             // number of accepted Tpc tracks   
  static UInt_t  mGoodFtpcTrackN;            // number of accepted Ftpc tracks
   
  static UInt_t  mEtaSymPosN;                // number of positive Eta tracks
  static UInt_t  mEtaSymNegN;                // number of negative Eta tracks
  static UInt_t  mEtaSymPosTpcN;             // number of positive Eta Tpc tracks
  static UInt_t  mEtaSymNegTpcN;             // number of negative Eta Tpc tracks
  static UInt_t  mEtaSymPosFtpcN;            // number of positive Eta Ftpc tracks
  static UInt_t  mEtaSymNegFtpcN;            // number of negative Eta Ftpc tracks
						
  static UInt_t  mFitPtsTpcCutN;             // number not accepted
  static Int_t   mFitPtsTpcCuts[2];          // range

  static UInt_t  mFitPtsFtpcCutN;            // number not accepted
  static Int_t   mFitPtsFtpcCuts[2];         // range

  static UInt_t  mFitOverMaxCutN;            // number not accepted (all)
  static UInt_t  mFitOverMaxTpcCutN;         // number not accepted (Tpc)
  static UInt_t  mFitOverMaxFtpcCutN;        // number not accepted (Ftpc)
  static Float_t mFitOverMaxCuts[2];         // range

  static UInt_t  mChiSqTpcCutN;              // number not accepted
  static Float_t mChiSqTpcCuts[2];           // range

  static UInt_t  mChiSqFtpcCutN;             // number not accepted
  static Float_t mChiSqFtpcCuts[2];          // range

  static UInt_t  mDcaFtpcCutN;               // number not accepted
  static Float_t mDcaFtpcCuts[2];            // range

  static UInt_t  mPtTpcCutN;                 // number not accepted
  static Float_t mPtTpcCuts[2];              // range

  static UInt_t  mPtFtpcCutN;                // number not accepted
  static Float_t mPtFtpcCuts[2];             // range

  static UInt_t  mEtaTpcCutN;                // number not accepted
  static Float_t mEtaTpcCuts[2];             // range

  static UInt_t  mChgTpcCutN;                // number not accepted
  static Int_t   mChgTpcCuts[2];             // range

  static UInt_t  mEtaFtpcCutN;               // number not accepted
  static Float_t mEtaFtpcCuts[4];            // range

  ClassDef(StFlowCutTrack,1)                 // macro for rootcint
}; 

inline UInt_t StFlowCutTrack::EtaSymPos() { return mEtaSymPosN; }

inline UInt_t StFlowCutTrack::EtaSymNeg() { return mEtaSymNegN; }

inline void StFlowCutTrack::EtaSymClear() { mEtaSymPosN = 0; mEtaSymNegN = 0; }

inline void StFlowCutTrack::SetFitPtsTpc(Int_t lo, Int_t hi) {
  mFitPtsTpcCuts[0] = lo; mFitPtsTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetFitPtsFtpc(Int_t lo, Int_t hi) {
  mFitPtsFtpcCuts[0] = lo; mFitPtsFtpcCuts[1] = hi; }

inline void StFlowCutTrack::SetFitOverMaxPts(Float_t lo, Float_t hi) {
  mFitOverMaxCuts[0] = lo; mFitOverMaxCuts[1] = hi; }

inline void StFlowCutTrack::SetChiSqTpc(Float_t lo, Float_t hi) {
  mChiSqTpcCuts[0] = lo; mChiSqTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetChiSqFtpc(Float_t lo, Float_t hi) {
  mChiSqFtpcCuts[0] = lo; mChiSqFtpcCuts[1] = hi; }

inline void StFlowCutTrack::SetDcaFtpc(Float_t lo, Float_t hi) {
  mDcaFtpcCuts[0] = lo; mDcaFtpcCuts[1] = hi; }

inline void StFlowCutTrack::SetPtTpc(Float_t lo, Float_t hi) {
  mPtTpcCuts[0] = lo; mPtTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetPtFtpc(Float_t lo, Float_t hi) {
  mPtFtpcCuts[0] = lo; mPtFtpcCuts[1] = hi; }

inline void StFlowCutTrack::SetEtaTpc(Float_t lo, Float_t hi) {
  mEtaTpcCuts[0] = lo; mEtaTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetChgTpc(Int_t lo, Int_t hi) {
  mChgTpcCuts[0] = lo; mChgTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetEtaFtpc(Float_t lo_neg, Float_t hi_neg,
				       Float_t lo_pos, Float_t hi_pos) {
  mEtaFtpcCuts[0] = lo_neg; mEtaFtpcCuts[1] = hi_neg; 
  mEtaFtpcCuts[2] = lo_pos; mEtaFtpcCuts[3] = hi_pos;}

#endif

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.h,v $
// Revision 1.15  2002/02/13 22:29:17  posk
// Pt Weight now also weights Phi Weights. Added Eta Weight, default=FALSE.
//
// Revision 1.14  2002/01/31 21:43:14  aihong
// add SetChgTpc()
//
// Revision 1.13  2001/11/09 21:10:34  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.12  2001/07/27 01:26:10  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.11  2001/05/22 20:17:23  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.10  2000/12/12 20:22:05  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.9  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.7  2000/12/08 17:03:38  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.6  2000/12/06 15:38:46  oldi
// Including FTPC.
//
// Revision 1.5  2000/10/12 22:46:35  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.4  2000/08/31 18:58:20  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
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
// Revision 1.4  1999/12/15 22:01:24  posk
// Added StFlowConstants.hh
//
// Revision 1.3  1999/11/30 18:52:50  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:11  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/05 00:06:45  posk
// First versions of Flow cut classes.
//
////////////////////////////////////////////////////////////////////////////
