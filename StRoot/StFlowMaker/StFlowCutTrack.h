////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutTrack.h,v 1.20 2003/09/02 17:58:11 perev Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Nov 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          MuDst enabled by Kirill Filimonov, LBNL, Jun 2002
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
#include <Stiostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StTrack;
class StFlowPicoTrack;
class StMuTrack;

class StFlowCutTrack {

 public:

                 StFlowCutTrack();
  virtual        ~StFlowCutTrack();

  static Int_t   CheckTrack(StTrack* pTrack);
  static Int_t   CheckTrack(StFlowPicoTrack* pPicoTrack);
  static Int_t   CheckTrack(StMuTrack* pMuTrack);
  static void    PrintCutList();
  static UInt_t  EtaSymPosTpc();
  static UInt_t  EtaSymNegTpc();
  static UInt_t  EtaSymPosFtpc();
  static UInt_t  EtaSymNegFtpc();
  static void    EtaSymClear();
  static void    SetFitPtsTpc(Int_t lo, Int_t hi);
  static void    SetFitPtsFtpc(Int_t lo, Int_t hi);
  static void    SetFitOverMaxPts(Float_t lo, Float_t hi);
  static void    SetChiSqTpc(Float_t lo, Float_t hi);
  static void    SetChiSqFtpc(Float_t lo, Float_t hi);
  static void    SetDcaFtpc(Float_t lo, Float_t hi);
  static void    SetDcaGlobalFtpc(Float_t lo, Float_t hi);
  static void    SetPtTpc(Float_t lo, Float_t hi);
  static void    SetPtFtpc(Float_t lo, Float_t hi);
  static void    SetEtaTpc(Float_t lo, Float_t hi);
  static void    SetEtaFtpc(Float_t lo_neg, Float_t hi_neg, Float_t lo_pos, Float_t hi_pos);
  static void    SetChgTpc(Int_t lo, Int_t hi);
  static void    SetChgFtpc(Int_t lo, Int_t hi);
  static void    IncludeTpcTracks(Bool_t tpc_include);
  static void    IncludeFtpcTracks(Bool_t ftpc_include);

 private:

  static UInt_t  mTrackN;                    // number of tracks
  static UInt_t  mTpcTrackN;                 // number of Tpc tracks
  static UInt_t  mFtpcTrackN;                // number of Ftpc tracks
  static UInt_t  mFtpcEastTrackN;            // number of Ftpc east tracks
  static UInt_t  mFtpcWestTrackN;            // number of Ftpc west tracks

  static UInt_t  mGoodTrackN;                // number of accepted tracks   
  static UInt_t  mGoodTpcTrackN;             // number of accepted Tpc tracks   
  static UInt_t  mGoodFtpcTrackN;            // number of accepted Ftpc tracks
   
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

  static UInt_t  mDcaGlobalFtpcCutN;         // number not accepted
  static Float_t mDcaGlobalFtpcCuts[2];      // range

  static UInt_t  mPtTpcCutN;                 // number not accepted
  static Float_t mPtTpcCuts[2];              // range

  static UInt_t  mPtFtpcCutN;                // number not accepted
  static Float_t mPtFtpcCuts[2];             // range

  static UInt_t  mEtaTpcCutN;                // number not accepted
  static Float_t mEtaTpcCuts[2];             // range

  static UInt_t  mEtaFtpcCutN;               // number not accepted
  static Float_t mEtaFtpcCuts[4];            // range

  static UInt_t  mChgTpcCutN;                // number not accepted
  static Int_t   mChgTpcCuts[2];             // range

  static UInt_t  mChgFtpcCutN;               // number not accepted
  static Int_t   mChgFtpcCuts[2];            // range

  static UInt_t  mTpcTrackCutN;              // number not accepted
  static Bool_t  mTpcTrackCut;               // cut or don't cut (kTRUE, kFALSE)

  static UInt_t  mFtpcTrackCutN;             // number not accepted
  static Bool_t  mFtpcTrackCut;              // cut or don't cut (kTRUE, kFALSE)

  ClassDef(StFlowCutTrack,1)                 // macro for rootcint
}; 

inline UInt_t StFlowCutTrack::EtaSymPosTpc() { return mEtaSymPosTpcN; }

inline UInt_t StFlowCutTrack::EtaSymNegTpc() { return mEtaSymNegTpcN; }

inline UInt_t StFlowCutTrack::EtaSymPosFtpc() { return mEtaSymPosFtpcN; }

inline UInt_t StFlowCutTrack::EtaSymNegFtpc() { return mEtaSymNegFtpcN; }

inline void StFlowCutTrack::EtaSymClear() { mEtaSymPosTpcN = 0; mEtaSymNegTpcN = 0; 
					    mEtaSymPosFtpcN = 0; mEtaSymNegFtpcN = 0; }

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

inline void StFlowCutTrack::SetDcaGlobalFtpc(Float_t lo, Float_t hi) {
  mDcaGlobalFtpcCuts[0] = lo; mDcaGlobalFtpcCuts[1] = hi; }

inline void StFlowCutTrack::SetPtTpc(Float_t lo, Float_t hi) {
  mPtTpcCuts[0] = lo; mPtTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetPtFtpc(Float_t lo, Float_t hi) {
  mPtFtpcCuts[0] = lo; mPtFtpcCuts[1] = hi; }

inline void StFlowCutTrack::SetEtaTpc(Float_t lo, Float_t hi) {
  mEtaTpcCuts[0] = lo; mEtaTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetEtaFtpc(Float_t lo_neg, Float_t hi_neg,
				       Float_t lo_pos, Float_t hi_pos) {
  mEtaFtpcCuts[0] = lo_neg; mEtaFtpcCuts[1] = hi_neg; 
  mEtaFtpcCuts[2] = lo_pos; mEtaFtpcCuts[3] = hi_pos; }

inline void StFlowCutTrack::SetChgTpc(Int_t lo, Int_t hi) {
  mChgTpcCuts[0] = lo; mChgTpcCuts[1] = hi; }

inline void StFlowCutTrack::SetChgFtpc(Int_t lo, Int_t hi) {
  mChgFtpcCuts[0] = lo; mChgFtpcCuts[1] = hi; }

inline void StFlowCutTrack::IncludeTpcTracks(Bool_t tpc_include) {
    mTpcTrackCut = !tpc_include; }

inline void StFlowCutTrack::IncludeFtpcTracks(Bool_t ftpc_include) {
    mFtpcTrackCut = !ftpc_include; }

#endif

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutTrack.h,v $
// Revision 1.20  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.19  2003/01/14 14:14:16  oldi
// Possibility to exclude TPC tracks completely (= FTPC only).
//
// Revision 1.18  2003/01/10 16:42:05  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.17  2002/06/10 22:50:59  posk
// pt and eta weighting now default.
// DcaGlobalPart default now 0 to 1 cm.
// Event cut order changed.
//
// Revision 1.16  2002/06/07 22:18:40  kirill
// Introduced MuDst reader
//
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
// Added chi2 and dca cuts. Multiplied EtaSym by ::sqrt(mult).
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
