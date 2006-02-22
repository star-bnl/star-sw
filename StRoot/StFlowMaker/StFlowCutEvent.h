////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCutEvent.h,v 1.14 2006/02/22 19:27:04 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings, LBNL, Oct 1999
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          MuDst enabled by Kirill Filimonov, LBNL, Jun 2002
//
// Description:  Class for applying flow event cuts
//               If lo >= hi no cut is applied
//               All functions and data members are static
//               Therefore, no need to instantiate
//               Just use StFlowCutEvent::func();
//
////////////////////////////////////////////////////////////////////////////

#ifndef _StFlowCutEvent_INCLUDED_
#define _StFlowCutEvent_INCLUDED_
#include <Stiostream.h>
#include <stdlib.h>
#include "Rtypes.h"
class StEvent;
class StFlowPicoEvent;
class StMuEvent;
class StMuDst;

class StFlowCutEvent {

 public:

               StFlowCutEvent();
  virtual      ~StFlowCutEvent();

  static Bool_t CheckEvent(StEvent* pEvent);
  static Bool_t CheckEvent(StFlowPicoEvent* pPicoEvent);
  static Bool_t CheckEvent(StMuDst* pMu);
  static Bool_t CheckEtaSymmetry(StEvent* pEvent);
  static Bool_t CheckEtaSymmetry(StFlowPicoEvent* pPicoEvent);
  static Bool_t CheckEtaSymmetry(StMuEvent* pMuEvent);
  static void   PrintCutList();
  static void   SetCent(const Int_t lo, const Int_t hi);
  static void   SetMult(const Int_t lo, const Int_t hi);
  static void   SetVertexX(const Float_t lo, const Float_t hi);
  static void   SetVertexY(const Float_t lo, const Float_t hi);
  static void   SetVertexZ(const Float_t lo, const Float_t hi);
  static void   SetEtaSymTpc(Float_t lo, Float_t hi);
  static void   SetEtaSymFtpc(Float_t lo, Float_t hi);
  static void   SetTrigger(const UInt_t value);   
  static UInt_t TriggersFound();
  static UInt_t GetFlowTriggerBitMap();

 private:

  static UInt_t  mEventN;                // number of events
  static UInt_t  mGoodEventN;            // number of accepted events   
						
  static UInt_t  mCentCut;               // number of not accepted events
  static Int_t   mCentCuts[2];           // range of multiplicity

  static UInt_t  mMultCut;               // number of not accepted events
  static Int_t   mMultCuts[2];           // range of multiplicity

  static UInt_t  mVertexXCut;            // number of not accepted events
  static Float_t mVertexXCuts[2];        // range of X vertex position

  static UInt_t  mVertexYCut;            // number of not accepted events
  static Float_t mVertexYCuts[2];        // range of Y vertex position

  static UInt_t  mVertexZCut;            // number of not accepted events
  static Float_t mVertexZCuts[2];        // range of Z vertex position

  static UInt_t  mEtaSymTpcCutN;         // number not accepted
  static Float_t mEtaSymTpcCuts[2];      // range

  static UInt_t  mEtaSymFtpcCutN;        // number not accepted
  static Float_t mEtaSymFtpcCuts[2];     // range

  static UInt_t  mTriggerCutN;           // number not accepted
  static UInt_t  mTriggerCut;            // allowed trigger value

  static UInt_t  mTriggersFound;
  static UInt_t  flowTriggerBitMap;

  ClassDef(StFlowCutEvent,1)             // macro for rootcint
}; 

inline void StFlowCutEvent::SetCent(const Int_t lo, const Int_t hi) {
  mCentCuts[0] = lo; mCentCuts[1] = hi; }

inline void StFlowCutEvent::SetMult(const Int_t lo, const Int_t hi) {
  mMultCuts[0] = lo; mMultCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexX(const Float_t lo, const Float_t hi) {
  mVertexXCuts[0] = lo; mVertexXCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexY(const Float_t lo, const Float_t hi) {
  mVertexYCuts[0] = lo; mVertexYCuts[1] = hi; }

inline void StFlowCutEvent::SetVertexZ(const Float_t lo, const Float_t hi) {
  mVertexZCuts[0] = lo; mVertexZCuts[1] = hi; }

inline void StFlowCutEvent::SetEtaSymTpc(Float_t lo, Float_t hi) {
  mEtaSymTpcCuts[0] = lo; mEtaSymTpcCuts[1] = hi; }

inline void StFlowCutEvent::SetEtaSymFtpc(Float_t lo, Float_t hi) {
  mEtaSymFtpcCuts[0] = lo; mEtaSymFtpcCuts[1] = hi; }

inline void StFlowCutEvent::SetTrigger(const UInt_t value) {
  mTriggerCut = value; }

inline UInt_t StFlowCutEvent::TriggersFound() { 
  return mTriggersFound; }

inline UInt_t StFlowCutEvent::GetFlowTriggerBitMap() {
  return flowTriggerBitMap; }

#endif

////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCutEvent.h,v $
// Revision 1.14  2006/02/22 19:27:04  posk
// Changes needed for the MuDst
// Stopped using eventSummary()
//
// Revision 1.13  2005/02/11 23:22:12  posk
// Made TriggersFound() work for pico files.
//
// Revision 1.12  2005/02/08 20:57:36  psoren
// trigger and centrality selections were updated for all runs after run 4 to be compatible with trigger collections. Added TriggersFound() and GetFlowTriggerBitMap() functions.
//
// Revision 1.11  2003/09/02 17:58:11  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.10  2003/01/10 16:41:56  oldi
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
// Revision 1.9  2002/06/10 22:50:57  posk
// pt and eta weighting now default.
// DcaGlobalPart default now 0 to 1 cm.
// Event cut order changed.
//
// Revision 1.8  2002/06/07 22:18:38  kirill
// Introduced MuDst reader
//
// Revision 1.7  2002/01/30 13:04:20  oldi
// Trigger cut implemented.
//
// Revision 1.6  2000/12/12 20:22:05  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.5  2000/08/31 18:58:19  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.4  2000/07/12 17:54:34  posk
// Added chi2 and dca cuts. Multiplied EtaSym by ::sqrt(mult).
// Apply cuts when reading picoevent file.
//
// Revision 1.3  2000/06/30 14:48:30  posk
// Using MessageMgr, changed Eta Symmetry cut.
//
// Revision 1.2  2000/05/26 21:29:26  posk
// Protected Track data members from overflow.
//
// Revision 1.1  2000/03/02 23:02:40  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.4  1999/12/15 22:01:23  posk
// Added StFlowConstants.hh
//
// Revision 1.3  1999/11/30 18:52:48  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:10  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/05 00:06:42  posk
// First versions of Flow cut classes.
//
////////////////////////////////////////////////////////////////////////////
