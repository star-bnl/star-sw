//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.h,v 1.34 2001/12/18 19:22:09 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//          FTPC added by Markus Oldenburg, MPI, Dec 2000
//          Cumulants added by Aihong Tang, KSU, Nov 2001
//
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent with flow functions
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowEvent_h
#define StFlowEvent_h
#include "StObject.h"
#include "StFlowTrackCollection.h"
#include "StTrackTopologyMap.h"
#include "StThreeVectorF.hh"
#include "StEnumerations.h"
#include "Rtypes.h"
#include "TVector2.h"
class StFlowSelection;

class StFlowEvent  : public StObject {

public:

  StFlowEvent();
  virtual        ~StFlowEvent();

  Double_t       PhiWeight(Int_t selN, Int_t harN, StFlowTrack* pFlowTrack) const;
  Int_t          EventID() const;
  Int_t          RunID() const;
  Double_t       CenterOfMassEnergy() const;
  Short_t        BeamMassNumberEast() const;
  Short_t        BeamMassNumberWest() const;
  UInt_t         OrigMult() const;
  UInt_t         L0TriggerWord() const;
  UInt_t         UncorrNegMult() const;
  UInt_t         UncorrPosMult() const;
  UInt_t         MultEta() const;
  UInt_t         FlowEventMult() const;
  UInt_t         Centrality() const;
  StThreeVectorF VertexPos() const;
  UInt_t         Mult(StFlowSelection*);
  TVector2       Q(StFlowSelection*);
  Float_t        q(StFlowSelection*);
  Float_t        MeanPt(StFlowSelection*);
  Float_t        Psi(StFlowSelection*);
  Double_t       G_New(StFlowSelection* pFlowSelect, Double_t Zx, Double_t Zy);
  Double_t       G_Old(StFlowSelection* pFlowSelect, Double_t Zx, Double_t Zy);
  Double_t       SumWeightSquare(StFlowSelection* pFlowSelect);
  Double_t       WgtMult_q4(StFlowSelection* pFlowSelect);
  Double_t       WgtMult_q6(StFlowSelection* pFlowSelect);
  TVector2       NormQ(StFlowSelection* pFlowSelect);
  Float_t        CTB() const;
  Float_t        ZDCe() const;
  Float_t        ZDCw() const;
  Bool_t         PtWgt() const;
  Bool_t         ProbPid() const;
  Char_t*        Pid();
  Bool_t         EtaSubs() const;
  StFlowTrackCollection* TrackCollection() const;

  void SetSelections();
  void SetPid(const Char_t*);
  void SetPidsDeviant();
  void SetPidsProb();
  void PrintSelectionList();
  void MakeSubEvents();
  void MakeEtaSubEvents();
  void SetEventID(const Int_t&);
  void SetRunID(const Int_t&);
  void SetCenterOfMassEnergy(const Double_t&);
  void SetBeamMassNumberEast(const Short_t&);
  void SetBeamMassNumberWest(const Short_t&);
  void SetOrigMult(const UInt_t&);
  void SetL0TriggerWord(const UInt_t&);
  void SetUncorrPosMult(const UInt_t&);
  void SetUncorrNegMult(const UInt_t&);
  void SetMultEta(const UInt_t&);
  void SetCentrality(const UInt_t&);
  void SetVertexPos(const StThreeVectorF&);
  void SetCTB(const Float_t ctb);
  void SetZDCe(const Float_t zdce);
  void SetZDCw(const Float_t zdcw);
#ifndef __CINT__		
  void SetPhiWeight(const Flow::PhiWgt_t &pPhiWgt);
  void SetPhiWeightFarEast(const Flow::PhiWgt_t &pPhiWgt);
  void SetPhiWeightEast(const Flow::PhiWgt_t &pPhiWgt);
  void SetPhiWeightWest(const Flow::PhiWgt_t &pPhiWgt);
  void SetPhiWeightFarWest(const Flow::PhiWgt_t &pPhiWgt);
  void SetPhiWeightFtpcEast(const Flow::PhiWgtFtpc_t &pPhiWgt);
  void SetPhiWeightFtpcWest(const Flow::PhiWgtFtpc_t &pPhiWgt);
#endif
  static void SetEtaTpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetPtTpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetEtaFtpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetPtFtpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetPiPlusCut(Float_t lo, Float_t hi);
  static void SetPiMinusCut(Float_t lo, Float_t hi);
  static void SetProtonCut(Float_t lo, Float_t hi);
  static void SetAntiProtonCut(Float_t lo, Float_t hi);
  static void SetKPlusCut(Float_t lo, Float_t hi);
  static void SetKMinusCut(Float_t lo, Float_t hi);
  static void SetElectronCut(Float_t lo, Float_t hi);
  static void SetPositronCut(Float_t lo, Float_t hi);
  static void SetDeuteronCut(Float_t lo, Float_t hi);
  static void SetAntiDeuteronCut(Float_t lo, Float_t hi);
  static void SetDcaGlobalCut(Float_t lo, Float_t hi);
  static void SetProbPid();
  static void SetEtaSubs();
  static void SetPtWgt(Bool_t);
  static void SetOnePhiWgt();

private:

  Int_t               mEventID;                                  // ID of the event
  Int_t               mRunID;                                    // ID of the run
  Double_t            mCenterOfMassEnergy;                       // 
  Short_t             mBeamMassNumberEast;                       //
  Short_t             mBeamMassNumberWest;                       //
  UInt_t              mOrigMult;                                 // number of tracks
  UInt_t              mL0TriggerWord;                            // L0 trigger word
  UInt_t              mUncorrNegMult;                            // number of h-
  UInt_t              mUncorrPosMult;                            // number of h+
  UInt_t              mMultEta;                                  // number of tracks
                                                // with pos. flag in 1.5 unit of eta
  UInt_t              mCentrality;                               // centrality bin
  StThreeVectorF      mVertexPos;                                // primary vertex position
  Float_t             mCTB;                                      // CTB value sum
  Float_t             mZDCe;                                     // ZDC east
  Float_t             mZDCw;                                     // ZDC west
  static Float_t      mEtaTpcCuts[2][Flow::nHars][Flow::nSels];  // range absolute values
  static Float_t      mEtaFtpcCuts[2][Flow::nHars][Flow::nSels]; // range absolute values
  static Float_t      mPtTpcCuts[2][Flow::nHars][Flow::nSels];   // range
  static Float_t      mPtFtpcCuts[2][Flow::nHars][Flow::nSels];  // range
  Flow::PhiWgt_t      mPhiWgt;                                   //!flattening weights
  Flow::PhiWgt_t      mPhiWgtFarEast;                            //!flattening weights FarEast
  Flow::PhiWgt_t      mPhiWgtEast;                               //!flattening weights East
  Flow::PhiWgt_t      mPhiWgtWest;                               //!flattening weights West
  Flow::PhiWgt_t      mPhiWgtFarWest;                            //!flattening weights FarWest
  Flow::PhiWgtFtpc_t  mPhiWgtFtpcEast;                           //!flattening weights Ftpc east
  Flow::PhiWgtFtpc_t  mPhiWgtFtpcWest;                           //!flattening weights Ftpc west
  static Float_t      mPiPlusCuts[2];                            // PID cuts
  static Bool_t       mPtWgt;                                    // flag for pt weighting
  static Char_t       mPid[10];                                  // pi-, pi+, pi, k+, k-, k, pr+, pr-, pr, e+, e-, e
  static Bool_t       mProbPid;                                  // flag for probability pid
  static Bool_t       mEtaSubs;                                  // flag for eta subevents
  static Bool_t       mOnePhiWgt;                                // flag for old phi weights
  static Float_t      mPiMinusCuts[2];
  static Float_t      mProtonCuts[2];
  static Float_t      mKMinusCuts[2];
  static Float_t      mKPlusCuts[2];
  static Float_t      mAntiProtonCuts[2];
  static Float_t      mDeuteronCuts[2];
  static Float_t      mAntiDeuteronCuts[2];
  static Float_t      mElectronCuts[2];
  static Float_t      mPositronCuts[2];
  static Float_t      mDcaGlobalCuts[2];

  StFlowEvent*           pFlowEvent;         //!
  StFlowTrackCollection* pTrackCollection;   //!

  ClassDef(StFlowEvent,1)                    // macro for rootcint

};

inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return pTrackCollection; }

inline Int_t StFlowEvent::EventID() const { return mEventID; }

inline Int_t StFlowEvent::RunID() const { return mRunID; }

inline Double_t StFlowEvent::CenterOfMassEnergy() const { return mCenterOfMassEnergy; }

inline Short_t StFlowEvent::BeamMassNumberEast() const { return mBeamMassNumberEast; }

inline Short_t StFlowEvent::BeamMassNumberWest() const { return mBeamMassNumberWest; }

inline UInt_t StFlowEvent::OrigMult() const { return mOrigMult; }

inline UInt_t StFlowEvent::L0TriggerWord() const { return mL0TriggerWord; }

inline UInt_t StFlowEvent::UncorrNegMult() const { return mUncorrNegMult; }

inline UInt_t StFlowEvent::UncorrPosMult() const { return mUncorrPosMult; }

inline UInt_t StFlowEvent::MultEta() const { return mMultEta; }

inline UInt_t StFlowEvent::FlowEventMult() const { return pTrackCollection->size(); }

inline UInt_t StFlowEvent::Centrality() const { return mCentrality; }

inline StThreeVectorF StFlowEvent::VertexPos() const { return mVertexPos; }

inline Float_t  StFlowEvent::CTB() const { return mCTB; }

inline Float_t  StFlowEvent::ZDCe() const { return mZDCe; }

inline Float_t  StFlowEvent::ZDCw() const { return mZDCw; }

inline Bool_t   StFlowEvent::PtWgt() const { return mPtWgt; }

inline Char_t*  StFlowEvent::Pid() { return mPid; }

inline Bool_t   StFlowEvent::ProbPid() const { return mProbPid; }

inline Bool_t   StFlowEvent::EtaSubs() const { return mEtaSubs; }

#ifndef __CINT__
inline void StFlowEvent::SetPhiWeight(const Flow::PhiWgt_t& pPhiWgt) {
  memcpy (mPhiWgt, pPhiWgt, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetPhiWeightFarEast(const Flow::PhiWgt_t& pPhiWgtFarEast) {
  memcpy (mPhiWgtFarEast, pPhiWgtFarEast, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetPhiWeightEast(const Flow::PhiWgt_t& pPhiWgtEast) {
  memcpy (mPhiWgtEast, pPhiWgtEast, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetPhiWeightWest(const Flow::PhiWgt_t& pPhiWgtWest) {
  memcpy (mPhiWgtWest, pPhiWgtWest, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetPhiWeightFarWest(const Flow::PhiWgt_t& pPhiWgtFarWest) {
  memcpy (mPhiWgtFarWest, pPhiWgtFarWest, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetPhiWeightFtpcEast(const Flow::PhiWgtFtpc_t& pPhiWgtFtpcEast) {
  memcpy (mPhiWgtFtpcEast, pPhiWgtFtpcEast, sizeof(Flow::PhiWgtFtpc_t)); }

inline void StFlowEvent::SetPhiWeightFtpcWest(const Flow::PhiWgtFtpc_t& pPhiWgtFtpcWest) {
  memcpy (mPhiWgtFtpcWest, pPhiWgtFtpcWest, sizeof(Flow::PhiWgtFtpc_t)); }
#endif

inline void StFlowEvent::SetEtaTpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mEtaTpcCuts[0][harN][selN] = lo; mEtaTpcCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetPtTpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mPtTpcCuts[0][harN][selN] = lo; mPtTpcCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetEtaFtpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mEtaFtpcCuts[0][harN][selN] = lo; mEtaFtpcCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetPtFtpcCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mPtFtpcCuts[0][harN][selN] = lo; mPtFtpcCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetEventID(const Int_t& id) { mEventID = id; }

inline void StFlowEvent::SetRunID(const Int_t& id) { mRunID = id; }

inline void StFlowEvent::SetCenterOfMassEnergy(const Double_t& cms) { mCenterOfMassEnergy = cms; }

inline void StFlowEvent::SetBeamMassNumberEast(const Short_t& bme) { mBeamMassNumberEast = bme; }

inline void StFlowEvent::SetBeamMassNumberWest(const Short_t& bmw) { mBeamMassNumberWest = bmw; }

inline void StFlowEvent::SetOrigMult(const UInt_t& tracks) {
  mOrigMult = tracks; }

inline void StFlowEvent::SetL0TriggerWord(const UInt_t& trigger) {
  mL0TriggerWord = trigger; }

inline void StFlowEvent::SetUncorrNegMult(const UInt_t& negtracks) {
  mUncorrNegMult = negtracks; }

inline void StFlowEvent::SetUncorrPosMult(const UInt_t& postracks) {
  mUncorrPosMult = postracks; }

inline void StFlowEvent::SetMultEta(const UInt_t& goodtracks) {
  mMultEta = goodtracks; }

inline void StFlowEvent::SetVertexPos(const StThreeVectorF& vertexPos) {
  mVertexPos = vertexPos; }

inline void StFlowEvent::SetPiPlusCut(Float_t lo, Float_t hi) { 
  mPiPlusCuts[0] = lo; mPiPlusCuts[1] = hi; }

inline void StFlowEvent::SetPiMinusCut(Float_t lo, Float_t hi) { 
  mPiMinusCuts[0] = lo; mPiMinusCuts[1] = hi; }

inline void StFlowEvent::SetProtonCut(Float_t lo, Float_t hi) { 
  mProtonCuts[0] = lo; mProtonCuts[1] = hi; }

inline void StFlowEvent::SetKMinusCut(Float_t lo, Float_t hi) { 
  mKMinusCuts[0] = lo; mKMinusCuts[1] = hi; }

inline void StFlowEvent::SetKPlusCut(Float_t lo, Float_t hi) { 
  mKPlusCuts[0] = lo; mKPlusCuts[1] = hi; }

inline void StFlowEvent::SetAntiProtonCut(Float_t lo, Float_t hi) { 
  mAntiProtonCuts[0] = lo; mAntiProtonCuts[1] = hi; }

inline void StFlowEvent::SetDeuteronCut(Float_t lo, Float_t hi) { 
  mDeuteronCuts[0] = lo; mDeuteronCuts[1] = hi; }

inline void StFlowEvent::SetAntiDeuteronCut(Float_t lo, Float_t hi) { 
  mAntiDeuteronCuts[0] = lo; mAntiDeuteronCuts[1] = hi; }

inline void StFlowEvent::SetElectronCut(Float_t lo, Float_t hi) { 
  mElectronCuts[0] = lo; mElectronCuts[1] = hi; }

inline void StFlowEvent::SetPositronCut(Float_t lo, Float_t hi) { 
  mPositronCuts[0] = lo; mPositronCuts[1] = hi; }

inline void StFlowEvent::SetDcaGlobalCut(Float_t lo, Float_t hi) { 
  mDcaGlobalCuts[0] = lo; mDcaGlobalCuts[1] = hi; }

inline void StFlowEvent::SetCTB(const Float_t ctb) { mCTB = ctb; }

inline void StFlowEvent::SetZDCe(const Float_t zdce) { mZDCe = zdce; }

inline void StFlowEvent::SetZDCw(const Float_t zdcw) { mZDCw = zdcw; }

inline void StFlowEvent::SetPid(const Char_t* pid)  { 
  strncpy(mPid, pid, 9); mPid[9] = '\0'; }

inline void StFlowEvent::SetProbPid() { mProbPid = kTRUE; }

inline void StFlowEvent::SetEtaSubs() { mEtaSubs = kTRUE; }

inline void StFlowEvent::SetOnePhiWgt() { mOnePhiWgt = kTRUE; }

inline void StFlowEvent::SetPtWgt(Bool_t PtWgt) { mPtWgt = PtWgt; }

#endif

//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.h,v $
// Revision 1.34  2001/12/18 19:22:09  posk
// "proton" and "antiproton" changed to "pr+" and "pr-".
// Compiles on Solaris.
//
// Revision 1.33  2001/12/11 21:33:50  posk
// Went from one to four sets of histograms for making the event plane isotropic.
// StFlowEvent::PhiWeight() has changed arguments and return value.
// The ptWgt saturates above 2 GeV/c.
//
// Revision 1.32  2001/11/09 21:10:42  posk
// Switched from CERNLIB to TMath. Little q is now normalized.
//
// Revision 1.31  2001/11/02 04:49:52  aihong
// add func. for cumulant maker
//
// Revision 1.30  2001/08/01 19:39:35  snelling
// Added the trigger word
//
// Revision 1.29  2001/07/27 01:26:14  snelling
// Added and changed variables for picoEvent. Changed trackCut class to StTrack
//
// Revision 1.28  2001/07/24 22:29:12  snelling
// First attempt to get a standard root pico file again, added variables
//
// Revision 1.27  2001/06/07 20:06:20  posk
// Global Dca cut for event plane particles.
// Removed SetPtWgt().
//
// Revision 1.26  2001/06/06 13:02:58  rcwells
// Added SetPtWgt(Bool_t) function to StFlowEvent
//
// Revision 1.25  2001/06/04 18:57:05  rcwells
// Adding filling from HbtEvents
//
// Revision 1.24  2001/05/23 18:11:09  posk
// Removed SetPids().
//
// Revision 1.23  2001/05/22 20:17:30  posk
// Now can do pseudorapidity subevents.
//
// Revision 1.22  2001/04/03 17:47:23  oldi
// Bug fix that excluded FTPC tracks from the determination of the reaction plane.
//
// Revision 1.21  2000/12/12 20:22:05  posk
// Put log comments at end of files.
// Deleted persistent StFlowEvent (old micro DST).
//
// Revision 1.20  2000/12/10 02:01:13  oldi
// A new member (StTrackTopologyMap mTopology) was added to StFlowPicoTrack.
// The evaluation of either a track originates from the FTPC or not is
// unambiguous now. The evaluation itself is easily extendible for other
// detectors (e.g. SVT+TPC). Old flowpicoevent.root files are treated as if
// they contain TPC tracks only (backward compatibility).
//
// Revision 1.19  2000/12/08 17:03:38  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.18  2000/10/12 22:46:36  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.16  2000/09/15 22:51:30  posk
// Added pt weighting for event plane calcualtion.
//
// Revision 1.15  2000/09/05 16:11:32  snelling
// Added global DCA, electron and positron
//
// Revision 1.14  2000/08/31 18:58:22  posk
// For picoDST, added version number, runID, and multEta for centrality.
// Added centrality cut when reading picoDST.
// Added pt and eta selections for particles corr. wrt event plane.
//
// Revision 1.13  2000/08/09 21:38:23  snelling
// PID added
//
// Revision 1.12  2000/08/05 22:07:18  fisyak
// less restrictive selection for ROOTCINT
//
// Revision 1.11  2000/08/05 21:21:33  fisyak
// hide from CINT inline functions
//
// Revision 1.10  2000/08/04 21:03:45  perev
// Leaks + Clear() cleanup
//
// Revision 1.9  2000/06/30 14:48:32  posk
// Using MessageMgr, changed Eta Symmetry cut.
//
// Revision 1.8  2000/06/20 16:34:25  snelling
// fixed cout/streamer problem for mPhiWgt under Solaris
//
// Revision 1.7  2000/05/26 21:29:27  posk
// Protected Track data members from overflow.
//
// Revision 1.5  2000/05/16 20:59:30  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.4  2000/05/12 22:42:04  snelling
// Additions for persistency and minor fix
//
// Revision 1.2  2000/03/15 23:28:51  posk
// Added StFlowSelection.
//
// Revision 1.1  2000/03/02 23:02:50  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.10  2000/02/29 22:00:54  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.9  2000/02/18 22:49:55  posk
// Added PID and centrality.
//
// Revision 1.5  1999/12/15 22:01:26  posk
// Added StFlowConstants.hh
//
// Revision 1.4  1999/12/04 00:10:33  posk
// Works with the new StEvent
//
// Revision 1.3  1999/11/30 18:52:52  snelling
// First modification for the new StEvent
//
// Revision 1.2  1999/11/24 18:17:14  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
// Revision 1.1  1999/11/04 19:02:06  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////
