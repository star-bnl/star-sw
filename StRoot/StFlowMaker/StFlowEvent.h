//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.h,v 1.18 2000/10/12 22:46:36 snelling Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent with flow functions
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.h,v $
// Revision 1.18  2000/10/12 22:46:36  snelling
// Added support for the new pDST's and the probability pid method
//
// Revision 1.17  2000/09/22 22:02:59  posk
// Clean up.
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
// Revision 1.6  2000/05/20 00:55:15  posk
// Condensed flownanoevent.root somewhat.
//
// Revision 1.5  2000/05/16 20:59:30  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.4  2000/05/12 22:42:04  snelling
// Additions for persistency and minor fix
//
// Revision 1.3  2000/05/11 20:00:34  posk
// Preparation for micro and nano DSTs.
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
// Revision 1.8  2000/02/11 20:53:09  posk
// Commented out random_shuffle and cout formatting so as to work under CC5.
//
// Revision 1.7  2000/01/31 22:16:59  posk
// CC5 compliant.
//
// Revision 1.6  1999/12/21 01:10:59  posk
// Added more quantities to StFlowEvent.
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
// Revision 1.1  1999/11/11 23:08:56  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:06  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowEvent_h
#define StFlowEvent_h
//#include "St_DataSet.h"
#include "StObject.h"
#include "StFlowTrackCollection.h"
#include "StThreeVectorF.hh"
#include "Rtypes.h"
#include "TVector2.h"
class StFlowSelection;

//class StFlowEvent  : public St_DataSet {
class StFlowEvent  : public StObject {

public:

  StFlowEvent();
  virtual        ~StFlowEvent();

  Double_t       PhiWeight(Float_t mPhi, Int_t selN, Int_t harN) const;
  Int_t          EventID() const;
  Int_t          RunID() const;
  UInt_t         OrigMult() const;
  UInt_t         MultEta() const;
  UInt_t         FlowEventMult() const;
  UInt_t         Centrality() const;
  StThreeVectorF VertexPos() const;
  UInt_t         Mult(StFlowSelection*);
  TVector2       Q(StFlowSelection*);
  Float_t        q(StFlowSelection*);
  Float_t        MeanPt(StFlowSelection*);
  Float_t        Psi(StFlowSelection*);
  Float_t        CTB() const;
  Float_t        ZDCe() const;
  Float_t        ZDCw() const;
  Bool_t         PtWgt() const;
  Bool_t         ProbPid() const;
  StFlowTrackCollection* TrackCollection() const;
 
  void SetSelections();
  void SetPids();
  void SetPidsDeviant();
  void SetPidsProb();
  void PrintSelectionList();
  void MakeSubEvents();
  void SetEventID(const Int_t&);
  void SetRunID(const Int_t&);
  void SetOrigMult(const UInt_t&);
  void SetMultEta(const UInt_t&);
  void SetCentrality(const UInt_t&);
  void SetVertexPos(const StThreeVectorF&);
  void SetCTB(const Float_t ctb);
  void SetZDCe(const Float_t zdce);
  void SetZDCw(const Float_t zdcw);
#ifndef __CINT__		
  void SetPhiWeight(const Flow::PhiWgt_t &pPhiWgt);
#endif
  static void SetEtaCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
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
  static void SetPtWgt();
  static void SetProbPid();

private:

  Int_t           mEventID;                             // ID of the event
  Int_t           mRunID;                               // ID of the run
  UInt_t          mOrigMult;                            // number of tracks
  UInt_t          mMultEta; // number of tracks with pos. flag in 1.5 unit of eta
  UInt_t          mCentrality;                          // centrality bin
  StThreeVectorF  mVertexPos;                           // primary vertex position
  Float_t         mCTB;                                 // CTB value sum
  Float_t         mZDCe;                                // ZDC east
  Float_t         mZDCw;                                // ZDC west
  static Float_t  mEtaCuts[2][Flow::nHars][Flow::nSels];// range absolute values
  static Float_t  mPtCuts[2][Flow::nHars][Flow::nSels]; // range
  Flow::PhiWgt_t  mPhiWgt;                              //!flattening weights
  static Float_t  mPiPlusCuts[2];                       // PID cuts
  static Bool_t   mPtWgt;                               // flag for pt weighting
  static Bool_t   mProbPid;                              // flag for probability pid
  static Float_t  mPiMinusCuts[2];
  static Float_t  mProtonCuts[2];
  static Float_t  mKMinusCuts[2];
  static Float_t  mKPlusCuts[2];
  static Float_t  mAntiProtonCuts[2];
  static Float_t  mDeuteronCuts[2];
  static Float_t  mAntiDeuteronCuts[2];
  static Float_t  mElectronCuts[2];
  static Float_t  mPositronCuts[2];

  StFlowEvent*           pFlowEvent;         //!
  StFlowTrackCollection* pTrackCollection;   //

  ClassDef(StFlowEvent,1)                    // macro for rootcint
};
inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return pTrackCollection; }

inline Int_t StFlowEvent::EventID() const { return mEventID; }

inline Int_t StFlowEvent::RunID() const { return mRunID; }

inline UInt_t StFlowEvent::OrigMult() const { return mOrigMult; }

inline UInt_t StFlowEvent::MultEta() const { return mMultEta; }

inline UInt_t StFlowEvent::FlowEventMult() const { return pTrackCollection->size(); }

inline UInt_t StFlowEvent::Centrality() const { return mCentrality; }

inline StThreeVectorF StFlowEvent::VertexPos() const { return mVertexPos; }

inline Float_t  StFlowEvent::CTB() const { return mCTB; }

inline Float_t  StFlowEvent::ZDCe() const { return mZDCe; }

inline Float_t  StFlowEvent::ZDCw() const { return mZDCw; }

inline Bool_t   StFlowEvent::PtWgt() const { return mPtWgt; }

inline Bool_t   StFlowEvent::ProbPid() const { return mProbPid; }

#ifndef __CINT__
inline void StFlowEvent::SetPhiWeight(const Flow::PhiWgt_t& pPhiWgt) {
  memcpy (mPhiWgt, pPhiWgt, sizeof(Flow::PhiWgt_t)); }
#endif

inline void StFlowEvent::SetEtaCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mEtaCuts[0][harN][selN] = lo; mEtaCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mPtCuts[0][harN][selN] = lo; mPtCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetEventID(const Int_t& id) { mEventID = id; }

inline void StFlowEvent::SetRunID(const Int_t& id) { mRunID = id; }

inline void StFlowEvent::SetOrigMult(const UInt_t& tracks) {
  mOrigMult = tracks; }

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

inline void  StFlowEvent::SetCTB(const Float_t ctb) { mCTB = ctb; }

inline void  StFlowEvent::SetZDCe(const Float_t zdce) { mZDCe = zdce; }

inline void  StFlowEvent::SetZDCw(const Float_t zdcw) { mZDCw = zdcw; }

inline void  StFlowEvent::SetPtWgt() { mPtWgt = kTRUE; }

inline void  StFlowEvent::SetProbPid() { mProbPid = kTRUE; }

#endif
