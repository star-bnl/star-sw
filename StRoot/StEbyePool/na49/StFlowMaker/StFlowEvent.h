//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.h,v 1.3 2001/11/06 17:05:30 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent with flow functions
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.h,v $
// Revision 1.3  2001/11/06 17:05:30  posk
// New 40 Gev centrality bins. Using only sin terms at 40 GeV.
//
// Revision 1.2  2001/05/14 23:04:33  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.18  2000/10/12 22:46:36  snelling
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowEvent_h
#define StFlowEvent_h
#include "StObject.h"
#include "StFlowTrackCollection.h"
#include "StThreeVectorF.hh"
#include "Rtypes.h"
#include "TVector2.h"
class StFlowSelection;

class StFlowEvent  : public StObject {

public:

  StFlowEvent();
  virtual        ~StFlowEvent();

  Double_t       PhiWeight(Float_t phi, Int_t selN, Int_t harN) const;
  Double_t       MeanCos(Float_t y, Float_t pt, Int_t harN) const;
  Double_t       MeanSin(Float_t y, Float_t pt, Int_t harN) const;
  Int_t          EventID() const;
  Int_t          RunID() const;
  UInt_t         OrigMult() const;
  UInt_t         FlowEventMult() const;
  Float_t        EVeto() const;
  UInt_t         Centrality() const;
  StThreeVectorF VertexPos() const;
  UInt_t         Mult(StFlowSelection*);
  TVector2       Q(StFlowSelection*);
  Float_t        SumPt(StFlowSelection*);
  Float_t        SumPt2(StFlowSelection*);
  Float_t        q(StFlowSelection*);
  Float_t        Psi(StFlowSelection*);
  Bool_t         PtWgt() const;
  Bool_t         YWgt() const;
  Bool_t         SinOnly() const;
  Int_t          Stripes() const;
  Char_t*        Pid();
  Bool_t         ProbPid() const;
  StFlowTrackCollection* TrackCollection() const;
 
  void SetSelections();
  void SetPids();
  void SetRapidities();
  void PrintSelectionList();
  void MakeSubEvents();
  void MakeStripedSubs();
  void SetEventID(const Int_t&);
  void SetRunID(const Int_t&);
  void SetOrigMult(const UInt_t&);
  void SetEVeto(const Float_t&);
  void SetCent(const Float_t);
  void SetVertexPos(const StThreeVectorF&);
  void SetPid(const Char_t*);
#ifndef __CINT__		
  void SetPhiWeight(const Flow::PhiWgt_t &pPhiWgt);
  void SetMeanCos(const Flow::MeanCos_t &pMeanCos);
  void SetMeanSin(const Flow::MeanSin_t &pMeanSin);
#endif
  static void SetYCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  static void SetMeanSinCosCut(Float_t lo, Float_t hi);
  static void SetPtWgt();
  static void SetYWgt();
  static void SetSinOnly();
  static void SetStripes(Int_t);
  static void SetProbPid();

private:

  Int_t           mEventID;                             // ID of the event
  Int_t           mRunID;                               // ID of the run
  UInt_t          mOrigMult;                            // number of tracks
  Float_t         mEVeto;                               // E veto cal.
  UInt_t          mCentrality;                          // centrality bin
  StThreeVectorF  mVertexPos;                           // primary vertex position
  static Float_t  mYCuts[2][Flow::nHars][Flow::nSels];  // range absolute values
  static Float_t  mPtCuts[2][Flow::nHars][Flow::nSels]; // range
  static Float_t  mMeanSinCosCuts[2];                   // range
  Flow::PhiWgt_t  mPhiWgt;                              //!flattening weights
  Flow::MeanCos_t mMeanCos;                             //!recentering
  Flow::MeanSin_t mMeanSin;                             //!recentering
  static Bool_t   mPtWgt;                               // flag for pt weighting
  static Bool_t   mYWgt;                                // flag for y weighting
  static Bool_t   mSinOnly;                             // flag for using sin terms only in the correlation term for v2
  static Int_t    mStripes;                             // flag for striped subevents
  static Char_t   mPid[10];                             // pi-, pi+, pi, pbar, proton, e+, e-
  static Bool_t   mProbPid;                             // flag for probability pid

  StFlowEvent*           pFlowEvent;         //!
  StFlowTrackCollection* pTrackCollection;   //

  ClassDef(StFlowEvent,1)                    // macro for rootcint
};
inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return pTrackCollection; }

inline Int_t StFlowEvent::EventID() const { return mEventID; }

inline Int_t StFlowEvent::RunID() const { return mRunID; }

inline UInt_t StFlowEvent::OrigMult() const { return mOrigMult; }

inline UInt_t StFlowEvent::FlowEventMult() const { return pTrackCollection->size(); }

inline Float_t StFlowEvent::EVeto() const { return mEVeto; }

inline UInt_t StFlowEvent::Centrality() const { return mCentrality; }

inline StThreeVectorF StFlowEvent::VertexPos() const { return mVertexPos; }

inline Bool_t   StFlowEvent::PtWgt() const { return mPtWgt; }

inline Bool_t   StFlowEvent::YWgt() const { return mYWgt; }

inline Bool_t   StFlowEvent::SinOnly() const { return mSinOnly; }

inline Int_t    StFlowEvent::Stripes() const { return mStripes; }

inline Char_t*  StFlowEvent::Pid() { return mPid; }

inline Bool_t   StFlowEvent::ProbPid() const { return mProbPid; }

#ifndef __CINT__
inline void StFlowEvent::SetPhiWeight(const Flow::PhiWgt_t& pPhiWgt) {
  memcpy (mPhiWgt, pPhiWgt, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetMeanCos(const Flow::MeanCos_t& pMeanCos) { 
  memcpy (mMeanCos, pMeanCos, sizeof(Flow::MeanCos_t)); }

inline void StFlowEvent::SetMeanSin(const Flow::MeanSin_t& pMeanSin) { 
  memcpy (mMeanSin, pMeanSin, sizeof(Flow::MeanSin_t)); }
#endif

inline void StFlowEvent::SetYCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mYCuts[0][harN][selN] = lo; mYCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mPtCuts[0][harN][selN] = lo; mPtCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetMeanSinCosCut(Float_t lo, Float_t hi)
{ mMeanSinCosCuts[0] = lo; mMeanSinCosCuts[1] = hi; }

inline void StFlowEvent::SetEventID(const Int_t& id) { mEventID = id; }

inline void StFlowEvent::SetRunID(const Int_t& id) { mRunID = id; }

inline void StFlowEvent::SetOrigMult(const UInt_t& tracks) {
  mOrigMult = tracks; }

inline void StFlowEvent::SetEVeto(const Float_t& eveto) {
  mEVeto = eveto; }

inline void StFlowEvent::SetCent(const Float_t cent) {
  mCentrality = (Int_t) cent; }

inline void StFlowEvent::SetVertexPos(const StThreeVectorF& vertexPos) {
  mVertexPos = vertexPos; }

inline void  StFlowEvent::SetPtWgt() { mPtWgt = kTRUE; }

inline void  StFlowEvent::SetYWgt() { mYWgt = kTRUE; }

inline void  StFlowEvent::SetSinOnly() { mYWgt = kTRUE; }

inline void  StFlowEvent::SetStripes(const Int_t stripe) { mStripes = stripe; }

inline void  StFlowEvent::SetPid(const Char_t* pid)  { 
  strncpy(mPid, pid, 9); mPid[9] = '\0'; }

inline void  StFlowEvent::SetProbPid() { mProbPid = kTRUE; }

#endif
