//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.hh,v 1.10 2000/02/29 22:00:54 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent with flow functions
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.hh,v $
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

#ifndef StFlowEvent_hh
#define StFlowEvent_hh
#include <iostream.h>
#include <stdlib.h>
#include "StFlowTrackCollection.hh"
#include "StFlowConstants.hh"
#include "StThreeVectorF.hh"
#include "Rtypes.h"
#include "SystemOfUnits.h"
class TVector2;

class StFlowEvent{

public:

                 StFlowEvent();
  virtual        ~StFlowEvent();

  Double_t       PhiWeight(Float_t mPhi, Int_t selN, Int_t harN) const;
  UInt_t         EventNumber() const;
  UInt_t         OrigMult() const;
  UInt_t         Centrality() const;
  StThreeVectorF VertexPos() const;
  UInt_t         Mult(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  TVector2       Q(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  Float_t        q(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  Float_t        MeanPt(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  Float_t        Psi(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  StFlowTrackCollection* TrackCollection() const;

  void SetEtaCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  void SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  void SetSelections();
  void SetPids();
  void PrintSelectionList();
  void MakeSubEvents();
  void SetEventNumber(const UInt_t&);
  void SetOrigMult(const UInt_t&);
  void SetCentrality(const UInt_t&);
  void SetVertexPos(const StThreeVectorF&);
  void SetPhiWeight(const Flow::PhiWgt_t &pPhiWgt);
  void SetPiPlusCut(Float_t lo, Float_t hi);
  void SetPiMinusCut(Float_t lo, Float_t hi);
  void SetProtonCut(Float_t lo, Float_t hi);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StFlowEvent& ev);
  friend istream& operator>>(istream& in,  StFlowEvent& ev);

private:

  Int_t checkInput(Int_t harN, Int_t selN, Int_t subN) const;

  UInt_t          mEventNumber;                        // number of the event
  UInt_t          mOrigMult;                           // number of tracks
  UInt_t          mCentrality;                         // centrality bin
  StThreeVectorF  mVertexPos;                          // primary vertex position
  static Float_t  mEtaCuts[2][Flow::nHars][Flow::nSels]; // range absolute values
  static Float_t  mPtCuts[2][Flow::nHars][Flow::nSels];  // range
  Flow::PhiWgt_t  mPhiWgt;
  static Float_t  mPiPlusCuts[2];                      // PID cuts
  static Float_t  mPiMinusCuts[2];
  static Float_t  mProtonCuts[2];

  StFlowEvent*           pFlowEvent;         //!
  StFlowTrackCollection* pTrackCollection;   //!

};

inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return pTrackCollection; }

inline UInt_t StFlowEvent::EventNumber() const { return mEventNumber; }

inline UInt_t StFlowEvent::OrigMult() const { return mOrigMult; }

inline UInt_t StFlowEvent::Centrality() const { return mCentrality; }

inline StThreeVectorF StFlowEvent::VertexPos() const { return mVertexPos; }

inline void StFlowEvent::SetPhiWeight(const Flow::PhiWgt_t& pPhiWgt) {
  memcpy (mPhiWgt, pPhiWgt, sizeof(Flow::PhiWgt_t)); }

inline void StFlowEvent::SetEtaCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mEtaCuts[0][harN][selN] = lo; mEtaCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mPtCuts[0][harN][selN] = lo; mPtCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetEventNumber(const UInt_t& event) {
  mEventNumber = event; }

inline void StFlowEvent::SetOrigMult(const UInt_t& tracks) {
  mOrigMult = tracks; }

inline void StFlowEvent::SetCentrality(const UInt_t& tracks) {
  int maxTracks = 4000; mCentrality = (6*tracks)/maxTracks + 1; }

inline void StFlowEvent::SetVertexPos(const StThreeVectorF& vertexPos) {
  mVertexPos = vertexPos; }

inline void StFlowEvent::SetPiPlusCut(Float_t lo, Float_t hi) { 
  mPiPlusCuts[0] = lo; mPiPlusCuts[1] = hi; }

inline void StFlowEvent::SetPiMinusCut(Float_t lo, Float_t hi) { 
  mPiMinusCuts[0] = lo; mPiMinusCuts[1] = hi; }

inline void StFlowEvent::SetProtonCut(Float_t lo, Float_t hi) { 
  mProtonCuts[0] = lo; mProtonCuts[1] = hi; }

#endif

