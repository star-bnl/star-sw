//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.hh,v 1.5 1999/12/15 22:01:26 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent with flow functions
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.hh,v $
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
#include "Rtypes.h"
#include "SystemOfUnits.h"
class TVector2;

class StFlowEvent{

public:

           StFlowEvent();
  virtual  ~StFlowEvent();

  Double_t PhiWeight(Float_t mPhi, Int_t selN, Int_t harN) const;
  UInt_t   EventNumber() const;
  UInt_t   Mult(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  TVector2 Q(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  Float_t  q(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  Float_t  MeanPt(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  Float_t  Psi(Int_t harN=1, Int_t selN=0, Int_t subN=-1);
  StFlowTrackCollection* TrackCollection() const;

  void     SetEtaCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  void     SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN);
  void     SetSelections();
  void     PrintSelectionList();
  void     MakeSubEvents();
  void     SetEventNumber(const UInt_t&);
  void     SetOrigTrackN(const UInt_t&);
  void     SetPhiWeight(const Flow::PhiWgt_t &pPhiWgt);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StFlowEvent& ev);
  friend istream& operator>>(istream& in,  StFlowEvent& ev);

private:

  Int_t   checkInput(Int_t harN, Int_t selN, Int_t subN) const;

  UInt_t  mEventNumber;                      // number of the event
  UInt_t  mOrigTrackN;                       // number of tracks
  static  Float_t mEtaCuts[2][Flow::nHars][Flow::nSels];  // range absolute values
  static  Float_t mPtCuts[2][Flow::nHars][Flow::nSels];   // range
  StFlowEvent*           pFlowEvent;         //!
  StFlowTrackCollection* pTrackCollection;   //!
  Flow::PhiWgt_t         mPhiWgt;

};

inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return pTrackCollection; }

inline  UInt_t StFlowEvent::EventNumber() const { return mEventNumber; }

inline void StFlowEvent::SetEtaCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mEtaCuts[0][harN][selN] = lo; mEtaCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetPtCut(Float_t lo, Float_t hi, Int_t harN, Int_t selN)
{ mPtCuts[0][harN][selN] = lo; mPtCuts[1][harN][selN] = hi; }

inline void StFlowEvent::SetEventNumber(const UInt_t& event) {
  mEventNumber = event; }

inline void StFlowEvent::SetOrigTrackN(const UInt_t& tracks) {
  mOrigTrackN = tracks; }

#endif

