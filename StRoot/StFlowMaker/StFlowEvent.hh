//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.hh,v 1.3 1999/11/30 18:52:52 snelling Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.hh,v $
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
#include "Rtypes.h"
class TVector2;


class StFlowEvent{

public:

           StFlowEvent();
  virtual  ~StFlowEvent();

  StFlowTrackCollection* TrackCollection() const;

  void     MakeSubEvents();
  void     SetEventNumber(const UInt_t&);
  void     SetOrigTrackN(const UInt_t&);
  void     SetPhiWeight(const Double_t* pPhiWgt);
  /// void SetPhiWeight(const PhiWgt_t &pPhiWgt);
  Double_t PhiWeight(Float_t mPhi, Int_t selN, Int_t harN) const;
  UInt_t   EventNumber() const;
  UInt_t   Mult(Int_t harN=1, Int_t selN=0, Int_t subN=0);
  TVector2 Q(Int_t harN=1, Int_t selN=0, Int_t subN=0);
  Float_t  q(Int_t harN=1, Int_t selN=0, Int_t subN=0);
  Float_t  MeanPt(Int_t harN=1, Int_t selN=0, Int_t subN=0);
  Float_t  Psi(Int_t harN=1, Int_t selN=0, Int_t subN=0);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StFlowEvent& ev);
  friend istream& operator>>(istream& in,  StFlowEvent& ev);

private:

  Int_t   checkInput(Int_t harN, Int_t selN, Int_t subN) const;
  UInt_t  mEventNumber;                    // number of the event
  UInt_t  nOrigTrack;                      // number of tracks
  StFlowEvent* pFlowEvent;                 //!
  StFlowTrackCollection* pTrackCollection; //!

  // C++ way to define constants in the header
  enum {nHars = 4, nSels=2, nSubs = 2};
  enum {nPhiBins = 60};
  /// typedef Double_t PhiWgt_t[nSel][nHars][nPhiBins];
  Double_t mPhiWgt[nSels][nHars][nPhiBins]; // To make event plane isotropic

};

inline void StFlowEvent::SetEventNumber(const UInt_t& event)
  {mEventNumber = event;}
inline void StFlowEvent::SetOrigTrackN(const UInt_t& tracks)
  {nOrigTrack = tracks;}

inline StFlowTrackCollection* StFlowEvent::TrackCollection() const {
  return pTrackCollection;}
inline  UInt_t StFlowEvent::EventNumber() const {return mEventNumber;}

#endif

