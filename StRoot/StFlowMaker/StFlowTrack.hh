//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.hh,v 1.1 1999/11/11 23:08:58 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowTrackCollection
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.hh,v $
// Revision 1.1  1999/11/11 23:08:58  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:08  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowTrack_hh
#define StFlowTrack_hh
#include "Rtypes.h"
//#include "StFlowMaker.hh"

class StFlowTrack{

public:

          StFlowTrack(){ };
  virtual ~StFlowTrack(){ };

  Char_t  Pid() const;
  Float_t Phi() const;
  Float_t Eta() const;
  Float_t Pt()  const;
  Int_t SubEvent() const;
  Int_t Harmonic() const;

  void SetPid(const Char_t&);
  void SetPhi(const Float_t&);
  void SetEta(const Float_t&);
  void SetPt(const Float_t&);
  void SetSubEvent(Int_t mSubEvent);
  void SetHarmonic(Int_t mHarmonic);

  // For I/O of this object -- functions defined in FlowIO.cc
  //  friend ostream& operator<<(ostream& out, StFlowTrack& trk);
  //  friend istream& operator>>(istream& in,  StFlowTrack& trk);

private:

  Char_t  mPid;
  Float_t mPhi;
  Float_t mEta;
  Float_t mPt;
  Int_t   mSubEvent;
  Int_t   mHarmonic;

};

inline void StFlowTrack::SetPid(const Char_t& pid){mPid=pid;}
inline void StFlowTrack::SetPhi(const Float_t& phi){mPhi = phi;}              
inline void StFlowTrack::SetEta(const Float_t& eta){mEta = eta;}              
inline void StFlowTrack::SetPt(const Float_t& pt){mPt = pt;}              
inline void StFlowTrack::SetSubEvent(Int_t subevent){mSubEvent = subevent;}
inline void StFlowTrack::SetHarmonic(Int_t harmonic){mHarmonic = harmonic;} 

inline Char_t  StFlowTrack::Pid() const {return mPid;}
inline Float_t StFlowTrack::Phi() const {return mPhi;}                
inline Float_t StFlowTrack::Eta() const {return mEta;}                
inline Float_t StFlowTrack::Pt() const {return mPt;}                
inline Int_t   StFlowTrack::SubEvent() const {return mSubEvent;}                
inline Int_t   StFlowTrack::Harmonic() const {return mHarmonic;}                

#endif
