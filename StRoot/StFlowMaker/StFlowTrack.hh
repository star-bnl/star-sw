//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.hh,v 1.2 1999/11/24 18:17:16 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: part of StFlowTrackCollection
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.hh,v $
// Revision 1.2  1999/11/24 18:17:16  posk
// Put the methods which act on the data in with the data in StFlowEvent.
//
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
#define PR(x) cout << "##### FlowTrack: " << (#x) << " = " << (x) << endl;

class StFlowTrack{

public:

          StFlowTrack(){ };
  virtual ~StFlowTrack(){ };

  Char_t  Pid() const;
  Float_t Phi() const;
  Float_t Eta() const;
  Float_t Pt()  const;
  Int_t   Select(Int_t harmonic, Int_t selection, Int_t subevent) const;

  void SetPid(Char_t);
  void SetPhi(Float_t);
  void SetEta(Float_t);
  void SetPt(Float_t);
  void SetSelect(Int_t harmonic, Int_t selection);
  void SetSubevent(Int_t harmonic, Int_t selection, Int_t subevent);

  // For I/O of this object -- functions defined in FlowIO.cc
  //  friend ostream& operator<<(ostream& out, StFlowTrack& trk);
  //  friend istream& operator>>(istream& in,  StFlowTrack& trk);

private:

  enum {nHars = 4, nSels = 2, nSubs = 2};

  Char_t  mPid;
  Float_t mPhi;
  Float_t mEta;
  Float_t mPt;
  UInt_t  mSelection;
  UInt_t  mSubevent[nHars][nSels];

};

inline void StFlowTrack::SetPid(Char_t pid){mPid=pid;}
inline void StFlowTrack::SetPhi(Float_t phi){mPhi = phi;}              
inline void StFlowTrack::SetEta(Float_t eta){mEta = eta;}              
inline void StFlowTrack::SetPt(Float_t pt){mPt = pt;}              

// The harmonic and selection arguments start at 0
// but the subevent argument starts at 1
inline void StFlowTrack::SetSelect(Int_t harmonic, Int_t selection) {
  Int_t bitShift = harmonic + nHars * selection;
  mSelection |= 1 << bitShift;
  //cout << harmonic << " " << selection << " " << mSelection << endl;
}
inline void StFlowTrack::SetSubevent(Int_t harmonic, Int_t selection,
 Int_t subevent) {mSubevent[harmonic][selection] = subevent;}

inline Char_t  StFlowTrack::Pid() const {return mPid;}
inline Float_t StFlowTrack::Phi() const {return mPhi;}                
inline Float_t StFlowTrack::Eta() const {return mEta;}                
inline Float_t StFlowTrack::Pt() const {return mPt;}                

inline Int_t   StFlowTrack::Select(Int_t harmonic, Int_t selection,
 Int_t subevent=0) const {
  if (!subevent || subevent == mSubevent[harmonic][selection]) {
    Int_t bitShift = harmonic + nHars * selection;
    return (mSelection & (1 << bitShift)) ? kTRUE : kFALSE;
  }
  return kFALSE;         
}

#endif
