//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.h,v 1.6 2000/05/26 21:29:34 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//
// Description: part of StFlowTrackCollection
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.h,v $
// Revision 1.6  2000/05/26 21:29:34  posk
// Protected Track data members from overflow.
//
// Revision 1.5  2000/05/20 00:55:20  posk
// Condensed flownanoevent.root somewhat.
//
// Revision 1.4  2000/05/16 20:59:35  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.3  2000/05/12 22:42:05  snelling
// Additions for persistency and minor fix
//
// Revision 1.1  2000/03/02 23:02:57  posk
// Changed extensions from .hh and .cc to .h and .cxx .
//
// Revision 1.10  2000/02/29 22:00:56  posk
// Made SetPhiWeight inline, changed ImpactPar to Dca, etc.
//
// Revision 1.9  2000/02/18 22:49:57  posk
// Added PID and centrality.
//
// Revision 1.7  1999/12/21 01:11:02  posk
// Added more quantities to StFlowEvent.
//
// Revision 1.6  1999/12/16 18:05:25  posk
// Fixed Linux compatability again.
//
// Revision 1.5  1999/12/15 22:01:29  posk
// Added StFlowConstants.hh
//
// Revision 1.4  1999/12/04 00:10:35  posk
// Works with the new StEvent
//
// Revision 1.3  1999/11/30 18:52:55  snelling
// First modification for the new StEvent
//
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

#ifndef StFlowTrack_h
#define StFlowTrack_h
#include <string.h>
#include "Rtypes.h"
#include "StObject.h"
#include "StFlowConstants.h"

class StFlowTrack : public StObject {

public:

                StFlowTrack();
  virtual       ~StFlowTrack();

  Float_t       PidPiPlus()  const;
  Float_t       PidPiMinus() const;
  Float_t       PidProton()  const;
  const Char_t* Pid()        const;
  Float_t       Phi()        const;
  Float_t       Eta()        const;
  Float_t       Pt()         const;
  Short_t       Charge()     const;
  Float_t       Dca()        const;
  Float_t       Chi2()       const;
  Int_t         FitPts()     const;
  Int_t         MaxPts()     const;
  Int_t Select(Int_t harmonic, Int_t selection, Int_t subevent= -1) const;

  void SetPidPiPlus(Float_t);
  void SetPidPiMinus(Float_t);
  void SetPidProton(Float_t);
  void SetPid(const Char_t*);
  void SetPhi(Float_t);
  void SetEta(Float_t);
  void SetPt(Float_t);
  void SetCharge(Short_t);
  void SetDca(Float_t);
  void SetChi2(Float_t);
  void SetFitPts(Int_t);
  void SetMaxPts(Int_t);
  void SetSelect(Int_t harmonic, Int_t selection);
  void SetSubevent(Int_t harmonic, Int_t selection, Int_t subevent);

private:

  Int_t   mPidPiPlus;
  Int_t   mPidPiMinus;
  Int_t   mPidProton;
  Char_t  mPid[10];
  Float_t mPhi;
  Float_t mEta;
  Float_t mPt;
  Short_t mCharge;
  UInt_t  mDca;
  UInt_t  mChi2;
  Int_t   mFitPts;
  Int_t   mMaxPts;
  Int_t   mSelection;
  Short_t mSubevent[Flow::nHars][Flow::nSels];
  static  Float_t maxInt;
  static  Float_t maxUInt;

  ClassDef(StFlowTrack, 1)                     // macro for rootcint
};

inline Float_t  StFlowTrack::PidPiPlus()  const { return mPidPiPlus/1000.; }
inline Float_t  StFlowTrack::PidPiMinus() const { return mPidPiMinus/1000.; }
inline Float_t  StFlowTrack::PidProton()  const { return mPidProton/1000.; }
inline const Char_t* StFlowTrack::Pid()   const { return mPid; }
inline Float_t  StFlowTrack::Phi()        const { return mPhi; }                
inline Float_t  StFlowTrack::Eta()        const { return mEta; }                
inline Float_t  StFlowTrack::Pt()         const { return mPt; }                
inline Short_t  StFlowTrack::Charge()     const { return mCharge; }   
inline Float_t  StFlowTrack::Dca()        const { return mDca/1000.; }
inline Float_t  StFlowTrack::Chi2()       const { return mChi2/1000.; } 
inline Int_t    StFlowTrack::FitPts()     const { return mFitPts; }  
inline Int_t    StFlowTrack::MaxPts()     const { return mMaxPts; }  

inline Int_t    StFlowTrack::Select(Int_t harmonic, Int_t selection,
 Int_t subevent) const {
  if (subevent == -1 || subevent == mSubevent[harmonic][selection]) {
    int bitShift = harmonic + Flow::nHars * selection;
    return (mSelection & (1 << bitShift)) ? kTRUE : kFALSE;
  }
  return kFALSE;         
}

inline void StFlowTrack::SetPidPiPlus(Float_t pid)  {
  if (pid > maxInt) pid = maxInt; mPidPiPlus = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidPiMinus(Float_t pid) {
  if (pid > maxInt) pid = maxInt; mPidPiMinus = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPidProton(Float_t pid)  {
  if (pid > maxInt) pid = maxInt; mPidProton = (Int_t)(pid*1000.); }

inline void StFlowTrack::SetPid(const Char_t* pid)  { strncpy(mPid, pid, 9);
                                                         mPid[9] = '\0'; }
inline void StFlowTrack::SetPhi(Float_t phi)        { mPhi = phi; }              

inline void StFlowTrack::SetEta(Float_t eta)        { mEta = eta; }              

inline void StFlowTrack::SetPt(Float_t pt)          { mPt = pt; }              

inline void StFlowTrack::SetCharge(Short_t charge)  { mCharge = charge; }     

inline void StFlowTrack::SetDca(Float_t dca)        {
  if (dca > maxUInt) dca = maxUInt; mDca = (UInt_t)(dca*1000.); }

inline void StFlowTrack::SetChi2(Float_t chi2)      {
  if (chi2 > maxUInt) chi2 = maxUInt; mChi2 = (UInt_t)(chi2*1000.); }

inline void StFlowTrack::SetFitPts(Int_t fitPts)    { mFitPts = fitPts; }

inline void StFlowTrack::SetMaxPts(Int_t maxPts)    { mMaxPts = maxPts; }

inline void StFlowTrack::SetSelect(Int_t harmonic, Int_t selection) {
  int bitShift = harmonic + Flow::nHars * selection;
  mSelection |= 1 << bitShift; }

inline void StFlowTrack::SetSubevent(Int_t harmonic, Int_t selection,
 Int_t subevent) { mSubevent[harmonic][selection] = subevent; }

#endif

