//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTrack.h,v 1.3 2001/08/17 22:10:31 posk Exp $
//
// Authors: Art Poskanzer, LBNL, and Alexander Wetzler, IKF, Dec 2000
//
// Description: part of StFlowTrackCollection
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTrack.h,v $
// Revision 1.3  2001/08/17 22:10:31  posk
// Now also can do 40 GeV data.
//
// Revision 1.2  2001/05/14 23:04:47  posk
// Can select PID for event plane particles. Protons not used for 1st har.
// event plane.
//
// Revision 1.15  2000/10/12 22:46:40  snelling
//
//////////////////////////////////////////////////////////////////////

#ifndef StFlowTrack_h
#define StFlowTrack_h
#include <string.h>
#include <math.h>
#include "Rtypes.h"
#include "StObject.h"
#include "StFlowConstants.h"

class StFlowTrack : public StObject {

public:

                StFlowTrack();
  virtual       ~StFlowTrack();

  const Char_t* Pid()        const;
  Float_t       Phi()        const;
  Float_t       Eta()        const;
  Float_t       Dedx()       const;
  Float_t       DedxMain()   const;
  Float_t       Pt()         const;
  Float_t       P()          const;
  Float_t       Y()          const;
  Short_t       Charge()     const;
  Float_t       Dca()        const;
  Float_t       Bx()         const;
  Float_t       By()         const;
  Float_t       Chi2()       const;
  Int_t         FitPts()     const;
  Int_t         MaxPts()     const;
  Int_t Select(Int_t harmonic, Int_t selection, Int_t subevent= -1) const;

  void SetPid(const Char_t*);
  void SetPhi(Float_t);
  void SetEta(Float_t);
  void SetDedx(Float_t);
  void SetDedxMain(Float_t);
  void SetPt(Float_t);
  void SetP(Float_t);
  void SetY(Float_t);
  void SetCharge(Short_t);
  void SetDca(Float_t);
  void SetBx(Float_t);
  void SetBy(Float_t);
  void SetChi2(Float_t);
  void SetFitPts(Int_t);
  void SetMaxPts(Int_t);
  void SetSelect(Int_t harmonic, Int_t selection);
  void SetSubevent(Int_t harmonic, Int_t selection, Int_t subevent);

private:

  Char_t  mPid[10];
  Float_t mPhi;
  Float_t mEta;
  Float_t mDedx;
  Float_t mDedxMain;
  Float_t mPt;
  Float_t mP;
  Float_t mY;
  Short_t mCharge;
  Float_t mDca;
  Float_t mBx;
  Float_t mBy;
  Float_t mChi2;
  Int_t   mFitPts;
  Int_t   mMaxPts;
  Int_t   mSelection;
  Short_t mSubevent[Flow::nHars][Flow::nSels];

  ClassDef(StFlowTrack, 1)                     // macro for rootcint
};

inline const Char_t* StFlowTrack::Pid()     const { return mPid; }
inline Float_t  StFlowTrack::Phi()          const { return mPhi; }   
inline Float_t  StFlowTrack::Eta()          const { return mEta; }     
inline Float_t  StFlowTrack::Dedx()         const { return mDedx; }     
inline Float_t  StFlowTrack::DedxMain()     const { return mDedxMain; }     
inline Float_t  StFlowTrack::Pt()           const { return mPt; }
inline Float_t  StFlowTrack::P()            const { return mP; }
inline Float_t  StFlowTrack::Y()            const { return mY; }
inline Short_t  StFlowTrack::Charge()       const { return mCharge; }   
inline Float_t  StFlowTrack::Dca()          const { return mDca; }
inline Float_t  StFlowTrack::Bx()           const { return mBx; }
inline Float_t  StFlowTrack::By()           const { return mBy; }
inline Float_t  StFlowTrack::Chi2()         const { return mChi2; } 
inline Int_t    StFlowTrack::FitPts()       const { return mFitPts; }  
inline Int_t    StFlowTrack::MaxPts()       const { return mMaxPts; }  

inline Int_t StFlowTrack::Select(Int_t harmonic, Int_t selection,
 Int_t subevent) const {
  if (subevent == -1 || subevent == mSubevent[harmonic][selection]) {
    int bitShift = harmonic + Flow::nHars * selection;
    return (mSelection & (1 << bitShift)) ? kTRUE : kFALSE;
  }
  return kFALSE;         
}

inline void StFlowTrack::SetPid(const Char_t* pid)  { strncpy(mPid, pid, 9);
                                                         mPid[9] = '\0'; }
inline void StFlowTrack::SetPhi(Float_t phi)        { mPhi = phi; }      

inline void StFlowTrack::SetEta(Float_t eta)        { mEta = eta; }       

inline void StFlowTrack::SetDedx(Float_t dedx)      { mDedx = dedx; }       

inline void StFlowTrack::SetDedxMain(Float_t dedx)  { mDedxMain = dedx; }

inline void StFlowTrack::SetPt(Float_t pt)          { mPt = pt; }              

inline void StFlowTrack::SetP(Float_t p)            { mP = p; }              

inline void StFlowTrack::SetY(Float_t y)            { mY = y; }              

inline void StFlowTrack::SetCharge(Short_t charge)  { mCharge = charge; }     

inline void StFlowTrack::SetDca(Float_t dca)        { mDca = dca; }

inline void StFlowTrack::SetBx(Float_t bx)          { mBx = bx; }

inline void StFlowTrack::SetBy(Float_t by)          { mBy = by; }

inline void StFlowTrack::SetChi2(Float_t chi2)      { mChi2 = chi2; }

inline void StFlowTrack::SetFitPts(Int_t fitPts)    { mFitPts = fitPts; }

inline void StFlowTrack::SetMaxPts(Int_t maxPts)    { mMaxPts = maxPts; }

inline void StFlowTrack::SetSelect(Int_t harmonic, Int_t selection) {
  int bitShift = harmonic + Flow::nHars * selection;
  mSelection |= 1 << bitShift; }

inline void StFlowTrack::SetSubevent(Int_t harmonic, Int_t selection,
 Int_t subevent) { mSubevent[harmonic][selection] = subevent; }

#endif

