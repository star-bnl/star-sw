// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 2 September 2009
//

#ifndef ST_JET_ELEMENT_H
#define ST_JET_ELEMENT_H

#include "TVector3.h"
#include "TRef.h"

#include "StJetCandidate.h"

class StJetElement : public TObject {
public:
  StJetElement(short id = -1, short detId = -1)
    : mId(id)
    , mDetectorId(detId)
    , mPt(0)
    , mEta(0)
    , mPhi(0)
    , mJet(0)
  {
  }

  short           id           () const { return mId; }
  short           detectorId   () const { return mDetectorId; }
  float           pt           () const { return mPt; }
  float           eta          () const { return mEta; }
  float           phi          () const { return mPhi; }
  TVector3        momentum     () const;
  TVector3        localMomentum() const;
  float           jt           () const { return localMomentum().Perp(); }
  float           ps           () const { return localMomentum().Px(); }
  float           pn           () const { return localMomentum().Py(); }
  float           pl           () const { return localMomentum().Pz(); }
  float           frag         () const { return momentum().Mag() / jet()->momentum().Mag(); }
  StJetCandidate* jet          () const { return (StJetCandidate*)mJet.GetObject(); }

  void setJet(const StJetCandidate* jet) { mJet = (TObject*)jet; }

protected:
  short mId;
  short mDetectorId;
  float mPt;
  float mEta;
  float mPhi;
  TRef  mJet;

  ClassDef(StJetElement, 1);
};

inline TVector3 StJetElement::momentum() const
{
  TVector3 mom;
  mom.SetPtEtaPhi(mPt,mEta,mPhi);
  return mom;
}

inline TVector3 StJetElement::localMomentum() const
{
  TVector3 longUnit = jet()->momentum().Unit();
  TVector3 normUnit = TVector3(0,0,1).Cross(longUnit).Unit();
  TVector3 sideUnit = longUnit.Cross(normUnit);
  TVector3 mom = momentum();
  return TVector3(mom.Dot(sideUnit),mom.Dot(normUnit),mom.Dot(longUnit));
}

#endif // ST_JET_ELEMENT_H
