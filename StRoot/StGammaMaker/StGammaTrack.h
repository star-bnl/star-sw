// -*- C++ -*-

#ifndef __StGammaTrack_h__
#define __StGammaTrack_h__

class StMuTrack;
class StGammaCandidate;

#include "TObject.h"
#include "TVector3.h"
#include "TRefArray.h"
#include "StPhysicalHelix.hh"

class StGammaTrack : public TObject {
public:
  class Exception {};

  StGammaTrack();
  StGammaTrack(StMuTrack* track);
  ~StGammaTrack() {}

  Short_t  id() const;    /// id of the track
  Short_t  flag() const;  /// track flag
  Short_t  type() const;  /// track type 0=global 1=primary ...
  Short_t  charge() const; /// track charge
  UShort_t nhits() const; /// nhits along track
  Float_t  pt() const;   /// pt at vertex
  Float_t  pz() const;   /// pz at vertex
  Float_t  eta() const;  /// eta at vertex
  Float_t  phi() const;  /// phi angle at vertex
  Float_t  dEdx() const; /// energy loss in keV
  TVector3& momentum();
  TVector3& dca();
  StPhysicalHelix& helix(); /// Returns inner helix (first measured point)
  StPhysicalHelix& outerHelix(); /// Returns outer helix (last measured point)
  TVector3 positionAtRadius(double radius) const throw(Exception);
  TVector3 positionAtZ(double z) const throw(Exception);
  TRefArray candidates; // Referencing candidates

private:
  Short_t  mId;
  Short_t  mFlag;
  Short_t  mType; // 0=global 1=primary, ... 
  Short_t  mCharge;
  TVector3 mMomentum;
  TVector3 mDca;
  UShort_t mNhits;
  Float_t  mdEdx;
  StPhysicalHelix mHelix;
  StPhysicalHelix mOuterHelix;

  ClassDef(StGammaTrack,1);
};

inline Short_t StGammaTrack::id() const { return mId; }
inline Short_t StGammaTrack::flag() const { return mFlag; }
inline Short_t StGammaTrack::charge() const { return mCharge; }
inline UShort_t StGammaTrack::nhits() const { return mNhits; }
inline Float_t StGammaTrack::pt() const { return mMomentum.Pt(); }
inline Float_t StGammaTrack::pz() const { return mMomentum.Pz(); }
inline Float_t StGammaTrack::eta() const { return mMomentum.Eta(); }
inline Float_t StGammaTrack::phi() const { return mMomentum.Phi(); }
inline Float_t StGammaTrack::dEdx() const { return mdEdx; }
inline Short_t StGammaTrack::type() const { return mType; }
inline TVector3& StGammaTrack::momentum() { return mMomentum; }
inline TVector3& StGammaTrack::dca() { return mDca; }
inline StPhysicalHelix& StGammaTrack::helix() { return mHelix; }
inline StPhysicalHelix& StGammaTrack::outerHelix() { return mOuterHelix; }

typedef std::vector<StGammaTrack> StGammaTrackVec_t;
typedef std::vector<StGammaTrack*> StGammaTrackPtrVec_t;

#endif
