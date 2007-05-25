// -*- C++ -*-

#ifndef __StGammaTrack_h__
#define __StGammaTrack_h__

class StMuTrack;

#include <TObject.h>
#include <vector>

#include "TRefArray.h"

class StGammaTrack : public TObject {
public:

  StGammaTrack();
  StGammaTrack(const StMuTrack* track);
  ~StGammaTrack() {}

  Short_t  id() const;    /// id of the track
  Short_t  flag() const;  /// track flag
  Short_t  type() const;  /// track type 0=global 1=primary ...
  UShort_t nhits() const; /// nhits along track
  Float_t  pt() const;   /// pt at vertex
  Float_t  eta() const;  /// eta at vertex
  Float_t  phi() const;  /// phi angle at vertex
  Float_t  dEdx() const; /// energy loss in keV

private:
  Short_t  mId;
  Short_t  mFlag;
  Short_t  mType; // 0=global 1=primary, ... 
  UShort_t mNhits;
  Float_t  mPt;
  Float_t  mEta;
  Float_t  mPhi;
  Float_t  mdEdx;

  TRefArray mCandidates; // referencing candidates

  ClassDef(StGammaTrack,1);
};

inline Short_t StGammaTrack::id() const { return mId; }
inline Short_t StGammaTrack::flag() const { return mFlag; }
inline UShort_t StGammaTrack::nhits() const { return mNhits; }
inline Float_t StGammaTrack::pt() const { return mPt; }
inline Float_t StGammaTrack::eta() const { return mEta; }
inline Float_t StGammaTrack::phi() const { return mPhi; }
inline Float_t StGammaTrack::dEdx() const { return mdEdx; }
inline Short_t StGammaTrack::type() const { return mType; }

typedef std::vector<StGammaTrack> StGammaTrackVec_t;
typedef std::vector<StGammaTrack*> StGammaTrackPtrVec_t;
#endif
