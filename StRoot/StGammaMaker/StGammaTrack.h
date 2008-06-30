// -*- C++ -*-

#ifndef __StGammaTrack_h__
#define __StGammaTrack_h__

class StMuTrack;
class StGammaCandidate;

#include "TVector3.h"
#include "TRefArray.h"
#include "StPhysicalHelix.hh"

class StGammaTrack : public TObject {
public:
  StGammaTrack();
  StGammaTrack(StMuTrack* track);
  ~StGammaTrack() {}

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGammaTrack.h,v 1.7 2008/06/30 14:58:45 jwebb Exp $ built "__DATE__" "__TIME__; return cvs;}


  Int_t    id;     /// id of the track
  Int_t    flag;   /// track flag
  Int_t    type;   /// track type 0=global 1=primary ...
  Int_t    charge; /// track charge
  Int_t    nhits;  /// nhits along track
  Float_t  dEdx;   /// energy loss in keV
  TVector3 momentum;
  TVector3 dca;	/// Distance of closest approach
  StPhysicalHelix helix; /// Returns inner helix (first measured point)
  StPhysicalHelix outerHelix; /// Returns outer helix (last measured point)
  TVector3 positionAtRadius(Double_t radius) const; /// Returns (0,0,0) if failed
  TVector3 positionAtZ(Double_t z) const;	/// Returns (0,0,0) if failed
  TRefArray candidates; /// Referencing candidates

  Float_t pt() const;   /// pt at vertex
  Float_t pz() const;   /// pz at vertex
  Float_t eta() const;  /// eta at vertex
  Float_t phi() const;  /// phi angle at vertex

private:

  ClassDef(StGammaTrack,2);
};

inline Float_t StGammaTrack::pt() const { return momentum.Pt(); }
inline Float_t StGammaTrack::pz() const { return momentum.Pz(); }
inline Float_t StGammaTrack::eta() const { return momentum.Eta(); }
inline Float_t StGammaTrack::phi() const { return momentum.Phi(); }

typedef std::vector<StGammaTrack> StGammaTrackVec_t;
typedef std::vector<StGammaTrack*> StGammaTrackPtrVec_t;

#endif
