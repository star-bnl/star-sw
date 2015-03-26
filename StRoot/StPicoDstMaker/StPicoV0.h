#ifndef StPicoV0_hh
#define StPicoV0_hh

class StPicoTrack;
class StMuEvent;

#include "TObject.h"
#include "StThreeVectorF.hh"

class StPicoV0 : public TObject {
 public:
  StPicoV0();
  StPicoV0(StPicoV0*);
  ~StPicoV0();
  StPicoV0(StPicoTrack*, StPicoTrack*, StMuEvent*, Int_t*);
  void Clear(const Option_t* opt="");

  StPicoTrack *track(const Int_t i) const;
  Int_t       index2Track(const Int_t i) const;
  StThreeVectorF momentum(const Int_t i) const;

  StThreeVectorF v0Pos() const { return mV0Pos; }
  Float_t dcaDaughters() const { return (Float_t)mDcaDaughters/1000.; }
  Float_t   dca2Vertex() const { return (Float_t)mDca2Vtx/1000.; }
  Float_t            m() const { return mM; }

  Float_t     decayLength() const;
  StThreeVectorF momentum() const;
  
  void setIndex2Track(const Int_t id_pos, const Int_t id_neg);
  void setParticleHypothesis(const Int_t ip_pos, const Int_t ip_neg);
  void rotateTrack(const Int_t i);
    
 protected:
  Short_t   mIndex2Track[2];
  StThreeVectorF mMomentum[2];
  StThreeVectorF mV0Pos;
  UShort_t       mDcaDaughters;
  UShort_t       mDca2Vtx;
  Float_t        mM;

  friend class StPicoDst;

  ClassDef(StPicoV0, 1)
};

#endif
