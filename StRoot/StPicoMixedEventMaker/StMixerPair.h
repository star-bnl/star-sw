#ifndef StMixerPair_hh
#define StMixerPair_hh

/* **************************************************
 *  Generic class calculating and storing pairs in Event Mixing
 *  Allows to combine:
 *  - two particles, using
 *      StMixerPair(StPicoTrack const * particle1, StPicoTrack const * particle2, ...
 *
 * **************************************************
 *
 *  Initial Authors: 
 *         ** Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  ** Code Maintainer 
 *
 * **************************************************
 */

#include "TObject.h"
#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

class StMixerTrack;

class StMixerPair : public TObject
{
 public:
  StMixerPair();
  StMixerPair(StMixerPair const*);

  StMixerPair(StMixerTrack const&  particle1, StMixerTrack const& particle2, 
	   float p1MassHypo, float p2MassHypo,
	   StThreeVectorF const& vtx1, StThreeVectorF const& vtx2,
	   float bField);

  ~StMixerPair() {;}
  

  StLorentzVectorF const & lorentzVector() const;
  StThreeVectorF const & decayVertex() const;
  float m()    const;
  float pt()   const;
  float eta()  const;
  float phi()  const;
  float pointingAngle() const;
  float decayLength() const;
  float particle1Dca() const;
  float particle2Dca() const;
  StThreeVectorF const & particle1Mom() const;
  StThreeVectorF const & particle2Mom() const;
  float dcaDaughters() const;
  float cosThetaStar() const;
  float v0x() const;
  float v0y() const;
  float v0z() const;
  float px() const;
  float py() const;
  float pz() const;

 private:
  StMixerPair(StMixerPair const &);
  StMixerPair& operator=(StMixerPair const &);
  StLorentzVectorF mLorentzVector; 
  StThreeVectorF   mDecayVertex; 

  float mPointingAngle;
  float mDecayLength;
  float mParticle1Dca;
  float mParticle2Dca;

  StThreeVectorF mParticle1Mom;
  StThreeVectorF mParticle2Mom;

  float mDcaDaughters;
  float mCosThetaStar;

  ClassDef(StMixerPair,1)
};
inline StLorentzVectorF const& StMixerPair::lorentzVector() const { return mLorentzVector;}
inline float StMixerPair::m()    const { return mLorentzVector.m();}
inline float StMixerPair::pt()   const { return mLorentzVector.perp();}
inline float StMixerPair::eta()  const { return mLorentzVector.pseudoRapidity();}
inline float StMixerPair::phi()  const { return mLorentzVector.phi();}
inline float StMixerPair::px()   const { return mLorentzVector.px();}
inline float StMixerPair::py()   const { return mLorentzVector.py();}
inline float StMixerPair::pz()   const { return mLorentzVector.pz();}
inline float StMixerPair::pointingAngle() const { return mPointingAngle;}
inline float StMixerPair::decayLength()   const { return mDecayLength;}
inline float StMixerPair::particle1Dca()  const { return mParticle1Dca;}
inline float StMixerPair::particle2Dca()  const { return mParticle2Dca;}
inline StThreeVectorF const & StMixerPair::particle1Mom() const { return mParticle1Mom; }
inline StThreeVectorF const & StMixerPair::particle2Mom() const { return mParticle2Mom; }
inline float StMixerPair::dcaDaughters() const { return mDcaDaughters;}
inline float StMixerPair::cosThetaStar() const { return mCosThetaStar;}
inline StThreeVectorF const & StMixerPair::decayVertex() const { return mDecayVertex;}
inline float StMixerPair::v0x() const { return mDecayVertex.x();}
inline float StMixerPair::v0y() const { return mDecayVertex.y();}
inline float StMixerPair::v0z() const { return mDecayVertex.z();}
#endif

