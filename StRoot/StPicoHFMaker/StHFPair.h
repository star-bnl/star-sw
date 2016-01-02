#ifndef StHFPair_hh
#define StHFPair_hh

/* **************************************************
 *  Generic class calculating and storing pairs in HF analysis
 *  Allows to combine:
 *  - two particles, using
 *      StHFPair(StPicoTrack const * particle1, StPicoTrack const * particle2, ...
 *  - a particle and another pair, using
 *      StHFPair(StPicoTrack const * particle1, StHFPair * particle2, ...
 *    - in the current implementation the incoming pair is seen as having charge = 0
 *    - after determining the vertex of particle and incoming pair, the 
 *      decay vertex (tertiary vertex) of incoming particle can be updated
 *    - straight line approximation is the default, but full helix can be used
 *
 * **************************************************
 *
 *  Initial Authors: 
 *            Xin Dong        (xdong@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *            Jochen Thaeder  (jmthader@lbl.gov)
 *          **Michael Lomnitz (mrlomnitz@lbl.gov)
 *
 *  ** Code Maintainer 
 *
 * **************************************************
 */

#include "TObject.h"
#include "TClonesArray.h"

#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

class StPicoTrack;

class StHFPair : public TObject
{
 public:
  StHFPair();
  StHFPair(StHFPair const *);

  StHFPair(StPicoTrack const * particle1, StPicoTrack const * particle2, 
	   float p1MassHypo, float p2MassHypo,
	   unsigned short p1Idx, unsigned short p2Idx,
	   StThreeVectorF const & vtx, float bField, bool useStraightLine = true);

  StHFPair(StPicoTrack const * particle1, StHFPair const * particle2, 
	   float p1MassHypo, float p2MassHypo,
	   unsigned short p1Idx, unsigned short p2Idx,
	   StThreeVectorF const & vtx, float bField, bool useStraightLine = true);

  ~StHFPair() {;}
  

  StLorentzVectorF const & lorentzVector() const;
  StThreeVectorF const & decayVertex() const;
  float m()    const;
  float pt()   const;
  float eta()  const;
  float phi()  const;
  float pointingAngle() const;
  float pointingAngle(StThreeVectorF const & vtx2) const;
  float decayLength() const;
  float decayLength(StThreeVectorF const & vtx2) const;
  float particle1Dca() const;
  float particle1Dca(StPicoTrack const * p1track, StThreeVectorF const & vtx2) const;
  float particle2Dca() const;
  float particle2Dca(StPicoTrack const * p1track, StThreeVectorF const & vtx2) const;
  unsigned short particle1Idx() const;
  unsigned short particle2Idx() const;
  float dcaDaughters() const;
  float cosThetaStar() const;
  float v0x() const;
  float v0y() const;
  float v0z() const;
  float px() const;
  float py() const;
  float pz() const;

 private:
  StHFPair(StHFPair const &);
  StHFPair& operator=(StHFPair const &);
  StLorentzVectorF mLorentzVector; 
  StThreeVectorF   mDecayVertex; 

  float mPointingAngle;
  float mDecayLength;
  float mParticle1Dca;
  float mParticle2Dca;

  unsigned short  mParticle1Idx; // index of track in StPicoDstEvent 
  unsigned short  mParticle2Idx; // index of track in StPicoDstEvent for particle, idx in tertiary vertex array for pair 

  float mDcaDaughters;
  float mCosThetaStar;

  ClassDef(StHFPair,2)
};
inline StLorentzVectorF const & StHFPair::lorentzVector() const { return mLorentzVector;}
inline float StHFPair::m()    const { return mLorentzVector.m();}
inline float StHFPair::pt()   const { return mLorentzVector.perp();}
inline float StHFPair::eta()  const { return mLorentzVector.pseudoRapidity();}
inline float StHFPair::phi()  const { return mLorentzVector.phi();}
inline float StHFPair::px()   const { return mLorentzVector.px();}
inline float StHFPair::py()   const { return mLorentzVector.py();}
inline float StHFPair::pz()   const { return mLorentzVector.pz();}
inline float StHFPair::pointingAngle() const { return mPointingAngle;}
inline float StHFPair::decayLength()   const { return mDecayLength;}
inline float StHFPair::particle1Dca()  const { return mParticle1Dca;}
inline float StHFPair::particle2Dca()  const { return mParticle2Dca;}
inline unsigned short StHFPair::particle1Idx() const { return mParticle1Idx;}
inline unsigned short StHFPair::particle2Idx() const { return mParticle2Idx;}
inline float StHFPair::dcaDaughters() const { return mDcaDaughters;}
inline float StHFPair::cosThetaStar() const { return mCosThetaStar;}
inline StThreeVectorF const & StHFPair::decayVertex() const { return mDecayVertex;}
inline float StHFPair::v0x() const { return mDecayVertex.x();}
inline float StHFPair::v0y() const { return mDecayVertex.y();}
inline float StHFPair::v0z() const { return mDecayVertex.z();}
#endif

