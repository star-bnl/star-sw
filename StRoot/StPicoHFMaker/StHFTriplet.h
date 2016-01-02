#ifndef StHFTriplet_hh
#define StHFTriplet_hh

/* **************************************************
 *  Generic class calculating and storing primary triplets in HF analysis
 *  Allows to combine:
 *  - three particles, using
 *      StHFTriplet(StPicoTrack const * particle1, StPicoTrack const * particle2, 
 *                  StPicoTrack const * particle3, ...
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
class StPicoEvent;

class StHFTriplet : public TObject
{
 public:
  StHFTriplet();
  StHFTriplet(StHFTriplet const *);
  StHFTriplet(StPicoTrack const * particle1, StPicoTrack const * particle2, StPicoTrack const * particle3, 
	     float p1MassHypo, float p2MassHypo, float p3MassHypo,
	     unsigned short p1Idx, unsigned short p2Idx, unsigned short p3Idx,
	     StThreeVectorF const & vtx, float bField);
  ~StHFTriplet() {;}

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
  float particle3Dca() const;
  unsigned short particle1Idx() const;
  unsigned short particle2Idx() const;
  unsigned short particle3Idx() const;
  float dcaDaughters12() const;
  float dcaDaughters23() const;
  float dcaDaughters31() const;
  float cosThetaStar() const;
  float v0x() const;
  float v0y() const;
  float v0z() const;
  float px() const;
  float py() const;
  float pz() const;

 private:
  StHFTriplet(StHFTriplet const &);
  StHFTriplet& operator=(StHFTriplet const &);
  StLorentzVectorF mLorentzVector; 
  StThreeVectorF   mDecayVertex; 

  float mPointingAngle;
  float mDecayLength;

  float mParticle1Dca;
  float mParticle2Dca;
  float mParticle3Dca;

  unsigned short  mParticle1Idx; // index of track in StPicoDstEvent
  unsigned short  mParticle2Idx;
  unsigned short  mParticle3Idx;

  float mDcaDaughters12;
  float mDcaDaughters23;
  float mDcaDaughters31;

  float mCosThetaStar; 
  
  ClassDef(StHFTriplet,2)
};
inline StLorentzVectorF const & StHFTriplet::lorentzVector() const { return mLorentzVector;}
inline float StHFTriplet::m()    const { return mLorentzVector.m();}
inline float StHFTriplet::pt()   const { return mLorentzVector.perp();}
inline float StHFTriplet::eta()  const { return mLorentzVector.pseudoRapidity();}
inline float StHFTriplet::phi()  const { return mLorentzVector.phi();}
inline float StHFTriplet::px()   const { return mLorentzVector.px();}
inline float StHFTriplet::py()   const { return mLorentzVector.py();}
inline float StHFTriplet::pz()   const { return mLorentzVector.pz();}
inline float StHFTriplet::pointingAngle() const { return mPointingAngle;}
inline float StHFTriplet::decayLength()   const { return mDecayLength;}
inline float StHFTriplet::particle1Dca()  const { return mParticle1Dca;}
inline float StHFTriplet::particle2Dca()  const { return mParticle2Dca;}
inline float StHFTriplet::particle3Dca()  const { return mParticle3Dca;}
inline unsigned short StHFTriplet::particle1Idx() const { return mParticle1Idx;}
inline unsigned short StHFTriplet::particle2Idx() const { return mParticle2Idx;}
inline unsigned short StHFTriplet::particle3Idx() const { return mParticle3Idx;}
inline float StHFTriplet::dcaDaughters12() const { return mDcaDaughters12;}
inline float StHFTriplet::dcaDaughters23() const { return mDcaDaughters23;}
inline float StHFTriplet::dcaDaughters31() const { return mDcaDaughters31;}
inline float StHFTriplet::cosThetaStar()   const { return mCosThetaStar;}
inline StThreeVectorF const & StHFTriplet::decayVertex() const { return mDecayVertex;}
inline float StHFTriplet::v0x() const { return mDecayVertex.x();}
inline float StHFTriplet::v0y() const { return mDecayVertex.y();}
inline float StHFTriplet::v0z() const { return mDecayVertex.z();}
#endif
