#ifndef StHFQuadruplet_hh
#define StHFQuadruplet_hh

/* **************************************************
 *  Generic class calculating and storing primary quadruplets in HF analysis
 *  Allows to combine:
 *  - four particles, using
 *      StHFQuadruplet(StPicoTrack const * particle1, StPicoTrack const * particle2, 
 *                  StPicoTrack const * particle3, ...
 *  - a pair and 4 particles using:
 *      StHFQuadruplet(StPicoTrack const * particle1, StPicoTrack const * particle2,
 *                     StPicoTrack const * particle3, StHFPair const * pair ...
 *
 * **************************************************
 *
 *  Initial Authors:                                                                                                                       
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
#include "StHFPair.h"
class StPicoTrack;
class StPicoEvent;
class StHFPair;

class StHFQuadruplet : public TObject
{
 public:
  StHFQuadruplet();
  StHFQuadruplet(StHFQuadruplet const *);
  StHFQuadruplet(StPicoTrack const * particle1, StPicoTrack const * particle2, StPicoTrack const * particle3, StPicoTrack const * particle4, 
		 float p1MassHypo, float p2MassHypo, float p3MassHypo, float p4MassHypo,
		 unsigned short p1Idx, unsigned short p2Idx, unsigned short p3Idx, unsigned short p4Idx,
		 StThreeVectorF const & vtx, float bField);

  StHFQuadruplet(StPicoTrack const * particle1, StPicoTrack const * particle2, StPicoTrack const * particle3, StHFPair const * particle4,
		 float p1MassHypo, float p2MassHypo, float p3MassHypo, float p4MassHypo,
		 unsigned short p1Idx, unsigned short p2Idx, unsigned short p3Idx, unsigned short p4Idx,
		 StThreeVectorF const & vtx, float bField);

  ~StHFQuadruplet() {;}

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
  float particle4Dca() const;
  unsigned short particle1Idx() const;
  unsigned short particle2Idx() const;
  unsigned short particle3Idx() const;
  unsigned short particle4Idx() const;
  float dcaDaughters12() const;
  float dcaDaughters13() const;
  float dcaDaughters14() const;
  float dcaDaughters23() const;
  float dcaDaughters24() const;
  float dcaDaughters34() const;
  float cosThetaStar() const;
  float v0x() const;
  float v0y() const;
  float v0z() const;
  float px() const;
  float py() const;
  float pz() const;


 private:
  StHFQuadruplet(StHFQuadruplet const &);
  StHFQuadruplet& operator=(StHFQuadruplet const &);
  StLorentzVectorF mLorentzVector; 
  StThreeVectorF   mDecayVertex; 

  float mPointingAngle;
  float mDecayLength;

  float mParticle1Dca;
  float mParticle2Dca;
  float mParticle3Dca;
  float mParticle4Dca;

  unsigned short  mParticle1Idx; // index of track in StPicoDstEvent
  unsigned short  mParticle2Idx;
  unsigned short  mParticle3Idx;
  unsigned short  mParticle4Idx;

  float mDcaDaughters12;
  float mDcaDaughters13;
  float mDcaDaughters14;
  float mDcaDaughters23;
  float mDcaDaughters24;
  float mDcaDaughters34;


  float mCosThetaStar; 
  ClassDef(StHFQuadruplet,2)
};
inline StLorentzVectorF const & StHFQuadruplet::lorentzVector() const { return mLorentzVector;}
inline float StHFQuadruplet::m()    const { return mLorentzVector.m();}
inline float StHFQuadruplet::pt()   const { return mLorentzVector.perp();}
inline float StHFQuadruplet::eta()  const { return mLorentzVector.pseudoRapidity();}
inline float StHFQuadruplet::phi()  const { return mLorentzVector.phi();}
inline float StHFQuadruplet::px()   const { return mLorentzVector.px();}
inline float StHFQuadruplet::py()   const { return mLorentzVector.py();}
inline float StHFQuadruplet::pz()   const { return mLorentzVector.pz();}
inline float StHFQuadruplet::pointingAngle() const { return mPointingAngle;}
inline float StHFQuadruplet::decayLength()   const { return mDecayLength;}
inline float StHFQuadruplet::particle1Dca()  const { return mParticle1Dca;}
inline float StHFQuadruplet::particle2Dca()  const { return mParticle2Dca;}
inline float StHFQuadruplet::particle3Dca()  const { return mParticle3Dca;}
inline float StHFQuadruplet::particle4Dca()  const { return mParticle4Dca;}
inline unsigned short StHFQuadruplet::particle1Idx() const { return mParticle1Idx;}
inline unsigned short StHFQuadruplet::particle2Idx() const { return mParticle2Idx;}
inline unsigned short StHFQuadruplet::particle3Idx() const { return mParticle3Idx;}
inline unsigned short StHFQuadruplet::particle4Idx() const { return mParticle4Idx;}
inline float StHFQuadruplet::dcaDaughters12() const { return mDcaDaughters12;}
inline float StHFQuadruplet::dcaDaughters13() const { return mDcaDaughters13;}
inline float StHFQuadruplet::dcaDaughters14() const { return mDcaDaughters14;}
inline float StHFQuadruplet::dcaDaughters23() const { return mDcaDaughters23;}
inline float StHFQuadruplet::dcaDaughters24() const { return mDcaDaughters24;}
inline float StHFQuadruplet::dcaDaughters34() const { return mDcaDaughters34;}
inline float StHFQuadruplet::cosThetaStar()   const { return mCosThetaStar;}
inline StThreeVectorF const & StHFQuadruplet::decayVertex() const { return mDecayVertex;}
inline float StHFQuadruplet::v0x() const { return mDecayVertex.x();}
inline float StHFQuadruplet::v0y() const { return mDecayVertex.y();}
inline float StHFQuadruplet::v0z() const { return mDecayVertex.z();}

#endif
