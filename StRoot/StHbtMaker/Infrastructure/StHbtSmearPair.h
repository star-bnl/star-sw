/***************************************
 * $Id: StHbtSmearPair.h,v 1.6 2003/05/22 20:59:03 perev Exp $
 *
 * Author: Mike Lisa, Ohio State lisa@mps.ohio-state.edu
 ****************************************
 *
 * Description: given a StHbtPair, this class provides a corresponding
 *     StHbtPair whose constituent StHbtParticles' momenta have been
 *     smeared according to a parameterization of the STAR momentum
 *     resolution.
 *     >> The input StHbtPair is not altered in anyway <<
 *
 ******************************************
 *
 * $Log: StHbtSmearPair.h,v $
 * Revision 1.6  2003/05/22 20:59:03  perev
 * virtual dtr to avoid warning
 *
 * Revision 1.5  2003/05/08 17:22:12  magestro
 * Un-made change for the second time that causes library not to load
 *
 * Revision 1.4  2003/04/30 20:37:23  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.3  2003/02/05 21:16:10  magestro
 * Reverted back to non-virtual destructor because library wouldn't load (not understood yet)
 *
 * Revision 1.2  2003/01/31 19:00:38  magestro
 * Destructor made virtual, due to virtual destructor of a data member
 *
 * Revision 1.1  2001/05/23 00:19:05  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 *
 ******************************************/

#ifndef StHbtSmearPair_h
#define StHbtSmearPair_h

#include "StHbtMaker/Infrastructure/StHbtPair.hh"

class StHbtSmearPair{
 public:

  StHbtSmearPair();
  StHbtSmearPair(const StHbtPair* unSmearedPair);
  virtual ~StHbtSmearPair(){};

  void SetUnsmearedPair(const StHbtPair* unSmearedPair);  // essentially same as c'tor

  StHbtPair& SmearedPair();  // access to the smeared pair

  //========= resolution parameters ==========
  // pT resolution parameterized by d(pT) = Frac*pT
  void SetFractionalPtRes(double);
  // phi resolution parameterized d(phi)= by a+b*P^alpha (Fabrice Retiere's way)
  void SetPhiRes_a(double a);
  void SetPhiRes_b(double b);
  void SetPhiRes_alpha(double alpha);
  // phi resolution parameterized by d(theta) = a+b*P^alpha (Fabrice Retiere's way)
  void SetThetaRes_a(double a);
  void SetThetaRes_b(double b);
  void SetThetaRes_alpha(double alpha);
  //==========================================

  StHbtLorentzVector SmearedMomentum(StHbtLorentzVector input);

 private:

  StHbtPair mSmearedPair;
  StHbtParticle mParticle1;
  StHbtParticle mParticle2;

  //========= resolution parameters ==========
  double mFracPtRes;
  double mPhi_a;
  double mPhi_b;
  double mPhi_alpha;
  double mTheta_a;
  double mTheta_b;
  double mTheta_alpha;
  //==========================================

  void setup();

#ifdef __ROOT__
  ClassDef(StHbtSmearPair, 0)
#endif

};

inline void StHbtSmearPair::SetFractionalPtRes(double val){mFracPtRes = val;}
inline void StHbtSmearPair::SetPhiRes_a(double val){mPhi_a = val;}
inline void StHbtSmearPair::SetPhiRes_b(double val){mPhi_b = val;}
inline void StHbtSmearPair::SetPhiRes_alpha(double val){mPhi_alpha = val;}
inline void StHbtSmearPair::SetThetaRes_a(double val){mTheta_a = val;}
inline void StHbtSmearPair::SetThetaRes_b(double val){mTheta_b = val;}
inline void StHbtSmearPair::SetThetaRes_alpha(double val){mTheta_alpha = val;}

inline StHbtPair& StHbtSmearPair::SmearedPair(){return mSmearedPair;}

#endif
