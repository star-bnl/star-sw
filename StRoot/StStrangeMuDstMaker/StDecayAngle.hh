/***********************************************************************
 *
 * $Id: StDecayAngle.hh,v 3.1 2001/11/28 05:14:59 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, 26-Nov-2001
 *
 ***********************************************************************
 *
 * Description: class for returning decay angle characteristics
 *              Particle 1 is the parent of the decay
 *              Particle 2 is the daughter of interest of the decay
 *              px, py, pz, and m are their momemta and mass
 *
 ***********************************************************************
 *
 * $Log: StDecayAngle.hh,v $
 * Revision 3.1  2001/11/28 05:14:59  genevb
 * Additional decay angle functions
 *
 *
 ***********************************************************************/
#ifndef StDecayAngle_hh
#define StDecayAngle_hh
#include "TLorentzVector.h"

// Default beam axis is z-axis (for polarization calcs)
static TVector3 StDecayAngleBeam(0.,0.,1.);

class StDecayAngle {
 public:
  StDecayAngle() {}
  virtual ~StDecayAngle() {}
  static Float_t decayCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static Float_t polarityCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static void setBeam(Float_t x, Float_t y, Float_t z);

  static void shiftToParentRest(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static TLorentzVector parent;
  static TLorentzVector daughter;
};


inline void StDecayAngle::setBeam(Float_t x, Float_t y, Float_t z) {
  StDecayAngleBeam.SetXYZ(x,y,z);
}

inline Float_t StDecayAngle::decayCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  shiftToParentRest(px1,py1,pz1,m1,px2,py2,pz2,m2);
  return TMath::Cos(daughter.Vect().Angle(parent.Vect()));
}

inline Float_t StDecayAngle::polarityCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  shiftToParentRest(px1,py1,pz1,m1,px2,py2,pz2,m2);
  return TMath::Cos(daughter.Vect().Angle(parent.Vect().Cross(StDecayAngleBeam)));
}

inline void StDecayAngle::shiftToParentRest(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  parent.SetXYZM(px1,py1,pz1,m1);
  daughter.SetXYZM(px2,py2,pz2,m2);
  daughter.Boost(-parent.BoostVector());
}

#endif
