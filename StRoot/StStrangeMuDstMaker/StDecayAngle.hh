/***********************************************************************
 *
 * $Id: StDecayAngle.hh,v 3.2 2001/11/28 17:19:42 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, 26-Nov-2001
 *
 ***********************************************************************
 *
 * Description: class for returning decay angle characteristics
 *              Particle 1 is the StDecayAngleParent of the decay
 *              Particle 2 is the daughter of interest of the decay
 *              px, py, pz, and m are their momemta and mass
 *
 ***********************************************************************
 *
 * $Log: StDecayAngle.hh,v $
 * Revision 3.2  2001/11/28 17:19:42  genevb
 * Some interface improvements to decay angle formulae
 *
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
static TLorentzVector StDecayAngleParent;
static TLorentzVector StDecayAngleDaughter;

class StDecayAngle {
 public:
  StDecayAngle() {}
  virtual ~StDecayAngle() {}
  static Float_t decayTheta();
  static Float_t decayTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static Float_t decayCosTheta();
  static Float_t decayCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);

  static Float_t decayThetaLab(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static Float_t decayCosThetaLab(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);

  static Float_t polarityTheta();
  static Float_t polarityTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static Float_t polarityCosTheta();
  static Float_t polarityCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);

  static void setBeam(Float_t x, Float_t y, Float_t z);
  static void setParentDaughter(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);
  static void shiftToRest();

};


inline void StDecayAngle::setBeam(Float_t x, Float_t y, Float_t z) {
  StDecayAngleBeam.SetXYZ(x,y,z);
}

inline Float_t StDecayAngle::decayCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2);
  shiftToRest();
  return decayCosTheta();
}

inline Float_t StDecayAngle::decayCosThetaLab(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2);
  return decayCosTheta();
}

inline Float_t StDecayAngle::polarityCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2);
  shiftToRest();
  return polarityCosTheta();
}

inline void StDecayAngle::setParentDaughter(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  StDecayAngleParent.SetXYZM(px1,py1,pz1,m1);
  StDecayAngleDaughter.SetXYZM(px2,py2,pz2,m2);
}

inline void StDecayAngle::shiftToRest() {
  StDecayAngleDaughter.Boost(-StDecayAngleParent.BoostVector());
}

inline Float_t StDecayAngle::decayTheta() {
  return StDecayAngleDaughter.Vect().Angle(StDecayAngleParent.Vect());
}

inline Float_t StDecayAngle::decayCosTheta() {
  return TMath::Cos(decayTheta());
}

inline Float_t StDecayAngle::polarityTheta() {
  return StDecayAngleDaughter.Vect().Angle(
    StDecayAngleParent.Vect().Cross(StDecayAngleBeam));
}

inline Float_t StDecayAngle::polarityCosTheta() {
  return TMath::Cos(polarityTheta());
}

#endif
