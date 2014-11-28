/***********************************************************************
 *
 * $Id: StDecayAngle.hh,v 3.5 2002/04/30 16:02:47 genevb Exp $
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
 * Revision 3.5  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.4  2002/02/10 15:29:09  genevb
 * Additional functions for momenta of decay daughters in CM frame
 *
 * Revision 3.3  2001/11/28 17:54:29  genevb
 * Some errors/omissions in last check-in
 *
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

#define SHIFTED kTRUE
#define UNSHIFTED kFALSE

// Default beam axis is z-axis (for polarization calcs)
static TVector3 StDecayAngleBeam(0.,0.,1.);
static TLorentzVector StDecayAngleParent;
static TLorentzVector StDecayAngleDaughter;
static TLorentzVector StDecayAngleParentCopy;    // Use T (not M) to store
static TLorentzVector StDecayAngleDaughterCopy;  //   value of m in Copy
static Bool_t StDecayAngleShifted = UNSHIFTED; // Is daughter shifted?

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

  static TVector3 getShiftedDaughter(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2);

  static void setBeam(Float_t x, Float_t y, Float_t z);
  static void setParentDaughter(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2,
    Bool_t shift=UNSHIFTED);
  static void shiftToRest();
  static Bool_t different(TLorentzVector& v,
    Float_t px , Float_t py , Float_t pz , Float_t m );

};


inline void StDecayAngle::setBeam(Float_t x, Float_t y, Float_t z) {
  StDecayAngleBeam.SetXYZ(x,y,z);
}

inline Float_t StDecayAngle::decayTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2,SHIFTED);
  return decayTheta();
}

inline Float_t StDecayAngle::decayCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2,SHIFTED);
  return decayCosTheta();
}

inline Float_t StDecayAngle::decayThetaLab(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2);
  return decayTheta();
}

inline Float_t StDecayAngle::decayCosThetaLab(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2);
  return decayCosTheta();
}

inline Float_t StDecayAngle::polarityTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2,SHIFTED);
  return polarityTheta();
}

inline Float_t StDecayAngle::polarityCosTheta(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2,SHIFTED);
  return polarityCosTheta();
}

inline void StDecayAngle::setParentDaughter(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2,
    Bool_t shift) {
  if (different(StDecayAngleParentCopy,px1,py1,pz1,m1)) {
    StDecayAngleParentCopy.SetXYZT(px1,py1,pz1,m1);
    StDecayAngleParent.SetXYZM(px1,py1,pz1,m1);
  }
  if (different(StDecayAngleDaughterCopy,px2,py2,pz2,m2)) {
    StDecayAngleDaughterCopy.SetXYZT(px2,py2,pz2,m2);
    StDecayAngleDaughter.SetXYZM(px2,py2,pz2,m2);
    StDecayAngleShifted = UNSHIFTED;
  } else if (StDecayAngleShifted && !shift) {
    StDecayAngleDaughter.SetXYZM(px2,py2,pz2,m2);
    StDecayAngleShifted = UNSHIFTED;
  }
  if (shift) shiftToRest();
}

inline void StDecayAngle::shiftToRest() {
  if (!StDecayAngleShifted) {
    StDecayAngleDaughter.Boost(-StDecayAngleParent.BoostVector());
    StDecayAngleShifted = SHIFTED;
  }
}

inline TVector3 StDecayAngle::getShiftedDaughter(
    Float_t px1, Float_t py1, Float_t pz1, Float_t m1,
    Float_t px2, Float_t py2, Float_t pz2, Float_t m2) {
  setParentDaughter(px1,py1,pz1,m1,px2,py2,pz2,m2,SHIFTED);
  return TVector3(StDecayAngleDaughter.Vect());
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

inline Bool_t StDecayAngle::different(TLorentzVector& v,
    Float_t px , Float_t py , Float_t pz , Float_t m ) {
  return ((px != v.X()) || (py != v.Y()) || (pz != v.Z()) || (m != v.T()));
}

#endif
