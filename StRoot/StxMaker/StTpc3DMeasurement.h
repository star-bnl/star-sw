#ifndef StTpc3DMeasurement_h
#define StTpc3DMeasurement_h
#include "TVectorD.h"
#include "GenFit/SpacepointMeasurement.h"
#if 0
class genfit::StateOnPlane;
#endif
class StTpcHit;
class StiHitErrorCalculator;
class StTpc3DMeasurement : public genfit::SpacepointMeasurement {
 public:
 StTpc3DMeasurement(const StTpcHit *tpcHit, genfit::TrackPoint* trackPoint);
  virtual ~StTpc3DMeasurement() {}

  virtual genfit::AbsMeasurement* clone() const {return new StTpc3DMeasurement(*this);}
#if 0
  virtual const TVectorD& getRawHitCoords(genfit::StateOnPlane *state=0) const;
  virtual const TMatrixDSym& getRawHitCov(genfit::StateOnPlane *state=0) const;
  virtual       TVectorD& getRawHitCoords(const genfit::StateOnPlane *state=0);
  virtual       TMatrixDSym& getRawHitCov(const genfit::StateOnPlane *state=0);
  static        void SetDebug(Int_t k) {fDebug = k;}
  virtual       Int_t Debug() {return fDebug;}
#endif
  virtual const StTpcHit *Hit() const {return fHit;}
 protected:
  const StTpcHit  *fHit; 
#if 0
  StiHitErrorCalculator *fErrCalc;
  static Int_t    fDebug;
#endif
 public:
  ClassDef(StTpc3DMeasurement,1)
};
#endif // StTpc3DMeasurement_h
