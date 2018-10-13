#ifndef StTpcPlanarMeasurement_h
#define StTpcPlanarMeasurement_h
#include "TVectorD.h"
#include "GenFit/PlanarMeasurement.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StTpcHit;
#if 0
class genfit::StateOnPlane;
#endif
class StTpcPlanarMeasurement : public genfit::PlanarMeasurement {
 public:
  StTpcPlanarMeasurement(int nDim = 2);
  StTpcPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, genfit::TrackPoint* trackPoint);
  StTpcPlanarMeasurement(const StTpcHit *hit, genfit::TrackPoint* trackPoint);
  virtual ~StTpcPlanarMeasurement() {}
  virtual genfit::AbsMeasurement* clone() const {return new StTpcPlanarMeasurement(*this);}
#if 0
  virtual const TVectorD& getRawHitCoords(genfit::StateOnPlane *state=0) const;
  virtual const TMatrixDSym& getRawHitCov(genfit::StateOnPlane *state=0) const;
#endif
  virtual       TVectorD& getRawHitCoords(const genfit::StateOnPlane *state=0);
  virtual       TMatrixDSym& getRawHitCov(const genfit::StateOnPlane *state=0);
  virtual       void SetDebug(Int_t k) {fDebug = k;}
  virtual       Int_t Debug() {return fDebug;}
  virtual const StTpcHit *Hit() const {return fHit;}
 protected:
  const StTpcHit  *fHit; 
  StiHitErrorCalculator *fErrCalc;
  static Int_t    fDebug;
 public:
  ClassDef(StTpcPlanarMeasurement,1)
};
#endif // StTpcPlanarMeasurement_h
