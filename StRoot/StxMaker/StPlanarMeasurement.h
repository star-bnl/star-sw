#ifndef StPlanarMeasurement_h
#define StPlanarMeasurement_h
#include "TVectorD.h"
#include "GenFit/PlanarMeasurement.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StHit;
#if 0
class genfit::StateOnPlane;
#endif
class StPlanarMeasurement : public genfit::PlanarMeasurement {
 public:
  StPlanarMeasurement(int nDim = 1);
  StPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, genfit::TrackPoint* trackPoint);
  StPlanarMeasurement(const StHit *hit, genfit::TrackPoint* trackPoint);
  virtual ~StPlanarMeasurement() {}
  virtual genfit::AbsMeasurement* clone() const {return new StPlanarMeasurement(*this);}
#if 0
  virtual const TVectorD& getRawHitCoords(const genfit::StateOnPlane *state=0) const;
  virtual const TMatrixDSym& getRawHitCov(const genfit::StateOnPlane *state=0) const;
#endif
  virtual       TVectorD& getRawHitCoords(const genfit::StateOnPlane *state=0);
  virtual       TMatrixDSym& getRawHitCov(const genfit::StateOnPlane *state=0);
 protected:
  const StHit  *fHit; 
  StiHitErrorCalculator *fErrCalc;
 public:
  ClassDef(StPlanarMeasurement,1)
};
#endif // StPlanarMeasurement_h
