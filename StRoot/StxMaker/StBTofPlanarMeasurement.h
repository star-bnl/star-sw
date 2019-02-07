#ifndef StBTofPlanarMeasurement_h
#define StBTofPlanarMeasurement_h
#include "TVectorD.h"
#include "StPlanarMeasurement.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StBTofHit;
#if 0
class genfit::StateOnPlane;
#endif
class StBTofPlanarMeasurement : public StPlanarMeasurement {
 public:
  StBTofPlanarMeasurement(int nDim = 2);
  StBTofPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, genfit::TrackPoint* trackPoint);
  StBTofPlanarMeasurement(const StBTofHit *hit, genfit::TrackPoint* trackPoint);
  virtual ~StBTofPlanarMeasurement() {}
  virtual genfit::AbsMeasurement* clone() const {return new StBTofPlanarMeasurement(*this);}
#if 0
  virtual const TVectorD& getRawHitCoords(genfit::StateOnPlane *state=0) const;
  virtual const TMatrixDSym& getRawHitCov(genfit::StateOnPlane *state=0) const;
#endif
  virtual       TVectorD& getRawHitCoords(const genfit::StateOnPlane *state=0);
  virtual       TMatrixDSym& getRawHitCov(const genfit::StateOnPlane *state=0);
 protected:
 public:
  ClassDef(StBTofPlanarMeasurement,1)
};
#endif // StBTofPlanarMeasurement_h
