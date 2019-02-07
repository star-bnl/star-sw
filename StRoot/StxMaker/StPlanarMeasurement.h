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
  virtual const StHit *Hit() const {return fHit;}
  virtual       StiHitErrorCalculator* ErrCalc() const {return fErrCalc;}
  static  void  SetDebug(Int_t k = 1) {fDebug = k;}
  static  Int_t Debug() {return fDebug;}
  static   void SetHitId(Int_t Id) {fHitId = Id;}
 protected:
  const StHit  *fHit; 
  StiHitErrorCalculator *fErrCalc;
  static Int_t    fDebug;
  static Int_t    fHitId;
 public:
  ClassDef(StPlanarMeasurement,1)
};
#endif // StPlanarMeasurement_h
