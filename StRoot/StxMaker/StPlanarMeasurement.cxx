#include "StPlanarMeasurement.h"
#include "GenFit/Exception.h"
#include "GenFit/AbsMeasurement.h"
#include "GenFit/RKTrackRep.h"
#include "GenFit/HMatrixU.h"
#include "GenFit/HMatrixV.h"
#include "GenFit/HMatrixUV.h"
#include "GenFit/StateOnPlane.h"
#include "StEvent/StHit.h"

#include <cassert>
using namespace genfit;
Int_t StPlanarMeasurement::fDebug = 0;
Int_t StPlanarMeasurement::fHitId = 0;
//________________________________________________________________________________
StPlanarMeasurement::StPlanarMeasurement(int nDim)
  : PlanarMeasurement(nDim), fHit(0), fErrCalc(0) {fHitId++;}
//________________________________________________________________________________
StPlanarMeasurement::StPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, TrackPoint* trackPoint)
  : PlanarMeasurement(rawHitCoords, rawHitCov, detId, hitId, trackPoint), fHit(0), fErrCalc(0) {}
//________________________________________________________________________________
StPlanarMeasurement::StPlanarMeasurement(const StHit *hit,TrackPoint* trackPoint) : PlanarMeasurement(2), fHit(hit), fErrCalc(0) {
  fHit = hit;
  detId_ = (Int_t) fHit->detector();
  fHitId++;
  hitId_ = fHitId;
  trackPoint_ = trackPoint;
}
//________________________________________________________________________________
TVectorD& StPlanarMeasurement::getRawHitCoords(const genfit::StateOnPlane *state) {return genfit::AbsMeasurement::getRawHitCoords(state);}
//________________________________________________________________________________
TMatrixDSym& StPlanarMeasurement::getRawHitCov(const genfit::StateOnPlane *state) {return genfit::AbsMeasurement::getRawHitCov(state);}
//________________________________________________________________________________
