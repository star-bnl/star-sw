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
//________________________________________________________________________________
StPlanarMeasurement::StPlanarMeasurement(int nDim)
  : PlanarMeasurement(nDim), fHit(0), fErrCalc(0) {}
//________________________________________________________________________________
StPlanarMeasurement::StPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, TrackPoint* trackPoint)
  : PlanarMeasurement(rawHitCoords, rawHitCov, detId, hitId, trackPoint), fHit(0), fErrCalc(0) {}
//________________________________________________________________________________
StPlanarMeasurement::StPlanarMeasurement(const StHit *hit,TrackPoint* trackPoint) : PlanarMeasurement(3), fHit(hit), fErrCalc(0) {
  detId_ = (Int_t) hit->detector();
  hitId_ = hit->id();
  trackPoint_ = 0; 
  rawHitCoords_ = TVectorD(3);
  rawHitCoords_(0) = hit->position().x();
  rawHitCoords_(1) = hit->position().y();
  rawHitCoords_(2) = hit->position().z();
  rawHitCov_(0,0) = hit->positionError().x()*hit->positionError().x();
  rawHitCov_(1,1) = hit->positionError().y()*hit->positionError().y();
  rawHitCov_(2,2) = hit->positionError().z()*hit->positionError().z();
}
#if 0
//________________________________________________________________________________
const TVectorD& StPlanarMeasurement::getRawHitCoords() const {return genfit::AbsMeasurement::getRawHitCoords();}
//________________________________________________________________________________
const TMatrixDSym& StPlanarMeasurement::getRawHitCov() const {return genfit::AbsMeasurement::getRawHitCov();}
//________________________________________________________________________________
TVectorD& StPlanarMeasurement::getRawHitCoords() {return genfit::AbsMeasurement::getRawHitCoords();}
//________________________________________________________________________________
TMatrixDSym& StPlanarMeasurement::getRawHitCov(genfit::StateOnPlane *state) {return genfit::AbsMeasurement::getRawHitCov();}
//________________________________________________________________________________
#endif
