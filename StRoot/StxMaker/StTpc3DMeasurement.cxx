#include "StTpc3DMeasurement.h"
#include "StEvent/StTpcHit.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

StTpc3DMeasurement::StTpc3DMeasurement(const StTpcHit *tpcHit, genfit::TrackPoint* trackPoint) : 
  genfit::SpacepointMeasurement(), fHit(tpcHit) {
  rawHitCoords_(0) = fHit->position().x();
  rawHitCoords_(1) = fHit->position().y();
  rawHitCoords_(2) = fHit->position().z();
  rawHitCov_(0,0) = 0.01;
  rawHitCov_(1,1) = 0.01;
  rawHitCov_(2,2) = 0.01;
  detId_ = fHit->detector();
  hitId_ = fHit->id();
  this -> initG();
}
