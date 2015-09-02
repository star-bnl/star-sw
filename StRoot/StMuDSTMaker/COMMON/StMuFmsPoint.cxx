/*****************************************************************************
 * 
 * $Id: StMuFmsPoint.cxx,v 1.2 2015/09/02 22:09:58 jdb Exp $
 *
 * Author: Thomas Burton, 2014
 *****************************************************************************
 *
 * Description: Implementation of StMuFmsPoint, the MuDST FMS "point" class
 *
 *****************************************************************************
 *
 * $Log: StMuFmsPoint.cxx,v $
 * Revision 1.2  2015/09/02 22:09:58  jdb
 * Added Akios changes to Fms
 *
 *
 *****************************************************************************/
#include "StMuFmsPoint.h"

#include <algorithm>  // For std::min
#include <cmath>

#include "StFmsPoint.h"
#include "StMuFmsCluster.h"

StMuFmsPoint::StMuFmsPoint(int detectorId, float energy,
                           float x, float y, int id)
    : mDetectorId(detectorId), mEnergy(energy), mX(x), mY(y), mId(id) { }

StMuFmsPoint::StMuFmsPoint(const StFmsPoint& point) {
  set(point);
}

StMuFmsPoint::~StMuFmsPoint() { }

StThreeVectorF StMuFmsPoint::momentum(float m, float zvertex) const {
  m = std::min(m, mEnergy);  // Prevent m > E
  StThreeVectorF v(mXYZ.x(),mXYZ.y(),mXYZ.z()-zvertex);
  if (std::fabs(m) > 0.f) {
    v.setMag(std::sqrt(std::pow(mEnergy, 2.f) - std::pow(m, 2.f)));
  } else {
    v.setMag(mEnergy);
  }  // if
  return v;
}

StLorentzVectorF StMuFmsPoint::fourMomentum(float m, float zvertex) const {
  return StLorentzVectorF(momentum(m,zvertex), mEnergy);
}

StMuFmsCluster* StMuFmsPoint::cluster() {
  return static_cast<StMuFmsCluster*>(mCluster.GetObject());
}

const StMuFmsCluster* StMuFmsPoint::cluster() const {
  return static_cast<const StMuFmsCluster*>(mCluster.GetObject());
}

void StMuFmsPoint::set(const StFmsPoint& point) {
  mDetectorId = point.detectorId();
  mEnergy = point.energy();
  mX = point.x();
  mY = point.y();
  mId= point.id();
  mXYZ = point.XYZ();
  // Calculate z coordinate from StFmsPoint 4-momentum as it doesn't store
  // z directly. z / x = pz / px, so...
  // const StLorentzVectorF vec4 = point.fourMomentum();
  // mZ = point.x() * vec4.pz() / vec4.px();
}

void StMuFmsPoint::setCluster(StMuFmsCluster* cluster) {
  mCluster = cluster;
}
