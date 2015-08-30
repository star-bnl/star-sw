/*****************************************************************************
 *
 * $Id: StMuFmsCluster.cxx,v 1.1 2015/08/28 18:36:04 jdb Exp $
 *
 * Author: Thomas Burton , 2014
 *****************************************************************************
 *
 * Description: Implementation of StMuFmsCluster, the MuDST FMS cluster class
 *
 *****************************************************************************
 *
 * $Log: StMuFmsCluster.cxx,v $
 * Revision 1.1  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 *
 *****************************************************************************/ 
#include "StMuFmsCluster.h"

#include "StFmsCluster.h"

StMuFmsCluster::StMuFmsCluster(int detectorId, int category, float energy,
                               float x, float y)
    : mDetectorId(detectorId), mCategory(category), mEnergy(energy),
      mX(x), mY(y) { }

StMuFmsCluster::StMuFmsCluster(const StFmsCluster& cluster)
    : mDetectorId(cluster.detectorId()), mCategory(cluster.category()),
      mX(cluster.x()), mY(cluster.y()) { }

StMuFmsCluster::~StMuFmsCluster() { }

void StMuFmsCluster::Clear(Option_t* /* option */) {
  mHits.Clear();
  mPhotons.Clear();
}
