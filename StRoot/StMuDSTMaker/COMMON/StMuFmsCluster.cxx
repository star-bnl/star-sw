/*****************************************************************************
 *
 * $Id: StMuFmsCluster.cxx,v 1.3 2016/06/14 17:11:34 jdb Exp $
 *
 * Author: Thomas Burton , 2014
 *****************************************************************************
 *
 * Description: Implementation of StMuFmsCluster, the MuDST FMS cluster class
 *
 *****************************************************************************
 *
 * $Log: StMuFmsCluster.cxx,v $
 * Revision 1.3  2016/06/14 17:11:34  jdb
 * Fixing Coverity Errors:
 * StMuFmsCluster.cxx : UNINIT_CTOR on member mEnergy
 * StMuFmsUtile.cxx : DEADCODE on check for null pointer
 *
 * Revision 1.2  2015/09/02 22:09:58  jdb
 * Added Akios changes to Fms
 *
 *
 *****************************************************************************/ 
#include "StMuFmsCluster.h"

#include "StFmsCluster.h"

StMuFmsCluster::StMuFmsCluster(int detectorId, int category, float energy,
                               float x, float y, float smin, float smax,
			       float chi1, float chi2, int id)
    : mDetectorId(detectorId), mCategory(category), mEnergy(energy),
      mX(x), mY(y), mSigmaMin(smin), mSigmaMax(smax), 
      mChi2Ndf1Photon(chi1), mChi2Ndf2Photon(chi2), mId(id){ }

StMuFmsCluster::StMuFmsCluster(const StFmsCluster& cluster)
    : mDetectorId(cluster.detectorId()), mCategory(cluster.category()), mEnergy(cluster.energy()),
      mX(cluster.x()), mY(cluster.y()), 
      mSigmaMin(cluster.sigmaMin()), mSigmaMax(cluster.sigmaMax()), 
      mChi2Ndf1Photon(cluster.chi2Ndf1Photon()), mChi2Ndf2Photon(cluster.chi2Ndf2Photon()), 
      mId(cluster.id())
{ }

StMuFmsCluster::~StMuFmsCluster() { }

void StMuFmsCluster::Clear(Option_t* /* option */) {
  mHits.Clear();
  mPhotons.Clear();
}
