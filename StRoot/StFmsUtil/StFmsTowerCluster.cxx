// $Id: StFmsTowerCluster.cxx,v 1.2 2015/09/02 15:01:32 akio Exp $
//
// $Log: StFmsTowerCluster.cxx,v $
// Revision 1.2  2015/09/02 15:01:32  akio
// Removing StFmsGeometry class, and now it uses StFmsDbMaker to get appropriate parameters.
//
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsTowerCluster.cxx
 \brief     Implementation of StFmsTowerCluster, a cluster of FMS towers
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#include "StFmsUtil/StFmsTowerCluster.h"

#include <cmath>

#include "TMath.h"
#include "TVector2.h"

#include "StEvent/StFmsCluster.h"
#include "StEvent/StFmsHit.h"

#include "StFmsUtil/StFmsTower.h"

namespace FMSCluster {
StFmsTowerCluster::StFmsTowerCluster(StFmsCluster* cluster)
    : mEnergyCutoff(0.5), mCluster(cluster) {
  Clear();
}

StFmsTowerCluster::~StFmsTowerCluster() {}

void StFmsTowerCluster::Clear(const char* /* option */) {
  mSigmaX = mSigmaY = mSigmaXY = mChiSquare = -1.;
  mThetaAxis = -10;
  mTowers.clear();
}

void StFmsTowerCluster::calculateClusterMoments(Double_t Ecoff) {
  mEnergyCutoff = Ecoff;
  Double_t w0, w1, mtmp, mx, my, sigx, sigy, sigXY;
  w0 = w1 = mtmp = mx = my = sigx = sigy = sigXY = 0;
  for (Towers::const_iterator i = mTowers.begin(); i != mTowers.end(); ++i) {
    const StFmsTower* tower = *i;
    Double_t xxx, yyy;
    xxx = tower->column() - 0.5;
    yyy = tower->row() - 0.5;
    mtmp = log(tower->hit()->energy() + 1. - Ecoff) > 0 ?
           log(tower->hit()->energy() + 1. - Ecoff) : 0;
    w1 += mtmp;
    w0 += tower->hit()->energy();
    mx += mtmp * xxx;
    my += mtmp * yyy;
    sigx += mtmp * xxx * xxx;
    sigy += mtmp * yyy * yyy;
    sigXY += mtmp * xxx * yyy;
  }  // for
  mCluster->setEnergy(w0);
  if (w1 > 0) {
    mCluster->setX(mx / w1);
    mCluster->setY(my / w1);
    mSigmaX = sqrt(fabs(sigx / w1 - std::pow(mCluster->x(), 2.)));
    mSigmaY = sqrt(fabs(sigy / w1 - std::pow(mCluster->y(), 2.)));
    mSigmaXY = sigXY / w1 - mCluster->x() * mCluster->y();
  } else {
    mCluster->setX(0.);
    mCluster->setY(0.);
    mSigmaX = 0;
    mSigmaY = 0;
    mSigmaXY = 0;
  }  // if
}

void StFmsTowerCluster::findClusterAxis() {
  Double_t dSigma2, aA, bB;
  dSigma2 = mSigmaX * mSigmaX - mSigmaY * mSigmaY;
  aA = sqrt(dSigma2 * dSigma2 + 4.0 * mSigmaXY * mSigmaXY) + dSigma2;
  bB = 2 * mSigmaXY;
  if (mSigmaXY < 1e-10) {
    if (aA < 1e-10) {
      bB = sqrt(dSigma2 * dSigma2 + 4.0 * mSigmaXY * mSigmaXY) - dSigma2;
      aA = 2 * mSigmaXY;
    }  // if
  }  // if
  mThetaAxis = atan2(bB, aA);
  Double_t myPi = TMath::Pi();
  while (mThetaAxis > (myPi / 2.0)) {
    mThetaAxis -= myPi;
  }  // while
  while (mThetaAxis < -(myPi / 2.0)) {
    mThetaAxis += myPi;
  }  // while
  mCluster->setSigmaMin(getSigma(mThetaAxis));
  mCluster->setSigmaMax(getSigma(mThetaAxis - TMath::Pi() / 2.0));
}

Double_t StFmsTowerCluster::getSigma(Double_t theta) const {
  Double_t sigma = 0;
  // 2-d vector vaxis define the axis
  TVector2 vaxis(cos(theta), sin(theta));
  // loop over all towers pointer in cluster
  double wnew =0;
  for (Towers::const_iterator i = mTowers.begin(); i != mTowers.end(); ++i) {
    const StFmsTower* tower = *i;
    // the 2-d vector from the "center" of cluster to tower
    // "center" are at 0.5, 1.5, etc! Need shift of 0.5
    TVector2 v1(tower->column() - 0.5 - mCluster->x(),
                tower->row() - 0.5 - mCluster->y());
    // perpendicular distance to the axis = length of the component of vector
    // "v1" that is norm to "vaxis"
    Double_t dis = (v1.Norm(vaxis)).Mod();
    // contribution to sigma
    double wtmp = log(tower->hit()->energy() + 1. - mEnergyCutoff) > 0 ?
                  log(tower->hit()->energy() + 1. - mEnergyCutoff) : 0;
    wnew += wtmp;
    sigma += wtmp * dis * dis;
  }  // for
  return wnew > 0 ? sqrt(sigma / wnew) : 0;
}
}  // namespace FMSCluster
