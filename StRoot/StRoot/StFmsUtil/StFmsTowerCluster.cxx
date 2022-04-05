// $Id: StFmsTowerCluster.cxx,v 1.7 2016/06/07 15:51:44 akio Exp $
//
// $Log: StFmsTowerCluster.cxx,v $
// Revision 1.7  2016/06/07 15:51:44  akio
// Making code better based on Coverity reports
//
// Revision 1.6  2016/01/26 14:42:48  akio
// better chi2 handling
//
// Revision 1.5  2015/11/02 22:44:49  akio
// Fix photonEnergyInTower()
//
// Revision 1.4  2015/10/21 15:58:05  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
// Revision 1.3  2015/10/01 19:55:48  akio
// *** empty log message ***
//
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
StFmsTowerCluster::StFmsTowerCluster(StFmsCluster* cluster, Int_t detectorId)
    : mIndex(0), mDetectorId(detectorId), mEtot(0.0), mEnergyCutoff(0.5), mCluster(cluster) {
  Clear();
}

StFmsTowerCluster::~StFmsTowerCluster() {}

void StFmsTowerCluster::Clear(const char* /* option */) {
  mSigmaX = mSigmaY = mSigmaXY = mChiSquare = mChiSquare1 = mChiSquare2 = -1.;
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
    if(tower->hit()->detectorId() != mDetectorId){ //this is small cell merged to large cell
	xxx = (tower->column() - 0.5)/1.5;
	yyy = (tower->row() - 0.5)/1.5 + 9.0;
    }else{
	xxx = tower->column() - 0.5;
	yyy = tower->row() - 0.5;
    }
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
  mEtot=w0;
  mCluster->setEnergy(w0);
  if (w1 > 0) {
    mCluster->setX(mx / w1);
    mCluster->setY(my / w1);
    mSigmaX = sqrt(fabs(sigx / w1 - std::pow(mCluster->x(), 2.)));
    mSigmaY = sqrt(fabs(sigy / w1 - std::pow(mCluster->y(), 2.)));
    mSigmaXY = sigXY / w1 - mCluster->x() * mCluster->y();
  }else if(w0 > 0){
      //if cluster has no tower above ecoff, do it without cutoff
      Double_t keepEcoff = mEnergyCutoff;
      calculateClusterMoments(0.0);
      mEnergyCutoff = keepEcoff;
  }else{
      mCluster->setX(0.);
      mCluster->setY(0.);
      mSigmaX = 0;
      mSigmaY = 0;
      mSigmaXY = 0;
  }  // if
}

void StFmsTowerCluster::findClusterAxis(Double_t xwidth, Double_t ywidth) {
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
  mCluster->setSigmaMin(getSigma(mThetaAxis,xwidth,ywidth));
  mCluster->setSigmaMax(getSigma(mThetaAxis - TMath::Pi() / 2.0,xwidth,ywidth));
}

Double_t StFmsTowerCluster::getSigma(Double_t theta,Double_t xwidth, Double_t ywidth) {
  Double_t sigma = 0;
  // 2-d vector vaxis define the axis
  TVector2 vaxis(cos(theta), sin(theta));
  // loop over all towers pointer in cluster
  double wnew =0;
  int isSmall=0;  
  float emax=0.0;
  for (Towers::const_iterator i = mTowers.begin(); i != mTowers.end(); ++i) {
    StFmsTower* tower = *i;
    // the 2-d vector from the "center" of cluster to tower
    // "center" are at 0.5, 1.5, etc! Need shift of 0.5
    Double_t xxx, yyy;    
    int flag=0;
    if(emax<tower->hit()->energy()) {emax=tower->hit()->energy(); flag=1;}
    if(tower->hit()->detectorId() != mDetectorId){ //small cell merged to large cell list
        xxx = (tower->column() - 0.5)/1.5;
        yyy = (tower->row() - 0.5)/1.5 + 9.0;
	if(flag==1) isSmall=1;
    }else{
        xxx = tower->column() - 0.5;
        yyy = tower->row() - 0.5;
	if(flag==1) isSmall=0;
    }
    tower->setXY(xxx*xwidth,yyy*ywidth);
    TVector2 v1(xxx - mCluster->x(),
                yyy - mCluster->y());
    // perpendicular distance to the axis = length of the component of vector
    // "v1" that is norm to "vaxis"
    Double_t dis = (v1.Norm(vaxis)).Mod();
    // contribution to sigma
    double wtmp = log(tower->hit()->energy() + 1. - mEnergyCutoff) > 0 ?
	          log(tower->hit()->energy() + 1. - mEnergyCutoff) : 0;
    wnew += wtmp;
    sigma += wtmp * dis * dis;
  }  // for
  if(isSmall==0){
      return wnew > 0 ? sqrt(sigma / wnew) : 0;
  }else{
      return wnew > 0 ? 1.5*sqrt(sigma / wnew) : 0; //if highest tower is small cell merged to large cell list, put back correct sigma
  }
}

StFmsCluster* StFmsTowerCluster::release(){
    mCluster->setChi2Ndf1Photon(mChiSquare1);
    mCluster->setChi2Ndf2Photon(mChiSquare2);
    return mCluster.release();
}
    
}  // namespace FMSCluster
