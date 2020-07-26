/*****************************************************************************
 *
 * $Id: StFcsCluster.cxx,v 1.3 2019/10/23 13:27:07 akio Exp $
 *
 * Author: Akio Ogawa 2018
 * ***************************************************************************
 *
 * Description: Implementation of StFcsCluster, a group of adjacent FCS towers
 *
 * ***************************************************************************
 *
 * $Log: StFcsCluster.cxx,v $
 * Revision 1.3  2019/10/23 13:27:07  akio
 * including StFcsPoint for StFcsPointMaker
 *
 * Revision 1.2  2019/06/21 17:43:21  akio
 * added neighbor
 *
 * Revision 1.1  2018/11/14 16:48:59  akio
 * FCS codes in offline/upgrade/akio
 *
 *
 *****************************************************************************/
#include "StFcsCluster.h"

#include "StMessMgr.h"
#include "StFcsHit.h"
#include "StFcsPoint.h"

static const char rcsid[] = "$Id: StFcsCluster.cxx,v 1.3 2019/10/23 13:27:07 akio Exp $";

StFcsCluster::StFcsCluster(): StObject(), mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StFcsCluster::~StFcsCluster() { /* no op */ }

void StFcsCluster::addNeighbor(StFcsCluster* neighbor){
  int n=nNeighbor();
  for(int i=0; i<n; i++) if(mNeighbor[i]==neighbor) return; //already there, do nothing
  mNeighbor.push_back(neighbor);
}

void StFcsCluster::addPoint(StFcsPoint* p){
  mPoints.push_back(p);
}

void StFcsCluster::addPoint(StFcsPoint* p1, StFcsPoint* p2){
  mPoints.push_back(p1);
  mPoints.push_back(p2);
}

void StFcsCluster::print(Option_t *option) const {
    cout << Form(
      "StFcsCluster id=%4d ctg=%1d n=%2d nNeigh=%1d nPoints=%1d loc=%7.2f %7.2f PXYZE=%7.2f %7.2f %7.2f %7.2f E=%7.2f sigMin/max=%7.2f %7.2f Chi2=%7.2f %7.2f",
      id(), category(), nTowers(), nNeighbor(),nPoints(),
      x(), y(), 
      fourMomentum().px(),fourMomentum().py(),fourMomentum().pz(),fourMomentum().e(),
      energy(), sigmaMin(), sigmaMax(), chi2Ndf1Photon(), chi2Ndf2Photon()) << endl;
}
