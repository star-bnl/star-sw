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
#include "StFcsCluster.h"
#include "StMuFcsCluster.h"

#include "StMessMgr.h"
#include "StFcsHit.h"
#include "StMuFcsPoint.h"

static const char rcsid[] = "$Id: StMuFcsCluster.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $";

StMuFcsCluster::StMuFcsCluster(): mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StMuFcsCluster::~StMuFcsCluster() { /* no op */ }

void StMuFcsCluster::addNeighbor(StMuFcsCluster* neighbor) {
    int n=nNeighbor();
    for(int i=0; i<n; i++) if(mNeighbor[i]==neighbor) return; //already there, do nothing
    mNeighbor.Add(neighbor);
}

void StMuFcsCluster::addPoint(StMuFcsPoint* p) {
    mPoints.Add(p);
}

void StMuFcsCluster::addPoint(StMuFcsPoint* p1, StMuFcsPoint* p2) {
    mPoints.Add(p1);
    mPoints.Add(p2);
}

void StMuFcsCluster::print(Option_t *option) const {
    cout << Form(
      "StFcsCluster id=%4d ctg=%1d n=%2d nNeigh=%1d nPoints=%1d loc=%7.2f %7.2f PXYZE=%7.2lf %7.2lf %7.2lf %7.2lf E=%7.2lf sigMin/max=%7.2f %7.2f Chi2=%7.2f %7.2f",
      id(), category(), nTowers(), nNeighbor(),nPoints(),
      x(), y(), 
      fourMomentum().Px(),fourMomentum().Py(),fourMomentum().Pz(),fourMomentum().E(),
      energy(), sigmaMin(), sigmaMax(), chi2Ndf1Photon(), chi2Ndf2Photon()) << endl;
}
