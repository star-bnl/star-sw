/*****************************************************************************
 *
 * $Id: StFmsCluster.cxx,v 2.2 2015/08/26 16:51:59 ullrich Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 * ***************************************************************************
 *
 * Description: Implementation of StFmsCluster, a group of adjacent FMS towers
 *
 * ***************************************************************************
 *
 * $Log: StFmsCluster.cxx,v $
 * Revision 2.2  2015/08/26 16:51:59  ullrich
 * Added print out fct and operator.
 *
 * Revision 2.1  2015/02/14 18:56:00  ullrich
 * Initial Revision.
 *
 *
 *****************************************************************************/
#include "StFmsCluster.h"

#include "StMessMgr.h"

static const char rcsid[] = "$Id: StFmsCluster.cxx,v 2.2 2015/08/26 16:51:59 ullrich Exp $";

StFmsCluster::StFmsCluster()
: StObject(), mCategory(0), mNTowers(0), mEnergy(0.), mX(0.),
mY(0.), mSigmaMin(0.), mSigmaMax(0.), mChi2Ndf1Photon(-1.),
mChi2Ndf2Photon(-1.), mId(0), mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StFmsCluster::~StFmsCluster() { /* no op */ }

void StFmsCluster::print(Option_t *option) const {cout<< *this <<endl;}

ostream& operator<<(ostream &os, const StFmsCluster& cluster) {
    os << Form("StFmsCluster(%08x) id=%4d catag=%1d nTow=%2d nPhoton=%1d x=%7.2f y=%7.2f E=%7.2f sigMax=%7.2f sigMin=%7.2f",
               reinterpret_cast<unsigned int>(&cluster), cluster.id(), cluster.category(), cluster.nTowers(), cluster.nPhotons(),
               cluster.x(), cluster.y(), cluster.energy(), cluster.sigmaMax(), cluster.sigmaMin());
    return os;
}
