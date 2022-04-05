/*****************************************************************************
 *
 * $Id: StFmsCluster.cxx,v 2.5 2016/06/07 15:51:34 akio Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 * ***************************************************************************
 *
 * Description: Implementation of StFmsCluster, a group of adjacent FMS towers
 *
 * ***************************************************************************
 *
 * $Log: StFmsCluster.cxx,v $
 * Revision 2.5  2016/06/07 15:51:34  akio
 * Making code better based on Coverity reports
 *
 * Revision 2.4  2015/10/21 14:52:09  ullrich
 * Modified print out format and added info.
 *
 * Revision 2.3  2015/09/01 18:29:01  ullrich
 * Changes due to adding StFpsSlat and interconnection between slats and points.
 *
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

static const char rcsid[] = "$Id: StFmsCluster.cxx,v 2.5 2016/06/07 15:51:34 akio Exp $";

StFmsCluster::StFmsCluster()
: StObject(), mDetectorId(0),mCategory(0), mNTowers(0), mEnergy(0.), mX(0.),
mY(0.), mSigmaMin(0.), mSigmaMax(0.), mChi2Ndf1Photon(-1.),
mChi2Ndf2Photon(-1.), mId(0), mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StFmsCluster::~StFmsCluster() { /* no op */ }

void StFmsCluster::print(Option_t *option) const {cout<< *this <<endl;}

ostream& operator<<(ostream &os, const StFmsCluster& cluster) {
    os << Form("StFmsCluster id=%4d categ=%1d nTow=%2d nPhoton=%1d loc=%7.2f %7.2f PXYZE=%7.2f %7.2f %7.2f %7.2f E=%7.2f sigMin/max=%7.2f %7.2f",
               cluster.id(), cluster.category(), cluster.nTowers(), cluster.nPhotons(),
               cluster.x(), cluster.y(), 
	       cluster.fourMomentum().px(),cluster.fourMomentum().py(),cluster.fourMomentum().pz(),cluster.fourMomentum().e(),
	       cluster.energy(), cluster.sigmaMin(), cluster.sigmaMax());
    return os;
}
