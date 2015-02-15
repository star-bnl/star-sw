/*****************************************************************************
 *
 * $Id: StFmsCluster.cxx,v 2.1 2015/02/14 18:56:00 ullrich Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 * ***************************************************************************
 *
 * Description: Implementation of StFmsCluster, a group of adjacent FMS towers
 *
 * ***************************************************************************
 *
 * $Log: StFmsCluster.cxx,v $
 * Revision 2.1  2015/02/14 18:56:00  ullrich
 * Initial Revision.
 *
 *
 *****************************************************************************/
#include "StFmsCluster.h"

#include "StMessMgr.h"

static const char rcsid[] = "$Id: StFmsCluster.cxx,v 2.1 2015/02/14 18:56:00 ullrich Exp $";

StFmsCluster::StFmsCluster()
: StObject(), mCategory(0), mNTowers(0), mEnergy(0.), mX(0.),
mY(0.), mSigmaMin(0.), mSigmaMax(0.), mChi2Ndf1Photon(-1.),
mChi2Ndf2Photon(-1.), mId(0), mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StFmsCluster::~StFmsCluster() { /* no op */ }

ostream& operator<<(ostream &os, const StFmsCluster& cluster) {

    os << "========StFmsCluster:\n\tcatag:\t" << cluster.category()
       << "\n\tnumberTower:\t" << cluster.nTowers()
       << "\n\tnPhoton:\t" << cluster.nPhotons()
       << "\n\tcluster energy:\t" << cluster.energy()
       << "\n\tx0:\t" << cluster.x()
       << "\n\ty0:\t" << cluster.y()
       << "\n\tsiggmaMax:\t" << cluster.sigmaMax()
       << "\n\tsigmaMax:\t" << cluster.sigmaMin()
       << "\n\tchi2NdfPh1:\t" << cluster.chi2Ndf1Photon()
       << "\n\tchi2NdfPh2:\t" << cluster.chi2Ndf2Photon()
       << "\n\tid:\t" << cluster.id()
       << "\n\tPhoton List:  \n" << endl;

    return os;
}
