/***************************************************************************
 *
 * $Id: StFmsPoint.cxx,v 2.3 2015/08/26 16:51:25 ullrich Exp $
 *
 * Author: Thomas Burton, Yuxi Pan, 2014
 ***************************************************************************
 *
 * Description: Implementation of StFmsPoint, the StEvent FMS
 *              photon structure
 *
 ***************************************************************************
 *
 * $Log: StFmsPoint.cxx,v $
 * Revision 2.3  2015/08/26 16:51:25  ullrich
 * Fixed bug in cluster() and added print out fct and operator.
 *
 * Revision 2.2  2015/08/19 19:22:34  ullrich
 * Major update (PID) by Akio.
 *
 *
 ***************************************************************************/
#include "StFmsPoint.h"

static const char rcsid[] = "$Id: StFmsPoint.cxx,v 2.3 2015/08/26 16:51:25 ullrich Exp $";

StFmsPoint::StFmsPoint()
    : mDetectorId(0), mEnergy(-1.0), mX(-99.0), mY(-99.0), mId(-1), mParentClusterId(-1), mNParentClusterPhotons(-1), mCluster(0)
{
  resetFps(); 
}

StFmsPoint::~StFmsPoint() { /* no op */ }

void StFmsPoint::print(Option_t *option) const {cout<< *this <<endl;}

ostream& operator<<(ostream& os, const StFmsPoint& v)
{
    return os << Form("StFmsPoint: Id=%4d Det=%2d ParentId=%3d(%08x) x=%7.2f y=%7.2f E=%7.2f",
                      v.id(), v.detectorId(), v.parentClusterId(),
                      reinterpret_cast<ULong_t>(v.cluster()), v.x(), v.y(), v.energy());
}
