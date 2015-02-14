/***************************************************************************
 *
 * $Id: StFmsPoint.cxx,v 2.1 2015/02/14 18:56:00 ullrich Exp $
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
 * Revision 2.1  2015/02/14 18:56:00  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StFmsPoint.h"

static const char rcsid[] = "$Id: StFmsPoint.cxx,v 2.1 2015/02/14 18:56:00 ullrich Exp $";

StFmsPoint::StFmsPoint()
    : mDetectorId(0), mEnergy(-1.0), mX(-99.0), mY(-99.0), mId(-1),
      mParentClusterId(-1), mNParentClusterPhotons(-1), mCluster(0)
{ /* no op */ }

StFmsPoint::~StFmsPoint() { /* no op */ }
