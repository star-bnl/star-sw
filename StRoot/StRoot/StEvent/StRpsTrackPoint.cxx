/***************************************************************************
 *
 * $Id: StRpsTrackPoint.cxx,v 2.1 2015/10/02 19:48:14 ullrich Exp $
 *
 * Author: Rafal Sikora, 1 Oct 2015
 *
 ***************************************************************************
 *
 * Description: StRpsTrackPoint class representing reconstructed (x,y,z)
 * position of the hit in single Roman Pot detector
 *
 ***************************************************************************
 *
 * $Log: StRpsTrackPoint.cxx,v $
 * Revision 2.1  2015/10/02 19:48:14  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StRpsTrackPoint.h"

ClassImp(StRpsTrackPoint)

StRpsTrackPoint::StRpsTrackPoint()
{
    mRpId = -1;
    for (unsigned int i=0; i<mNumberOfPlanesInRp; ++i) mClusterId[i] = -1;
    for (unsigned int i=0; i<mNumberOfPmtsInRp; ++i) mTime[i] = -1;
    mQuality = rpsNotSet;
}

StRpsTrackPoint::StRpsTrackPoint(const StRpsTrackPoint& trackPoint)
{
    *this = trackPoint;
}

StRpsTrackPoint::~StRpsTrackPoint() { /* no op */ }

StRpsTrackPoint& StRpsTrackPoint::operator=(const StRpsTrackPoint& trackPoint)
{
    if (this != &trackPoint) {
        mPosition = trackPoint.positionVec();
        mRpId = trackPoint.rpId();
        for (unsigned int i=0; i<mNumberOfPlanesInRp; ++i ) mClusterId[i] = trackPoint.clusterId(i);
        for (unsigned int i=0; i<mNumberOfPmtsInRp; ++i ) mTime[i] = trackPoint.time(i);
        mQuality = trackPoint.quality();
    }
    return *this;
}

unsigned int StRpsTrackPoint::planesUsed() const
{
    unsigned int nPlanesUsed = 0;
    for(unsigned int i=0; i<mNumberOfPlanesInRp; ++i)
        if (mClusterId[i]>-1) ++nPlanesUsed;
    return nPlanesUsed;
}