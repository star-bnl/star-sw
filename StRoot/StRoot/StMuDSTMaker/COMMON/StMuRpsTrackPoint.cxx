#include "StMuRpsTrackPoint.h"
#include "StEvent/StRpsTrackPoint.h"

ClassImp(StMuRpsTrackPoint)

StMuRpsTrackPoint::StMuRpsTrackPoint() {
    mRpId = -1;
    for (unsigned int i=0; i<StRpsTrackPoint::mNumberOfPlanesInRp; ++i) mClusterId[i] = -1;
    for (unsigned int i=0; i<StRpsTrackPoint::mNumberOfPmtsInRp; ++i) mTime[i] = -1;
    mQuality = StMuRpsTrackPoint::StMuRpsTrackPointQuality::rpsNotSet;
}

StMuRpsTrackPoint::StMuRpsTrackPoint(const StMuRpsTrackPoint& trackPoint) {
    *this = trackPoint;
}

StMuRpsTrackPoint::~StMuRpsTrackPoint() { /* no op */ }

StMuRpsTrackPoint& StMuRpsTrackPoint::operator=(const StMuRpsTrackPoint& trackPoint) {
    if (this != &trackPoint) {
        mPosition = trackPoint.positionVec();
        mRpId = trackPoint.rpId();
        for (unsigned int i=0; i<StRpsTrackPoint::mNumberOfPlanesInRp; ++i ) mClusterId[i] = trackPoint.clusterId(i);
        for (unsigned int i=0; i<StRpsTrackPoint::mNumberOfPmtsInRp; ++i ) mTime[i] = trackPoint.time(i);
        mQuality = (StMuRpsTrackPoint::StMuRpsTrackPointQuality)trackPoint.quality();
    }
    return *this;
}

unsigned int StMuRpsTrackPoint::planesUsed() const {
    unsigned int nPlanesUsed = 0;
    for(unsigned int i=0; i<StRpsTrackPoint::mNumberOfPlanesInRp; ++i)
        if (mClusterId[i]>-1) ++nPlanesUsed;
    return nPlanesUsed;
}