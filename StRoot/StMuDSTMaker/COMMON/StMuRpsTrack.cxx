#include "StMuRpsTrackPoint.h"
#include "StMuRpsTrack.h"

#include "StEvent/StRpsTrackPoint.h"
#include "StEvent/StRpsTrack.h"

#include <cmath>

ClassImp(StMuRpsTrack)

StMuRpsTrack::StMuRpsTrack(){
    for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i)
        mTrackPoints[ i ] = nullptr;
    mBranch = -1;
    mType = rpsUndefined;
}

StMuRpsTrack::StMuRpsTrack(const StMuRpsTrack& track) {
    *this = track;
}

StMuRpsTrack::~StMuRpsTrack() { /* no op */ }

StMuRpsTrack& StMuRpsTrack::operator=(const StMuRpsTrack& track) {
    if (this != &track) {
        for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i){
            mTrackPoints[i] = const_cast<StMuRpsTrackPoint*>(track.trackPoint(i));
        }
        mP = track.pVec();
        mType = track.type();
    }
    return *this;
}

unsigned int StMuRpsTrack::planesUsed() const {
    unsigned int nPlanes = 0;
    for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i)
        nPlanes += mTrackPoints[i].GetObject() ? trackPoint(i)->planesUsed() : 0;
    return nPlanes;
}

double StMuRpsTrack::thetaRp(unsigned int coordinate) const {
    if(coordinate>rpsAngleTheta) return 0.0;
    if(mType==rpsLocal) return theta(coordinate);
    TVector3 deltaVector = trackPoint(1)->positionVec() - trackPoint(0)->positionVec();
    return atan((coordinate<rpsAngleTheta ? deltaVector[coordinate] : deltaVector.Perp())/abs(deltaVector.z()));
}

double StMuRpsTrack::phiRp() const{
    if(mType==rpsLocal) return phi();
    TVector3 deltaVector = trackPoint(1)->positionVec() - trackPoint(0)->positionVec();
    return deltaVector.Phi();
}

double StMuRpsTrack::time() const{
    double sumTime=0.0;
    unsigned int numberOfPmtsWithSignal=0;
    for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i){
        if(trackPoint(i))
            for(int j=0; j<trackPoint(i)->mNumberOfPmtsInRp; ++j){
                if(trackPoint(i)->time(j)>0){
                    sumTime += trackPoint(i)->time(j);
                    ++numberOfPmtsWithSignal;
                }
            }
    }
    return numberOfPmtsWithSignal>0 ? sumTime/numberOfPmtsWithSignal : -1;
}

double StMuRpsTrack::theta(unsigned int coordinate) const{
    return coordinate < mNumberOfAngleTypes ? atan((coordinate<rpsAngleTheta ? mP[coordinate] : mP.Perp())/abs(mP.z())) : 0.0;
}
