/***************************************************************************
 *
 * $Id: StRpsTrack.cxx,v 2.3 2015/10/22 20:37:34 ullrich Exp $
 *
 * Author: Rafal Sikora, 1 Oct 2015
 *
 ***************************************************************************
 *
 * Description: StRpsTrack class representing reconstructed track in
 * the Roman Pot system, with all associated observables, such as
 * momentum (px, py, pz) etc..
 *
 ***************************************************************************
 *
 * $Log: StRpsTrack.cxx,v $
 * Revision 2.3  2015/10/22 20:37:34  ullrich
 * Moved t(double) to header file made inline. Modified t code.
 *
 * Revision 2.2  2015/10/07 17:30:11  ullrich
 * Changed const to enums and related changes.
 *
 * Revision 2.1  2015/10/02 19:48:14  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#include "StRpsTrackPoint.h"
#include "StRpsTrack.h"
#include <cmath>

ClassImp(StRpsTrack)

StRpsTrack::StRpsTrack(){
    for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i)
        mTrackPoints.push_back(nullptr);
    mBranch = -1;
    mType = rpsUndefined;
}

StRpsTrack::StRpsTrack(const StRpsTrack& track) {
    *this = track;
}

StRpsTrack::~StRpsTrack() { /* no op */ }

StRpsTrack& StRpsTrack::operator=(const StRpsTrack& track) {
    if (this != &track) {
        for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i)
            mTrackPoints[i] = track.trackPoint(i);
        mP = track.pVec();
        mType = track.type();
    }
    return *this;
}

unsigned int StRpsTrack::planesUsed() const {
    unsigned int nPlanes = 0;
    for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i)
        nPlanes += mTrackPoints[i] ? mTrackPoints[i]->planesUsed() : 0;
    return nPlanes;
}

double StRpsTrack::thetaRp(unsigned int coordinate) const {
    if(coordinate>rpsAngleTheta) return 0.0;
    if(mType==rpsLocal) return theta(coordinate);
    StThreeVectorF deltaVector = mTrackPoints[1]->positionVec() - mTrackPoints[0]->positionVec();
    return atan((coordinate<rpsAngleTheta ? deltaVector[coordinate] : deltaVector.perp())/abs(deltaVector.z()));
}

double StRpsTrack::phiRp() const{
    if(mType==rpsLocal) return phi();
    StThreeVectorF deltaVector = mTrackPoints[1]->positionVec() - mTrackPoints[0]->positionVec();
    return deltaVector.phi();
}

double StRpsTrack::time() const{
    double sumTime=0.0;
    unsigned int numberOfPmtsWithSignal=0;
    for(unsigned int i=0; i<mNumberOfStationsInBranch; ++i){
        if(mTrackPoints[i])
            for(int j=0; j<mTrackPoints[i]->mNumberOfPmtsInRp; ++j){
                if(mTrackPoints[i]->time(j)>0){
                    sumTime += mTrackPoints[i]->time(j);
                    ++numberOfPmtsWithSignal;
                }
            }
    }
    return numberOfPmtsWithSignal>0 ? sumTime/numberOfPmtsWithSignal : -1;
}

double StRpsTrack::theta(unsigned int coordinate) const
{
    return coordinate < mNumberOfAngleTypes ? atan((coordinate<rpsAngleTheta ? mP[coordinate] : mP.perp())/abs(mP.z())) : 0.0;
}
