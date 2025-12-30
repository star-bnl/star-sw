#include "StFwdTrack.h"
#include "StEvent/StFcsCluster.h"
#include "St_base/StMessMgr.h"

StFwdTrack::StFwdTrack() {}

StFwdTrack::~StFwdTrack() {}

/* momentum
 * get the track momentum at the first point (PV if included)
 */
StThreeVectorD StFwdTrack::momentum() const{
   return mPrimaryMomentum;
}

/* momentumAt
 * get the track momentum at the nthh point (if available)
 */
StThreeVectorD StFwdTrack::momentumAt(size_t _id) const{
    if ( _id >= mProjections.size() )
        return StThreeVectorD( 0, 0, 0 );

    return mProjections[_id].mMom;
}

char StFwdTrack::charge() const{
    return mCharge;
}

bool StFwdTrack::didFitConverge() const {
    return mDidFitConverge;
}

bool StFwdTrack::didFitConvergeFully() const {
    return mDidFitConvergeFully;
}

short StFwdTrack::numberOfFailedPoints() const {
    return mNumberOfFailedPoints;
}

double StFwdTrack::chi2() const {
    return mChi2;
}

double StFwdTrack::ndf() const {
    return mNDF;
}

double StFwdTrack::pval() const {
    return mPval;
}

short StFwdTrack::numberOfFitPoints() const {
    return mNumberOfFitPoints;
}

short StFwdTrack::numberOfSeedPoints() const {
    return mNumberOfSeedPoints;
}


StPtrVecFcsCluster& StFwdTrack::ecalClusters() { return mEcalClusters; }
const StPtrVecFcsCluster& StFwdTrack::ecalClusters() const { return mEcalClusters; }
void StFwdTrack::addEcalCluster(StFcsCluster* p){mEcalClusters.push_back(p);}
void StFwdTrack::sortEcalClusterByET() {
    std::sort(mEcalClusters.begin(), mEcalClusters.end(), [](StFcsCluster* a, StFcsCluster* b) {
            return b->fourMomentum().perp() < a->fourMomentum().perp();
        });
}

StPtrVecFcsCluster& StFwdTrack::hcalClusters() { return mHcalClusters; }
const StPtrVecFcsCluster& StFwdTrack::hcalClusters() const { return mHcalClusters; }
void StFwdTrack::addHcalCluster(StFcsCluster* p){mHcalClusters.push_back(p);}
void StFwdTrack::sortHcalClusterByET() {
    std::sort(mHcalClusters.begin(), mHcalClusters.end(), [](StFcsCluster* a, StFcsCluster* b) {
            return b->fourMomentum().perp() < a->fourMomentum().perp();
        });
}

StFwdTrackProjection StFwdTrack::getProjectionFor(  int detectorId, 
                                    size_t index ){
    StFwdTrackProjection result;
    size_t count = 0;
    for ( auto proj : mProjections ){
        if (proj.mDetId == detectorId){
            result.set( proj );
            if ( count == index )
                return result;
            count++;
        }
    }
    StFwdTrackProjection resultNull;
    return resultNull;
}