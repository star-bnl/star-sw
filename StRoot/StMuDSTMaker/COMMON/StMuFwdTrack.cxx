#include "StMuFwdTrack.h"
#include <vector>

#include "StEvent/StFwdTrack.h"

StMuFwdTrack::StMuFwdTrack() : mDidFitConverge(0),mDidFitConvergeFully(0),mNumberOfFailedPoints(0),mNumberOfSeedPoints(0),mNumberOfFitPoints(0),mChi2(0),mNDF(0),mPval(0),mCharge(0),mPrimaryMomentum(0,0,0),mIdTruth(0),mQATruth(0) {

}

void StMuFwdTrack::set( StFwdTrack * evTrack) {
    mDidFitConverge = evTrack->didFitConverge();
    mDidFitConvergeFully = evTrack->didFitConvergeFully();
    mNumberOfFailedPoints = evTrack->numberOfFailedPoints();
    mNumberOfSeedPoints = evTrack->numberOfSeedPoints();
    mNumberOfFitPoints = evTrack->numberOfFitPoints();
    mChi2 = evTrack->chi2();
    mNDF = evTrack->ndf();
    mPval = evTrack->pval();
    mCharge = evTrack->charge();
    mPrimaryMomentum = TVector3( evTrack->momentum().x(), evTrack->momentum().y(), evTrack->momentum().z() );

    mIdTruth = evTrack->idTruth();
    mQATruth = evTrack->qaTruth();
    
    //copy the projections
    for ( auto proj : evTrack->mProjections ){
        mProjections.push_back(
            StMuFwdTrackProjection( proj.mDetId, TVector3( proj.mXYZ.x(), proj.mXYZ.y(), proj.mXYZ.z() ), TVector3( proj.mMom.x(), proj.mMom.y(), proj.mMom.z() ), proj.mCov )
        );
    }

    //copy the FTT Seed Points
    for ( auto sp : evTrack->mFTTPoints ){
        mFTTPoints.push_back(
            StMuFwdTrackSeedPoint( TVector3( sp.mXYZ.x(), sp.mXYZ.y(), sp.mXYZ.z() ), sp.mSector, sp.mTrackId, sp.mCov )
        );
    }

    //copy the FST Seed Points
    for ( auto sp : evTrack->mFSTPoints ){
        mFSTPoints.push_back(
            StMuFwdTrackSeedPoint( TVector3( sp.mXYZ.x(), sp.mXYZ.y(), sp.mXYZ.z() ), sp.mSector, sp.mTrackId, sp.mCov )
        );
    }

    setDCA(evTrack->dca().x(), evTrack->dca().y(), evTrack->dca().z());
    mIdTruth = evTrack->idTruth();
    mQATruth = evTrack->qaTruth();
    mVtxIndex = evTrack->vertexIndex();

}


void StMuFwdTrack::addEcalCluster( StMuFcsCluster* clu){
    int n=mEcalClusters.GetSize();
    for(int i=0; i<n; i++) if(mEcalClusters[i]==clu) return; //already there, do nothing
        mEcalClusters.Add(clu);
}
void StMuFwdTrack::addHcalCluster( StMuFcsCluster* clu){
    int n=mHcalClusters.GetSize();
    for(int i=0; i<n; i++) if(mHcalClusters[i]==clu) return; //already there, do nothing
        mHcalClusters.Add(clu);
}

/* momentum
 * get the track momentum at the first point (PV if included)
 */
TVector3 StMuFwdTrack::momentum() const{
   return mPrimaryMomentum;
}

/* momentumAt
 * get the track momentum at the nthh point (if available)
 */
TVector3 StMuFwdTrack::momentumAt(size_t _id) const{
    if ( _id >= mProjections.size() )
        return TVector3( 0, 0, 0 );

    return mProjections[_id].mMom;
}

char StMuFwdTrack::charge() const{
    return mCharge;
}

bool StMuFwdTrack::didFitConverge() const {
    return mDidFitConverge;
}

bool StMuFwdTrack::didFitConvergeFully() const {
    return mDidFitConvergeFully;
}

short StMuFwdTrack::numberOfFailedPoints() const {
    return mNumberOfFailedPoints;
}

double StMuFwdTrack::chi2() const {
    return mChi2;
}

double StMuFwdTrack::ndf() const {
    return mNDF;
}

double StMuFwdTrack::pval() const {
    return mPval;
}

short StMuFwdTrack::numberOfFitPoints() const {
    return mNumberOfFitPoints;
}

short StMuFwdTrack::numberOfSeedPoints() const {
    return mNumberOfSeedPoints;
}


// StPtrVecFcsCluster& StMuFwdTrack::ecalClusters() { return mEcalClusters; }
// const StPtrVecFcsCluster& StMuFwdTrack::ecalClusters() const { return mEcalClusters; }
// void StMuFwdTrack::addEcalCluster(StFcsCluster* p){mEcalClusters.push_back(p);}
// void StMuFwdTrack::sortEcalClusterByET() {
//     std::sort(mEcalClusters.begin(), mEcalClusters.end(), [](StFcsCluster* a, StFcsCluster* b) {
//             return b->fourMomentum().perp() < a->fourMomentum().perp();
//         });
// }

// StPtrVecFcsCluster& StMuFwdTrack::hcalClusters() { return mHcalClusters; }
// const StPtrVecFcsCluster& StMuFwdTrack::hcalClusters() const { return mHcalClusters; }
// void StMuFwdTrack::addHcalCluster(StFcsCluster* p){mHcalClusters.push_back(p);}
// void StMuFwdTrack::sortHcalClusterByET() {
//     std::sort(mHcalClusters.begin(), mHcalClusters.end(), [](StFcsCluster* a, StFcsCluster* b) {
//             return b->fourMomentum().perp() < a->fourMomentum().perp();
//         });
// }

bool StMuFwdTrack::getProjectionFor(  int detectorId, 
                                    StMuFwdTrackProjection &rProj, 
                                    size_t index ){
    size_t count = 0;
    for ( auto proj : mProjections ){
        if (proj.mDetId == detectorId){
            rProj.set( proj );
            if ( count == index )
                return true;
            count++;
        }
    }
    return false;
}