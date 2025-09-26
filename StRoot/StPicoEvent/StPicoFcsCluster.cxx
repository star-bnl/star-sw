#include "StPicoFcsCluster.h"

StPicoFcsCluster::StPicoFcsCluster() : TObject(), mIndex(0), mId(0), 
                                       mDetectorId(0), mCategory(0), 
                                       mNTowers(0), mX(0.0), mY(0.0), 
                                       mSigmaMin(0.0), mSigmaMax(0.0), 
                                       mTheta(0.0), mChi2Ndf1Photon(0.0), 
                                       mChi2Ndf2Photon(0.0), 
                                       mFourMomentumX(0.0), 
                                       mFourMomentumY(0.0), 
                                       mFourMomentumZ(0.0), 
                                       mFourMomentumT(0.0) {}

StPicoFcsCluster::StPicoFcsCluster(const StPicoFcsCluster &clu){
    mIndex = clu.mIndex; // Copy the index
    mId=clu.mId;
    mDetectorId=clu.mDetectorId;
    mCategory=clu.mCategory;
    mNTowers=clu.mNTowers;
    mX=clu.mX;
    mY=clu.mY;
    mSigmaMin=clu.mSigmaMin;
    mSigmaMax=clu.mSigmaMax;
    mTheta=clu.mTheta;
    mChi2Ndf1Photon=clu.mChi2Ndf1Photon;
    mChi2Ndf2Photon=clu.mChi2Ndf2Photon;
    mFourMomentumX=clu.mFourMomentumX;
    mFourMomentumY=clu.mFourMomentumY;
    mFourMomentumZ=clu.mFourMomentumZ;
    mFourMomentumT=clu.mFourMomentumT;
    mFwdTrackMatchIndex = clu.mFwdTrackMatchIndex; // Copy the vector
}

StPicoFcsCluster::~StPicoFcsCluster(){

}

//_________________
void StPicoFcsCluster::Print(const Char_t* option __attribute__((unused))) const {
  
}
