#include "StPicoFcsCluster.h"

StPicoFcsCluster::StPicoFcsCluster() : TObject() {
    /* No Op*/
}

StPicoFcsCluster::StPicoFcsCluster(const StPicoFcsCluster &clu){
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
}

StPicoFcsCluster::~StPicoFcsCluster(){

}

//_________________
void StPicoFcsCluster::Print(const Char_t* option __attribute__((unused))) const {
  
}
