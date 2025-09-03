#include "StPicoFcsHit.h"

StPicoFcsHit::StPicoFcsHit() : TObject(), mDetectorId(0), mId(0), 
                               mFourMomentumX(0.0), mFourMomentumY(0.0), 
                               mFourMomentumZ(0.0), mFourMomentumT(0.0) {}

StPicoFcsHit::StPicoFcsHit(const StPicoFcsHit &hit){
    mDetectorId=hit.mDetectorId;
    mId=hit.mId;
    mFourMomentumX=hit.mFourMomentumX;
    mFourMomentumY=hit.mFourMomentumY;
    mFourMomentumZ=hit.mFourMomentumZ;
    mFourMomentumT=hit.mFourMomentumT;
}

StPicoFcsHit::~StPicoFcsHit(){
}

void StPicoFcsHit::Print(const Char_t* option __attribute__((unused))) const {
    printf("StPicoFcsHit: DetectorId=%d, Id=%d, FourMomentum=(%.3f, %.3f, %.3f, %.3f)\n",
           mDetectorId, mId, mFourMomentumX, mFourMomentumY, mFourMomentumZ, mFourMomentumT);
}
