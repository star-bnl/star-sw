#include "StPicoFcsHit.h"

StPicoFcsHit::StPicoFcsHit() : TObject() {
}

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
}
