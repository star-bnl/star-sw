

#include "StMuFcsInfo.h"

ClassImp(StMuFcsInfo)

StMuFcsInfo::StMuFcsInfo(){
    mFcsReconstructionFlag = 0;
    for ( unsigned int i = 0; i < kFcsNDet + 2; i++ )
        mHitIndex[i] = 0;
    for ( unsigned int i = 0; i < kFcsNDet + 1; i++ ){
        mClusterIndex[i] = 0;
        mPointIndex[i]   = 0;
    }
}

StMuFcsInfo::~StMuFcsInfo(){

}