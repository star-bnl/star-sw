/***************************************************************************
 *
 * $Id: StMuFcsInfo.cxx,v 1.0 2021/11/17 16:07:31 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 ***************************************************************************
 *
 * Description: StMuFcsInfo is event level data for Fcs. 
 *  Stores hit indices for each detector
 *
 ***************************************************************************/

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