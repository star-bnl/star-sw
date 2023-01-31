/***************************************************************************
 *
 * StFttPoint.cxx
 *
 * Author: jdb 2021
 ***************************************************************************
 *
 * Description: Implementation of StFttPoint, the StEvent point structure
 *
 ***************************************************************************/
#include "StFttPoint.h"
#include "St_base/StMessMgr.h"


StFttPoint::StFttPoint() :  StObject(), mClusters{0,0,0,0} { /* no op */ }

StFttPoint::~StFttPoint() { /* no op */ }

void StFttPoint::print(int opt) {
}


int StFttPoint::nClusters() const {
    int n = 0;
    for ( size_t i = 0; i < 4; i++ ){
        if ( mClusters[i] != nullptr )
        n++;
    }
    return n;
}