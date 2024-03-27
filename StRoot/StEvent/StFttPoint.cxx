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
    LOG_INFO << "Point with " << this->nClusters() <<" clusters " 
             << ",x = " << this->x() << ", y = " << this->y() 
             << ", d1 = " << this->d1() << ", d2 = " << this->d2()
             << " at Plane : " << (int)this->plane() 
             << " at Quad : " << (int)this->quadrant() 
             << endm;
}


int StFttPoint::nClusters() const {
    int n = 0;
    for ( size_t i = 0; i < 4; i++ ){
        if ( mClusters[i] != nullptr )
        n++;
    }
    return n;
}