/*****************************************************************************
 * 
 * $Id: StMuFcsPoint.cxx,v 1.0 2021/11/17 22:09:58 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 *****************************************************************************
 *
 * Description: Implementation of StMuFcsPoint, the MuDST FCS "point" class
 *
 *****************************************************************************
 *
 *
 *****************************************************************************/
#include "StMuFcsPoint.h"
#include "StMuFcsCluster.h"

StMuFcsPoint::StMuFcsPoint() :  mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StMuFcsPoint::~StMuFcsPoint() { /* no op */ }

void StMuFcsPoint::print(int opt) {
    cout << Form("StMuFcsPoint: Det=%2d ClusterId=%2d local=%7.2f %7.2lf xyz=%7.2lf %7.2lf %7.2lf E=%7.2lf ET=%6.2lf",
                 detectorId(), parentClusterId(),
                 x(), y(), 
         xyz().X(), xyz().Y(), xyz().Z(), 
         energy(), fourMomentum().Pt())
     << endl;
}

unsigned int StMuFcsPoint::parentClusterId() const { 
    StMuFcsCluster * clu = static_cast<StMuFcsCluster*>( mCluster.GetObject() );
    if ( nullptr == clu ) 
        return 9999999; // TODO: a better default for none?
    return clu->id(); 
} //parent cluster Id


StMuFcsCluster* StMuFcsPoint::cluster() { 
    return static_cast<StMuFcsCluster*>( mCluster.GetObject() ); 
} //  Parent cluster of the photon.

void StMuFcsPoint::setCluster(StMuFcsCluster* cluster) 
{ 
    mCluster = cluster; 
}