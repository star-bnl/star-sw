/***************************************************************************
 *
 * StMuFttPoint.cxx
 *
 * Author: jdb 2021
 ***************************************************************************
 *
 * Description: Implementation of StMuFttPoint, the StEvent point structure
 *
 ***************************************************************************/
#include "StMuFttPoint.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StFttPoint.h"

ClassImp(StMuFttPoint)

StMuFttPoint::StMuFttPoint() :  TObject() { /* no op */ }

StMuFttPoint::~StMuFttPoint() { /* no op */ }

void StMuFttPoint::print(int opt) {
}

void StMuFttPoint::set( StFttPoint *point ){
    mPlane = point->plane();
    mQuadrant = point->quadrant();
    mX = point->x();
    mY = point->y();
    mXYZ = TVector3( point->xyz().x(), point->xyz().y(), point->xyz().z() );
} // set from StEvent