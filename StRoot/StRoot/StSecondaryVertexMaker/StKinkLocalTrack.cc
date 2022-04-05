/*!
 * \class  StKinkLocalTrack
 * \brief  auxiliary class for the kink finder
 * \author Camelia Mironov, KSU
 * \date   Jan,2004
 *
 *
 */

#include "StKinkLocalTrack.hh"
#include "SystemOfUnits.h"
#include "StTrackGeometry.h"
#include "StTrack.h"
#include <iostream>
using namespace std;
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


StKinkLocalTrack::StKinkLocalTrack()
{
  mEndRadius2D = 0.;
  mStartRadius2D = 0.;
}

StKinkLocalTrack::StKinkLocalTrack(StTrack* trk)
{
  StTrackGeometry* trkGeom = trk->geometry();
  mStartPoint = trkGeom->origin();//origin of the track
  mStartRadius2D = mStartPoint.perp();//r at start

  StTrackGeometry* trkGeomo = trk->outerGeometry();
  mLastPoint = trkGeomo->origin();//last point of the track
  mEndRadius2D = mLastPoint.perp();//r at his last point

  mTrack = trk;
}

Int_t StKinkLocalTrack::Compare(const TObject *obj) const
{
  if( mStartRadius2D == ((StKinkLocalTrack*)obj)->mStartRadius2D ) return 0;
  if( mStartRadius2D <  ((StKinkLocalTrack*)obj)->mStartRadius2D ) return -1;
  return 1;
}

Bool_t StKinkLocalTrack::IsEqual(const TObject *obj) const
{
  if (this == obj) return 1;
  if (StKinkLocalTrack::Class() != obj->IsA()) return 0;
  return mStartRadius2D == ((StKinkLocalTrack*)obj)->mStartRadius2D;
}
