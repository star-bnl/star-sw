/**********************************************************
 * $Id: StRichTrackFilter.h,v 1.1 2000/04/03 19:36:09 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.h,v $
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#ifndef STRICHTRACKFILTER_H
#define STRICHTRACKFILTER_H


#include "StTrack.h"
#include "StRichTrackFilter.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRichMaterialsDb.h"

#include "StPhysicalHelixD.hh"
#include "StPhysicalHelix.hh"

#include "StThreeVectorD.hh"
#include "StThreeVector.hh"
#include "SystemOfUnits.h"

// used in track coordinate transformations
#include "StRrsMaker/StRichMomentumTransform.h"
#include "StRrsMaker/StGlobalCoordinate.h"
#include "StRrsMaker/StRichRawCoordinate.h"
#include "StRrsMaker/StRichLocalCoordinate.h"
#include "StRrsMaker/StRichCoordinateTransform.h"


class StRichTrackFilter {
public:
  StRichTrackFilter();
  StRichTrackFilter(StTrack* tpcTrack, double magField);
  StRichTrackFilter(StPhysicalHelixD& tpcHelix, double magField);
  
  ~StRichTrackFilter();

  StThreeVector<double>& getLocalMomentumAtRadiator();
  StThreeVector<double>& getLocalImpactPointAtRadiator();  
  StThreeVector<double>& getLocalImpactPointAtPadPlane();

  bool onPadPlane();
  bool onRadiator();
  bool momentumIsAbove(double limit);
  bool incidentAngleCheck();

protected:
  void setLocalMomentumAtRadiator(StThreeVector<double>& mom);
  void setLocalImpactPointAtPadPlane(StThreeVector<double>& pt);
  void setLocalImpactPointAtRadiator(StThreeVector<double>& pt);

private:
  StThreeVector<double> mLocalImpactPointAtRadiator;
  StThreeVector<double> mLocalImpactPointAtPadPlane;
  StThreeVector<double> mLocalMomentumAtRadiator;
  StThreeVector<double> mLocalMomentumAtPadPlane;
  bool mOnPadPlane;
  bool mOnRadiator;
  bool mIncidentAngleCheck;

};

#endif











