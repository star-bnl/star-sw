/**********************************************************
 * $Id: StRichTrackFilter.cxx,v 1.2 2000/04/04 14:14:48 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrackFilter.cxx,v $
 *  Revision 1.2  2000/04/04 14:14:48  horsley
 *  modified StRichTrackFilter to use StRichGeometryDb.
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#include "StRichTrackFilter.h"
#include "StRichMaterialsDb.h"
#include "StRrsMaker/StRichGeometryDb.h"
#include "StEventTypes.h"
#include "StRrsMaker/StRichGeometryDb.h"
#include "SystemOfUnits.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichTrackFilter::StRichTrackFilter()  {}

StRichTrackFilter::StRichTrackFilter(StTrack* tpcTrack, double magField)  {
  
  // define transformations 
  StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();

  StRichCoordinateTransform* coordinateTransformation =
        StRichCoordinateTransform::getTransform(myGeometryDb);

  StRichMomentumTransform* momentumTransformation 
    = StRichMomentumTransform::getTransform(myGeometryDb);

  StThreeVectorD richNormal(myGeometryDb->normalVectorToPadPlane().x(),
			    myGeometryDb->normalVectorToPadPlane().y(),
			    myGeometryDb->normalVectorToPadPlane().z());


  StRichLocalCoordinate entryPointOfRichRadiator_Local(0.0,
						       0.0,
						       myGeometryDb->proximityGap() +
						       myGeometryDb->quartzDimension().z() +
						       myGeometryDb->radiatorDimension().z() );
  
  StRichLocalCoordinate entryPointOfRichPadPlane_Local(0.0,
						       0.0,
						       0.0);
						       
						       
  StGlobalCoordinate entryPointOfRichRadiator_Global;
  (*coordinateTransformation)(entryPointOfRichRadiator_Local,entryPointOfRichRadiator_Global);
  
  StGlobalCoordinate entryPointOfRichPadPlane_Global;
  (*coordinateTransformation)(entryPointOfRichPadPlane_Local,entryPointOfRichPadPlane_Global);
  

  StThreeVectorD entryPointOfRichRadiator(entryPointOfRichRadiator_Global.position().x(),
					  entryPointOfRichRadiator_Global.position().y(),
					  entryPointOfRichRadiator_Global.position().z());

  StThreeVectorD entryPointOfRichPadPlane(entryPointOfRichPadPlane_Global.position().x(),
					  entryPointOfRichPadPlane_Global.position().y(),
					  entryPointOfRichPadPlane_Global.position().z());
  
 
  // create helix from StTrack, 
  // do momentum transformation, extrapolation 
  StPhysicalHelixD  mHelix = tpcTrack->geometry()->helix();
  double mPathLengthAtRadiator  = mHelix.pathLength(entryPointOfRichRadiator,richNormal);
  double mPathLengthAtPadPlane  = mHelix.pathLength(entryPointOfRichPadPlane,richNormal);

  StThreeVectorD tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,magField*tesla);
  StThreeVector<double> globalMomentumAtRadiator(tpcMom.x(), tpcMom.y(), tpcMom.z());

  StThreeVector<double> localMomentumAtRadiator;
  momentumTransformation->localMomentum(globalMomentumAtRadiator,
					 localMomentumAtRadiator);
  
  // do the impact point transformations here

  // radiator
  StGlobalCoordinate globalImpactPointAtRadiator(mHelix.x(mPathLengthAtRadiator),
						 mHelix.y(mPathLengthAtRadiator),
						 mHelix.z(mPathLengthAtRadiator));

  StRichLocalCoordinate localImpactPointAtRadiator_temp;
  (*coordinateTransformation)(globalImpactPointAtRadiator,
			      localImpactPointAtRadiator_temp);
  StThreeVector<double> localImpactPointAtRadiator(localImpactPointAtRadiator_temp.position().x(),
						     localImpactPointAtRadiator_temp.position().y(),
						     localImpactPointAtRadiator_temp.position().z());

  // pad plane
 StGlobalCoordinate globalImpactPointAtPadPlane(mHelix.x(mPathLengthAtPadPlane),
						mHelix.y(mPathLengthAtPadPlane),
						mHelix.z(mPathLengthAtPadPlane));

  StRichLocalCoordinate localImpactPointAtPadPlane_temp;
  (*coordinateTransformation)(globalImpactPointAtPadPlane,
			      localImpactPointAtPadPlane_temp);
  StThreeVector<double> localImpactPointAtPadPlane(localImpactPointAtPadPlane_temp.position().x(),
						     localImpactPointAtPadPlane_temp.position().y(),
						     localImpactPointAtPadPlane_temp.position().z());

  // set data members
  setLocalMomentumAtRadiator(localMomentumAtRadiator);
  setLocalImpactPointAtRadiator(localImpactPointAtRadiator);
  setLocalImpactPointAtPadPlane(localImpactPointAtPadPlane);



  // determine if on pad plane, radiator
  mOnPadPlane = false;
  StRichGeometryDb* mRichGeometryDb = StRichGeometryDb::getDb();

  if ( (localImpactPointAtRadiator.x() > -mRichGeometryDb->radiatorDimension().x() && 
	localImpactPointAtRadiator.x() <  mRichGeometryDb->radiatorDimension().x() ) && 
       
       (localImpactPointAtRadiator.y() > -mRichGeometryDb->radiatorDimension().y() && 
	localImpactPointAtRadiator.y() <  mRichGeometryDb->radiatorDimension().y() ) ) { 
    mOnPadPlane = true;
  }


  mOnRadiator = false;
  if ( (localImpactPointAtPadPlane.x() > -mRichGeometryDb->radiatorDimension().x() && 
	localImpactPointAtPadPlane.x() <  mRichGeometryDb->radiatorDimension().x() ) && 
       
       (localImpactPointAtPadPlane.y() > -mRichGeometryDb->radiatorDimension().y() && 
	localImpactPointAtPadPlane.y() <  mRichGeometryDb->radiatorDimension().y() ) ) { 
    mOnRadiator = true;
  }


  // determine if incident Angle OK
  StThreeVector<double> normalVector(0,0,-1);
  mIncidentAngleCheck = false;
  double trackTheta = acos(normalVector.dot(localMomentumAtRadiator)/localMomentumAtRadiator.mag());
  if (trackTheta < M_PI/2.0) mIncidentAngleCheck=true;

}


bool StRichTrackFilter::incidentAngleCheck() {
  return mIncidentAngleCheck;
}

StRichTrackFilter::StRichTrackFilter(StPhysicalHelixD& tpcHelix, double magField)  {
 // define transformations 
  StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();

  StRichCoordinateTransform* coordinateTransformation =
        StRichCoordinateTransform::getTransform(myGeometryDb);

  StRichMomentumTransform* momentumTransformation 
    = StRichMomentumTransform::getTransform(myGeometryDb);

  StThreeVectorD richNormal(myGeometryDb->normalVectorToPadPlane().x(),
			    myGeometryDb->normalVectorToPadPlane().y(),
			    myGeometryDb->normalVectorToPadPlane().z());


  StRichLocalCoordinate entryPointOfRichRadiator_Local(0.0,
						       0.0,
						       myGeometryDb->proximityGap() +
						       myGeometryDb->quartzDimension().z() +
						       myGeometryDb->radiatorDimension().z() );
  
  StRichLocalCoordinate entryPointOfRichPadPlane_Local(0.0,
						       0.0,
						       0.0);
						       
						       
  StGlobalCoordinate entryPointOfRichRadiator_Global;
  (*coordinateTransformation)(entryPointOfRichRadiator_Local,entryPointOfRichRadiator_Global);
  
  StGlobalCoordinate entryPointOfRichPadPlane_Global;
  (*coordinateTransformation)(entryPointOfRichPadPlane_Local,entryPointOfRichPadPlane_Global);
  

  StThreeVectorD entryPointOfRichRadiator(entryPointOfRichRadiator_Global.position().x(),
					  entryPointOfRichRadiator_Global.position().y(),
					  entryPointOfRichRadiator_Global.position().z());

  StThreeVectorD entryPointOfRichPadPlane(entryPointOfRichPadPlane_Global.position().x(),
					  entryPointOfRichPadPlane_Global.position().y(),
					  entryPointOfRichPadPlane_Global.position().z());
  

  // do momentum transformation, extrapolation 
  double mPathLengthAtRadiator  = tpcHelix.pathLength(entryPointOfRichRadiator,richNormal);
  double mPathLengthAtPadPlane  = tpcHelix.pathLength(entryPointOfRichPadPlane,richNormal);

  StThreeVectorD tpcMom = tpcHelix.momentumAt(mPathLengthAtRadiator,magField*tesla);
  StThreeVector<double> globalMomentumAtRadiator(tpcMom.x(), tpcMom.y(), tpcMom.z());

  StThreeVector<double> localMomentumAtRadiator;
  momentumTransformation->localMomentum(globalMomentumAtRadiator,
					 localMomentumAtRadiator);
  
  // do the impact point transformations here

  // radiator
  StGlobalCoordinate globalImpactPointAtRadiator(tpcHelix.x(mPathLengthAtRadiator),
						 tpcHelix.y(mPathLengthAtRadiator),
						 tpcHelix.z(mPathLengthAtRadiator));

  StRichLocalCoordinate localImpactPointAtRadiator_temp;
  (*coordinateTransformation)(globalImpactPointAtRadiator,
			      localImpactPointAtRadiator_temp);
  StThreeVector<double> localImpactPointAtRadiator(localImpactPointAtRadiator_temp.position().x(),
						   localImpactPointAtRadiator_temp.position().y(),
						   localImpactPointAtRadiator_temp.position().z());

  // pad plane
 StGlobalCoordinate globalImpactPointAtPadPlane(tpcHelix.x(mPathLengthAtPadPlane),
						tpcHelix.y(mPathLengthAtPadPlane),
						tpcHelix.z(mPathLengthAtPadPlane));

  StRichLocalCoordinate localImpactPointAtPadPlane_temp;
  (*coordinateTransformation)(globalImpactPointAtPadPlane,
			      localImpactPointAtPadPlane_temp);
  StThreeVector<double> localImpactPointAtPadPlane(localImpactPointAtPadPlane_temp.position().x(),
						   localImpactPointAtPadPlane_temp.position().y(),
						   localImpactPointAtPadPlane_temp.position().z());

  // set data members
  setLocalMomentumAtRadiator(localMomentumAtRadiator);
  setLocalImpactPointAtRadiator(localImpactPointAtRadiator);
  setLocalImpactPointAtPadPlane(localImpactPointAtPadPlane);



  // determine if on pad plane, radiator
  mOnPadPlane = false;
  StRichGeometryDb* mRichGeometryDb = StRichGeometryDb::getDb();

  if ( (localImpactPointAtRadiator.x() > -mRichGeometryDb->radiatorDimension().x() && 
	localImpactPointAtRadiator.x() <  mRichGeometryDb->radiatorDimension().x() ) && 
       
       (localImpactPointAtRadiator.y() > -mRichGeometryDb->radiatorDimension().y() && 
	localImpactPointAtRadiator.y() <  mRichGeometryDb->radiatorDimension().y() ) ) { 
    mOnPadPlane = true;
  }


  mOnRadiator = false;
  if ( (localImpactPointAtPadPlane.x() > -mRichGeometryDb->radiatorDimension().x() && 
	localImpactPointAtPadPlane.x() <  mRichGeometryDb->radiatorDimension().x() ) && 
       
       (localImpactPointAtPadPlane.y() > -mRichGeometryDb->radiatorDimension().y() && 
	localImpactPointAtPadPlane.y() <  mRichGeometryDb->radiatorDimension().y() ) ) { 
    mOnRadiator = true;
  }


  // determine if incident Angle OK
  StThreeVector<double> normalVector(0,0,-1);
  mIncidentAngleCheck = false;
  double trackTheta = acos(normalVector.dot(localMomentumAtRadiator)/localMomentumAtRadiator.mag());
  if (trackTheta < M_PI/2.0) mIncidentAngleCheck=true;
  
}


StRichTrackFilter::~StRichTrackFilter() {}

void StRichTrackFilter::setLocalMomentumAtRadiator(StThreeVector<double>& mom) {
  mLocalMomentumAtRadiator = mom;
}

void StRichTrackFilter::setLocalImpactPointAtRadiator(StThreeVector<double>& pt) {
  mLocalImpactPointAtRadiator = pt;
}

void StRichTrackFilter::setLocalImpactPointAtPadPlane(StThreeVector<double>& pt) {
  mLocalImpactPointAtPadPlane = pt;
}

StThreeVector<double>& StRichTrackFilter::getLocalMomentumAtRadiator() {
  return mLocalMomentumAtRadiator;
}

StThreeVector<double>& StRichTrackFilter::getLocalImpactPointAtRadiator() {
  return mLocalImpactPointAtRadiator;
}

StThreeVector<double>& StRichTrackFilter::getLocalImpactPointAtPadPlane() {
  return mLocalImpactPointAtPadPlane;
}



bool StRichTrackFilter::momentumIsAbove(double limit) {
  
  if (mLocalMomentumAtRadiator.mag() > limit) return true;
  return false;
}

bool StRichTrackFilter::onPadPlane() {
  return mOnPadPlane;
}


bool StRichTrackFilter::onRadiator() {
  return mOnRadiator;
}








