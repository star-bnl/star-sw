/**********************************************************
 * $Id: StRichRingPoint.cxx,v 1.1 2000/04/03 19:36:08 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingPoint.cxx,v $
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 *
 *  
 *
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
#include "StRrsMaker/StRichGeometryDb.h"
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 **********************************************************/

#include "StRichRingPoint.h"
#include "SystemOfUnits.h"
#include "StRichMaterialsDb.h"
#include <values.h> // Needed for MAXFLOAT

				 StRichRingDefinition type) {
using namespace units;

StRichRingPoint::StRichRingPoint(StRichTrack* track, 
    abort();}
  mParticle = 0;
  richMaterialsDb  = StRichMaterialsDb::getDb();
  mInnerWavelength = 219.999*nanometer; 
  mOuterWavelength = 169.0*nanometer;

  if (mInnerWavelength == 0 || mOuterWavelength == 0) {
    cout << 
      "StRichRingPoint:: problems accessing StRichMaterialsDb, aborting." << endl; 
    abort();
  }

  // detector parameters used in light propagation to pad plane

  // depth
  StRichGeometryDb* richGeometryDb  = StRichGeometryDb::getDb();
  mDepthRad  = richGeometryDb->radiatorDimension().z()*centimeter;
  

  // detector parameters used in light propagation to pad plane depth
  // index of refraction
  // here we have changed the proximity depth to include the mwpc gap!!!
  mDepthProx = richGeometryDb->proximityGap()*centimeter;
  

  // index of refraction's
  // radiator
    = richMaterialsDb->indexOfRefractionOfC6F14At(mOuterWavelength);

  mIndexRad[eMeanRing] 
    = richMaterialsDb->indexOfRefractionOfC6F14At(mMeanWavelength);

  // quartz
    = richMaterialsDb->indexOfRefractionOfQuartzAt(mOuterWavelength);

  mIndexQuartz[eMeanRing] 
    = richMaterialsDb->indexOfRefractionOfQuartzAt(mMeanWavelength);
  
  // methane
  mIndexMeth[eOuterRing] 
    = richMaterialsDb->indexOfRefractionOfMethaneAt(mOuterWavelength);

  mIndexMeth[eMeanRing] 
    = richMaterialsDb->indexOfRefractionOfMethaneAt(mMeanWavelength);

  // track parameters  
  mTrack       = track;
  mMomentum    = track->getMomentum().mag()*GeV;
  
  mTrackTheta  = track->getTheta();
  mTrackPhi    = track->getPhi();
   
  mImpactPoint = track->getImpactPoint()*centimeter;
  mRingType    = type;
  mFastEnough  = false;
  // define "fast" trig functions
  mTrackCosTheta = cos(mTrackTheta); 
  mTrackSinTheta = sin(mTrackTheta); 
  mRefractedAway.setX(HUGE_VAL);
  mRefractedAway.setY(HUGE_VAL);
  mRefractedAway.setZ(HUGE_VAL);  
  
  mRefractedAway.setZ(MAXFLOAT);  
}
  return mParticle;
}

  mFastEnough = mTrack->fastEnough(particle);

    cout << "StRichRingPoint::setParticleType(): problem! abort!! "<< endl;
  mTanCher = tan(mCher);
  } 

  mFastEnough = mTrack->fastEnough(particle);
  
  // define "fast" trig functions
  mTanCher = tan(mCher);  
}


void StRichRingPoint::setPoint(StThreeVector<double>& sPoint) {

StRichTrack* StRichRingPoint::getTrack() {
  return mTrack;
bool StRichRingPoint::getPoint(double psi, StThreeVector<double>& point) {
}
  // initailize point
  point = mRefractedAway;

  mMeanPathInRadiator = 0;
  mMeanPathInQuartz   = 0;  
  // z = -1 because z coordinate points towards TPC origin
  StThreeVector<double> mLightRay(mTanCher*cosPsi*centimeter,
				  mTanCher*sinPsi*centimeter,
				  -1.0*centimeter);


  StThreeVector<double> mRotatedLightRay(mTrackCosTheta*mTrackCosPhi*mLightRay.x() - 
					 mTrackSinPhi*mLightRay.y() + 
					 mTrackSinTheta*mTrackCosPhi*mLightRay.z(),
					 
					 mTrackCosTheta*mTrackSinPhi*mLightRay.x() + 
					 mTrackCosPhi*mLightRay.y() + 
					 mTrackSinTheta*mTrackSinPhi*mLightRay.z(),
					 
					 -mTrackSinTheta*mLightRay.x() + 
					 mTrackCosTheta*mLightRay.z());
  
				  mTrackCosPhi*mLightRay.y() + 
				  mTrackSinTheta*mTrackSinPhi*mLightRay.z(),
				  
				 -mTrackSinTheta*mLightRay.x() + 
				  mTrackCosTheta*mLightRay.z());

  mPsiPrime = mRotatedLightRay.phi();
  mRadiatorAngle = acos(-mRotatedLightRay.z()/mRotatedLightRay.mag());
  // define "fast" trig functions
  double cosPsiPrime = cos(mPsiPrime);
  double sinPsiPrime = sin(mPsiPrime);

  // angles cone makes with refractive boundaries
  mRadiatorAngle = acos(mRotatedLightRay.z()/mRotatedLightRay.mag());
  if (mRadiatorAngle >= 0.5*M_PI) {return false;}

  double tempQVal = (mIndexRad[mRingType]/mIndexQuartz[mRingType])*sin(mRadiatorAngle);
  if (tempQVal >= 1.0)           {return false;}

  if (tempMVal >= 1.0)           {return false;}
  mMethaneAngle = asin(tempMVal);
  // inner ring  
  if (mRingType==eInnerRing) {
  // propagation to pad plane
  // inner ring
  StThreeVector<double> mPropagatedLightRay(mDepthRad*tan(mTrackTheta)*mTrackCosPhi,
			      	              mDepthRad*tan(mTrackTheta)*mTrackSinPhi,
				              0.0); 
    mPropagatedLightRay.setX(mDepthRad*mTrackTanTheta*mTrackCosPhi);
    mPropagatedLightRay.setY(mDepthRad*mTrackTanTheta*mTrackSinPhi);
    double mTanRAngle = tan(mRadiatorAngle);  
    mPropagatedLightRay.setZ(0.0);
    mPropagatedLightRay.setY(mDepthRad*mTanRAngle*sinPsiPrime);} 
    mMeanPathInRadiator = (mDepthRad-mDepthRad*mMeanDepthRad)*mTanRAngle;
    mMeanPathInQuartz   = mDepthQuar*mTanQAngle;

  } 



  double tempXVal = 
    mImpactPoint.x() + 
    mPropagatedLightRay.x() +       
    mDepthQuar*mTanQAngle*cosPsiPrime + 
    mDepthProx*mTanMAngle*cosPsiPrime;
  
    
    point.setY(tempYVal);     
    abort();
  }
    point.setX(tempXVal);
    point.setY(tempYVal);
    point.setZ(0.0);
  
  StThreeVector<double> rotatedPoint(mTrackCosPhi*tempPoint.x() + 
				       mTrackSinPhi*tempPoint.y(),
				       
				      -mTrackSinPhi*tempPoint.x() + 
				       mTrackCosPhi*tempPoint.y(),
				     
				       0.0);
			      mTrackSinPhi*tempPoint.y(),
  return (rotatedPoint - minPoint).mag(); 
			     -mTrackSinPhi*tempPoint.x() + 
			      mTrackCosPhi*tempPoint.y(),
}

double StRichRingPoint::getMeanPathInQuartz() {
  return mMeanPathInQuartz;
}





















