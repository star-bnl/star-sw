/**********************************************************
 * $Id: StRichRingPoint.cxx,v 1.2 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingPoint.cxx,v $
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
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
				 StRichRingDefinition type) {
  
  mParticle = 0;
  richMaterialsDb  = StRichMaterialsDb::getDb();

  mInnerWavelength = richMaterialsDb->longestWaveLength(); 
  mOuterWavelength = richMaterialsDb->shortestWaveLength(); 
  mMeanWavelength  = richMaterialsDb->meanWaveLength(); 
  richMaterialsDb = StRichMaterialsDb::getDb();

  if (mInnerWavelength == 0 || mOuterWavelength == 0) {
    cout << 
      "StRichRingPoint:: problems accessing StRichMaterialsDb, aborting." << endl; 
    abort();
  }

  // detector parameters used in light propagation to pad plane
  // depth
  StRichGeometryDb* richGeometryDb  = StRichGeometryDb::getDb();
  mDepthRad      = richGeometryDb->radiatorDimension().z()*centimeter;
  richGeometryDb  = StRichGeometryDb::getDb();
  mOuterWavelength = richMaterialsDb->outerWavelength(); 
  

  // detector parameters used in light propagation to pad plane depth
  mDepthQuar = richGeometryDb->quartzDimension().z()*centimeter;

  
  // here we have changed the proximity depth to include the mwpc gap!!!
  mDepthProx = richGeometryDb->proximityGap()*centimeter;
  

  // index of refraction's
  // radiator
  mIndexRad[eInnerRing]
    = richMaterialsDb->indexOfRefractionOfC6F14At(mInnerWavelength);

  mIndexRad[eOuterRing] 
    = richMaterialsDb->indexOfRefractionOfC6F14At(mOuterWavelength);

  mIndexRad[eMeanRing] 
    = richMaterialsDb->indexOfRefractionOfC6F14At(mMeanWavelength);

  // quartz
  mIndexQuartz[eInnerRing] 
    = richMaterialsDb->indexOfRefractionOfQuartzAt(mInnerWavelength);


  mIndexQuartz[eOuterRing] 
    = richMaterialsDb->indexOfRefractionOfQuartzAt(mOuterWavelength);

  mIndexQuartz[eMeanRing] 
    = richMaterialsDb->indexOfRefractionOfQuartzAt(mMeanWavelength);
  
  // methane
  mIndexMeth[eInnerRing] 
    = richMaterialsDb->indexOfRefractionOfMethaneAt(mInnerWavelength);
  


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

void StRichRingPoint::setParticleType(StParticleDefinition* particle) {

  if (mCher>M_PI/2.0) {
  mParticle   = particle;
  mMass       = particle->mass()*GeV;
  mBeta       = mMomentum/sqrt(mMomentum*mMomentum + mMass*mMass);
  mCher       = acos(1.0/(mBeta*mIndexRad[mRingType]));
  if (mCher>M_PI/2.0 || mCher<0) {
  mFastEnough = mTrack->fastEnough(particle);

    cout << "StRichRingPoint::setParticleType(): problem! abort!! "<< endl;
    abort();
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
  StThreeVector<double> mLightRay(mTanCher*cosPsi*centimeter,
				    mTanCher*sinPsi*centimeter,
				    1.0*centimeter);
  double sinPsi = sin(psi);

  StThreeVector<double> mRotatedLightRay(mTrackCosTheta*mTrackCosPhi*mLightRay.x() - 
					   mTrackSinPhi*mLightRay.y() + 
					   mTrackSinTheta*mTrackCosPhi*mLightRay.z(),
					 
					   mTrackCosTheta*mTrackSinPhi*mLightRay.x() + 
					   mTrackCosPhi*mLightRay.y() + 
					   mTrackSinTheta*mTrackSinPhi*mLightRay.z(),
					 
					  -mTrackSinTheta*mLightRay.x() + 
					   mTrackCosTheta*mLightRay.z());
				  mTrackCosTheta*mTrackSinPhi*mLightRay.x() + 
				  mTrackCosPhi*mLightRay.y() + 
				  mTrackSinTheta*mTrackSinPhi*mLightRay.z(),
				  
				 -mTrackSinTheta*mLightRay.x() + 
				  mTrackCosTheta*mLightRay.z());

  mPsiPrime = mRotatedLightRay.phi();
  
  // define "fast" trig functions
  double cosPsiPrime = cos(mPsiPrime);
  double sinPsiPrime = sin(mPsiPrime);

  // angles cone makes with refractive boundaries
  mRadiatorAngle = acos(mRotatedLightRay.z()/mRotatedLightRay.mag());
  if (mRadiatorAngle >= 0.5*M_PI) {return false;}

  double tempQVal = (mIndexRad[mRingType]/mIndexQuartz[mRingType])*sin(mRadiatorAngle);
  if (tempQVal >= 1.0)           {return false;}
  mQuartzAngle = asin(tempQVal);

  double tempMVal = (mIndexQuartz[mRingType]/mIndexMeth[mRingType])*sin(mQuartzAngle);
  if (tempMVal >= 1.0)           {return false;}
  mMethaneAngle = asin(tempMVal);

  // define "fast" trig functions
  double mTanRAngle = tan(mRadiatorAngle);
  StThreeVector<double> mPropagatedLightRay;
  double mTanMAngle = tan(mMethaneAngle);

 

  // propagation to pad plane
  StThreeVectorF mPropagatedLightRay;

  // inner ring  
  if (mRingType==eInnerRing) {
    mPropagatedLightRay.setX(mDepthRad*mTrackTanTheta*mTrackCosPhi);
    mPropagatedLightRay.setY(mDepthRad*mTrackTanTheta*mTrackSinPhi);
    mPropagatedLightRay.setZ(0.0);
  }
  

  // outer ring  
  if (mRingType==eOuterRing) {
    mPropagatedLightRay.setX(mDepthRad*mTanRAngle*cosPsiPrime);
    mPropagatedLightRay.setY(mDepthRad*mTanRAngle*sinPsiPrime);
    mPropagatedLightRay.setZ(0.0);
  } 

  // mean ring  
    mPropagatedLightRay.setX((mDepthRad*mMeanDepthRad)*mTrackTanTheta*mTrackCosPhi 

    mPropagatedLightRay.setZ(0.0);

    mMeanPathInRadiator = (mDepthRad-mDepthRad*mMeanDepthRad)*mTanRAngle;
    mMeanPathInQuartz   = mDepthQuar*mTanQAngle;

  } 



  double tempXVal = 
    mImpactPoint.x() + 
    mPropagatedLightRay.x() +       
    mDepthQuar*mTanQAngle*cosPsiPrime + 
    mDepthProx*mTanMAngle*cosPsiPrime;
  
    
    cout << " mPropagatedLightRay = " <<  mPropagatedLightRay << endl;
    cout << "mDepthProx*mTanMAngle*sinPsiPrime = " << mDepthProx 
	 << "   " << mTanMAngle << "   " << sinPsiPrime << endl;
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





















