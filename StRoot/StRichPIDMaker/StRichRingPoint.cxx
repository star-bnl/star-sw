/**********************************************************
 * $Id: StRichRingPoint.cxx,v 2.9 2003/11/19 18:12:33 perev Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingPoint.cxx,v $
 *  Revision 2.9  2003/11/19 18:12:33  perev
 *  Fix Float Point Exception
 *
 *  Revision 2.8  2003/09/02 17:58:54  perev
 *  gcc 3.2 updates + WarnOff
 *
 *  Revision 2.7  2001/02/07 16:01:06  lasiuk
 *  no units with mass of particle in c'tor
 *  momentum loss (uses local copy) taken into account
 *  inline functions where possible
 *
 *  Revision 2.6  2000/12/08 04:57:09  lasiuk
 *  small fix to units quantities
 *
 *  Revision 2.5  2000/11/01 17:41:26  lasiuk
 *  use math.h for FLT_MAX definition
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.3  2000/10/01 00:57:18  gans
 *  Added #include <values.h> neeaded for MAXFLOAT
 *
 *  Revision 2.2  2000/09/29 17:55:51  horsley
 *  fixed bug in Minimization routine, included StMagF stuff (commented out)
 *  changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/
#include "StRichRingPoint.h"

#include <math.h>

#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StRichMaterialsDb.h"


StRichRingPoint::StRichRingPoint(StRichTrack* track, 
				 StRichRingDefinition type) {
  
  mParticle = 0;

  if (!track) {
    cerr << "WARNING: StRichRingPoint passed NULL pointer!" << endl;
    abort();
  }
  
  // get data bases
  richMaterialsDb = StRichMaterialsDb::getDb();
  richGeometryDb  = StRichGeometryDb::getDb();

  mInnerWavelength = richMaterialsDb->innerWavelength();
  mOuterWavelength = richMaterialsDb->outerWavelength(); 
  mMeanWavelength  = richMaterialsDb->meanWavelength(); 
  mMeanDepthRad    = richMaterialsDb->meanRadiatorDepth()*centimeter;
  
  mMeanPathInRadiator = 0.0;
  mMeanPathInQuartz   = 0.0;

  // detector parameters used in light propagation to pad plane depth
  mDepthRad  = richGeometryDb->radiatorDimension().z()/centimeter;  
  mDepthQuar = richGeometryDb->quartzDimension().z()/centimeter;
  
  // here we have changed the proximity depth to include the mwpc gap!!!
  mDepthProx = richGeometryDb->proximityGap()/centimeter;
  

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
  mMomentum    = track->getMomentum().mag()/GeV;
  
  mTrackTheta  = track->getTheta();
  mTrackPhi    = track->getPhi();
   
  mImpactPoint = track->getImpactPoint()/centimeter;
  mRingType    = type;
  mFastEnough  = false;
  
  // define "fast" trig functions
  mTrackCosTheta = cos(mTrackTheta); 
  mTrackSinTheta = sin(mTrackTheta); 
  mTrackCosPhi   = cos(mTrackPhi); 
  mTrackSinPhi   = sin(mTrackPhi); 
  mTrackTanTheta = tan(mTrackTheta);
  
  // use this StThreeVectorD as a return 
  // if ring is refracted away to infinity
  mRefractedAway.setX(FLT_MAX);
  mRefractedAway.setY(FLT_MAX);
  mRefractedAway.setZ(FLT_MAX);  
}

StRichRingPoint::~StRichRingPoint() { /* nopt */ }

void StRichRingPoint::setParticleType(StParticleDefinition* particle) {
  
    mParticle   = particle;
    mMass       = particle->mass();

    //
    // Adjust for the energy loss
    //
    
    mMomentum -= mTrack->getMomentumLoss(particle);
    
    mBeta       = mMomentum/::sqrt(mMomentum*mMomentum + mMass*mMass);
    mCher       = acos(1.0/(mBeta*mIndexRad[mRingType]));

    
    if (mCher>M_PI/2.0 || mCher<0) {
	cout << "StRichRingPoint::setParticleType()\n";
	cout << "\tERROR\n";
	cout << "\tProblem with the Chernekov angle!\n";
	cout << "\tmCher= " << mCher;
	cout << "\tabort!! "<< endl;
	abort();
    } 

    mFastEnough = mTrack->fastEnough(particle);
  
    mTanCher = tan(mCher);
}

bool StRichRingPoint::getPoint(double psi, StThreeVectorF& point) {

  // initailize point
  point = mRefractedAway;

  mMeanPathInRadiator = 0;
  mMeanPathInQuartz   = 0;  
  
  // define "fast" trig functions
  double cosPsi = cos(psi);
  double sinPsi = sin(psi);

  // light cone in radiator (unit height!)
  StThreeVectorF mLightRay(mTanCher*cosPsi*centimeter,
			   mTanCher*sinPsi*centimeter,
			   1.0*centimeter);
  
  // rotated cherenkov cone
  StThreeVectorF mRotatedLightRay(mTrackCosTheta*mTrackCosPhi*mLightRay.x() - 
				  mTrackSinPhi*mLightRay.y() + 
				  mTrackSinTheta*mTrackCosPhi*mLightRay.z(),
				  
				  mTrackCosTheta*mTrackSinPhi*mLightRay.x() + 
				  mTrackCosPhi*mLightRay.y() + 
				  mTrackSinTheta*mTrackSinPhi*mLightRay.z(),
				  
				 -mTrackSinTheta*mLightRay.x() + 
				  mTrackCosTheta*mLightRay.z());

  mPsiPrime = mRotatedLightRay.phi();
  
  double cosPsiPrime = cos(mPsiPrime);
  double sinPsiPrime = sin(mPsiPrime);

  //
  // angles cone makes with refractive boundaries
  //
  mRadiatorAngle = acos(mRotatedLightRay.z()/mRotatedLightRay.mag());
  if (mRadiatorAngle >= 0.5*M_PI) {return false;}

  double tempQVal = (mIndexRad[mRingType]/mIndexQuartz[mRingType])*sin(mRadiatorAngle);
  if (tempQVal >= 1.0) {return false;}
  mQuartzAngle = asin(tempQVal);

  double tempMVal = (mIndexQuartz[mRingType]/mIndexMeth[mRingType])*sin(mQuartzAngle);
  if (tempMVal >= 1.0)  {return false;}
  mMethaneAngle = asin(tempMVal);

  double mTanRAngle = tan(mRadiatorAngle);
  double mTanQAngle = tan(mQuartzAngle);
  double mTanMAngle = tan(mMethaneAngle);

  //
  // propagation of inner, outer, and mean
  // rings to the pad plane
  //
  
  StThreeVectorF mPropagatedLightRay;

  if(mRingType==eInnerRing) {
      mPropagatedLightRay.setX(mDepthRad*mTrackTanTheta*mTrackCosPhi);
      mPropagatedLightRay.setY(mDepthRad*mTrackTanTheta*mTrackSinPhi);
      mPropagatedLightRay.setZ(0.0);
  }
  
  if(mRingType==eOuterRing) {
      mPropagatedLightRay.setX(mDepthRad*mTanRAngle*cosPsiPrime);
      mPropagatedLightRay.setY(mDepthRad*mTanRAngle*sinPsiPrime);
      mPropagatedLightRay.setZ(0.0);
  } 

  if(mRingType==eMeanRing) {
    mPropagatedLightRay.setX((mDepthRad*mMeanDepthRad)*mTrackTanTheta*mTrackCosPhi 
			   + (mDepthRad-mDepthRad*mMeanDepthRad)*mTanRAngle*cosPsiPrime);

    mPropagatedLightRay.setY((mDepthRad*mMeanDepthRad)*mTrackTanTheta*mTrackSinPhi 
			   + (mDepthRad-mMeanDepthRad)*mTanRAngle*sinPsiPrime);

    mPropagatedLightRay.setZ(0.0);

    mMeanPathInRadiator = (mDepthRad-mDepthRad*mMeanDepthRad)*mTanRAngle;
    mMeanPathInQuartz   = mDepthQuar*mTanQAngle;

  } 

  double tempXVal = 
      mImpactPoint.x() + 
      mPropagatedLightRay.x() +       
      mDepthQuar*mTanQAngle*cosPsiPrime + 
      mDepthProx*mTanMAngle*cosPsiPrime;
  
  double tempYVal = 
      mImpactPoint.y() + 
      mPropagatedLightRay.y() + 
      mDepthQuar*mTanQAngle*sinPsiPrime + 
      mDepthProx*mTanMAngle*sinPsiPrime;
  
  if (isnan(tempXVal) || isnan(tempYVal)) {
      cout << "StRichRingPoint::getPoint()\n";
      cout << "\tERROR:\n";
      cout << "impactxy = " << mImpactPoint << endl;
      cout << "psi = " << psi/degree << endl;
      cout << "mtanCher = " << mTanCher << endl;
      cout << " cosPsi  sinPsi = " << cosPsi << "   " << sinPsi << endl;
      cout << "mLightRay = " << mLightRay << endl;
      cout << "mRotatedLightRay = " << mRotatedLightRay << endl;
      cout << "mTrackCosTheta     mTrackCosPhi        mTrackSinPhi        mTrackSinTheta   " 
	   << mTrackCosTheta  << "  " <<    mTrackCosPhi    << "  " <<     mTrackSinPhi    
	   << "  " <<     mTrackSinTheta << endl;

      cout << "mPsiPrime = " << mPsiPrime << endl;
      cout << "cosPsiPrime    sinPsiPrime  = " << cosPsiPrime << "   " <<    sinPsiPrime << endl;
      cout << "mRingType = " << static_cast<int>(mRingType) << endl;
    
      cout << " mDepthQuar*mTanQAngle*cosPsiPrime  = " <<  mDepthQuar << "   " 
	   << mTanQAngle << "   " << cosPsiPrime << endl;
    
      cout << " mPropagatedLightRay = " <<  mPropagatedLightRay << endl;
      cout << "mDepthProx*mTanMAngle*sinPsiPrime = " << mDepthProx 
	   << "   " << mTanMAngle << "   " << sinPsiPrime << endl;
      abort();
  }
  
  point.setX(tempXVal);
  point.setY(tempYVal);
  point.setZ(0.0);
  
  return true;
}

double StRichRingPoint::rotatedFunction(double psi) {

    if (!getPoint(psi,tempPoint)) return 1.e+10;

    tempPoint = tempPoint - mImpactPoint;

    StThreeVectorF rotatedPoint(mTrackCosPhi*tempPoint.x() + 
				mTrackSinPhi*tempPoint.y(),
			      
				-mTrackSinPhi*tempPoint.x() + 
				mTrackCosPhi*tempPoint.y(),

				0.0);
  
    return (rotatedPoint - minPoint).perp(); 
}
