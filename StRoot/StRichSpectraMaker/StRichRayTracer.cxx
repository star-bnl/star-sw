/***************************************************************************
 *
 * $id: StRichRayTracer.cxx,v 1.1 2001/02/25 22:11:20 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichRayTracer.cxx,v $
 * Revision 1.8  2002/05/21 22:07:13  lasiuk
 * revision of index of refraction
 * and ray tracing
 *
 * Revision 1.7  2002/02/19 04:26:50  lasiuk
 * addition of filling StEvent for inclusion in chain
 *
 * Revision 1.6  2002/02/09 19:03:47  lasiuk
 * float.h header included for 7.2
 *
 * Revision 1.5  2002/02/01 17:45:56  lasiuk
 * Mods for gcc(7.2)
 * outer helix usage
 * histo mods
 *
 * Revision 1.4  2002/01/12 00:10:23  lasiuk
 * debin addition; quartz cerenkov angle, tuple modification, shift
 * to 183 nm for ray tracing, no temperature effect yet
 *
 * Revision 1.3  2001/11/21 20:36:07  lasiuk
 * azimuth angle calculation, trace and retracing algorithms, rotation
 * matrices, clean up intersection calculation.  Addition of quick
 * rings for graphics, change definition of ntuples, and
 * removal of old PID method
 *
 * Revision 1.2  2001/08/21 17:58:34  lasiuk
 * for 2000 analysis
 *
 * Revision 1.1  2001/02/25 22:11:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichRayTracer.h"
#include <float.h>
#include "StGlobals.hh"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRichPIDMaker/StRichMaterialsDb.h"

StRichRayTracer::StRichRayTracer()
{
    cout << "StRichRayTracer::StRichRayTracer()\n";
    cout << "\tERROR\n";
    cout << "\tCANNOT call. Must specify a mean wavelength and freon index." << endl;
    abort();
    
}

// ----------------------------------------------------
StRichRayTracer::StRichRayTracer(double wavelength, double freonIndex)
{
    this->init(wavelength, freonIndex);
}

// ----------------------------------------------------
StRichRayTracer::StRichRayTracer(double wavelength,
				 StThreeVectorF& pLocal,
				 StThreeVectorF& radiationPoint,
				 StThreeVectorF& quartzRadiationPoint, double freonIndex)
{
    this->init(wavelength, freonIndex);
    this->setTrack(pLocal, radiationPoint, quartzRadiationPoint, freonIndex);
}

// ----------------------------------------------------
StRichRayTracer::~StRichRayTracer() {/*nopt*/}

// ----------------------------------------------------
void StRichRayTracer::setTrack(StThreeVectorF& pLocal,
			       StThreeVectorF& radiationPoint,
			       StThreeVectorF& quartzRadiationPoint,
			       double index) {

    mLocalTrackMomentum           = pLocal;
    mExpectedRadiationPoint       = radiationPoint;
    mExpectedQuartzRadiationPoint = quartzRadiationPoint;

    mIndexFreon = index;
    cout << "Resetting index " << mIndexFreon << endl;
//     PR(mRadiationPlanePoint);
//     PR(mQuartzRadiationPlanePoint);

    //
    // this defines the axis from which the
    // azimuthal distribution of the photon
    // is calculated
    //
    
    mAxis = pLocal;
    mAxis.setZ(0.);
//     PR(mAxis);
    
    this->calculateTrackAngle();
}

// ----------------------------------------------------
void StRichRayTracer::init(double wavelength, double freonIndex)
{
//     cout << "StRichRayTracer::init()" << endl;

    mMeanWavelength = wavelength;
//     PR(mMeanWavelength);

    //
    // Db initialization
    //

    StRichMaterialsDb* materialsDb = StRichMaterialsDb::getDb();

//    mIndexQuartz = 1.6;
//    mIndexFreon  = 1.28;
//    mIndexCH4    = 1.00044;

//     mIndexFreon  = materialsDb->indexOfRefractionOfC6F14At(mMeanWavelength);
    mIndexQuartz = materialsDb->indexOfRefractionOfQuartzAt(mMeanWavelength);
    mIndexCH4    = materialsDb->indexOfRefractionOfMethaneAt(mMeanWavelength);

    mIndexFreon = freonIndex;
     PR(mIndexFreon);
     PR(mIndexQuartz);
     PR(mIndexCH4);
     cerr << "Freon Index in Tracer " << mIndexFreon << endl;
     
     double mLower = 165.*nanometer;
     mnF1 = materialsDb->indexOfRefractionOfC6F14At(mLower);
     mnQ1 = materialsDb->indexOfRefractionOfQuartzAt(mLower);
     mnG1 = materialsDb->indexOfRefractionOfMethaneAt(mLower);
     double mUpper = 200.*nanometer;
     mnF2 = materialsDb->indexOfRefractionOfC6F14At(mUpper);
     mnQ2 = materialsDb->indexOfRefractionOfQuartzAt(mUpper);
     mnG2 = materialsDb->indexOfRefractionOfMethaneAt(mUpper);

//      PR(mnF1);
//      PR(mnQ1);
//      PR(mnG1);
//      PR(mnF2);
//      PR(mnQ2);
//      PR(mnG2);
     
    StRichGeometryDb* geometryDb = StRichGeometryDb::getDb();
    mYGap  = fabs(geometryDb->quadrantX0(1));
    mXGap  = fabs(geometryDb->quadrantY0(4));
    mXEdge = fabs(geometryDb->quadrantX0(2));
    mYEdge = fabs(geometryDb->quadrantY0(2));

//     PR(mYGap);
//     PR(mXGap);
//     PR(mXEdge);
//     PR(mYEdge);
    
    mRadiatorThickness =  geometryDb->radiatorDimension().z();  //1.0*centimeter;
    mQuartzThickness = geometryDb->quartzDimension().z();       //5.*millimeter;
    mGapThickness    = geometryDb->proximityGap();              //8.0*centimeter;

//      PR(mRadiatorThickness/millimeter);
//      PR(mQuartzThickness/millimeter);
//      PR(mGapThickness/millimeter);

    //mxyPrecision = 1.*millimeter;
    mxyPrecision = 50.*micrometer;
    mzPrecision  = 1.*millimeter;

    mInitialIncrement = 1.*degree;
    mDeltaTheta = mInitialIncrement;

    mMaximumNumberOfIterations = 50;

    this->calculateGeometry();
}

// ----------------------------------------------------
void StRichRayTracer::setxyPrecision(double p) {

    cout << "StRichRayTracer::setxyPrecision()\n";
    cout << "\tWARNING:\n";
    cout << "\tDo you know what you are doing?\n" << endl;
    cout << "\t" << (mxyPrecision/millimeter) << " mm changing to ";

    mxyPrecision = p;
    cout << (mxyPrecision/millimeter) << " mm" << endl;
}

// ----------------------------------------------------
void StRichRayTracer::calculateGeometry()
{
//     cout << "StRichRayTracer::calculateGeometry()" << endl;

    mDetectorNormal     = StThreeVectorF(0., 0., 1.);
    mBottomQuartzPoint  = StThreeVectorF(0., 0., mGapThickness);
    mTopQuartzPoint   = StThreeVectorF(0., 0., mGapThickness + mQuartzThickness);
    mTopRadiatorPoint  = StThreeVectorF(0., 0., mTopRadiatorPoint.z() + mRadiatorThickness);

    // MUST BE THE SAME AS THAT IN SPECTRA_MAKER
    //mRadiationPlanePoint = StThreeVectorF(0., 0., mNormalTopQuartz.z() + mRadiatorThickness/2.);
//     PR(mDetectorNormal
//     PR(mBottomQuartzPoint);
//     PR(mTopRadiatorPoint);
//     PR(mRadiationPlanePoint);
}

// ----------------------------------------------------
void StRichRayTracer::calculateTrackAngle()
{
    //
    // Calculate the incident track angle (mAlpha):
    //

    mAlpha = M_PI - mLocalTrackMomentum.theta();
}

// ----------------------------------------------------
void StRichRayTracer::setPhotonPosition(StThreeVectorF& pos)
{
//     cout << "StRichRayTracer::setPhotonPosition()" << endl;

    mLocalPhotonPosition = pos;

    //
    // reset the step for optimizing theta
    //
    mDeltaTheta = mInitialIncrement;
}

// ----------------------------------------------------
StThreeVectorF StRichRayTracer::intersectionWithPlane(StThreeVectorF& n, StThreeVectorF& r,
						      StThreeVectorF& b, StThreeVectorF& a)
{
    //
    // r = point in plane
    // n = plane normal
    //
    // b = point to start
    // a = propagator direction
    //
    double s = (r.dot(n)-b.dot(n))/(a.dot(n));
    return b+s*a;
}

// ----------------------------------------------------
StThreeVectorF StRichRayTracer::rotateToAxis(double angle, StThreeVectorF& vec)
{
    //
    // in x-y plane
    // axis defined by the track momentum in the xy plane
    // 
    
    StThreeVectorF newPt(vec.x()*cos(angle)-vec.y()*sin(angle),
			 vec.x()*sin(angle)+vec.y()*cos(angle),
			 vec.z());
    return newPt;
}


// ----------------------------------------------------
StThreeVectorF StRichRayTracer::rotateToTrackInclination(double angle, StThreeVectorF& vec)
{
    // in xz plane
    // defined by the track incident angle
    // (alpha)

    StThreeVectorF newPt(vec.x()*cos(angle)-vec.z()*sin(angle),
			 vec.y(),
			 vec.x()*sin(angle)+vec.z()*cos(angle));

    return newPt;
}

// ----------------------------------------------------
void StRichRayTracer::traceDown()
{
    cout << "********StRichRayTracer::traceDown()*******" << endl;
    StThreeVectorF unrotatedPhotonPropagator(sin(mCerenkovAngle)*cos(mPhi),
					     sin(mCerenkovAngle)*sin(mPhi),
					     -cos(mCerenkovAngle));
    //
    // rotate by track inclination angle (alpha)
    // in x-z plane
    //
    
    StThreeVectorF photonPropagatorxz =
	rotateToTrackInclination(mAlpha,unrotatedPhotonPropagator);

    StThreeVectorF photonPropagator =
	rotateToAxis(mAxis.phi(), photonPropagatorxz);

    StThreeVectorF topQuartz =
	intersectionWithPlane(mDetectorNormal, mTopQuartzPoint,
			      mExpectedRadiationPoint, photonPropagator);
	    
    //
    // recover phi
    //
//     cout << "  ---- Recover phi? ----- " << endl;
//     StThreeVectorF prop1 = (topQuartz-mExpectedRadiationPoint).unit();
//     PR(prop1);
//     StThreeVectorF unrotprop1 = rotateToAxis(-mAxis.phi(), prop1);
//     PR(unrotprop1);
    
//     StThreeVectorF prop = rotateToTrackInclination(-mAlpha,unrotprop1);
//     PR(prop);
//     PR(prop.phi()/degree);
//     PR(mPhi/degree);
//     cout << "  ^^^^^^ Recover phi? ^^^^^^ " << endl;
    //
    //
    //

    double openingAngle = acos( (photonPropagator.unit()).dot(mDetectorNormal.unit()) );	    
    double incidentAngle = M_PI - openingAngle;
    double sinr = mIndexFreon*sin(incidentAngle)/mIndexQuartz;
    if(sinr>1) {
	cout << "StRichRayTracer::traceDown()\n";
	cout << "ERROR --> freon/quartz refraction: sinr = " << sinr << ")" << endl;
	return;
    }
	    
    double refractedAngle = asin(sinr);
    double newPropagatorAngle = M_PI - refractedAngle;    
    photonPropagator.setTheta(newPropagatorAngle);

    StThreeVectorF bottomQuartz =
	intersectionWithPlane(mDetectorNormal, mBottomQuartzPoint,
			      topQuartz, photonPropagator);
	    
    openingAngle = acos( (photonPropagator.unit()).dot(mDetectorNormal.unit()) );
	    
    incidentAngle = M_PI - openingAngle;
    sinr = mIndexQuartz*sin(incidentAngle)/mIndexCH4;
    if(sinr>1) {
	cout << "StRichRayTracer::traceDown()\n";
	cout << "ERROR --> CH4/quartz refraction: sinr = " << sinr << ")" << endl;
	return;
    }
	    
    refractedAngle = asin(sinr);
    newPropagatorAngle = M_PI - refractedAngle;
    photonPropagator.setTheta(newPropagatorAngle);
	    
    StThreeVectorF bottomDetector =
	intersectionWithPlane(mDetectorNormal, mLocalPhotonPosition,
			      bottomQuartz, photonPropagator);

    //
    // these can be compared for convergence
    //
//     PR(bottomDetector);
//     PR(mLocalPhotonPosition);
}

// -------------------------------------------------------
void StRichRayTracer::traceUp()
{
    cout << "\nStRichRayTracer::traceUp()" << endl;

    PR(mGapPropagator);
    StThreeVectorF propagator = mGapPropagator;
    PR(propagator.theta()/degree);
    //
    // propagate to the bottom quartz
    //
    
    StThreeVectorF zAtBottomQuartz =
	this->intersectionWithPlane(mDetectorNormal, mBottomQuartzPoint,
				    mLocalPhotonPosition, propagator);
    
    PR(zAtBottomQuartz);

    //
    // angle wrt quartz?
    //

    double cosGapAngle = (propagator.unit()).dot(mDetectorNormal.unit());
    double gapAngle = acos( cosGapAngle );
    PR(gapAngle/degree);

    double quartzAngle = asin(mIndexCH4/mIndexQuartz*sin(gapAngle));
    PR(quartzAngle/degree);

    propagator.setTheta(quartzAngle);
    PR(propagator);
    PR(propagator.theta()/degree);

    //
    // at what point does it hit the top of the quartz
    //

    StThreeVectorF mTopOfQuartzPlate =
	this->intersectionWithPlane(mDetectorNormal, mTopQuartzPoint,
				    zAtBottomQuartz, propagator);
    
    PR(mTopOfQuartzPlate);    
    
    //
    // angle wrt freon
    //

    double cosQuartzAngle = (propagator.unit()).dot(mDetectorNormal.unit());
    quartzAngle = acos(cosQuartzAngle);

    double freonAngle = asin(mIndexQuartz/mIndexFreon*sin(quartzAngle));
    PR(freonAngle/degree);

    propagator.setTheta(freonAngle);
    PR(propagator);
    PR(propagator.theta()/degree);

    //
    // at what point does it hit the 1/2 way point of the radiator
    //

    StThreeVectorF theCalculatedRadiationPoint =
	this->intersectionWithPlane(mDetectorNormal, mRadiationPlanePoint,
				    mTopOfQuartzPlate, propagator);

    PR(theCalculatedRadiationPoint);
    PR(mExpectedRadiationPoint);

    PR(propagator.unit());
    double cosCerenkovAngle =
	-1.*(propagator.unit()).dot(mLocalTrackMomentum.unit());
    PR(acos( cosCerenkovAngle )/degree);        
}


// ----------------------------------------------------
bool StRichRayTracer::initialPropagator()
{
//     cout << "\nStRichRayTracer::initalPropagator()" << endl;

    //
    // Take an initial guess at the angle the photon
    // path makes with the pad plane
    // by taking a line (propagator) from the
    // photon position on the pad plane and extrapolating
    // it to the expected radiation point.
    //

//     PR(mExpectedRadiationPoint);
//     PR(mLocalPhotonPosition);

    mGapPropagator = (mExpectedRadiationPoint - mLocalPhotonPosition).unit();
    
    mPhotonRadPointTransverseDistance =
	(mExpectedRadiationPoint - mLocalPhotonPosition).perp();
    
//     PR(mPhotonRadPointTransverseDistance);

    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::checkConvergence(StThreeVectorF& point) const
{
    bool convergence = false;
    
    if( ( (point-mExpectedRadiationPoint).perp() < mxyPrecision) ) {
//  	cout << "StRichRayTracer::checkConvergence()\n";
//  	cout << "\tConvergence!!!" << endl;
//  	cout << "\tCerenkov Angle= ";
//  	cout << (mCerenkovAngle/degree) <<  " degrees" << endl;
		    
 	convergence = true;
     }
	    
    return convergence;
}

// ----------------------------------------------------
bool StRichRayTracer::findAzimuth()
{
//     cout << "StRichRayTracer::findAzimuth()" << endl;
    //
    // calculate the phi (azimuth) of the photon
    // wrt the axis defined by the local momentum
    //

    StThreeVectorF shiftedPhoton = mTopOfQuartzPlate - mTheCalculatedRadiationPoint;

    StThreeVectorF photonRotatedToXAxis =
	this->rotateToAxis(-mAxis.phi(),shiftedPhoton);

    StThreeVectorF photonTrackAngleRemoved =
	this->rotateToTrackInclination(-mAlpha,photonRotatedToXAxis);


    mPhi = photonTrackAngleRemoved.phi();

    return true;
}

// ----------------------------------------------------
bool  StRichRayTracer::processPhoton(double* angle) {

//     cout << "StRichRayTracer::processPhoton()" << endl;
    
    if(this->findCerenkovAngle()) {
	//cout << "\n**** Convergence..." << endl;
	*angle = mCerenkovAngle;
	this->findAzimuth();
    }
    else {
	// cout << "\n**** No Convergence..." << endl;
	*angle = FLT_MAX;
    }

//      PR(mCerenkovAngle/degree);
//     PR(mPhi/degree);
    
    //
    // tests for convergence and performance
    // can be put here
    //
//     PR(mExpectedRadiationPoint);
//     PR(mTheCalculatedRadiationPoint);
//     this->traceDown();
//     this->traceUp();

    //
    // Quartz Cerenkov Angle
    //
    this->findQuartzCerenkovAngle();

    return true;
}

// -------------------------------------------------------
bool StRichRayTracer::propagateToRadiator()
{
//     cout << "\nStRichRayTracer::propagateToRadiator()" << endl;

    //
    // find the linear distance (mLOne) which is covered
    // while the light ray propagates through the
    // proximity focussing gap.
    // Calculate the intersection at the bottom
    // of the quartz window
    //
    //

    StThreeVectorF propagator = mGapPropagator;

    //
    // propagate to the bottom quartz
    //
    
    StThreeVectorF zAtBottomQuartz =
	this->intersectionWithPlane(mDetectorNormal, mBottomQuartzPoint,
				    mLocalPhotonPosition, propagator);
    
    //
    // angle wrt quartz?
    //

    double cosGapAngle = (propagator.unit()).dot(mDetectorNormal.unit());
    double gapAngle = acos( cosGapAngle );

    double quartzAngle = asin(mIndexCH4/mIndexQuartz*sin(gapAngle));
    propagator.setTheta(quartzAngle);

    //
    // at what point does it hit the top of the quartz
    //

    mTopOfQuartzPlate =
	this->intersectionWithPlane(mDetectorNormal, mTopQuartzPoint,
				    zAtBottomQuartz, propagator);
    
    //
    // angle wrt freon
    //

    double cosQuartzAngle = (propagator.unit()).dot(mDetectorNormal.unit());
    quartzAngle = acos(cosQuartzAngle);

    double freonAngle = asin(mIndexQuartz/mIndexFreon*sin(quartzAngle));
    propagator.setTheta(freonAngle);

    //
    // at what point does the propagator impact
    // the 1/2 way point (radiation point) of the radiator
    //

    mTheCalculatedRadiationPoint =
	this->intersectionWithPlane(mDetectorNormal, mRadiationPlanePoint,
				    mTopOfQuartzPlate, propagator);

    mConvergenceValue =
	(mTheCalculatedRadiationPoint-mLocalPhotonPosition).perp();

    double cosCerenkovAngle =
	-1.*(propagator.unit()).dot(mLocalTrackMomentum.unit());
    mCerenkovAngle = acos(cosCerenkovAngle);

        
    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::findCerenkovAngle()
{
//     cout << "StRichRayTracer::findCerenkovAngle()" << endl;
    if(!this->initialPropagator()) {
	cout << "initialPropagator is BAD***" << endl;
	return false;
    }
    else {
//  	cout << "initialPropagator is okay" << endl;
    }
    
    //
    // the first iteration to get a starting point
    //
    if(!this->propagateToRadiator()) {
	cout << "StRichRayTracer::findCerenkovAngle()\n";
	cout << "FATAL::1st iteration of propagateToRadiator()" << endl;
	return false;
    };

    StThreeVectorF oldRadiationPoint = mTheCalculatedRadiationPoint;

    if(this->checkConvergence(oldRadiationPoint)) {
 	cout << "*StRichRayTracer::findCerenkovAngle()\n";
 	cout << "\tConvergence in first iteration" << endl;
	return true;
    }
    
    //
    // adjust theta and continue with next iterations
    // if x^2 + y^2 bounds are NOT exceeded
    //
    
    if(mConvergenceValue > mPhotonRadPointTransverseDistance) {
	//
	// went to far(overshot)
 	// decrease theta (saves an iteration)
 	//
	mDeltaTheta *= -1.;
    }
    
    mGapPropagator.setTheta(mGapPropagator.theta() + mDeltaTheta);

    int iteration=0;
    do {
	iteration++;

	if(!this->propagateToRadiator()) {
	    cout << "StRichRayTracer::findCerenekovAngle()\n";
	    cout << "FATAL ERROR 2 on iteration (" << iteration << ")" << endl;
	    return false;
	}
	
 	StThreeVectorF newRadiationPoint = mTheCalculatedRadiationPoint;
	
	//
	// Check for convergence and recalculate the
	// stepsize for the next iteration if necessary
	//

	if(this->checkConvergence(newRadiationPoint)) {
//  	    cout << "*StRichRayTracer::findCerenkovAngle()\n";
//  	    cout << " \tConvergence in (" << iteration << ") iteration" << endl;
// 	    cout << "final gap propagator " << mGapPropagator << endl;
	    return true;
	}

	if( abs(oldRadiationPoint-mExpectedRadiationPoint) >
	    abs(newRadiationPoint-mExpectedRadiationPoint) ) {
	    //
	    // good, leave deltaTheta alone
	    //
	}
	else {
	    //
	    // Wrong way:  Change of sign of the
	    // step size and decrease by a factor of 2
	    //
	    mDeltaTheta *= -0.5;
	}

	oldRadiationPoint = newRadiationPoint;
	
	mGapPropagator.setTheta(mGapPropagator.theta() + mDeltaTheta);

    } while (iteration < mMaximumNumberOfIterations);

    return false;
}


// ----------------------------------------------------
bool StRichRayTracer::checkQuartzConvergence(StThreeVectorF& point) const
{
    bool convergence = false;

    if( ( (point-mExpectedQuartzRadiationPoint).perp() < mxyPrecision) ) {
//   	cout << "StRichRayTracer::checkQuartzConvergence()\n";
//   	cout << "\tConvergence!!!" << endl;
//   	cout << "\tQuartzCerenkov Angle= ";
//   	cout << (mQuartzCerenkovAngle/degree) <<  " degrees" << endl;
		    
 	convergence = true;
     }
	    
    return convergence;
}


// -------------------------------------------------------
bool StRichRayTracer::propagateToQuartz()
{
    //cout << "StRichRayTracer::propagateToQuartz()" << endl;

    //
    // find the linear distance (mLOne) which is covered
    // while the light ray propagates through the
    // proximity focussing gap.
    // Calculate the intersection at the bottom
    // of the quartz window
    //

    StThreeVectorF propagator = mGapPropagatorForQuartz;
    
    //
    // propagate to the bottom quartz
    //

    StThreeVectorF zAtBottomQuartz =
	this->intersectionWithPlane(mDetectorNormal, mBottomQuartzPoint,
				    mLocalPhotonPosition, propagator);    
    //
    // angle wrt quartz?
    //
    double cosGapAngle = (propagator.unit()).dot(mDetectorNormal.unit());
    double gapAngle = acos( cosGapAngle );

    double sinAngle = (mIndexCH4/mIndexQuartz*sin(gapAngle));
    double quartzAngle;
    if(fabs(sinAngle) <=1) {
	quartzAngle = asin(mIndexCH4/mIndexQuartz*sin(gapAngle));
	propagator.setTheta(quartzAngle);
    }
    else {
	cout << "StRichRayTracer::propagateToQuartz()\n";
	cout << "\tInternally reflected" << endl;
	return false;
    }

    //
    // at what point does it hit the Mid-Point (plane) of the quartz
    //
    mTheCalculatedQuartzRadiationPoint =
	this->intersectionWithPlane(mDetectorNormal, mQuartzRadiationPlanePoint,
				    zAtBottomQuartz, propagator);

    mQuartzConvergenceValue =
	(mTheCalculatedQuartzRadiationPoint-mLocalPhotonPosition).perp();

    double cosQuartzCerenkovAngle =
	-1.*(propagator.unit()).dot(mLocalTrackMomentum.unit());
    mQuartzCerenkovAngle = acos(cosQuartzCerenkovAngle);

    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::findQuartzCerenkovAngle()
{    
//     cout << "StRichRayTracer::findQuartzCerenkovAngle()" << endl;
    
    mDeltaTheta = mInitialIncrement/2.;
    
    mGapPropagatorForQuartz =
	(mExpectedQuartzRadiationPoint - mLocalPhotonPosition).unit();

    //
    // the first iteration to get a starting point
    //

    if(!this->propagateToQuartz()) {
	cout << "StRichRayTracer::findQuartzCerenkovAngle()\n";
	cout << "FATAL::1st iteration of propagateToQuartz()" << endl;
	return false;
    };

    StThreeVectorF oldRadiationPoint = mTheCalculatedQuartzRadiationPoint;
    
    if(this->checkQuartzConvergence(oldRadiationPoint)) {
 	cout << "*StRichRayTracer::findQuartzCerenkovAngle()\n";
 	cout << "\tConvergence in first iteration" << endl;
	return true;
    }
    
    //
    // adjust theta and continue with next iterations
    // if x^2 + y^2 bounds are NOT exceeded
    //

    double comparisonValue = (mLocalPhotonPosition-mExpectedQuartzRadiationPoint).perp();
    if( mQuartzConvergenceValue >  comparisonValue) {
	//
	// went to far(overshot)
 	// decrease theta (saves an iteration)
 	//
	mDeltaTheta *= -1.;
    }

    mGapPropagatorForQuartz.setTheta(mGapPropagatorForQuartz.theta() + mDeltaTheta);
    
    int iteration=0;
    do {
	iteration++;

	if(!this->propagateToQuartz()) {
	    cout << "StRichRayTracer::findQuartzCerenekovAngle()\n";
	    cout << "FATAL ERROR 2 on iteration (" << iteration << ")" << endl;
	    return false;
	}
	
 	StThreeVectorF newRadiationPoint = mTheCalculatedQuartzRadiationPoint;

	//
	// Check for convergence and recalculate the
	// stepsize for the next iteration if necessary
	//

	if(this->checkQuartzConvergence(newRadiationPoint)) {
  	    //cout << "*StRichRayTracer::findQuartzCerenkovAngle()\n";
  	    //cout << " \tConvergence in (" << iteration << ") iteration" << endl;
 	    //cout << "final gap propagator " << mGapPropagator << endl;
	    return true;
	}

	if( abs(oldRadiationPoint-mExpectedQuartzRadiationPoint) >
	    abs(newRadiationPoint-mExpectedQuartzRadiationPoint) ) {
	    //
	    // good, leave deltaTheta alone
	    //
	}
	else {
	    //
	    // Wrong way:  Change of sign of the
	    // step size and decrease by a factor of 2
	    //
	    mDeltaTheta *= -0.5;
	}

	oldRadiationPoint = newRadiationPoint;
	
	mGapPropagatorForQuartz.setTheta(mGapPropagatorForQuartz.theta() + mDeltaTheta);
    } while (iteration < mMaximumNumberOfIterations);

    return false;
}

// ----------------------------------------------------
void StRichRayTracer::status() const
{
    cout << "StRichRayTracer::status()\n";
    cout << "-------------------------------------------" << endl;
    cout << "CerenkovAngle   Azimuth    Quartz Cerenkov" << endl;
    cout << "-------------------------------------------" << endl;
    cout << (this->cerenkovAngle()/degree) << '\t'
	 << (this->azimuth()/degree)     << '\t'
	 << (this->quartzCerenkovAngle()/degree) << endl;
    cout << "===========================================" << endl;
}

// ----------------------------------------------------
bool StRichRayTracer::checkPlane(StThreeVectorF& pt)
{
    if( (fabs(pt.x())>mXEdge)  ||
	(fabs(pt.x())<mXGap)   ||
	(fabs(pt.y())>mYEdge)  ||
	(fabs(pt.y())<mYGap) )
	return false;
    else
	return true;
}

// ----------------------------------------------------
vector<StThreeVectorF>
StRichRayTracer::calculatePoints(StThreeVectorF& radPt, double mass)
{
//     cout << "StRichRayTracer::calculatePoints()" << endl;
    vector<StThreeVectorF> pts;
    //
    // track must already be set
    // --> Generate points on the plane for a ring
    //
    double beta2 =
	sqr(abs(mLocalTrackMomentum))/(sqr(abs(mLocalTrackMomentum))+sqr(mass));
    double beta = sqrt(beta2);

    double nF;
    double nQ;
    double nG;

    if(radPt.z()>8.5) {
	//
	// use the lower bounds
	//
	nF = mnF1;
	nQ = mnQ1;
	nG = mnG1;
    }
    else if(radPt.z() == 9.0) {
	// intermediate value
	cout << "intermediate" << endl;
	nF = mIndexFreon;
	nQ = mIndexQuartz;
	nG = mIndexCH4;
    }
    else {
	nF = mnF2;
	nQ = mnQ2;
	nG = mnG2;
    }

    StThreeVectorF shift(radPt.x(), radPt.y(),0.);

    double cosCerenkovAngle = 1./(nF*beta);
    if(cosCerenkovAngle>1) return pts;

    double cerenkovAngle = acos(cosCerenkovAngle);

    StThreeVectorF axis(mLocalTrackMomentum.x(), mLocalTrackMomentum.y(), 0.);
    double alpha = M_PI - mLocalTrackMomentum.theta();
    
    //
    // Initialize the length calculations
    //
    StThreeVectorF firstPoint;
    StThreeVectorF lastPoint;
    StThreeVectorF firstPointOnPlane;
    StThreeVectorF lastPointOnPlane;
    double totalLength = 0.;
    double totalLengthOnPlane = 0.;
    int status = 1;
    
    double dphi = 1.*degree;
    for(double phi=0.*degree; phi<360.*degree; phi+=dphi) {
	
	StThreeVectorF unrotatedPhotonPropagator(sin(cerenkovAngle)*cos(phi),
						 sin(cerenkovAngle)*sin(phi),
						 -cos(cerenkovAngle));
	
	// rotate by track inclination angle (alpha)
	// in x-z plane
	
	StThreeVectorF photonPropagatorxz =
	    rotateToTrackInclination(alpha,unrotatedPhotonPropagator);

	StThreeVectorF photonPropagator =
	    rotateToAxis(axis.phi(), photonPropagatorxz);

	
	StThreeVectorF topQuartz =
	    intersectionWithPlane(mDetectorNormal, mTopQuartzPoint,
				  radPt, photonPropagator);
		
	//
	// incident angle wrt normal
	//
	double openingAngle =
	    acos( (photonPropagator.unit()).dot(mDetectorNormal.unit()) );
	double incidentAngle = M_PI - openingAngle;
	double sinr = nF*sin(incidentAngle)/nQ;
	if(sinr>1) continue;
	
	double refractedAngle = asin(sinr);
	double newPropagatorAngle = M_PI - refractedAngle;   
	photonPropagator.setTheta(newPropagatorAngle);
	
	StThreeVectorF bottomQuartz =
	    intersectionWithPlane(mDetectorNormal, mBottomQuartzPoint,
				  topQuartz, photonPropagator);
	
	openingAngle = acos( (photonPropagator.unit()).dot(mDetectorNormal.unit()) );
	    
	incidentAngle = M_PI - openingAngle;
	sinr = nQ*sin(incidentAngle)/nG;
	if(sinr>1) continue;
	
	refractedAngle = asin(sinr);
	newPropagatorAngle = M_PI - refractedAngle;
	photonPropagator.setTheta(newPropagatorAngle);
	
	StThreeVectorF bottomDetector =
	    intersectionWithPlane(mDetectorNormal, mLocalPhotonPosition,
				  bottomQuartz, photonPropagator);

	firstPoint = bottomDetector;
	if( abs(lastPoint)>0 ) {
	    totalLength += abs(lastPoint - firstPoint);
	}
	lastPoint = firstPoint;

	if(this->checkPlane(bottomDetector)) {
	    if(status==0) {
		firstPointOnPlane = bottomDetector;
		lastPointOnPlane  = bottomDetector;
		status = 1;
	    }
	    lastPointOnPlane = firstPointOnPlane;
	    firstPointOnPlane = bottomDetector;
	    status = 1;
	}
	else {
	    status = 0;
	    firstPointOnPlane = bottomDetector;
	    lastPointOnPlane = firstPointOnPlane;
	}
	if( abs(lastPointOnPlane)>0 ) {
	    totalLengthOnPlane += abs(lastPointOnPlane - firstPointOnPlane);
	    lastPointOnPlane = firstPointOnPlane;
	}
	
	if( (fabs(bottomDetector.x())>70) || (fabs(bottomDetector.y())>50)) continue;
	pts.push_back(bottomDetector);
    }

//     PR(totalLength);
//     PR(totalLengthOnPlane);
    mLineIntegralOnPlane = totalLengthOnPlane;
    mLineIntegralRatio = totalLengthOnPlane/totalLength;
    
    return pts;
}
