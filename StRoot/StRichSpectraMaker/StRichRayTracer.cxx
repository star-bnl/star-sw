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
 * Revision 1.2  2001/08/21 17:58:34  lasiuk
 * for 2000 analysis
 *
 * Revision 1.1  2001/02/25 22:11:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichRayTracer.h"
#include "StGlobals.hh"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRichPIDMaker/StRichMaterialsDb.h"

StRichRayTracer::StRichRayTracer()
{
    cout << "StRichRayTracer::StRichRayTracer()\n";
    cout << "\tERROR\n";
    cout << "\tCANNOT call. Must specify a mean wavelength." << endl;
    abort();
    
}

// ----------------------------------------------------
StRichRayTracer::StRichRayTracer(double wavelength)
{
    this->init(wavelength);    
}

// ----------------------------------------------------
StRichRayTracer::StRichRayTracer(double wavelength,
				 StThreeVectorF& pLocal,
				 StThreeVectorF& radiationPoint,
				 StThreeVectorF& normalRadiationPoint)
{
    this->init(wavelength);
    this->setTrack(pLocal, radiationPoint, normalRadiationPoint);

//     PR(mNormalRadiationPoint);
}

// ----------------------------------------------------
StRichRayTracer::~StRichRayTracer() {/*nopt*/}

// ----------------------------------------------------
void StRichRayTracer::setTrack(StThreeVectorF& pLocal,
			       StThreeVectorF& radiationPoint,
			       StThreeVectorF& normalRadiationPoint) {

    mLocalTrackMomentum     = pLocal;
    mExpectedRadiationPoint = radiationPoint;
    mNormalRadiationPoint   = normalRadiationPoint;

    this->calculateTrackAngle();
}

// ----------------------------------------------------
void StRichRayTracer::init(double wavelength)
{
//     cout << "StRichRayTracer::init()" << endl;
    m2Pi   = 2.*M_PI;

    mMeanWavelength = wavelength;
//     PR(mMeanWavelength);

    //
    // Db initialization
    //

    StRichMaterialsDb* materialsDb = StRichMaterialsDb::getDb();

//    mIndexQuartz = 1.6;
//    mIndexFreon  = 1.28;
//    mIndexCH4    = 1.00044;

    mIndexFreon  = materialsDb->indexOfRefractionOfC6F14At(mMeanWavelength);
    mIndexQuartz = materialsDb->indexOfRefractionOfQuartzAt(mMeanWavelength);
    mIndexCH4    = materialsDb->indexOfRefractionOfMethaneAt(mMeanWavelength);

     PR(mIndexFreon);
     PR(mIndexQuartz);
     PR(mIndexCH4);

    StRichGeometryDb* geometryDb = StRichGeometryDb::getDb();
    
    mRadiatorThickness =  geometryDb->radiatorDimension().z();  //1.0*centimeter;
    mQuartzThickness = geometryDb->quartzDimension().z();       //5.*millimeter;
    mGapThickness    = geometryDb->proximityGap();              //8.0*centimeter;

//     PR(mRadiatorThickness/millimeter);
//     PR(mQuartzThickness/millimeter);
//     PR(mGapThickness/millimeter);

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
    
    mNormalBottomQuartz = StThreeVectorF(0., 0., mGapThickness);
    mNormalTopQuartz    = StThreeVectorF(0., 0., mGapThickness + mQuartzThickness);
    mNormalTopRadiator  = StThreeVectorF(0., 0., mNormalTopQuartz.z() + mRadiatorThickness);

    // MUST BE THE SAME AS THAT IN SPECTRA_MAKER
    //mNormalRadiationPoint = StThreeVectorF(0., 0., mNormalTopQuartz.z() + mRadiatorThickness/2.);

//     PR(mNormalBottomQuartz);
//     PR(mNormalTopQuartz);
//     PR(mNormalTopRadiator);
//     PR(mNormalRadiationPoint);
}

// ----------------------------------------------------
void StRichRayTracer::calculateTrackAngle()
{
//     cout << "StRichRayTracer::calculateTrackAngle()" << endl;
    //
    // Calculate the incident track angle (mAlpha):
    //
    // BUT...must determine the sign first:
    //
    // The sign of mAlpha is given by the sign of px
    // if (px>0) rotation is in the negative sense
    //

    //
    //    pt
    //   \  |
    //  p \a| pz  tan(a) = pt/pz
    //     \|         ^
    //                |
    //                 -- mAlpha
    //
    //
    
//     PR(mLocalTrackMomentum/GeV);
//     PR(mLocalTrackMomentum.theta()/degree);

    if( sign(mLocalTrackMomentum.x()) > 0 ) {
	//cout << " +alpha " << endl;
	mAlpha = M_PI + mLocalTrackMomentum.theta();
    }
    else  {
	//
	// this should also cover the case for normal tracks
	// i.e. pz = |p|
	//
	//cout << " -alpha " << endl;
	mAlpha = M_PI - mLocalTrackMomentum.theta();
    }

    if(mAlpha > M_PI)
	mAlpha -= m2Pi;
    
//     PR(mAlpha/degree);
}

// ----------------------------------------------------
bool StRichRayTracer::adjustPropagator()
{
    cout << "StRichRayTracer::adjustPropagator()" << endl;
    
    return true;
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

    mGapPropagator = mExpectedRadiationPoint - mLocalPhotonPosition;
//     PR(mGapPropagator);

    //
    // angle with the normal
    //

    //mTheta = mGapPropagator.theta();
//     PR(mGapPropagator.theta()/degree);
    
    mPhotonRadPointTransverseDistance = mGapPropagator.perp();

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
bool  StRichRayTracer::processPhoton(double* angle) {

//     cout << "StRichRayTracer::processPhoton()" << endl;
    
    if(!this->initialPropagator()) {
	cout << "initialPropagator is BAD***" << endl;
    }
    else {
// 	cout << "initialPropagator is okay" << endl;
    }
    
    //mGapPropagator.setTheta(70.*degree);
    //cout << "--> findCerenkovAngle" << endl;

    if(this->findCerenkovAngle()) {
	// cout << "\n**** Convergence..." << endl;
	*angle = mCerenkovAngle;
    }
    else {
	// cout << "\n**** No Convergence..." << endl;
	*angle = FLT_MAX;
    }
    return true;
    
}

// -------------------------------------------------------
bool StRichRayTracer::propagateToRadiator()
{
//      cout << "\nStRichRayTracer::propagateToRadiator()" << endl;

    //
    // find the linear distance (mLOne) which is covered
    // while the light ray propagates through the
    // proximity focussing gap.
    // Calculate the intersection at the bottom
    // of the quartz window
    //
    //

//     PR(mGapPropagator);

    StThreeVectorF propagator = mGapPropagator;

//     PR(propagator);
//     PR(propagator.theta()/degree);
//     PR(propagator.phi()/degree);
    
//     PR(mNormalBottomQuartz);


    //
    // propagate to the bottom quartz
    //
    
    double scale =
	(mLocalPhotonPosition.dot(mNormalBottomQuartz) - mNormalBottomQuartz.dot(mNormalBottomQuartz))/
	(propagator.dot(mNormalBottomQuartz));

//     PR(scale);

    StThreeVectorF zAtBottomQuartz =
	mLocalPhotonPosition - scale*propagator;

//     PR(zAtBottomQuartz);
//     PR(zAtBottomQuartz.perp());
//     PR((zAtBottomQuartz-mLocalPhotonPosition).perp());

    //
    // angle wrt quartz?
    //

    double cosGapAngle = (propagator.unit()).dot(mNormalBottomQuartz.unit());
    double gapAngle = acos( cosGapAngle );
//     PR(gapAngle/degree);

    double quartzAngle = asin(mIndexCH4/mIndexQuartz*sin(gapAngle));
//     PR(quartzAngle/degree);

//     PR(quartzAngle/degree);
//     PR(propagator);
    propagator.setTheta(quartzAngle);
//     PR(propagator);
//     PR(propagator.phi()/degree);

    //
    // at what point does it hit the top of the quartz
    //

    scale =
	(zAtBottomQuartz.dot(mNormalTopQuartz) - mNormalTopQuartz.dot(mNormalTopQuartz))/
	(propagator.dot(mNormalTopQuartz));
//     PR(scale);

    StThreeVectorF zAtTopQuartz =
	zAtBottomQuartz - scale*propagator;
//     PR(zAtTopQuartz);
//     PR(zAtTopQuartz.perp());
//     PR((zAtTopQuartz-mLocalPhotonPosition).perp());
	
    
    //
    // angle wrt freon
    //

    double cosQuartzAngle = (propagator.unit()).dot(mNormalTopQuartz.unit());
    quartzAngle = acos(cosQuartzAngle);

    double freonAngle = asin(mIndexQuartz/mIndexFreon*sin(quartzAngle));
//     PR(freonAngle/degree);

//     PR(freonAngle/degree);
//     PR(propagator);
    propagator.setTheta(freonAngle);
//     PR(propagator);
//     PR(propagator.phi()/degree);

    //
    // at what point does it hit the 1/2 way point of the radiator
    //

    scale =
	(zAtTopQuartz.dot(mNormalRadiationPoint) - mNormalRadiationPoint.dot(mNormalRadiationPoint))/
	(propagator.dot(mNormalRadiationPoint));
//     PR(scale);

    mTheCalculatedRadiationPoint = zAtTopQuartz - scale*propagator;
//     PR(mTheCalculatedRadiationPoint);
//     PR(mTheCalculatedRadiationPoint.perp());
//     PR((mTheCalculatedRadiationPoint-mLocalPhotonPosition).perp());

    mConvergenceValue = (mTheCalculatedRadiationPoint-mLocalPhotonPosition).perp();
//     PR(mConvergenceValue);

    mPhi = propagator.phi();
//     PR(mPhi);
    
    double cosCerenkovAngle =
	-1.*(propagator.unit()).dot(mLocalTrackMomentum.unit());
    mCerenkovAngle = acos(cosCerenkovAngle);

//     PR(mCerenkovAngle/degree);
    
    
    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::findCerenkovAngle()
{
//     cout << "StRichRayTracer::findCerenkovAngle()" << endl;
    bool validAngle = false;

    //
    // the first iteration to get a starting point
    //

    int safetyMargin = 100;
    int ctr = 0;
    while (!validAngle) {
	ctr++;
	//cout << "...new iteration" << endl;
	validAngle = this->propagateToRadiator();
	if(ctr>safetyMargin) break;
    };

    StThreeVectorF oldRadiationPoint = mTheCalculatedRadiationPoint;
//     PR(oldRadiationPoint);

    if(this->checkConvergence(oldRadiationPoint)) {
// 	cout << " StRichRayTracer::findCerenkovAngle()\n";
// 	cout << " \tConvergence in first iteration" << endl;
	return true;
    }
    
    //
    // adjust theta and continue with next iterations
    // if x^2 + y^2 bounds are NOT exceeded

    if(mConvergenceValue > mPhotonRadPointTransverseDistance) {
// 	cout << "mConvergenceValue > mPhotonRadPointTransverseDistance" << endl;
	//
	// went to far(overshot)
 	// decrease theta (saves an iteration)
 	//
	mDeltaTheta *= -1.;
    }
    
//     PR(mGapPropagator.theta()/degree);
    mGapPropagator.setTheta(mGapPropagator.theta() + mDeltaTheta);
//     PR(mGapPropagator);
//     PR(mGapPropagator.theta()/degree);

    int iteration=0;
    do {
	iteration++;
	validAngle = false;

	ctr=0;
	while (!validAngle) {
	    ctr++;
	    validAngle = this->propagateToRadiator();
	    if(ctr>safetyMargin) break;
	    
	};
	
 	StThreeVectorF newRadiationPoint = mTheCalculatedRadiationPoint;
//  	PR(newRadiationPoint);
	
	//
	// Did we converge?
	//

	if(this->checkConvergence(newRadiationPoint)) {
// 	    cout << " StRichRayTracer::findCerenkovAngle()\n";
// 	    cout << " \tConvergence in (" << iteration << ") iteration" << endl;
	    return true;
	}

// 	cout << "test for convergence: old=" << abs(oldRadiationPoint-mExpectedRadiationPoint)
// 	     << " newL3=" << abs(newRadiationPoint-mExpectedRadiationPoint) << endl;
// 	PR(abs(oldRadiationPoint-mExpectedRadiationPoint));
// 	PR(abs(newRadiationPoint-mExpectedRadiationPoint))
	if( abs(oldRadiationPoint-mExpectedRadiationPoint) >
	    abs(newRadiationPoint-mExpectedRadiationPoint) ) {
	    //
	    // good, leave deltaTheta alone
	    //
// 	    cout << "okay" << endl;
	}
	else {
	    //
	    // Wrong way
	    // change of sign
	    //
// 	    cout << "wrong way scale -.5" << endl;
	    mDeltaTheta *= -0.5;
	}

	oldRadiationPoint = newRadiationPoint;
	
// 	PR(mDeltaTheta/degree);
// 	PR(mGapPropagator.theta()/degree);
	mGapPropagator.setTheta(mGapPropagator.theta() + mDeltaTheta);

    } while (iteration < mMaximumNumberOfIterations);

    return false;
}

// ----------------------------------------------------
void StRichRayTracer::status() const
{
    cout << "StRichRayTracer::status()\n";
    cout << "===========================" << endl;
}

