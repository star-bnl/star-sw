/***************************************************************************
 *
 * $Id: StRichRayTracer.cxx,v 1.1 2001/02/25 22:11:20 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichRayTracer.cxx,v $
 * Revision 1.1  2001/02/25 22:11:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichRayTracer.h"
#include "StGlobals.hh"

StRichRayTracer::StRichRayTracer()
{
    cout << "Should never be called" << endl;
}

// ----------------------------------------------------
StRichRayTracer::StRichRayTracer(StThreeVectorF& pLocal,
				 StPairD& mipLocal,
				 StPairD& radIntercept)
    : mLocalTrackMomentum(pLocal),
      mLocalMipPosition(mipLocal),
      mLocalRadiatorIntercept(radIntercept)
{
    this->init();
    this->shiftOriginToMipIntersectionOnPadPlane();
}

// ----------------------------------------------------
StRichRayTracer::~StRichRayTracer() {/*nopt*/}

// ----------------------------------------------------
void StRichRayTracer::init()
{
    m2Pi   = 2.*M_PI;
    mPiBy2 = M_PI/2.;
    
    //
    // Db initialization
    //
    indexQuartz = 1.6;
    indexFreon  = 1.26;
    indexCH4    = 1.000;

    gapThickness    = 8.0*centimeter;
    quartzThickness = 5.*millimeter;
    radiatorThickness = 1.0*centimeter;

    mPrecision = 1.*millimeter;
    //mPrecision = 50.*micrometer;
    mDeltaTheta = 1.*degree;

    mMaximumNumberOfIterations = 50;
    
    this->calculateTrackAngle();
    this->calculateRotatedGeometry();

}

// ----------------------------------------------------
void StRichRayTracer::calculateTrackAngle()
{
    cout << "\nStRichRayTracer::calculateTrackAngle()" << endl;
    //
    // Calculate the incident track angle (mAlpha):
    //
    //    pt
    //   \  |
    //  p \a| pz  tan(a) = pt/pz
    //     \|         ^
    //                |
    //                 -- mAlpha
    //
    //  This is accompanied by finding the axis about which
    //  the photons must be rotated about in the x-y plane
    //  before being shifted out of plane by the angle mAlpha
    //
    
    //mAlpha = atan(mLocalTrackMomentum.perp()/mLocalTrackMomentum.z());
    PR(mLocalTrackMomentum);
    PR(mLocalTrackMomentum.theta()/degree);

    //
    // the track angle,
    // mAlpha = acos(pz/p)
    //       OR
    //          asin(abs(axis))
    //
    // BUT...must determine the sign first:
    //
    // The sign of mAlpha is given by the sign of px
    // if (px>0) rotation is in the negative sense
    //

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
	
    mSinAlpha = sin(mAlpha);
    mCosAlpha = cos(mAlpha);
    mTanAlpha = tan(mAlpha);

    PR(mAlpha/degree);
    PR(mSinAlpha);
    PR(mCosAlpha);
    PR(mTanAlpha);

    //
    // Exception handling:
    // if (px == 0), angle between py/px is 0
    //
    
    double angleBetweenPxPy = 0;
    if( mLocalTrackMomentum.x() ) {
	angleBetweenPxPy = mLocalTrackMomentum.phi();
    }
    
    PR(angleBetweenPxPy/degree);

    if(abs(angleBetweenPxPy) > mPiBy2) {
	//
	// Adjustment is necessary
	//
	if(angleBetweenPxPy < 0) {
	    cout << " - " << endl;
	    angleBetweenPxPy = angleBetweenPxPy - M_PI;
	}
	else {
	    cout << " + " << endl;
	    angleBetweenPxPy = M_PI + angleBetweenPxPy;
	}
    }

    PR(angleBetweenPxPy/degree);
    ////////////////////////////////////////////////
//     if(angleBetweenPxPy > mPiBy2) {
// 	cout << " pos" << endl;	
// 	angleBetweenPxPy -= M_PI;
//     }
//     if(angleBetweenPxPy < -mPiBy2) {
// 	cout << " neg" << endl;
// 	angleBetweenPxPy += M_PI;
//     }

    /////////////////////////////////////////////////
    
    
    StThreeVectorF pz(0.,0.,mLocalTrackMomentum.z());
     StThreeVectorF axis = (pz.unit()).cross(mLocalTrackMomentum.unit());
     PR(axis);

    mPhotonInPlaneRotation = -angleBetweenPxPy;


    PR(mPhotonInPlaneRotation/degree);
    
    cout << endl;
}

// ----------------------------------------------------
void StRichRayTracer::shiftOriginToMipIntersectionOnPadPlane()
{
    cout << "StRichRayTracer::shiftOriginToMipIntersectionOnPadPlane()" << endl;
    // defined as the orgin
    mTrackOnPadPlane =
	mLocalMipPosition - mLocalMipPosition;

    mTrackOnRadiator = 
	mLocalRadiatorIntercept - mLocalMipPosition;
}

// ----------------------------------------------------
void StRichRayTracer::calculateRotatedGeometry()
{
    cout << "StRichRayTracer::calculateRotatedGeometry()" << endl;
    
    mRGapThickness = gapThickness/mCosAlpha;
    mRQuartzThickness = quartzThickness/mCosAlpha;
    mRRadiatorThickness = radiatorThickness/mCosAlpha;

    mRBottomQuartz = mRGapThickness;
    mRTopQuartz    = mRGapThickness + mRQuartzThickness;
    mRTopRadiator  = mRTopQuartz + mRRadiatorThickness;
    
    mRRadiationPoint = mRTopQuartz + mRRadiatorThickness/2.;

    PR(mRGapThickness);
    PR(mRQuartzThickness);
    PR(mRRadiatorThickness);

    PR(mRBottomQuartz);
    PR(mRTopQuartz);
    PR(mRTopRadiator);

    PR(mRRadiationPoint);

}

// ----------------------------------------------------
void StRichRayTracer::setPhotonPosition(StPairD& pos)
{
    cout << "StRichRayTracer::setPhotonPosition()" << endl;
    mLocalPhotonPosition = pos;

    mPhotonOnPadPlane =
	mLocalPhotonPosition - mLocalMipPosition;

    PR(mTrackOnPadPlane);
    PR(mTrackOnRadiator);
    PR(mPhotonOnPadPlane);
    
}

// ----------------------------------------------------
void StRichRayTracer::doTrackRotation()
{
    cout << "StRichRayTracer::doTrackRotation()" << endl;

    mTrackOnPadPlane.rotate(mAlpha);
    mTrackOnRadiator.rotate(mAlpha);
    
    PR(mTrackOnPadPlane);
    PR(mTrackOnRadiator);
}

void StRichRayTracer::doPhotonRotation()
{
    cout << "StRichRayTracer::doPhotonRotation()" << endl;

    //
    // do the rotation in the x-y plane, then
    // about the track
    //
    // StPairD(l,z)
    //
    PR(mPhotonOnPadPlane);

    mPhotonOnPadPlane.rotate(mPhotonInPlaneRotation);

    PR(mPhotonOnPadPlane);

    //
    //  now rotation with respect to the track
    //
    
    mPhotonOnPadPlane.rotate(mAlpha);
    PR(mPhotonOnPadPlane);
    
}

// ----------------------------------------------------
void StRichRayTracer::calculateFastTrigFunctions()
{
    mTanTheta = tan(mTheta);
}

// ----------------------------------------------------
bool StRichRayTracer::setTheta(double theta)
{
    cout << "StRichRayTracer::setTheta()" << endl;
    PR(mTheta/degree);
    PR(theta/degree);
    //
    // Must assign:
    //   mTheta
    //   mSlope
    //   mLZero
    //

    if(mTheta == mPiBy2) {
	cout << "StRichRayTracer::setTheta()\n";
	cout << "\tCannot assign angle as 90 degrees\n";
	cout << "\tSkip this!" << endl;
	return false;
    }

    mTheta = theta;
    mSlope = tan(theta);

    //
    // eqn for the new line:
    // z(l) = tan(theta)*(l-l2) + z2
    //   where:
    //     (z2,l2) are the rotated photon points
    //

    mLZero =
	mPhotonOnPadPlane.l() - (mPhotonOnPadPlane.z())/mSlope;
    cout << "**** setTheta()\n";
    cout << "\tmLZero= (" << mLZero << ")" << endl;

    mTanTheta = tan(mTheta);

    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::initialTheta()
{
    cout << "StRichRayTracer::initalTheta()" << endl;
    //
    // Take an initial guess at the angle the photon
    // path makes with the pad plane
    // by taking a line from the
    // rotated photon position and extrapolating
    // it to the freon/quartz interface.  Check that
    // there was a finite rotation done.
    //
    // Quantities calculated:
    //    mSlope
    //    mLZero
    //    mTheta
    //

    double epsilon = 2.*millimeter;
    if( abs(mPhotonOnPadPlane.z()) < epsilon) {
	cout << "Warning: mPhotonOnPadPlane.z()=(";
	cout << mPhotonOnPadPlane.z() << ")" << endl;
    }

    double dz = mRTopQuartz-mPhotonOnPadPlane.z();
    double dl = -mPhotonOnPadPlane.l();
    mSlope = dz/dl;

    PR(mSlope);

    mLZero =
	mPhotonOnPadPlane.l() - mPhotonOnPadPlane.z()/(mSlope);

    PR(mLZero);

    mTheta = atan2(dz,dl);
    
    PR(mTheta/degree);

    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::propagateToRadiator()
{
    cout << "\nStRichRayTracer::propagateToRadiator()" << endl;

    // is there a better place for this?
    this->calculateFastTrigFunctions();

    PR(mTheta/degree);
    PR(mTanTheta);
    PR(mTanAlpha);
    PR(mLZero);

    //
    // find the linear distance (mLOne) which is covered
    // while the light ray propagates through the
    // proximity focussing gap.
    // Calculate the intersection at the bottom
    // of the quartz window
    //
    // eqn of light ray:
    //  z(l) = (lZero-l)*tan(theta)
    // eqn of quartz:
    //  z(l) = +tan(alpha)*l + mRBottomQuartz
    //

    do {
	mLOne =
	    ( (mLZero * mTanTheta) + mRBottomQuartz )/
	    ( mTanTheta - mTanAlpha );

	PR(mLOne);
	
	if( sign(mLOne * mLZero) < 0 ) {
	    cout << "StRichRayTracer::propagateToRadiator()\n";
	    cout << "\tWARNING\n";
	    cout << "\tLOne & lZero have opposite signs\n";
	    cout << "\t--> decrease theta " << endl;
	    this->setTheta( (mTheta -= mDeltaTheta) );
	    return false;
	}
	else {
	    cout << " valid mLOne...BREAK " << endl;
	    break;
	}
	
    } while ( sign(mLOne * mLZero) > 0);

    double zAtBottomQuartz = mTanAlpha*mLOne + mRGapThickness;
    PR(zAtBottomQuartz);
	
	
    //
    // calculate the refraction at the quartz/methane boundary
    // but it is altered because of the rotated track
    // Make sure you are in the absolute angular coordinate
    // system
    //
    // n_CH4 sin(theta_CH4) = n_Q sin(theta_Q)
    //
    // theta_CH4 = mPiBy2 - theta + alpha
    //   testAngle = theta - alpha
    // theta_CH4 = mPiBy2 - (testAngle)
    //

    double testAngle = mTheta - mAlpha;
    PR(testAngle/degree);
    if(testAngle < 0) {
	testAngle += m2Pi;
	PR(testAngle/degree);
    }

    //
    // I'm not suer this is a necessary check
    //

    PR(testAngle/degree);
     if(testAngle < mPiBy2) {
 	cout << "StRichRayTracer::propagateToRadiator()\n";
 	cout << "\tWARNING\n";
 	cout << "\tConvergence not possible\n";
 	cout << "\tIncrease mTheta" << endl;
 	cout << "........................." << endl;
// 	this->setTheta( (mTheta += mDeltaTheta) );
// 	return false;
     }
    
    double methaneAngle = mPiBy2 - testAngle;
    PR(methaneAngle/degree);
    double quartzAngle =
	asin(indexCH4/indexQuartz*sin(methaneAngle));
    PR(quartzAngle/degree);

//     testAngle = quartzAngle - mAlpha;
//     PR(testAngle);
//     if(testAngle < 0) {
// 	cout << "StRichRayTracer::propagateToRadiator()\n";
// 	cout << "The quartz angle will not converge:\n";
// 	cout << "Decrease mTheta" << endl;
// 	this->setTheta( (mTheta -= mDeltaTheta) );
// 	return false;
//     }

    //
    // find the linear distance (mLTwo) which is covered
    // while the light ray propagates through the
    // quartz.
    // Calculate the intersection at the top
    // of the quartz window
    //
    // eqn of light ray in quartz:
    //  z(l) = tan(quartzPropagationAngle)*(mL-mLOne) + zAtBottomQuartz
    // eqn of quartz:
    //  z(l) = +tan(alpha)*l + mRTopQuartz
    //

    testAngle = mAlpha - quartzAngle;
    PR(testAngle/degree);

    if(testAngle < 0)
	testAngle += m2Pi;
    else if(testAngle > m2Pi)
	testAngle -= m2Pi;

    PR(testAngle/degree);
    double quartzPropagationAngle = mPiBy2 + (testAngle);
    PR(quartzPropagationAngle/degree);
    if(quartzPropagationAngle>m2Pi) {
	quartzPropagationAngle -= m2Pi;
    }
    PR(quartzPropagationAngle);
    double quartzRaySlope = tan(quartzPropagationAngle);
    PR(quartzRaySlope);

    mLTwo =
	( mRTopQuartz - zAtBottomQuartz + mLOne*(quartzRaySlope) )/
	( quartzRaySlope -mTanAlpha );

    PR(mLTwo);


    //
    // not needed
    //
//     if(mLTwo<0) {
// 	cout << "StRichRayTracer::propagateToRadiator()\n";
// 	cout << "\tWARNING\n";
// 	cout << "\tmLTwo<0" << endl;
//     }
	
    if( sign(mLTwo * mLZero) < 0 ) {
	cout << "StRichRayTracer::propagateToRadiator()\n";
	cout << "\tquartz Distance & lZero have opposite signs\n";
	cout << "\t--> decrease theta" << endl;
	this->setTheta( (mTheta -= mDeltaTheta) );
	return false;
    }

	
    double zAtTopQuartz = mTanAlpha*mLTwo + mRTopQuartz;
    PR(zAtTopQuartz);
		
    //
    // calculate the refraction at the quartz/freon boundary
    // and propogate the light ray to the centroid of the
    // radiator.  This is the same operation as at the
    // first quartz interface
    //
    // light ray in the radiator:
    //  z(l) = tan(freonPropagationAngle)*l+zAtTopQuartz
    // radiator mid plane:
    //  z(l) = +tan(alpha)*l+(rGap + rQuartz + rRadiator/2);
    //
    
    double freonAngle =
	asin(indexQuartz/indexFreon*sin(quartzAngle));
    PR(freonAngle/degree);

    testAngle = mAlpha - freonAngle;
    PR(testAngle/degree);

    if(testAngle > m2Pi) {
	testAngle -= m2Pi;
    }

    PR(testAngle/degree);
    if(testAngle<0) {
	cout << "StRichRayTracer::propagateToRadiator()\n";
	cout << "\tWARNING freonAngle will not converge:\n";
	cout << "\tDecrease theta" << endl;
	cout << ".............." << endl;
	//this->setTheta( (mTheta -= mDeltaTheta) );
	//return false;
    }

    double freonPropagationAngle = mPiBy2 + testAngle;
    PR(freonPropagationAngle/degree);

    double freonRaySlope = tan(freonPropagationAngle);
    PR(freonRaySlope);
    
    mLThree =
	( mRRadiationPoint - zAtTopQuartz  + mLTwo*freonRaySlope )/
	( freonRaySlope - mTanAlpha );

    PR(mLThree);


    mCerenkovAngle = (freonPropagationAngle > mPiBy2) ?
	freonPropagationAngle - mPiBy2 :
	mPiBy2 - freonPropagationAngle;
	
    PR(mCerenkovAngle/degree);

//     if(mCerenkovAngle > m2Pi) {
// 	cout << "2pi" << endl;
// 	mCerenkovAngle -= m2Pi;
//     }

//     if(mCerenkovAngle > mPiBy2) {
// 	cout << "pi" << endl;
// 	mCerenkovAngle = M_PI - mPiBy2;
//     }
//     PR(mCerenkovAngle/degree);

    //
    // Decide what to do with the next iteration
    //
	
	
    return true;
}

// ----------------------------------------------------
bool StRichRayTracer::checkConvergence(double l3) const
{
    bool convergence = false;
    if( (abs(l3) < mPrecision) ) {
	cout << "StRichRayTracer::checkConvergence()\n";
	cout << "\tConvergence!!!" << endl;
	cout << "\tCerenkov Angle= ";
	cout << (mCerenkovAngle/degree) <<  " degrees" << endl;
		    
	convergence = true;
    }
	    
    return convergence;
}

// ----------------------------------------------------
bool  StRichRayTracer::processPhoton(double* angle) {

    this->doPhotonRotation();

    if(!this->initialTheta()) {
	cout << "Argh initial theta" << endl;
    }
    else {
	cout << " initial theta okay" << endl;
    }
    
    //if(!this->setTheta(70.*degree)) { cout << "Error in setting theta" << endl;}
    cout << "--> findCerenkovAngle" << endl;
    if(this->findCerenkovAngle()) {
	cout << "\n**** Convergence..." << endl;
	*angle = mCerenkovAngle;
    }
    else {
	cout << "\n**** No Convergence..." << endl;
	*angle = FLT_MAX;
    }
    return true;
    
}

// ----------------------------------------------------
bool StRichRayTracer::findCerenkovAngle()
{
    cout << "StRichRayTracer::findCerenkovAngle()" << endl;
    bool validAngle = false;

    //
    // the first iteration to get a starting point
    //
    
    while (!validAngle) {
	cout << "...new iteration" << endl;
	validAngle = this->propagateToRadiator();
	PR(validAngle);
    };

    double oldL3 = mLThree;
    PR(oldL3);

    if(this->checkConvergence(oldL3)) {
	cout << " StRichRayTracer::findCerenkovAngle()\n";
	cout << " \tConvergence in first iteration" << endl;
	return true;
    }
    
    //
    // adjust theta and continue with next iterations
    //

    cout << "Adjust mTheta" << endl;

    if(oldL3 < 0) {
	//
	// decrease theta (saves an iteration)
	//
	mDeltaTheta *= -1.;
    }
    PR(mDeltaTheta/degree);
    this->setTheta( (mTheta += mDeltaTheta) );

    int iteration=0;
    do {
	iteration++;
	validAngle = false;
	while (!validAngle) {
	    validAngle = this->propagateToRadiator();
	};
	
	double newL3 = mLThree;
	PR(newL3);
	
	//
	// Did we converge?
	//

	if(this->checkConvergence(newL3)) {
	    cout << " StRichRayTracer::findCerenkovAngle()\n";
	    cout << " \tConvergence in (" << iteration << ") iteration" << endl;
	    return true;
	}
	
	if( abs(oldL3) > abs(newL3) ) {
	    //
	    // good, leave deltaTheta alone
	    //
	}
	else {
	    //
	    // Wrong way
	    // change of sign
	    //
	    mDeltaTheta *= -0.5;
	}

	oldL3 = newL3;
	
	PR(mDeltaTheta/degree);
	this->setTheta( (mTheta += mDeltaTheta) );

    } while (iteration < mMaximumNumberOfIterations);

    return false;
}

// ----------------------------------------------------
double StRichRayTracer::cerenkovAngle() const {return mCerenkovAngle;}

// ----------------------------------------------------
void StRichRayTracer::status() const
{
    cout << "StRichRayTracer::status()\n";
    cout << "  ==> mLZero= (" << mLZero << ")" << endl;
    cout << "  ==> mSlope= (" << mSlope << ")" << endl;
}
