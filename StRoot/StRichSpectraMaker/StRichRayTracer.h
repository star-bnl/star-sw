/***************************************************************************
 *
 * $Id: StRichRayTracer.h,v 1.1 2001/02/25 22:11:20 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichRayTracer.h,v $
 * Revision 1.1  2001/02/25 22:11:20  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "StThreeVectorF.hh"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StPairD.h"

class StRichRayTracer {

public:
    StRichRayTracer();
    StRichRayTracer(StThreeVectorF&, StPairD&, StPairD&);
    ~StRichRayTracer();

    void setPhotonPosition(StPairD&);
    void doTrackRotation();
    void doPhotonRotation();
    bool setTheta(double);
    bool initialTheta();

    bool propagateToRadiator();
    bool findCerenkovAngle();
    bool processPhoton(double*);

    bool checkConvergence(double) const;
    double cerenkovAngle() const;

    void  status() const;
    
protected:
    
    void init();
    void calculateRotatedGeometry();
    void calculateTrackAngle();
    void calculateFastTrigFunctions();
    void shiftOriginToMipIntersectionOnPadPlane();
    
private:
    
    //
    // Track parameters
    //
    
    StThreeVectorF mLocalTrackMomentum;
    StPairD mLocalMipPosition;
    StPairD mLocalRadiatorIntercept;

    double mAlpha;  // Track incident angle (rotation)
    double mCosAlpha, mSinAlpha, mTanAlpha;
    double mTanTheta;

    //
    // Rotated Chamber Geometry
    //
    double mRGapThickness;
    double mRQuartzThickness;
    double mRRadiatorThickness;
    
    double mRBottomQuartz;
    double mRTopQuartz;
    double mRTopRadiator;

    double mRRadiationPoint;

    //
    // Photon Quantities
    //
    StPairD  mLocalPhotonPosition;
    double   mPhotonInPlaneRotation;
    
    //
    // local angle calculcation:
    StPairD  mTrackOnPadPlane;
    StPairD  mTrackOnRadiator;
    StPairD  mPhotonOnPadPlane;
    
    //
    // Database Quantities
    //
    double indexQuartz;
    double indexFreon;
    double indexCH4;

    double gapThickness;
    double quartzThickness;
    double radiatorThickness;

    double mPrecision;
    

    //    ^
    //  z |
    //    |
    //    |
    //    -------> l
    //
    // (l,z) where:
    //   l is on pad plane
    //   z is distance from pad plane
    //

    //
    // calulator Quantities
    //
    double m2Pi;
    double mPiBy2;
    double mSlope;
    double mLZero;
    double mLOne;
    double mLTwo;
    double mLThree;

    double mTheta;
    double mDeltaTheta;
    double mCerenkovAngle;

    double mMaximumNumberOfIterations;
};
