/***************************************************************************
 *
 * $Id: StRichRayTracer.h,v 1.3 2001/11/21 20:36:07 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichRayTracer.h,v $
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
#ifndef StRichRayTracer_h
#define StRichRayTracer_h

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <vector>

#include "StThreeVectorF.hh"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
using std::vector;
#endif

class StRichRayTracer {

public:
    StRichRayTracer();
    StRichRayTracer(double);
    StRichRayTracer(double, StThreeVectorF&, StThreeVectorF&, StThreeVectorF&);
    ~StRichRayTracer();

    //StRichRayTracer(const StRichRayTracer&) {/* use default */}
    //operator=(const StRichRayTracer&) {/* use default */}

    void setTrack(StThreeVectorF&, StThreeVectorF&, StThreeVectorF&);
    void setPhotonPosition(StThreeVectorF&);
    bool initialPropagator();

    bool propagateToRadiator();
    bool findCerenkovAngle();
    bool findAzimuth();
    bool processPhoton(double*);

    bool checkConvergence(StThreeVectorF&) const;

    //
    // simple accessors and assignment
    //

    void setxyPrecision(double);
    
    double cerenkovAngle()    const;
    double azimuth()          const;
    double trackAngle()       const;
    double convergenceValue() const;
    double epsilon()          const;

    StThreeVectorF photonAtQuartzPlate() const;

    vector<StThreeVectorF> calculatePoints(StThreeVectorF&, double);
    
    void  status() const;
    
protected:    

    void init(double);
    void calculateGeometry();
    void calculateTrackAngle();

    bool adjustPropagator();
    StThreeVectorF intersectionWithPlane(StThreeVectorF&, StThreeVectorF&,
					 StThreeVectorF&, StThreeVectorF&);
    StThreeVectorF rotateToAxis(double, StThreeVectorF&);
    StThreeVectorF rotateToTrackInclination(double, StThreeVectorF&);

    void traceDown();
    void traceUp();
private:
    
    //
    // Track parameters
    //
    
    StThreeVectorF mLocalTrackMomentum;
    StThreeVectorF mExpectedRadiationPoint;
    StThreeVectorF mTheCalculatedRadiationPoint;

    StThreeVectorF mTopOfQuartzPlate;

    double mAlpha;   // track incident angle

    //
    // Photon Quantities
    //
    StThreeVectorF  mLocalPhotonPosition;



    //
    // calulator/propagator Quantities
    //
    double m2Pi;

    //
    //
    StThreeVectorF mGapPropagator;
    double mPhotonRadPointTransverseDistance;
    double mConvergenceValue;
    
    double mDeltaTheta;
    double mInitialIncrement;
    double mCerenkovAngle;
    double mPhi;

    //
    // Parameters
    //
    
    double mMaximumNumberOfIterations;
    double mzPrecision;
    double mxyPrecision;

    //
    // Database Quantities and
    // chamber geometry
    //
    double mMeanWavelength;
    double mIndexFreon;
    double mIndexQuartz;
    double mIndexCH4;
    double mnF1,mnQ1,mnG1;
    double mnF2,mnQ2,mnG2;
    
    double mRadiatorThickness;
    double mQuartzThickness;
    double mGapThickness;

    StThreeVectorF mDetectorNormal;
    StThreeVectorF mBottomQuartzPoint;
    StThreeVectorF mTopQuartzPoint;
    StThreeVectorF mTopRadiatorPoint;
    StThreeVectorF mRadiationPlanePoint;
    StThreeVectorF mAxis;
};

inline double StRichRayTracer::azimuth() const {return mPhi; }
inline double StRichRayTracer::cerenkovAngle() const {return mCerenkovAngle;}
inline double StRichRayTracer::trackAngle() const {return mAlpha; }
inline double StRichRayTracer::convergenceValue() const {return mConvergenceValue; }
inline double StRichRayTracer::epsilon() const {return (mConvergenceValue - mPhotonRadPointTransverseDistance); }
inline StThreeVectorF StRichRayTracer::photonAtQuartzPlate() const {return mTopOfQuartzPlate;}

#endif
