/***************************************************************************
 *
 * $Id: StRichRayTracer.h,v 1.5 2002/05/21 22:07:13 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichRayTracer.h,v $
 * Revision 1.5  2002/05/21 22:07:13  lasiuk
 * revision of index of refraction
 * and ray tracing
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
    StRichRayTracer(double, double);
    StRichRayTracer(double, StThreeVectorF&, StThreeVectorF&, StThreeVectorF&, double);
    ~StRichRayTracer();

    //StRichRayTracer(const StRichRayTracer&) {/* use default */}
    //operator=(const StRichRayTracer&) {/* use default */}

    void setFreonRadiationPlane(StThreeVectorF&);
    void setQuartzRadiationPlane(StThreeVectorF&);
    void setTrack(StThreeVectorF&, StThreeVectorF&, StThreeVectorF&,double);
    void setPhotonPosition(StThreeVectorF&);
    bool initialPropagator();

    bool propagateToRadiator();
    bool findCerenkovAngle();
    bool findAzimuth();
    bool processPhoton(double*);
    bool checkConvergence(StThreeVectorF&) const;
    double calculatePathIntegral(double);

    bool findQuartzCerenkovAngle();
    bool propagateToQuartz();    
    bool checkQuartzConvergence(StThreeVectorF&) const;

    //
    // simple accessors and assignment
    //
    void setxyPrecision(double);
    
    double cerenkovAngle()       const;
    double azimuth()             const;
    double lineIntegral(double*) const;
    double trackAngle()          const;
    double convergenceValue()    const;
    double epsilon()             const;

    double quartzCerenkovAngle()    const;
    double quartzConvergenceValue() const;

    StThreeVectorF photonAtQuartzPlate() const;
    
    void  status() const;

    //
    // For Drawing and Line Integrals
    //
    vector<StThreeVectorF> calculatePoints(StThreeVectorF&, double);

protected:    

    void init(double, double);
    void calculateGeometry();
    void calculateTrackAngle();

    StThreeVectorF intersectionWithPlane(StThreeVectorF&, StThreeVectorF&,
					 StThreeVectorF&, StThreeVectorF&);
    StThreeVectorF rotateToAxis(double, StThreeVectorF&);
    StThreeVectorF rotateToTrackInclination(double, StThreeVectorF&);

    bool checkPlane(StThreeVectorF&);
    void traceDown();
    void traceUp();

private:
    
    //
    // Track parameters
    //
    double mAlpha;  // track incident angle
    StThreeVectorF mLocalTrackMomentum;
    StThreeVectorF mExpectedRadiationPoint;
    StThreeVectorF mExpectedQuartzRadiationPoint;
    StThreeVectorF mTheCalculatedRadiationPoint;
    StThreeVectorF mTheCalculatedQuartzRadiationPoint;
    StThreeVectorF mAxis;
    
    StThreeVectorF mTopOfQuartzPlate;

    //
    // Photon Quantities
    //
    StThreeVectorF  mLocalPhotonPosition;

    //
    // calulator/propagator Quantities
    //
    StThreeVectorF mGapPropagator;
    StThreeVectorF mGapPropagatorForQuartz;
    double mPhotonRadPointTransverseDistance;
    double mConvergenceValue;
    double mQuartzConvergenceValue;
    
    double mDeltaTheta;
    double mInitialIncrement;
    double mCerenkovAngle;
    double mQuartzCerenkovAngle;
    double mPhi;
    double mLineIntegralOnPlane;
    double mLineIntegralRatio;
    
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

    double mYGap, mXGap;
    double mXEdge, mYEdge;
    
    StThreeVectorF mDetectorNormal;
    StThreeVectorF mBottomQuartzPoint;
    StThreeVectorF mTopQuartzPoint;
    StThreeVectorF mTopRadiatorPoint;

    StThreeVectorF mRadiationPlanePoint;
    StThreeVectorF mQuartzRadiationPlanePoint;
};

inline void StRichRayTracer::setFreonRadiationPlane(StThreeVectorF& p) {mRadiationPlanePoint = p;}
inline void StRichRayTracer::setQuartzRadiationPlane(StThreeVectorF& p) {mQuartzRadiationPlanePoint=p;}
inline double StRichRayTracer::azimuth() const {return mPhi; }
inline double StRichRayTracer::cerenkovAngle() const {return mCerenkovAngle;}
inline double StRichRayTracer::quartzCerenkovAngle() const {return mQuartzCerenkovAngle;}
inline double StRichRayTracer::lineIntegral(double *ratio) const {*ratio = mLineIntegralRatio; return mLineIntegralOnPlane; }

inline double StRichRayTracer::trackAngle() const {return mAlpha; }
inline double StRichRayTracer::convergenceValue() const {return mConvergenceValue; }
inline double StRichRayTracer::quartzConvergenceValue() const {return mQuartzConvergenceValue; }
inline double StRichRayTracer::epsilon() const {return (mTheCalculatedRadiationPoint - mExpectedRadiationPoint).perp(); }
inline StThreeVectorF StRichRayTracer::photonAtQuartzPlate() const {return mTopOfQuartzPlate;}

#endif
