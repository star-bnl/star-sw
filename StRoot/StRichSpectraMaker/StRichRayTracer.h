/***************************************************************************
 *
 * $Id: StRichRayTracer.h,v 1.2 2001/08/21 17:58:34 lasiuk Exp $
 *
 * Author:  bl Feb 21, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              Cerenkov Ray Tracing Algorithm
 ***************************************************************************
 *
 * $Log: StRichRayTracer.h,v $
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

#include "StThreeVectorF.hh"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
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

    void  status() const;
    
protected:    

    void init(double);
    void calculateGeometry();
    void calculateTrackAngle();

    bool adjustPropagator();

private:
    
    //
    // Track parameters
    //
    
    StThreeVectorF mLocalTrackMomentum;
    StThreeVectorF mExpectedRadiationPoint;
    StThreeVectorF mTheCalculatedRadiationPoint;
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
    
    //double mTheta;
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

    double mRadiatorThickness;
    double mQuartzThickness;
    double mGapThickness;

    StThreeVectorF mNormalBottomQuartz;
    StThreeVectorF mNormalTopQuartz;
    StThreeVectorF mNormalTopRadiator;
    StThreeVectorF mNormalRadiationPoint;
};

inline double StRichRayTracer::azimuth() const {return mPhi; }
inline double StRichRayTracer::cerenkovAngle() const {return mCerenkovAngle;}
inline double StRichRayTracer::trackAngle() const {return mAlpha; }
inline double StRichRayTracer::convergenceValue() const {return mConvergenceValue; }
inline double StRichRayTracer::epsilon() const {return (mConvergenceValue - mPhotonRadPointTransverseDistance); }

#endif
