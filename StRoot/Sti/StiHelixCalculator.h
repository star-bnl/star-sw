//StiHelixCalculator.h
//M.L. Miller (Yale Software)
//10/01

#ifndef StiHelixCalculator_HH
#define StiHelixCalculator_HH

#include <math.h>

#include "StiCircleCalculator.h"

class StiHelixCalculator : public StiCircleCalculator
{
public:
    StiHelixCalculator();
    virtual ~StiHelixCalculator();

    //action

    ///It is assumed that pt2 is 'between' pt1 and pt3 in radius
    virtual void calculate(const StThreeVector<double>& pt1,
			   const StThreeVector<double>& pt2,
			   const StThreeVector<double>& pt3);
    
    //gets

    ///Tangent(lambda) in the standard STAR helix model.
    double tanLambda() const;

    ///Reference point on the z-axis in the standard STAR helix model.
    double z0() const;

    ///Signed Curvature
    double curvature() const;
    
protected:

    void calculateLine(const StThreeVector<double>& pt1,
		       const StThreeVector<double>& pt2,
		       const StThreeVector<double>& pt3);
    
    double mTanLambda;
    double mZ0;
};

//inlines

inline double StiHelixCalculator::tanLambda() const
{
    return mTanLambda;
}

inline double StiHelixCalculator::z0() const
{
    return mZ0;
}

inline double StiHelixCalculator::curvature() const
{
    return (mRadius<=0.) ? DBL_MAX : mProbableH / mRadius;
}

#endif
