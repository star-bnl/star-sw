//StiCircleCalculator.h
//M.L. Miller (Yale Software)
//10/01

/*! \class StiCircleCalculator
  StiCircleCalculator calculates the radius and center of a circle.  It is
  assumed that the circle is in the x-y plane.

  \author Thomas Ullrich
  \author M.L. Miller (Yale Software)

  \note StiCircleCalculator is adapted from the class EtCircleCalculator
  from the Yale Elastic Tracking prototype.  Many thanks to Thomas and
  Brian for the math.
  
*/

#ifndef StiCircleCalculator_HH
#define StiCircleCalculator_HH

#include "Messenger.h"
#include "StThreeVector.hh"

class StiCircleCalculator
{
public:

    StiCircleCalculator();
    virtual ~StiCircleCalculator();
    
    //Action
    virtual void calculate(const StThreeVector<double>& pt1,
			   const StThreeVector<double>& pt2,
			   const StThreeVector<double>& pt3);
    
    //gets
    double radius() const;
    double xCenter() const;
    double yCenter() const;
    double probableH() const;
    
protected:
    //The circle quantities (we store here, to avoid class proliferation)
    Messenger& mMessenger;
    
    double mRadius;
    double mXCenter;
    double mYCenter;
    double mProbableH;
    
    void calculateRadius(const StThreeVector<double>& pt1,
			 const StThreeVector<double>& pt2,
			 const StThreeVector<double>& pt3);
    
    void calculateCenter(const StThreeVector<double>& pt1,
			 const StThreeVector<double>& pt2,
			 const StThreeVector<double>& pt3);
    
    void calculateProbableH(const StThreeVector<double>& pt1,
			    const StThreeVector<double>& pt2,
			    const StThreeVector<double>& pt3);

};

//inlines

inline double StiCircleCalculator::radius() const
{
    return mRadius;
}

inline double StiCircleCalculator::xCenter() const
{
    return mXCenter;
}

inline double StiCircleCalculator::yCenter() const
{
    return mYCenter;
}

inline double StiCircleCalculator::probableH() const
{
    return mProbableH;
}

#endif

