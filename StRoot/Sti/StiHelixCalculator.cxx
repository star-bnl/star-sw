//StiHelixCaclulator.cxx
//M.L. Miller (Yale Software)
//10/01

//std
#include <Stiostream.h>
#include <math.h>

//scl
#include "StThreeVectorF.hh"
#include "StThreeVector.hh"

//sti
#include "StiHelixCalculator.h"

StiHelixCalculator::StiHelixCalculator()
{
}

StiHelixCalculator::~StiHelixCalculator()
{
}

void StiHelixCalculator::calculate(const StThreeVector<double>& pt1,
				   const StThreeVector<double>& pt2,
				   const StThreeVector<double>& pt3)
{
    //mMessenger <<"StiHelixCalculator::calculate()"<<endl;
    //mMessenger <<"pt1: "<<pt1<<" pt2: "<<pt2<<" pt3: "<<pt3<<endl;

    mTanLambda=0.;
    mZ0=0.;
    
    //Find circle parameters
    StiCircleCalculator::calculate(pt1, pt2, pt3);

    //Find line paramters
    calculateLine(pt1, pt2, pt3);
}

void StiHelixCalculator::calculateLine(const StThreeVector<double>& pt1,
				       const StThreeVector<double>& pt2,
				       const StThreeVector<double>& pt3)
{
    /*
      note: R is 3d distance from origin
      
      Calculate dipAngle:
      The exact expression is (dz/ds) but
      we can approximate as (dz/dR)
      and adjust with a small
      fudge factor for: (dR/ds)
    */
    
    //          dz     dz   dR
    // sin L =  --  =  --  (--)
    //          ds     dR   ds   
    
    //mMessenger <<"StiHelixCalculator::calculateLine()"<<endl;
    //mMessenger <<"pt1: "<<pt1<<" pt2: "<<pt2<<" pt2: "<<pt2<<endl;

    double dzdr = (pt2.z()-pt1.z())/(abs(pt2-pt1));

    //mMessenger <<"dzdr: "<<dzdr<<"\tasin(dzdr): "<<asin(dzdr)
    //<<"\ttan( asin(dzdr)): "<<tan(asin(dzdr))<<endl;

    mTanLambda =  tan ( asin(dzdr) );
    //mMessenger <<"set tanLambda to: "<<mTanLambda<<" access: "<<tanLambda()<<endl;
    
    //Find z0
}

