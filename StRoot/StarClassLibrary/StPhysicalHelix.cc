/***************************************************************************
 *
 * $Id: StPhysicalHelix.cc,v 1.2 1999/02/12 01:01:04 wenaus Exp $
 *
 * Author: Brian Lasiuk, Sep 1997
 ***************************************************************************
 *
 * Description: 
 * Parametrization of a physical helix.
 * 
 ***************************************************************************
 *
 * $Log: StPhysicalHelix.cc,v $
 * Revision 1.2  1999/02/12 01:01:04  wenaus
 * Fix bug in momentum calculation
 *
 * Revision 1.2  1999/02/12 01:01:04  wenaus
 * Fix bug in momentum calculation
 *
 * Revision 1.1  1999/01/30 03:59:04  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:21  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <math.h>
#include "StHelix.hh"
#include "StPhysicalHelix.hh"
#include "PhysicalConstants.h" 
#include "SystemOfUnits.h"

StPhysicalHelix::StPhysicalHelix(){}

StPhysicalHelix::~StPhysicalHelix() { /* nop */ }

StPhysicalHelix::StPhysicalHelix(const StThreeVector<double>& p,
				 const StThreeVector<double>& o,
				 double B, double q)
{
    mH = (q*B <= 0) ? 1 : -1;
    if(p.y() == 0 && p.x() == 0)
	setPhase((M_PI/4)*(1-2.*mH));
    else
	setPhase(atan2(p.y(),p.x())-mH*M_PI/2);
    setDipAngle(atan2(p.z(),p.perp()));
    mOrigin = o;

#ifndef ST_NO_NAMESPACES
    {
	using namespace units;
#endif
    setCurvature(fabs((c_light*nanosecond/meter*q*B/tesla)/
		      (abs(p)/GeV*mCosDipAngle)/meter));   
#ifndef ST_NO_NAMESPACES
    }
#endif
}

StPhysicalHelix::StPhysicalHelix(double c, double d, double phase,
				 const StThreeVector<double>& o, int h)
    : StHelix(c, d, phase, o, h) { /* nop */}


StThreeVector<double> StPhysicalHelix::momentum(double B) const
{
    if(mSingularity)
	return(StThreeVector<double>(0,0,0));
    else {
#ifndef ST_NO_NAMESPACES
	{
	    using namespace units;
#endif
	    double pt =
		fabs(c_light*nanosecond/meter*B/tesla)/(fabs(mCurvature)*meter)/GeV;
    
	    return (StThreeVector<double>(pt*cos(mPhase+mH*M_PI/2),   // pos part pos field
					  pt*sin(mPhase+mH*M_PI/2),
					  pt*tan(mDipAngle)));
#ifndef ST_NO_NAMESPACES
	}
#endif
    }
}

StThreeVector<double> StPhysicalHelix::momentumAt(double S, double B)
{
    this->moveOrigin(S);
    StThreeVector<double> p = this->momentum(B);
    this->moveOrigin(-S);
    return p;
}

int StPhysicalHelix::charge(double B) const
{
    return (B > 0 ? -mH : mH);
}
