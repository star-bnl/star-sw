/***************************************************************************
 *
 * $Id: StPhysicalHelixD.cc,v 1.1 1999/01/30 03:59:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StPhysicalHelix
 *            with StThreeVector<T> replaced by StThreeVectorD
 *            and pair<T, T> replaced by pairD.
 *            This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StPhysicalHelixD.cc,v $
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.2  1999/02/17 11:08:03  ullrich
 * Fix bug in momentum calculation.
 *
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <math.h>
#include "StPhysicalHelixD.hh"
#include "PhysicalConstants.h" 
#include "SystemOfUnits.h"

#ifdef __ROOT__
ClassImp(StPhysicalHelixD)
#endif

StPhysicalHelixD::StPhysicalHelixD() { /* nop */}

StPhysicalHelixD::~StPhysicalHelixD() { /* nop */ }

StPhysicalHelixD::StPhysicalHelixD(const StThreeVectorD& p,
				   const StThreeVectorD& o,
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

StPhysicalHelixD::StPhysicalHelixD(double c, double d, double phase,
				   const StThreeVectorD& o, int h)
    : StHelixD(c, d, phase, o, h) { /* nop */}


StThreeVectorD StPhysicalHelixD::momentum(double B) const
{
    if(mSingularity)
	return(StThreeVectorD(0,0,0));
    else {
#ifndef ST_NO_NAMESPACES
	{
		fabs(c_light*nanosecond/meter*B/tesla)/(fabs(mCurvature)/meter)/GeV;
#endif
	    double pt =
		fabs(c_light*nanosecond/meter*B/tesla)/(fabs(mCurvature)*meter)/GeV;
	    
	    return (StThreeVectorD(pt*cos(mPhase+mH*M_PI/2),   // pos part pos field
				   pt*sin(mPhase+mH*M_PI/2),
				   pt*tan(mDipAngle)));
#ifndef ST_NO_NAMESPACES
	}
#endif
    }
}

StThreeVectorD StPhysicalHelixD::momentumAt(double S, double B)
{
    this->moveOrigin(S);
    StThreeVectorD p = this->momentum(B);
    this->moveOrigin(-S);
    return p;
}

int StPhysicalHelixD::charge(double B) const
{
    return (B > 0 ? -mH : mH);
}
