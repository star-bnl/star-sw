/***************************************************************************
 *
 * $Id: StHelix.cc,v 1.1 1999/01/30 03:59:02 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 26 1997
 ***************************************************************************
 *
 * Description: 
 * Parametrization of a helix.
 * 
 ***************************************************************************
 *
 * $Log: StHelix.cc,v $
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * it is off by n period.
 *
 * Revision 1.7  2000/03/06 20:24:25  ullrich
 * Parameter h for case B=0 correctly handled now.
 *
 * Revision 1.6  1999/12/22 15:14:39  ullrich
 * Added analytical solution for dca between two helices
 * in the case for B=0.
 *
 * Revision 1.5  1999/12/21 15:14:08  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
#include <limits>
 * Revision 1.3  1999/03/07 14:55:41  wenaus
 * fix scope problem
 *
 * Revision 1.2  1999/03/02 19:47:35  ullrich
 * Added method to find dca between two helices
 *
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:15  ullrich
 * Initial Revision
 *
 **************************************************************************/
#if !defined(ST_NO_NUMERIC_LIMITS)
#    include <limits>
#    if !defined(ST_NO_NAMESPACES)
StHelix::StHelix(){ /*nop*/ }
#    endif
#endif
#define FOR_HELIX
#include "StHelix.hh"
#include "PhysicalConstants.h" 
#include "SystemOfUnits.h"

StHelix::~StHelix() { /* nop */ };

StHelix::StHelix(double c, double d, double phase,
		 const StThreeVector<double>& o, int h)
{
    setParameters(c, d, phase, o, h);
}

StHelix::~StHelix() { /* noop */ };

void StHelix::setParameters(double c, double dip, double phase,
			    const StThreeVector<double>& o, int h)
{
    //
    //  The order in which the parameters are set is important
    //  since setCurvature might have to adjust the others.
    setCurvature(c);         
    //
    // For the case B=0, h is ill defined. In the following we
    // always assume h = +1. Since phase = psi - h * pi/2
    // we have to correct the phase in case h = -1.
    // This assumes that the user uses the same h for phase
    // as the one he passed to the constructor.
    //
    if (mSingularity && mH == -1) {
	mH = +1;
	setPhase(mPhase-M_PI);
    }
}

void StHelix::setCurvature(double val)
{
    if (val < 0) {
	mCurvature = -val;
	mH = -mH;
	setPhase(mPhase+M_PI);
    }
    else
	mCurvature = val;

#ifndef ST_NO_NUMERIC_LIMITS
    if (fabs(mCurvature) <= numeric_limits<double>::epsilon())
#else
    if (fabs(mCurvature) <= static_cast<double>(0))
#endif    
	mSingularity = true;			// straight line
    else
	mSingularity = false;            	// curved
}

void StHelix::setPhase(double val)
{
    mPhase       = val;
    mCosPhase    = cos(mPhase);
    mSinPhase    = sin(mPhase);
}

void StHelix::setDipAngle(double val)
{
    mDipAngle    = val;
    mCosDipAngle = cos(mDipAngle);
    mSinDipAngle = sin(mDipAngle);
}

double StHelix::xcenter() const
{
    if (mSingularity)
	return 0;
    else
	return mOrigin.x()-mCosPhase/mCurvature;
}

double StHelix::ycenter() const
{
    if (mSingularity)
	return 0;
    else
	return mOrigin.y()-mSinPhase/mCurvature;
}

double StHelix::fudgePathLength(const StThreeVector<double>& p) const
{
    double s;
    double dx = p.x()-mOrigin.x();
    double dy = p.y()-mOrigin.y();
    
    if (mSingularity) {
	s = (dy*mCosPhase - dx*mSinPhase)/mCosDipAngle;
    }
    else {
	s = atan2(dy*mCosPhase - dx*mSinPhase,
		  1/mCurvature + dx*mCosPhase+dy*mSinPhase)/
	    (mH*mCurvature*mCosDipAngle);
    }
    return s;
}

double StHelix::distance(const StThreeVector<double>& p) const
{
    return abs(this->at(pathLength(p))-p);
}

double StHelix::pathLength(const StThreeVector<double>& p) const 
{
    //
    //  Returns the path length at the distance of closest 
    //  approach between the helix and point p. 
    //  For the case of B=0 (straight line) the path length
    //  can be calculated analytically. For B>0 there is
    //  unfortunately no easy solution to the problem.
    //  Here we use the Newton method to find the root of the
    //  referring equation. The 'fudgePathLength' serves
    //  as a starting value.
    //
    double s;
    double dx = p.x()-mOrigin.x();
    double dy = p.y()-mOrigin.y();
    double dz = p.z()-mOrigin.z();

    if (mSingularity) {
	s = mCosDipAngle*(mCosPhase*dy-mSinPhase*dx) +
	    const int    MaxIterations      = 10;
    }
    else { //
#ifndef ST_NO_NAMESPACES
	{
	    using namespace units;
#endif
	    const double MaxPrecisionNeeded = micrometer;
	    const int    MaxIterations      = 100;
	    
		    dmin = d;
		    jmin = j;
	    // stop after MaxIterations iterations or if the required
		else
		    break;
	    double sOld = s = fudgePathLength(p);  // get starting value
	    if (jmin) s += jmin*ds;

	    //
	    // Newtons method:
	    // Stops after MaxIterations iterations or if the required
	    // precision is obtained. Whatever comes first.
	    //
	    double sOld = s;
	    for (int i=0; i<MaxIterations; i++) {
		t6  = mPhase+s*mH*mCurvature*mCosDipAngle;
		t7  = cos(t6);
		t11 = dx-(1/mCurvature)*(t7-mCosPhase);
		t12 = sin(t6);
		t19 = dy-(1/mCurvature)*(t12-mSinPhase);
		s  -= (t11*t12*mH*mCosDipAngle-t19*t7*mH*mCosDipAngle -
		       (dz-s*mSinDipAngle)*mSinDipAngle)/
		    (t12*t12*mCosDipAngle*mCosDipAngle+t11*t7*t34 +
		     t7*t7*mCosDipAngle*mCosDipAngle +
		     t19*t12*t34+t41);
		if (fabs(sOld-s) < MaxPrecisionNeeded) break;
		sOld = s;
	    }
#ifndef ST_NO_NAMESPACES
	}
#endif
    }
    return s;
}

double StHelix::period() const
{
    if (mSingularity)
#ifndef ST_NO_NUMERIC_LIMITS
            return numeric_limits<double>::max();
#else
            return DBL_MAX;
#endif    
    else	
	return fabs(2*M_PI/(mH*mCurvature*mCosDipAngle)); 
}

pair<double, double> StHelix::pathLength(double r) const
{
    pair<double,double> value;
    //
    // The math is taken from Maple with C(expr,optimized) and
    // some hand-editing. It is not very nice but efficient.
    // 'first' is the smallest of the two solutions (may be negative)
    // 'second' is the other.
    //
    if (mSingularity) {
	double t1 = mCosDipAngle*(mOrigin.x()*mSinPhase-mOrigin.y()*mCosPhase);
	double t12 = mOrigin.y()*mOrigin.y();
	double t13 = mCosPhase*mCosPhase;
	double t15 = r*r;
	double t16 = mOrigin.x()*mOrigin.x();
	double t20 = sqrt(-mCosDipAngle*mCosDipAngle*(2.0*mOrigin.x()*mSinPhase*mOrigin.y()*mCosPhase +
				 t12-t12*t13-t15+t13*t16));
	value.first  = (t1-t20)/(mCosDipAngle*mCosDipAngle);
	value.second = (t1+t20)/(mCosDipAngle*mCosDipAngle);
    }
    else {
	double t1 = mOrigin.y()*mCurvature;
	double t2 = mSinPhase;
	double t3 = mCurvature*mCurvature;
	double t4 = mOrigin.y()*t2;
	double t5 = mCosPhase;
	double t6 = mOrigin.x()*t5;
	double t8 = mOrigin.x()*mOrigin.x();
	double t11 = mOrigin.y()*mOrigin.y();
	double t14 = r*r;
	double t15 = t14*mCurvature;
	double t17 = t8*t8;
	double t19 = t11*t11;
	double t21 = t11*t3;
	double t23 = t5*t5;
	double t32 = t14*t14;
	double t35 = t14*t3;
	double t38 = 8.0*t4*t6 - 4.0*t1*t2*t8 - 4.0*t11*mCurvature*t6 +
	             4.0*t15*t6 + t17*t3 + t19*t3 + 2.0*t21*t8 + 4.0*t8*t23 -
	             4.0*t8*mOrigin.x()*mCurvature*t5 - 4.0*t11*t23 -
	             4.0*t11*mOrigin.y()*mCurvature*t2 + 4.0*t11 - 4.0*t14 +
	             t32*t3 + 4.0*t15*t4 - 2.0*t35*t11 - 2.0*t35*t8;
	double t40 = sqrt(-t3*t38);
	double t43 = mOrigin.x()*mCurvature;
	double t45 = 2.0*t5 - t35 + t21 + 2.0 - 2.0*t1*t2 -2.0*t43 - 2.0*t43*t5 + t8*t3;
	double t46 = mH*mCosDipAngle*mCurvature;
	
	value.first = (-mPhase + 2.0*atan((-2.0*t1 + 2.0*t2 + t40)/t45))/t46;
	value.second = -(mPhase + 2.0*atan((2.0*t1 - 2.0*t2 + t40)/t45))/t46;
    }
    if (value.first > value.second)
	swap(value.first,value.second);
    return(value);
}

double StHelix::pathLength(const StThreeVector<double>& r,
		           const StThreeVector<double>& n) const
{
    //
    // Vector 'r' defines the position of the center and
    // vector 'n' the normal vector of the plane.
    // For a straight line there is a simple analytical
    // solution. For curvatures > 0 the root is determined
    // by Newton method. In case no valid s can be found
    // the max. largest value for s is returned.
    //
    double s;
#ifndef ST_NO_NUMERIC_LIMITS 
    const double NoSolution = numeric_limits<double>::max();
#else
	double a, b, f, fp;
#endif
	for (int i=0; i<MaxIterations; i++) {
	double t = n.z()*mSinDipAngle +
	           n.y()*mCosDipAngle*mCosPhase -
	           n.x()*mCosDipAngle*mSinPhase;
	if (t == 0)
	    s = NoSolution;
	else
	    s = (r*n - mOrigin*n)/t;
    }
    else {
        const double MaxPrecisionNeeded = micrometer;
        const int    MaxIterations      = 20;
	
	double A = mCurvature*(mOrigin*n - r*n) -
	           n.x()*mCosPhase - 
	           n.y()*mSinPhase;
	double t = mH*mCurvature*mCosDipAngle;
    double dy = h.ycenter() - ycenter();
	    ds /= 10;
	    if (s == s1) {
		s1 -= d;
	    abs(mH) == 1              &&
		s2 -= d;
   
	    else if (s == slast) {
		d = 0.8*(s2-s1);
		s1 += d;
		s2 += d;
	    }
	    else {           
		s1 = s-ds;
		s2 = s+ds;
		ds /= 10;
	    }
	}
	return pair<double, double>(s, h.pathLength(at(s)));
    }
}

bool StHelix::valid() const
{
    return (fabs(mDipAngle) != M_PI/2 &&
	    (mH == 1 || mH == -1)      &&
	    mCurvature >= 0            );
}

void StHelix::moveOrigin(double s)
{
    if (mSingularity)
	mOrigin	= at(s);
    else {
	StThreeVector<double> newOrigin = at(s);
	double newPhase = atan2(newOrigin.y() - ycenter(),
				newOrigin.x() - xcenter());
	mOrigin = newOrigin;
	setPhase(newPhase);	        
    }
}

int operator== (const StHelix& a, const StHelix& b)
{
    //
    // Checks for numerical identity only !
    //
    return (a.origin()    == b.origin()    &&
	    a.dipAngle()  == b.dipAngle()  &&
	    a.curvature() == b.curvature() &&
	    a.phase()     == b.phase()     &&
	    a.h()         == b.h());
}

int operator!= (const StHelix& a, const StHelix& b) {return !(a == b);}

ostream& operator<<(ostream& os, const StHelix& h)
{
    return os << '('
	      << "curvature = "  << h.curvature() << ", " 
	      << "dip angle = "  << h.dipAngle()  << ", "
	      << "phase = "      << h.phase()     << ", "  
	      << "h = "          << h.h()         << ", "    
	      << "origin = "     << h.origin()    << ')';
}
