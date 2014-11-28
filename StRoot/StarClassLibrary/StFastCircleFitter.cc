/***************************************************************************
 *
 * $Id: StFastCircleFitter.cc,v 1.2 2003/09/02 17:59:34 perev Exp $
 *
 * Author: Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 * Fast fitting routine using a iterational linear regression 
 * method (ILRM). Reference: N.Chernov, G.A.Ososkov, Computer  
 * Physics Communication 33 (1984) 329-333.                   
 *
 ***************************************************************************
 *
 * $Log: StFastCircleFitter.cc,v $
 * Revision 1.2  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/12/21 16:28:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFastCircleFitter.hh"
#include <math.h>

StFastCircleFitter::StFastCircleFitter()
{
    clear();
}

StFastCircleFitter::~StFastCircleFitter() {/* nop */}

void StFastCircleFitter::addPoint(double x, double y)
{
    mX.push_back(x);
    mY.push_back(y);
}

void StFastCircleFitter::clear()
{
    mX.clear();
    mY.clear();
    mRadius   = 0;     
    mXCenter  = 0;    
    mYCenter  = 0;
    mVariance = 0;
    mRC	= 0;
}

unsigned int StFastCircleFitter::numberOfPoints() const {return mX.size();}

double StFastCircleFitter::radius() const {return mRadius;}   

double StFastCircleFitter::xcenter() const {return mXCenter;}    

double StFastCircleFitter::ycenter() const {return mYCenter;}    

double StFastCircleFitter::variance() const {return mVariance;}   

int StFastCircleFitter::rc() const {return mRC;}

bool StFastCircleFitter::fit()
{
    int i;
    double xx, yy, xx2, yy2;
    double f, g, h, p, q, t, g0, g02, a, b, c, d;
    double xroot, ff, fp, xd, yd, g1;
    double dx, dy, dradius2, xnom;
    
    double xgravity = 0.0;
    double ygravity = 0.0;
    double x2 = 0.0;
    double y2 = 0.0;
    double xy = 0.0;
    double xx2y2 = 0.0;
    double yx2y2 = 0.0;
    double x2y22 = 0.0;
    double radius2 = 0.0;

    mRC = 0;
    
    int npoints = mX.size();
    
    if (npoints <= 3) {
	mRC = 1;
	return false;
    }
    for (i=0; i<npoints; i++) {
	xgravity += mX[i];
	ygravity += mY[i];
    }
    xgravity /= npoints;
    ygravity /= npoints;
    
    for (i=0; i<npoints; i++) {
	xx  = mX[i]-xgravity;
	yy  = mY[i]-ygravity;
	xx2 = xx*xx;
	yy2 = yy*yy;
	x2  += xx2;
	y2  += yy2;
	xy  += xx*yy;
	xx2y2 += xx*(xx2+yy2);
	yx2y2 += yy*(xx2+yy2);
	x2y22 += (xx2+yy2)*(xx2+yy2);
    }
    if (xy == 0.) {
	mRC = 2;
	return false;
    }
    f = (3.*x2+y2)/npoints;
    g = (x2+3.*y2)/npoints;
    h = 2*xy/npoints;
    p = xx2y2/npoints;
    q = yx2y2/npoints;
    t = x2y22/npoints;
    g0 = (x2+y2)/npoints;
    g02 = g0*g0;
    a = -4.0;
    b = (f*g-t-h*h)/g02;
    c = (t*(f+g)-2.*(p*p+q*q))/(g02*g0);
    d = (t*(h*h-f*g)+2.*(p*p*g+q*q*f)-4.*p*q*h)/(g02*g02);
    xroot = 1.0;
    for (i=0; i<5; i++) {
	ff = (((xroot+a)*xroot+b)*xroot+c)*xroot+d;
	fp = ((4.*xroot+3.*a)*xroot+2.*b)*xroot+c;
	xroot -= ff/fp;
    }
    g1 = xroot*g0;
    xnom = (g-g1)*(f-g1)-h*h;
    if (xnom == 0.) {
	mRC = 3;
	return false;
    }
    yd = (q*(f-g1)-h*p)/xnom;
    xnom = f-g1;
    if (xnom == 0.) {
	mRC = 4;
	return false;
    }
    xd = (p-h*yd )/xnom;
    
    radius2 = xd*xd+yd*yd+g1;
    mXCenter = xd+xgravity;
    mYCenter = yd+ygravity;
    
    for (i=0; i<npoints; i++) {
	dx = mX[i]-(mXCenter);
	dy = mY[i]-(mYCenter);
	dradius2 = dx*dx+dy*dy;
	mVariance += dradius2+radius2-2.*::sqrt(dradius2*radius2);
    }
    mVariance /= npoints-3.0;
    
    mRadius  = ::sqrt(radius2);
    mRC      = 0;
    
    return true;
}
