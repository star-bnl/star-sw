/**********************************************************
 * $Id: StRichMinimization.cxx,v 2.6 2000/11/21 16:24:22 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMinimization.cxx,v $
 *  Revision 2.6  2000/11/21 16:24:22  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
 *  Revision 2.5  2000/11/01 17:39:59  lasiuk
 *  use of SystemOfUnits for definition of degree
 *
 *  Revision 2.4  2000/10/19 18:11:09  lasiuk
 *  definition of degree
 *
 *  Revision 2.3  2000/10/19 01:13:22  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.2  2000/09/29 17:55:51  horsley
 *  fixed bug in Minimization routine, included StMagF stuff (commented out)
 *  changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.3  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichTDrawableRings to comply with sun compiler
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#include "StRichMinimization.h"

#include <unistd.h>
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <math.h>

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichMinimization::StRichMinimization(StRichRingPoint* rp)
    : ringPoint(rp),  mMeanPathInRadiator(0.), mMeanPathInQuartz(0.) {
  mTolerance = 0.00000005;  // what are the units
}

StRichMinimization::~StRichMinimization() { /* nopt */ }

StThreeVectorF StRichMinimization::rotatedMin(StThreeVectorF& point) {

  StRichTrack* t =  ringPoint->getTrack();
  double phi = t->getPhi();
  mMeanPathInRadiator = 0.0;
  mMeanPathInQuartz   = 0.0;

  // define "fast" trig functions
  double mTrackCosPhi = cos(phi);
  double mTrackSinPhi = sin(phi);

  StThreeVectorF tempPoint = point - t->getImpactPoint();

  StThreeVectorF rotatedPoint(mTrackCosPhi*tempPoint.x() + 
			      mTrackSinPhi*tempPoint.y(),
			      
			     -mTrackSinPhi*tempPoint.x() + 
			      mTrackCosPhi*tempPoint.y(),
			      
			      0.0);  
  
  ringPoint->setPoint(rotatedPoint);

  //
  // call to Numerical Recipes minimization routine here
  //
  double returnPsia,returnPsib,returnPsic;
  double minDistancea = brent(0.0,M_PI/2.0,M_PI,&returnPsia);
  double minDistanceb = brent(0.0,-M_PI/2.0,-M_PI,&returnPsib);
  double minDistancec = brent(0.0,M_PI,2.0*M_PI,&returnPsic);

  if (minDistancea<minDistanceb) {
    minDistance = minDistancea;
    returnPsi = returnPsia;
  }
  else {
    minDistance = minDistanceb;
    returnPsi = returnPsib;
  }
  
  
  if (minDistancec<minDistance) {
    minDistance = minDistancec;
    returnPsi = returnPsic;
    if (returnPsi>M_PI) returnPsi = returnPsi - 2.0*M_PI; 
  }
  
 
 status              = ringPoint->getPoint(returnPsi,returnThisPoint);
 mMeanPathInRadiator = ringPoint->getMeanPathInRadiator();
 mMeanPathInQuartz   = ringPoint->getMeanPathInQuartz();
 
 return returnThisPoint;
}


double StRichMinimization::getMeanPathInRadiator() { return mMeanPathInRadiator;}
double StRichMinimization::getMeanPathInQuartz()   { return mMeanPathInQuartz;}

//
// this is the numerical recipes minimization routine brent
//

/* CAUTION: This is the ANSI C (only) version of the Numerical Recipes
   utility file nrutil.c.  Do not confuse this file with the same-named
   file nrutil.c that is supplied in the same subdirectory or archive
   as the header file nrutil.h.  *That* file contains both ANSI and
   traditional K&R versions, along with #ifdef macros to select the
   correct version.  *This* file contains only ANSI C.               */


#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))
#define ITMAX 200
#define CGOLD 0.3819660
#define ZEPS  1.0e-10
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);


double StRichMinimization::brent(double ax, double bx, double cx,double *xmin)
{
    int iter;
    double tol = mTolerance;
    double a,b,d,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
    double e=0.0;
    d=0.0;
   
    a=(ax < cx ? ax : cx);
    b=(ax > cx ? ax : cx);
    x=w=v=bx;
    fw=fv=fx=ringPoint->rotatedFunction(x);
	
	for (iter=1;iter<=ITMAX;iter++) {
	        xm=0.5*(a+b);
		tol2=2.0*(tol1=tol*fabs(x)+ZEPS);
		if (fabs(x-xm) <= (tol2-0.5*(b-a))) {
		        *xmin=x;
			return fx;
		}
		if (fabs(e) > tol1) {
			r=(x-w)*(fx-fv);
			q=(x-v)*(fx-fw);
			p=(x-v)*q-(x-w)*r;
			q=2.0*(q-r);
			if (q > 0.0) p = -p;
			q=fabs(q);
			etemp=e;
			e=d;
			if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x))
				d=CGOLD*(e=(x >= xm ? a-x : b-x));
			else {
				d=p/q;
				u=x+d;
				if (u-a < tol2 || b-u < tol2)
					d=SIGN(tol1,xm-x);
			}
		} else {
			d=CGOLD*(e=(x >= xm ? a-x : b-x));
		}
		u=(fabs(d) >= tol1 ? x+d : x+SIGN(tol1,d));
		fu=ringPoint->rotatedFunction(u);
	     
		if (fu <= fx) {
			if (u >= x) a=x; else b=x;
			SHFT(v,w,x,u)
			SHFT(fv,fw,fx,fu)
		} else {
			if (u < x) a=u; else b=u;
			if (fu <= fw || w == x) {
				v=w;
				w=u;
				fv=fw;
				fw=fu;
			} else if (fu <= fv || v == x || v == w) {
				v=u;
				fv=fu;
			}
		}
	}
	
	*xmin=x;
	return fx;
}



////////////////////////////////////////////

#undef ITMAX
#undef CGOLD
#undef ZEPS
#undef SHFT
/* (C) Copr. 1986-92 Numerical Recipes Software &1245.@1. */




















