/**********************************************************
 * $Id: StRichMinimization.cxx,v 1.2 2000/05/19 19:06:10 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMinimization.cxx,v $
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
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
#include "StRichTrack.h"

#include <unistd.h>
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <math.h>
#include <stddef.h>
#include <stdlib.h>


StRichMinimization::StRichMinimization(StRichRingPoint* rp) {
  ringPoint  = rp;

  // should use system of units
StThreeVector<double> StRichMinimization::rotatedMin(StThreeVector<double>& point) {

StRichMinimization::~StRichMinimization() { /* nopt */ }
  

StThreeVectorF StRichMinimization::rotatedMin(StThreeVectorF& point) {

  StRichTrack* t =  ringPoint->getTrack();
  StThreeVector<double> tempPoint = point - t->getImpactPoint();
  mMeanPathInRadiator = 0.0;
  StThreeVector<double> rotatedPoint(mTrackCosPhi*tempPoint.x() + 
				     mTrackSinPhi*tempPoint.y(),
				     
				    -mTrackSinPhi*tempPoint.x() + 
				     mTrackCosPhi*tempPoint.y(),
				     
				     0.0);  
  StThreeVectorF tempPoint = point - t->getImpactPoint();


  if (rotatedPoint.y() > 0) {
    minDistance = brent(0.0,M_PI/2.0,M_PI,&returnPsi);}

  else {
    minDistance = brent(0.0,-M_PI/2.0,-M_PI,&returnPsi);}
    
  status = ringPoint->getPoint(returnPsi,returnThisPoint);
  return returnThisPoint;
 //
 if (returnPsi>M_PI)  {returnPsi = returnPsi - 2.0*M_PI;}
 mMeanPathInRadiator = ringPoint->getMeanPathInRadiator();
}


double StRichMinimization::getMeanPathInRadiator() { return mMeanPathInRadiator;}
double StRichMinimization::getMeanPathInQuartz()   { return mMeanPathInQuartz;}

//
// this is the numerical recipes minimization routine brent
void NEWnrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
	fprintf(stderr,"Numerical Recipes run-time error...\n");
	fprintf(stderr,"%s\n",error_text);
	fprintf(stderr,"...now exiting to system...\n");
	exit(1);
}

 

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
	NEWnrerror("Too many iterations in brent");
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




















