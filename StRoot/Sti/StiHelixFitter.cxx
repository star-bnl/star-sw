//StiHelixFitter
//M.L. Miller (Yale Software)
//10/01

//std
#include <iostream.h>
#include <math.h>

#include <algorithm>
using std::sort;
using std::for_each;

//SCL
#include "StThreeVectorF.hh"

//Sti
#include "StlUtilities.h"
#include "StiHit.h"
#include "StiHelixFitter.h"

ostream& operator<<(ostream& os, const StiHit&);

double g2dPathlength(double radius, double xcenter, double ycenter,
		     const StThreeVectorF& hit1, const StThreeVectorF& hit2);

StiHelixFitter::StiHelixFitter() : mCircleFitter(), mLineFitter(),
				   mOrigin(0.,0.,0.), mCenter(0.,0.,0.)
{
    cout <<"StiHelixFitter::StiHelixFitter()"<<endl;
}

StiHelixFitter::~StiHelixFitter()
{
}

bool StiHelixFitter::fit(const StiHitVector& hits)
{
    //cout <<"----------- New Track ------------- "<<endl;
    
    mValid=false;
    
    if (hits.size()<3) {
	cout <<"StiHelixFitter::refit(). ERROR:\t";
	cout <<"Less than 3 hits.  Abort"<<endl;
	return false;
    }
    
    reset();
    
    //for_each(hits.rbegin(), hits.rend(), PtrStreamer<StiHit>());

    //First, we need circle fit in local coordinates to get H right.
    /*
      for (StiHitVector::const_iterator it=hits.begin(); it!=hits.end(); ++it) {
      mCircleFitter.addPoint( (*it)->x(), (*it)->y() );
      }
      bool local_circle_rc = mCircleFitter.fit();
      if (!local_circle_rc) {
      cout <<"StiHelixFitter::fit(). ERROR:\t"
      <<"Local fit failed.  Abort"<<endl;
      mValid=false;
      return false;
      }
    */
 
    //Now calculate h, or the sign of the curvature:
    //assumes a strict less-than ordering in radius
    //calculateH(hits);
    
    //Now do global circle fit to get origin
    //Do circle fit in x-yOrder doesn't matter for circle fit
    //for_each(hits.begin(), hits.end(), mCircleInserter);
    
    for (StiHitVector::const_iterator it=hits.begin(); it!=hits.end(); ++it) {
	mCircleFitter.addPoint( (*it)->globalPosition().x(),
				(*it)->globalPosition().y() );
    }
    bool circle_rc = mCircleFitter.fit();

    mH=1.;
    if (!circle_rc) {
	cout <<"StiHelixFitter::refit(). ERROR:\t";
	cout <<"Circle Fit Failed.  abort"<<endl;
	mValid=false;
	return false;
    }

    //Do line fit in s-z, where s is the 2-d pathlength along circle
    //Order matters, must start from innermost and go out, and 

    const StThreeVectorF& firsthit = hits.back()->globalPosition();
    
    for (StiHitVector::const_reverse_iterator it=hits.rbegin();
	 it!=hits.rend(); ++it) {
	
	double s2d = g2dPathlength(mCircleFitter.radius(), mCircleFitter.xcenter(),
				   mCircleFitter.ycenter(), firsthit,
				   (*it)->globalPosition());
	
	mLineFitter.addPoint(s2d, (*it)->globalPosition().z(), 1.); //Use a dummy weight of 1
    }
        
    bool line_rc = mLineFitter.fit();
    
    if (!line_rc) {
	cout <<"StiHelixFitter::refit(). ERROR:\t";
	cout <<"Line Fit Failed.  abort"<<endl;
	mValid=false;
	return false;
    }
    
    //I'll list all of these to be explicit
    
    //double curvature = 1./mCircleFitter->radius();
    //double tanLambda = mlinefitter->slope();
    //xCenter = mCircleFitter.xCenter();
    //yCenter = mCircleFitter.yCenter();
    //z0 = mLineFitter.intercept();
    
    //Now check for potential singularities
    mValid = (circle_rc && line_rc && (mCircleFitter.radius()>=0.) );
    
    return mValid;
}

void StiHelixFitter::calculateH(const StiHitVector& hits)
{
    // We are in local coordinates, so this is easy.

    //first find the point with localX closest to xCenter
    //We do a linear search since hits.size()~10 at worst, and we usually
    //only have to go half way
    /*
      double dx=0;
      double dxMin=DBL_MAX;
      for (StiHitVector::const_iterator it=hits.begin(); it!=hits.end(); ++it) {
      dx = fabs( mCircleFitter.xcenter()
      mH = (beta>0) ? -1. : 1.;
    */
    mH = 1.;
}

double g2dPathlength(double radius, double xcenter, double ycenter,
		     const StThreeVectorF& hit1, const StThreeVectorF& hit2)
{
    double phi1 = atan2( (hit1.y()-ycenter), (hit1.x()-xcenter) );
    double phi2 = atan2( (hit2.y()-ycenter), (hit2.x()-xcenter) );
    return fabs(radius*(phi2-phi1));
}
