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

StiHelixFitter::StiHelixFitter() : mCircleFitter(), mLineFitter()
{
    cout <<"StiHelixFitter::StiHelixFitter()"<<endl;
}

StiHelixFitter::~StiHelixFitter()
{
}

bool StiHelixFitter::fit(const StiHitVector& hits)
{
    //cout <<"----------- New Track ------------- "<<endl;
    
    if (hits.size()<3) {
	cout <<"StiHelixFitter::refit(). ERROR:\t";
	cout <<"Less than 3 hits.  Abort"<<endl;
	mValid=false;
	return false;
    }
    
    reset();

    //for_each(hits.rbegin(), hits.rend(), PtrStreamer<StiHit>());

    //Now do global circle fit to get origin
    //Do circle fit in x-yOrder doesn't matter for circle fit
    
    for (StiHitVector::const_iterator it=hits.begin(); it!=hits.end(); ++it) {
	mCircleFitter.addPoint( (*it)->globalPosition().x(),
				(*it)->globalPosition().y() );
    }
    bool circle_rc = mCircleFitter.fit();

    if (!circle_rc) {
	cout <<"StiHelixFitter::refit(). ERROR:\t";
	cout <<"Circle Fit Failed.  abort"<<endl;
	mValid=false;
	return false;
    }

    //Now calculate h, or the sign of the curvature:
    //assumes a strict less-than ordering in radius
    if (calculateH(hits) == false ) {
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

bool StiHelixFitter::calculateH(const StiHitVector& hits)
{
    //To speed this up, I should be able to cache 4 3-vectors and manipulate instead of cnstr'ing
    
    mCenter.setX( mCircleFitter.xcenter() );
    mCenter.setY( mCircleFitter.ycenter() );
    mCenter.setZ(0.);
    
    mInside = hits.back()->globalPosition();
    mInside.setZ(0.);
    mOutside = hits.front()->globalPosition();
    mOutside.setZ(0.);
    mMiddle = (*(hits.begin()+hits.size()/2))->globalPosition();
    mMiddle.setZ(0.);

    //cout <<"mInside: "<<mInside<<" mMiddle: "<<mMiddle<<" mOutside: "<<mOutside<<" mCenter: "<<mCenter<<endl;
    
    //Now shift to the frame that has 'mInside' as the origin
    mOutside = mOutside-mInside;
    mCenter = mCenter - mInside;
    mMiddle = mMiddle-mInside;
    mInside = mInside-mInside;
    
    //cout <<"\n Shift origin to mInside point"<<endl;
    //cout <<"mInside: "<<mInside<<" mMiddle: "<<mMiddle<<" mOutside: "<<mOutside<<" mCenter: "<<mCenter<<endl;
    
    //Now rotate to the frame defined by the angle between the mInside and mOutside points
    if ( (mOutside.y()-mInside.y())==0. || (mOutside.x()-mInside.x())==0.) {
	cout <<"StiHelixFitter::calculateH(). ERROR:\t"
	     <<"(mOutside.y()-mInside.y())==0. || (mOutside.x()-mInside.x())==0.).  Abort"<<endl;
	return false;
    }
    
    double alpha = atan2(mOutside.y()-mInside.y(), mOutside.x()-mInside.x() );
    double beta = M_PI/2.-alpha;
    
    //cout <<"\nalpha: "<<alpha<<"\tRotate by beta: "<<beta<<endl;
    //mInside.rotateZ(beta); //Probably don't have to do this one, but I just wanna check
    //mOutside.rotateZ(beta);// ditto
    mMiddle.rotateZ(beta);
    mCenter.rotateZ(beta);
    //cout <<"mInside: "<<mInside<<" mMiddle: "<<mMiddle<<" mOutside: "<<mOutside<<" mCenter: "<<mCenter<<endl;

    //Now decide whether the mCenter is to the left or right to the mMiddle point.
    double d = mMiddle.x() - mCenter.x();
    // d>0 -> right handed when viewed from above in transverse plane -> h=1
    // d<0 -> left handed when viewd from above in transverse plane -> h=-1
    mH = (d>0.) ? 1. : -1.;
    //cout <<"H: \t"<<mH<<endl;
    return true;
}

double g2dPathlength(double radius, double xcenter, double ycenter,
		     const StThreeVectorF& hit1, const StThreeVectorF& hit2)
{
    double phi1 = atan2( (hit1.y()-ycenter), (hit1.x()-xcenter) );
    double phi2 = atan2( (hit2.y()-ycenter), (hit2.x()-xcenter) );
    return fabs(radius*(phi2-phi1));
}
