//StiLocalTrackSeedFinder.cxx M.L. Miller (Yale Software) 10/01

//std
#include <math.h>

//scl
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StThreeVector.hh"

//Sti
#include "StiIOBroker.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiLocalTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiDetector&);

StiLocalTrackSeedFinder::StiLocalTrackSeedFinder(StiDetectorContainer* det,
						 StiHitContainer* hits)
    : StiTrackSeedFinder(det, hits), mSubject(StiIOBroker::instance()), mDoHelixFit(false)
{
    mMessenger <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder()"<<endl;
    mSubject->attach(this);
    getNewState();
}

StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()
{
    mMessenger <<"StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()"<<endl;
    if (mSubject) {
	mSubject->detach(this);
    }
}

bool StiLocalTrackSeedFinder::hasMore()
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::hasMore()"<<endl;
#endif
    
    bool val = (mCurrentDet>=mDetVec.begin() && mCurrentDet<mDetVec.end())
	&& (mCurrentHit>=mHitsBegin && mCurrentHit<mHitsEnd );

#ifdef DEBUG
    mMessenger <<"\t returning:\t"<<val<<endl;
#endif
    
    return val;
}

void StiLocalTrackSeedFinder::reset()
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::reset()"<<endl;
#endif

    //Set to first detector
    mCurrentDet = mDetVec.begin();
    mCurrentRadius = (*mCurrentDet)->getPlacement()->getCenterRadius();
    
    //Set to hits for detector
    initHitVec();
    
    //Cleanup the base-class
    StiTrackSeedFinder::reset();

#ifdef DEBUG
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::reset()"<<endl;
#endif
}

void StiLocalTrackSeedFinder::triggerPartition()
{
    double newRadius = (*mCurrentDet)->getPlacement()->getCenterRadius();
    // cout << "StiLocalTrackSeedFinder::triggerPartition()"<<endl;
    if (newRadius!=mCurrentRadius) {
	cout <<"Partition Triggered. "
	     <<"OldRadius: "<<mCurrentRadius<<"\tNewRadius: "<<newRadius<<endl;
	mCurrentRadius=newRadius;
	mHitStore->partitionUsedHits();
	cout <<"\tdone"<<endl;
    }
}

void StiLocalTrackSeedFinder::initHitVec()
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::initHitVec()"<<endl;
#endif

    bool go=true;
    while (mCurrentDet<mDetVec.end() && go) {

#ifdef DEBUG
	mMessenger <<"\tCurrent detector: "<<**mCurrentDet<<endl;
#endif
	mHitsBegin = mHitStore->hitsBegin(*mCurrentDet);
	mHitsEnd = mHitStore->hitsEnd(*mCurrentDet);

	if (mHitsBegin<mHitsEnd) {
	    go=false;
	    mCurrentHit = mHitsBegin;
#ifdef DEBUG
	    mMessenger <<"\tStopping on this detector"<<endl;
#endif
	}
	else {
	    
#ifdef DEBUG
	    mMessenger <<"\tGo to next detector"<<endl;
#endif
	    ++mCurrentDet;
	    //triggerPartition();
	}
    }
#ifdef DEBUG
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::initHitVec()"<<endl;
#endif
}

void StiLocalTrackSeedFinder::increment()
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::increment()"<<endl;
#endif
    //Check for hit increment:
    if ( mCurrentHit<mHitsEnd ) {
	
#ifdef DEBUG
	mMessenger <<"\tincrementing mCurrentHit"<<endl;
#endif
	++mCurrentHit;
    }
    
    //Check to see if we went past the end
    if ( mCurrentHit<mHitsEnd==false) {
#ifdef DEBUG
	mMessenger <<"\tincrementing mCurrentDet"<<endl;
#endif
	
	++mCurrentDet;
	//triggerPartition();

#ifdef DEBUG
	mMessenger <<"\tcalling initHitVec()"<<endl;
#endif
	initHitVec();
    }
#ifdef DEBUG
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::increment()"<<endl;
#endif
    
}

StiKalmanTrack* StiLocalTrackSeedFinder::next()
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::next()"<<endl;
    mMessenger <<"\t Using hits from: "<<**mCurrentDet<<endl;
#endif
    StiKalmanTrack* track = 0;

    while (hasMore() && track==0) {
	//For now, do one hit at a time, regardless of validity
	if ( (*mCurrentHit)->isUsed() == false ) {
	    track =makeTrack(*mCurrentHit);
	}
	increment();
    }
#ifdef DEBUG
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::next()"<<endl;
#endif
    return track;
}

bool StiLocalTrackSeedFinder::extendHit(StiHit* hit)
{
    //Now look for a hit in the next layer in:
    mDetStore->setToDetector( hit->detector() );
    
    //Test to see if move in worked
    if ( mDetStore->moveIn()==false ) {
#ifdef DEBUG
	mMessenger<<"StiLocalTrackSeedFinder::makeTrack(StiHit* hit). ERROR:\t"
		  <<"Nowhere to move in to.  Abort"<<endl;
#endif
	return false;
    }
    
    const StiDetector* newLayer = **mDetStore;
#ifdef DEBUG
    mMessenger <<"query hit container for extension hits"<<endl;
#endif
    //Now get hits:
    mHitStore->setDeltaD(mDeltaY);
    mHitStore->setDeltaZ(mDeltaZ);
    mHitStore->setRefPoint(newLayer->getPlacement()->getCenterRadius(),
			   newLayer->getPlacement()->getCenterRefAngle(),
			   hit->y(), hit->z());
    
    //Loop on hits, find closest in z:
    //This too should be replaced by an algorithm call which can be inlined.
    //Add it to the tbd list!
    
    unsigned int nhits=0;
    StiHit* closestHit = 0;
    double dz = DBL_MAX;
    
    while (mHitStore->hasMore()) {
	++nhits;
	StiHit* theHit = mHitStore->getHit();//Get hit and increment
	if (theHit->isUsed()==false) {
	    double theDeltaZ = fabs( theHit->z()-hit->z() );
	    if ( theDeltaZ < dz ) {
		closestHit = theHit;
		dz = theDeltaZ;
	    }
	}
    }
    
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder.  Found "<<nhits<<" Candidate hits"<<endl;
#endif
    //Check if we satisfied the search:
    if ( !closestHit ) {
#ifdef DEBUG
	mMessenger<<"StiLocalTrackSeedFinder::makeTrack(StiHit* hit). ERROR:\t"
		  <<"No hits found in next layer.  Abort"<<endl;
#endif
	return false;
    }

    mSeedHitVec.push_back(closestHit);
    return true;
}

StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(StiHit* hit)
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::makeTrack()"<<endl;
#endif
    StiKalmanTrack* track = 0;
    mSeedHitVec.clear();
    mSeedHitVec.push_back(hit);

    //Recursively extend track:
    bool go=true;
    while ( go && mSeedHitVec.size()<mSeedLength) {
	go = extendHit( mSeedHitVec.back() );
    }

    //Check to see if we failed
    if ( mSeedHitVec.size()<mSeedLength ) {
#ifdef DEBUG
	mMessenger <<"StiLocalTrackSeedFidnder::makeTrack(). ERROR:\t"
		   <<"Hit extension failed"<<endl;
#endif
	return track;
    }

    //now use straight line propogation to recursively extend
    mSkipped = 0;
    go=true;
    while ( go && mSkipped<=mMaxSkipped && mSeedHitVec.size()<=mSeedLength+mExtrapMaxLength) {
	go = extrapolate();
    }
    
    //Check to see if we failed
    if ( mSeedHitVec.size()<=mSeedLength+mExtrapMinLength) {
	return track;
    }
    
    //else, seed the track:
    if (!mFactory) {
	cout <<"StiLocalTrackSeedFidnder::makeTrack(). ERROR:\t"
	     <<"Factory is null pointer!"<<endl;
    }
    
    track = mFactory->getObject();
    track->reset();
    
    initializeTrack(track);

#ifdef DEBUG
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::makeTrack()"<<endl;
#endif
    
    return track;
}

/* We define the following picture
   r
   ^
   |
   |
   ----> y or z, depending on projection

   -------------- x -------------- pt3


   ------------------- x --------- pt2
   \
   \   
   ---------------------- x ------ pt1

   We try to extrapolate the segment (pt1->pt2) to predict pt3.  We
   do this in two projections (r,y) and (r,z) where y is the distance
   along pad, z is the global z, and r is the inward pointing sector
   normal (i.e., StiPlacement->getCenterRadius() )

   In the r,y plane, e.g., we define r = m*y + b s.t.
   m = (r2-r1) / (y2-y1)
   b = r2 - m2
   then
   y3 = (r3 - b) / m, which is our prediction, since we know r3
   
*/
bool StiLocalTrackSeedFinder::extrapolate()
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::extrapolate()"<<endl;
#endif
    //Calculate slope and offset in r-z and r-y projections, then extend
    const StiHit* hit1 = *( mSeedHitVec.begin()+mSeedHitVec.size() -2 ); //second to last
    const StiHit* hit2 = mSeedHitVec.back();

    //Get the next detector plane:
    
    double dr = hit2->x()-hit1->x();
    double dy = hit2->y()-hit1->y();
    double dz = hit2->z()-hit1->z();
    if (dr==0. || dy==0. || dz==0.) {
	cout <<"StiLocalTrackSeedFinder::extrapolate(). Error:\t"
	     <<"dr==0. || dy==0 || dz==0.  Abort seed"<<endl;
	return false;
    }

    //Now look for a hit in the next layer in:
    mDetStore->setToDetector( hit2->detector() );
    //Test to see if move in worked
    for (unsigned int i=0; i<=mSkipped; ++i) {
	if ( mDetStore->moveIn()==false ) {
#ifdef DEBUG
	    mMessenger<<"StiLocalTrackSeedFinder::extrapolate(). ERROR:\t"
		      <<"Nowhere to move in to.  Abort"<<endl;
#endif
	    return false;
	}
    }

    const StiDetector* newLayer = **mDetStore;
    double r3 = newLayer->getPlacement()->getCenterRadius();
    
    //First, r-y plane
    double m_ry = dr/dy;
    double b_ry = hit2->x() - m_ry * hit2->y();
    double y3 = (r3 - b_ry) / m_ry;

    //Now calculate the projection of window onto that plane:
    double beta_ry = atan2(dr, dy);
    double rho_ry = sqrt(dr*dr + dy*dy);
    double alpha_ry = atan2(mExtrapDeltaY, 2.*rho_ry);
    double tanplus_ry = tan(beta_ry+alpha_ry);
    double tanminus_ry = tan(beta_ry-alpha_ry);
    if (tanplus_ry==0. || tanminus_ry==0.) {
#ifdef DEBUG
	mMessenger<<"StiLocalTrackSeedFidner::extrapolate(). ERROR:\t"
		  <<"tanplus_ry==0. || tanminus_ry==0."<<endl;
#endif
    }
    double y3_minus = (r3-hit1->x())/tanplus_ry + hit1->y();
    double y3_plus = (r3-hit1->x())/tanminus_ry + hit1->y();
    
    //Next, r-z plane
    double m_rz = dr/dz;
    double b_rz = hit2->x() - m_rz * hit2->z();
    double z3 = (r3 - b_rz) / m_rz;

    double beta_rz = atan2(dr, dz);
    double rho_rz = sqrt(dr*dr + dz*dz);
    double alpha_rz = atan2(mExtrapDeltaZ, 2.*rho_rz);
    double tanplus_rz = tan(beta_rz+alpha_rz);
    double tanminus_rz = tan(beta_rz-alpha_rz);
    if (tanplus_rz==0. || tanminus_rz==0.) {
#ifdef DEBUG
	mMessenger<<"StiLocalTrackSeedFidner::extrapolate(). ERROR:\t"
		  <<"tanplus_rz==0. || tanminus_rz==0."<<endl;
#endif
    }
    double z3_minus = (r3-hit1->x())/tanplus_rz + hit1->z();
    double z3_plus = (r3-hit1->x())/tanminus_rz + hit1->z();

#ifdef DEBUG
    mMessenger<<"beta_ry: "<<beta_ry<<" alpha_ry: "<<alpha_ry
	      <<" y3+: "<<y3_plus<<" y3: "<<y3<<" y3-: "<<y3_minus<<endl;
    mMessenger<<"beta_rz: "<<beta_rz<<" alpha_rz: "<<alpha_rz
	      <<" z3+: "<<z3_plus<<" z3: "<<z3<<" z3-: "<<z3_minus<<endl;
    
#endif
#ifdef DEBUG
    mMessenger <<"query hit container for extension hits"<<endl;
#endif
    
    //Now get hits:
    mHitStore->setDeltaD( fabs(y3_plus-y3_minus) /2.);
    mHitStore->setDeltaZ( fabs(z3_plus-z3_minus) /2. );
    mHitStore->setRefPoint(r3, newLayer->getPlacement()->getCenterRefAngle(),
			   y3, z3);
    
    //Loop on hits, find closest to z3 prediction:
    //This too should be replaced by an algorithm call which can be inlined.
    //Add it to the tbd list!
    
    unsigned int nhits=0;
    StiHit* closestHit = 0;
    dz = DBL_MAX;
    
    while (mHitStore->hasMore()) {
	++nhits;
	StiHit* theHit = mHitStore->getHit(); //Get hit and increment
	if (theHit->isUsed()==false) {
	    double theDeltaZ = fabs( theHit->z() - z3 );
	    if ( theDeltaZ < dz ) {
		closestHit = theHit;
		dz = theDeltaZ;
	    }
	}
    }

#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder.  Found "<<nhits<<" Candidate hits"<<endl;
#endif
    //Check if we satisfied the search:
    if ( !closestHit ) {
#ifdef DEBUG
	mMessenger<<"StiLocalTrackSeedFinder::extrapolate(extrapolate). ERROR:\t"
		  <<"No hits found in next layer.  Abort"<<endl;
#endif
	++mSkipped;
	return true;
    }

    //else
    mSkipped=0;
    mSeedHitVec.push_back(closestHit);
    return true;
}

void StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack* track)
{
#ifdef DEBUG
    mMessenger <<"StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack*)"<<endl;
#endif

    if (mDoHelixFit) {
	if (mSeedHitVec.size()>=3) { //if false, continue on to calculate
	    fit(track);
	    return;
	}
    }

    if (mUseOrigin==false && mSeedHitVec.size()>=3) { 
	calculate(track);
    }
    else { //No choice!, too few points to exclude vertex assumption
	calculateWithOrigin(track);
    }

#ifdef DEBUG
    mMessenger<<"done."<<endl;
#endif

}

void StiLocalTrackSeedFinder::fit(StiKalmanTrack* track)
{
#ifdef DEBUG
    mMessenger<<"StiLocalTrackSeedFinder::fit(StiKalmanTrack*)"<<endl;
#endif

    mHelixFitter.reset();
    mHelixFitter.fit( mSeedHitVec );
    
    if (mHelixFitter.valid()==false ) {
#ifdef DEBUG
	mMessenger<<"StiLocalTrackSeedFinder::fit(StiKalmanTrack*). ERROR:\t"
		  <<"Helix Fit failed.  abort"<<endl;
#endif
	return;
    }

#ifdef DEBUG
    mMessenger <<"origin: "<<mHelixFitter.xCenter()<<" "<<mHelixFitter.yCenter()<<" "
	       <<mHelixFitter.z0()<<" "
	       <<" curvature: "<<mHelixFitter.curvature()<<" "
	       <<" tanLambda: "<<mHelixFitter.tanLambda()<<endl;
    mMessenger<<"\tInitialzie Track:\t";
#endif
    
    track->initialize( mHelixFitter.curvature(), mHelixFitter.tanLambda(),
		       StThreeVectorD(mHelixFitter.xCenter(), mHelixFitter.yCenter(), 0.),
		       mSeedHitVec);    
}

void StiLocalTrackSeedFinder::calculate(StiKalmanTrack* track)
{
#ifdef DEBUG
    mMessenger<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*)"<<endl;
#endif

    const StThreeVectorF& outside = mSeedHitVec.front()->globalPosition();
    const StThreeVectorF& middle = (*(mSeedHitVec.begin()+mSeedHitVec.size()/2))->globalPosition();
    const StThreeVectorF& inside = mSeedHitVec.back()->globalPosition();

#ifdef DEBUG
    mMessenger<<"\tCalculate circle parameters:\t";
#endif
    mHelixCalculator.calculate( StThreeVector<double>( inside.x(), inside.y(), inside.z() ),
				StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
				StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );

#ifdef DEBUG
    mMessenger<<"\tdone."<<endl;
    mMessenger <<"origin: "<<mHelixCalculator.xCenter()<<" "<<mHelixCalculator.yCenter()<<" "
	       <<mHelixCalculator.z0()<<" "
	       <<" curvature: "<<mHelixCalculator.curvature()<<" "
	       <<" tanLambda: "<<mHelixCalculator.tanLambda()<<endl;

    mMessenger<<"\tInitialzie Track:\t";
#endif
    track->initialize( mHelixCalculator.curvature(), mHelixCalculator.tanLambda(),
		       StThreeVectorD(mHelixCalculator.xCenter(), mHelixCalculator.yCenter(), 0.),
		       mSeedHitVec);    
}

void StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack* track)
{
#ifdef DEBUG
    mMessenger<<"StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack*)"<<endl;
#endif
    const StThreeVectorF& outside = mSeedHitVec.front()->globalPosition();
    const StThreeVectorF& middle = mSeedHitVec.back()->globalPosition();

#ifdef DEBUG
    mMessenger<<"\tCalculate circle parameters:\t";
#endif
    mHelixCalculator.calculate( StThreeVector<double>(0., 0., 0.),
				StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
				StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );

#ifdef DEBUG
    mMessenger<<"\tdone."<<endl;
    mMessenger <<"origin: "<<mHelixCalculator.xCenter()<<" "<<mHelixCalculator.yCenter()<<" "
	       <<mHelixCalculator.z0()<<" "
	       <<" curvature: "<<mHelixCalculator.curvature()<<" "
	       <<" tanLambda: "<<mHelixCalculator.tanLambda()<<endl;

    mMessenger<<"\tInitialzie Track:\t";
#endif
    track->initialize( mHelixCalculator.curvature(), mHelixCalculator.tanLambda(),
		       StThreeVectorD(mHelixCalculator.xCenter(), mHelixCalculator.yCenter(), 0.),
		       mSeedHitVec);    
}

void StiLocalTrackSeedFinder::getNewState()
{
    const StiIOBroker* broker = StiIOBroker::instance();
    mDeltaY = broker->ltsfYWindow();
    mDeltaZ = broker->ltsfZWindow();
    mSeedLength = broker->ltsfSeedLength();
    mExtrapDeltaY = broker->ltsfExtrapYWindow();
    mExtrapDeltaZ = broker->ltsfExtrapZWindow();
    mMaxSkipped = broker->ltsfExtrapMaxSkipped();
    mExtrapMinLength = broker->ltsfExtrapMinLength();
    mExtrapMaxLength = broker->ltsfExtrapMaxLength();
    mUseOrigin = broker->ltsfUseVertex();
    mDoHelixFit = broker->ltsfDoHelixFit();
}

void StiLocalTrackSeedFinder::addLayer(StiDetector* det)
{
    mDetVec.push_back(det);
    sort(mDetVec.begin(), mDetVec.end(), RPhiLessThan());
}


//Non members

//sort in descending order in radius, and ascending order in phi
bool RPhiLessThan::operator()(const StiDetector* lhs, const StiDetector* rhs)
{
    StiPlacement* lhsp = lhs->getPlacement();
    StiPlacement* rhsp = rhs->getPlacement();
    
    if (lhsp->getCenterRadius()<rhsp->getCenterRadius())
	return false;
    else if (lhsp->getCenterRadius()>rhsp->getCenterRadius()) 
	return true;
    else
	return (lhsp->getCenterRefAngle()<rhsp->getCenterRefAngle());
}

void StiLocalTrackSeedFinder::print() const
{
    cout <<"StiLocalTrackSeedFinder Detectors:\n";
    
    for (vector<StiDetector*>::const_iterator it=mDetVec.begin(); it!=mDetVec.end(); ++it) {
	cout << **it <<endl;
    }
    cout <<"\n Search Window in Y:\t"<<mDeltaY<<endl;
    cout <<"\n Search Window in Z:\t"<<mDeltaZ<<endl;
}
