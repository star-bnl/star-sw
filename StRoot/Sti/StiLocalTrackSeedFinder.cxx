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
    mMessenger <<"StiLocalTrackSeedFinder::hasMore()"<<endl;
    
    bool val = (mCurrentDet>=mDetVec.begin() && mCurrentDet<mDetVec.end())
	&& (mCurrentHit>=mHitsBegin && mCurrentHit<mHitsEnd );
    
    mMessenger <<"\t returning:\t"<<val<<endl;

    return val;
}

void StiLocalTrackSeedFinder::reset()
{
    mMessenger <<"StiLocalTrackSeedFinder::reset()"<<endl;

    //Set to first detector
    mCurrentDet = mDetVec.begin();
    mCurrentRadius = (*mCurrentDet)->getPlacement()->getCenterRadius();
    
    //Set to hits for detector
    initHitVec();
    
    //Cleanup the base-class
    StiTrackSeedFinder::reset();

    mMessenger <<"\t leaving StiLocalTrackSeedFinder::reset()"<<endl;
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
    mMessenger <<"StiLocalTrackSeedFinder::initHitVec()"<<endl;

    bool go=true;
    while (mCurrentDet<mDetVec.end() && go) {
	
	mMessenger <<"\tCurrent detector: "<<**mCurrentDet<<endl;
	mHitsBegin = mHitStore->hitsBegin(*mCurrentDet);
	mHitsEnd = mHitStore->hitsEnd(*mCurrentDet);

	if (mHitsBegin<mHitsEnd) {
	    go=false;
	    mCurrentHit = mHitsBegin;
	    mMessenger <<"\tStopping on this detector"<<endl;
	}
	else {
	    mMessenger <<"\tGo to next detector"<<endl;
	    ++mCurrentDet;
	    //triggerPartition();
	}
    }
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::initHitVec()"<<endl;
}

void StiLocalTrackSeedFinder::increment()
{
    mMessenger <<"StiLocalTrackSeedFinder::increment()"<<endl;
    //Check for hit increment:
    if ( mCurrentHit<mHitsEnd ) {
	mMessenger <<"\tincrementing mCurrentHit"<<endl;
	++mCurrentHit;
    }
    
    //Check to see if we went past the end
    if ( mCurrentHit<mHitsEnd==false) {
	mMessenger <<"\tincrementing mCurrentDet"<<endl;
	
	++mCurrentDet;
	//triggerPartition();
	
	mMessenger <<"\tcalling initHitVec()"<<endl;
	initHitVec();
    }
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::increment()"<<endl;
    
}

StiKalmanTrack* StiLocalTrackSeedFinder::next()
{
    mMessenger <<"StiLocalTrackSeedFinder::next()"<<endl;
    mMessenger <<"\t Using hits from: "<<**mCurrentDet<<endl;
    StiKalmanTrack* track = 0;

    while (hasMore() && track==0) {
	//For now, do one hit at a time, regardless of validity
	if ( (*mCurrentHit)->isUsed() == false ) {
	    track =makeTrack(*mCurrentHit);
	}
	increment();
    }   
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::next()"<<endl;
    return track;
}

bool StiLocalTrackSeedFinder::extendHit(StiHit* hit)
{
    //Now look for a hit in the next layer in:
    mDetStore->setToDetector( hit->detector() );
    
    //Test to see if move in worked
    if ( mDetStore->moveIn()==false ) {
	mMessenger<<"StiLocalTrackSeedFinder::makeTrack(StiHit* hit). ERROR:\t"
		  <<"Nowhere to move in to.  Abort"<<endl;
	return false;
    }
    
    const StiDetector* newLayer = **mDetStore;
    mMessenger <<"query hit container for extension hits"<<endl;
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
	StiHit* theHit = mHitStore->getHit(); //Get hit and increment
	double theDeltaZ = fabs( theHit->z()-hit->z() );
	if ( theDeltaZ < dz ) {
	    closestHit = theHit;
	    dz = theDeltaZ;
	}
    }

    mMessenger <<"StiLocalTrackSeedFinder.  Found "<<nhits<<" Candidate hits"<<endl;
    //Check if we satisfied the search:
    if ( !closestHit ) {
	mMessenger<<"StiLocalTrackSeedFinder::makeTrack(StiHit* hit). ERROR:\t"
		  <<"No hits found in next layer.  Abort"<<endl;
	return false;
    }

    mSeedHitVec.push_back(closestHit);
    return true;
}

StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(StiHit* hit)
{
    mMessenger <<"StiLocalTrackSeedFinder::makeTrack()"<<endl;
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
	mMessenger <<"StiLocalTrackSeedFidnder::makeTrack(). ERROR:\t"
		   <<"Hit extension failed"<<endl;
	return track;
    }

    //now use straight line propogation to recursively extend
    mSkipped = 0;
    go=true;
    while ( go && mSkipped<=mMaxSkipped && mSeedHitVec.size()<mSeedLength+mExtrapLength) {
	go = extrapolate();
    }
    
    //Check to see if we failed
    if ( mSeedHitVec.size()<mExtrapLength) {
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
    
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::makeTrack()"<<endl;
    
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
    mMessenger <<"StiLocalTrackSeedFinder::extrapolate()"<<endl;
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
	    mMessenger<<"StiLocalTrackSeedFinder::extrapolate(). ERROR:\t"
		      <<"Nowhere to move in to.  Abort"<<endl;
	    return false;
	}
    }

    const StiDetector* newLayer = **mDetStore;
    double r3 = newLayer->getPlacement()->getCenterRadius();
    
    //First, r-y plane
    double m_ry = dr/dy;
    double b_ry = hit2->x() - m_ry * hit2->y();
    double y3 = (r3 - b_ry) / m_ry;
    
    //Next, r-z plane
    double m_rz = dr/dz;
    double b_rz = hit2->x() - m_rz * hit2->z();
    double z3 = (r3 - b_rz) / m_rz;
    
    mMessenger <<"query hit container for extension hits"<<endl;

    //Now get hits:
    mHitStore->setDeltaD(mExtrapDeltaY);
    mHitStore->setDeltaZ(mExtrapDeltaZ);
    mHitStore->setRefPoint(r3,
			   newLayer->getPlacement()->getCenterRefAngle(),
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
	double theDeltaZ = fabs( theHit->z() - z3 );
	if ( theDeltaZ < dz ) {
	    closestHit = theHit;
	    dz = theDeltaZ;
	}
    }

    mMessenger <<"StiLocalTrackSeedFinder.  Found "<<nhits<<" Candidate hits"<<endl;
    //Check if we satisfied the search:
    if ( !closestHit ) {
	mMessenger<<"StiLocalTrackSeedFinder::extrapolate(extrapolate). ERROR:\t"
		  <<"No hits found in next layer.  Abort"<<endl;
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
    mMessenger <<"StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack*)"<<endl;

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

    mMessenger<<"done."<<endl;

}

void StiLocalTrackSeedFinder::fit(StiKalmanTrack* track)
{
    mMessenger<<"StiLocalTrackSeedFinder::fit(StiKalmanTrack*)"<<endl;

    mHelixFitter.reset();
    mHelixFitter.fit( mSeedHitVec );
    
    if (mHelixFitter.valid()==false ) {
	mMessenger<<"StiLocalTrackSeedFinder::fit(StiKalmanTrack*). ERROR:\t"
		  <<"Helix Fit failed.  abort"<<endl;
	return;
    }
    
    mMessenger <<"origin: "<<mHelixFitter.xCenter()<<" "<<mHelixFitter.yCenter()<<" "
	       <<mHelixFitter.z0()<<" "
	       <<" curvature: "<<mHelixFitter.curvature()<<" "
	       <<" tanLambda: "<<mHelixFitter.tanLambda()<<endl;

    mMessenger<<"\tInitialzie Track:\t";
    track->initialize( mHelixFitter.curvature(), mHelixFitter.tanLambda(),
		       StThreeVectorD(mHelixFitter.xCenter(), mHelixFitter.yCenter(), 0.),
		       mSeedHitVec);    
}

void StiLocalTrackSeedFinder::calculate(StiKalmanTrack* track)
{
    mMessenger<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*)"<<endl;

    const StThreeVectorF& outside = mSeedHitVec.front()->globalPosition();
    const StThreeVectorF& middle = (*(mSeedHitVec.begin()+mSeedHitVec.size()/2))->globalPosition();
    const StThreeVectorF& inside = mSeedHitVec.back()->globalPosition();
    
    mMessenger<<"\tCalculate circle parameters:\t";
    mHelixCalculator.calculate( StThreeVector<double>( inside.x(), inside.y(), inside.z() ),
				StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
				StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );

    mMessenger<<"\tdone."<<endl;
    mMessenger <<"origin: "<<mHelixCalculator.xCenter()<<" "<<mHelixCalculator.yCenter()<<" "
	       <<mHelixCalculator.z0()<<" "
	       <<" curvature: "<<mHelixCalculator.curvature()<<" "
	       <<" tanLambda: "<<mHelixCalculator.tanLambda()<<endl;

    mMessenger<<"\tInitialzie Track:\t";
    track->initialize( mHelixCalculator.curvature(), mHelixCalculator.tanLambda(),
		       StThreeVectorD(mHelixCalculator.xCenter(), mHelixCalculator.yCenter(), 0.),
		       mSeedHitVec);    
}

void StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack* track)
{
    mMessenger<<"StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack*)"<<endl;

    const StThreeVectorF& outside = mSeedHitVec.front()->globalPosition();
    const StThreeVectorF& middle = mSeedHitVec.back()->globalPosition();

    mMessenger<<"\tCalculate circle parameters:\t";
    mHelixCalculator.calculate( StThreeVector<double>(0., 0., 0.),
				StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
				StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );

    mMessenger<<"\tdone."<<endl;
    mMessenger <<"origin: "<<mHelixCalculator.xCenter()<<" "<<mHelixCalculator.yCenter()<<" "
	       <<mHelixCalculator.z0()<<" "
	       <<" curvature: "<<mHelixCalculator.curvature()<<" "
	       <<" tanLambda: "<<mHelixCalculator.tanLambda()<<endl;

    mMessenger<<"\tInitialzie Track:\t";
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
    mExtrapLength = broker->ltsfExtrapLength();
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
    mMessenger <<"StiLocalTrackSeedFinder Detectors:\n";
    
    for (vector<StiDetector*>::const_iterator it=mDetVec.begin(); it!=mDetVec.end(); ++it) {
	mMessenger << **it <<endl;
    }
    cout <<"\n Search Window in Y:\t"<<mDeltaY<<endl;
    cout <<"\n Search Window in Z:\t"<<mDeltaZ<<endl;
}
