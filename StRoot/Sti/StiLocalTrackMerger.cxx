//StiLocalTrackMerger.cxx
//M.L. Miller (Yale Software)
//12/01

//std
#include "Stiostream.h"
#include <cmath>
#include <algorithm>
using namespace std;
#include "StiKalmanTrack.h"
#include "StiTrackContainer.h"
#include "StiKalmanTrackNode.h"
#include "StiLocalTrackMerger.h"

StiLocalTrackMerger::StiLocalTrackMerger(StiTrackContainer* store)
    : StiTrackMerger(store), mDeltaR(1.)
{
    cout <<"StiLocalTrackMerger::StiLocalTrackMerger()"<<endl;
    mMaxTrack.reset();
    mMaxTrackNode.reset();
}

StiLocalTrackMerger::~StiLocalTrackMerger()
{
    cout <<"StiLocalTrackMerger::~StiLocalTrackMerger()"<<endl;
}

void StiLocalTrackMerger::mergeTracks()
{
    cout <<"StiLocalTrackMerger::mergeTracks()"<<endl;
    
    if (!mTrackStore) 
      {
	cout <<"StiLocalTrackMerger::mergeTracks(). ERROR:\t"
	     <<"mTrackStore==0.  Abort"<<endl;
	return;
      }
    
    cout <<"\t N-tracks before merge: "<<mTrackStore->size()<<endl;
		
    for (vector<StiTrack*>::iterator lower=mTrackStore->begin(); lower!=mTrackStore->end(); ++lower)
      {
	if (configureMaxTrack( static_cast<StiKalmanTrack*>(*lower) )) 
	  {
	    
	    /*
	      //Now find the bounds of search:
	      vector<StiTrack*>::iterator upper = mTrackStore->upper_bound(&mMaxTrack);
	      if (lower!=upper) {
	      vector<StiTrack*>::iterator start = lower;
	      ++start;
	      int i=0;
	      for (vector<StiTrack*>::iterator it = start; it!=upper; ++it) {
	      ++i;
	      if (sameTrack( static_cast<StiKalmanTrack*>(*start), 
	      static_cast<StiKalmanTrack*>(*it) ) ) {
	      cout <<"sameTrack==true"<<endl;
	      }
	      }
	      cout <<"Considered: "<<i<<" pairs for merging"<<endl;
	      }
	    */
	  }
      }
    cout <<"\t N-tracks before merge: "<<mTrackStore->size()<<endl;
}


bool StiLocalTrackMerger::configureMaxTrack(StiKalmanTrack* lowerTrack)
{
    //cout <<"StiLocalTrackMerger::configureMaxTrack()"<<endl;
    if (!lowerTrack) {
	cout <<"StiLocalTrackMerger::configureMaxTrack(). ERROR:\t"
	     <<"lowerTrack==0.  Skip this track"<<endl;
	return false;
    }
    
    StiKalmanTrackNode* lowerNode = lowerTrack->getLastNode();
    if (lowerNode->getCurvature()==0.) {
	cout <<"StiLocalTrackMerger::configureMaxTrack(). ERROR:\t"
	     <<"lowerNode->getCurvature()==0.  Skip this track"<<endl;
	return false;
    }
    
    //Now find the upper limit for search:
    mMaxTrack.reset();
    mMaxTrackNode.reset();
    
    double radius = 1./lowerNode->getCurvature();  //Careful, this number is signed
    double maxRadius=0.;
    double dR = radius*mDeltaR;
    if (radius<0) {
	maxRadius = radius-dR;
    }
    else {
	maxRadius = radius+dR;
    }
    cout <<"radius: "<<radius<<" dR: "<<dR<<" maxRadius: "<<maxRadius<<endl;
    mMaxTrackNode.setCurvature(1./maxRadius);
    return true;
}

bool StiLocalTrackMerger::sameTrack(StiKalmanTrack* lhs, StiKalmanTrack* rhs)
{
    return false;
}
