//StUpsilonMuEvent.cxx

//std
#include <iostream>
#include <cmath>
#include <vector>
using namespace std;

//ROOT
#include "TObject.h"
#include "TTree.h"
#include "TClonesArray.h"

//SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"

//StEvent
#include "StEventTypes.h"

//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//UpsilonAna
#include "StUpsilonMuEvent.h"

ClassImp(StUpsilonMuEvent)
    
    StUpsilonMuEvent::StUpsilonMuEvent() : mTracks(new TClonesArray("StMuTrack",100))
{
    //cout <<"StUpsilonMuEvent::StUpsilonMuEvent()"<<endl;
}

StUpsilonMuEvent::~StUpsilonMuEvent()
{
    //cout <<"StUpsilonMuEvent::~StUpsilonMuEvent()"<<endl;
}

bool StUpsilonMuEvent::fill(StMuDstMaker* maker)
{
    StMuEvent* e = maker->muDst()->event();
    if (!e) {
	cout <<"Error, null event.  Return False"<<endl;
	return false;
    }

    reset();
    
    if (accept(maker)) {
	return true;
    }
    return false;
}

bool StUpsilonMuEvent::accept(StMuDstMaker* maker)
{    
    StMuEvent* e = maker->muDst()->event();
    if (!e) {
	cout <<"StUpsilonMuEvent::accept(StMuDstMaker*). Error:\t"
	     <<"Null event.  Return false"<<endl;
	return false;
    }

    fillBasicTypes(e);
    vector<StMuTrack*> vec;
	
    int n = maker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
	
    for (int i=0; i<n; ++i) {
	StMuTrack* track = maker->muDst()->primaryTracks(i);     // get pointer to primary track
	    
	if (acceptTrack(track)) {
	    //push back vector
	    vec.push_back(track);
	}
    }

    if (mCuts.verbose) {
	cout <<"\t Accepted "<<vec.size()<<" tracks of "<<n<<" possible"<<endl;
    }
	
    unsigned int j, k;
    short q;
    double mass;
    StMuTrack *t1, *t2;
    for (j=0; j<vec.size(); j++) {
	t1 = vec[j];
	StThreeVectorF p1(t1->momentum());	    
	StLorentzVectorF q1(p1, p1.massHypothesis(electron_mass_c2));
	for (k=j+1; k<vec.size(); k++) {
	    t2 = vec[k];
	    StThreeVectorF p2(t2->momentum());	    
	    StLorentzVectorF q2(p2, p2.massHypothesis(electron_mass_c2));
	    mass = abs(q1+q2);
	    if (mass > mCuts.lowerInvariantMassCut && mass < mCuts.upperInvariantMassCut) {
		q = t1->charge()+t2->charge();
		if (q > 0) {
		    mPlusPlus++;
		}		
		else if(q < 0) {
		    mMinusMinus++;
		}
		else {
		    mPlusMinus++;
		}
	    }
	}
    }
	
    if (mCuts.verbose) {
	cout << "Pairs: (++)=" << mPlusPlus
	     << ", (--)=" << mMinusMinus
	     << ", (+-)=" << mPlusMinus << endl;
    }

    if (mPlusMinus || mPlusPlus || mMinusMinus) {
	
	//cout <<"\t-------------- vector      ------------"<<endl;
	//Fill tracks in TClonesArray:
	for (vector<StMuTrack*>::iterator it=vec.begin(); it!=vec.end(); ++it) {
	    StMuTrack& track = **it;
	    //cout <<"momentum:\t"<<track.p()<<endl;
	    TClonesArray& cArr = *mTracks;
	    new(cArr[mTrackCounter++]) StMuTrack(track); //I know this works...
	}
	/*
	//Test
	cout <<"\t-------------- TClonesArray ------------"<<endl;
	int nTracks = mTracks->GetLast()+1; //don't know why, but they don't have a size method
	for (int i=0; i<nTracks; ++i) {
	TClonesArray& cArr = *mTracks;
	StMuTrack* track = static_cast<StMuTrack*>(cArr[i]);
	cout <<"momentum:\t"<<track->p()<<endl;
	}
	*/
	
	return true;
    }
    else {
	return false;
    }
}

bool StUpsilonMuEvent::acceptTrack(StMuTrack* track)
{
    if (track->flag() >= 0 &&
	track->momentum().mag() > mCuts.electronMomentumCut &&
	fabs(track->momentum().pseudoRapidity()) < mCuts.pseudoRapidityCutOff &&
	track->nHitsFit() >= mCuts.minNumberOfFitPoints &&
	track->nHits() >= mCuts.minNumberOfPoints) {
	return true;
    }
    else {
	return false;
    }
}

void StUpsilonMuEvent::reset()
{
    mPlusPlus = 0;
    mMinusMinus = 0;
    mPlusMinus = 0;
    mL3Fired=false;

    mEventId=0;
    mEventNumber=0;
    mRunId=0;
    mRunNumber=0;
    mTriggerWord=0;
    
    mRefMultPos=0;
    mRefMultNeg=0;
    mRefMult=0;
    
    mMagneticField=0.;
    mZdcAdcAttenuatedSumWest=0.;
    mZdcAdcAttenuatedSumEast=0.;
    mCtbMultiplicity=0.;
    
    mPrimaryVertexPosition.setX(0.);
    mPrimaryVertexPosition.setY(0.);
    mPrimaryVertexPosition.setZ(0.);

    mTrackCounter=0;
    mTracks->Clear();
}

void StUpsilonMuEvent::fillBasicTypes(StMuEvent* event)
{
    //Basic event info
    mEventId = event->eventId();
    mEventNumber = event->eventNumber();
    mRunId = event->runId();
    mRunNumber = event->runNumber();
    mTriggerWord = event->l0Trigger().triggerWord();  //StEvent->l0Trigger()->triggerWord();
    
    //Event characterization
    mRefMultPos = event->refMultPos();
    mRefMultNeg = event->refMultNeg();
    mRefMult = event->refMult();
    
    mMagneticField = event->magneticField();
    mZdcAdcAttenuatedSumWest = event->zdcAdcAttentuatedSumWest();
    mZdcAdcAttenuatedSumEast = event->zdcAdcAttentuatedSumEast();
    mCtbMultiplicity = event->ctbMultiplicity();
    mPrimaryVertexPosition = event->primaryVertexPosition();
    
    //reactionPlane(unsigned short);
    //reactionPlanePtWgt(unsigned short);
}
