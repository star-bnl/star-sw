
//std
#include <iostream>
#include <cmath>
#include <vector>
using namespace std;

//ROOT
#include "TH1.h"

//SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"

//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//UpsilonAna
#include "StUpsilonMuEvent.h"
#include "HistMaker.h"
    
ClassImp(HistMaker)
    
    HistMaker::HistMaker() :
	mPlusPlus(0), mMinusMinus(0), mPlusMinus(0)
{
    mPlusPlus = new TH1D("plusPlus","Plus-Plus Invariant Mas", 32, 6., 14.);
    mMinusMinus = new TH1D("minusMinus","Minus-Minus Invariant Mas", 32, 6., 14.);
    mPlusMinus = new TH1D("plusMinus","Plus-Minus Invariant Mas", 32, 6., 14.);

    mCandidateEvents=0;
    
    cout <<"HistMaker::HistMaker()"<<endl;
}

HistMaker::~HistMaker()
{
    cout <<"HistMaker::~HistMaker()"<<endl;
}

void HistMaker::fill(StUpsilonMuEvent* e)
{
    if (mCuts.verbose) {
	cout <<"HistMaker::fill(StUpsilonMuEvent*)"<<endl;
    }
    
    mMinusMinusPairs = 0;        
    mPlusPlusPairs = 0;
    mUnlikeSignPairs = 0;

    if (!e) {
	cout <<"StHistMaker::fill(StUpsilonEvent*). ERROR:\t"
	     <<"null event.  Return w/o action"<<endl;
    }
    
    vector<StMuTrack*> vec;
	
    int n = e->tracks()->GetLast()+1;  // get number of primary tracks
    //cout <<"n="<<n<<"\tverbose:\t"<<mCuts.verbose<<endl;
    TClonesArray& tracks = *(e->tracks());

    for (int i=0; i<n; ++i) {
	StMuTrack* track = static_cast<StMuTrack*>(tracks[i]);     // get pointer to primary track
	    
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
		    mPlusPlusPairs++;
		    mPlusPlus->Fill(mass);
		}		
		else if(q < 0) {
		    mMinusMinusPairs++;
		    mMinusMinus->Fill(mass);
		}
		else {
		    mUnlikeSignPairs++;
		    mPlusMinus->Fill(mass);
		}
	    }
	}
    }
    
    if (mCuts.verbose) {
	cout << "Pairs: (++)=" << mPlusPlusPairs
	     << ", (--)=" << mMinusMinusPairs
	     << ", (+-)=" << mUnlikeSignPairs << endl;
    }
    
    if (mUnlikeSignPairs || mPlusPlusPairs || mMinusMinusPairs) {
	mCandidateEvents++;
	if (mPlusPlusPairs || mMinusMinusPairs) mUnlikeSignEvents++;
	if (mUnlikeSignPairs) mLikeSignEvents++;
    }
}

bool HistMaker::acceptTrack(StMuTrack* track)
{
    if (track->flag() >= 0 &&
	track->momentum().perp() > mCuts.electronMomentumCut && //try w/ pt instead of p
	//track->momentum().mag() > mCuts.electronMomentumCut &&
	fabs(track->momentum().pseudoRapidity()) < mCuts.pseudoRapidityCutOff &&
	track->nHitsFit() >= mCuts.minNumberOfFitPoints &&
	track->nHits() >= mCuts.minNumberOfPoints) {
	return true;
    }
    else {
	return false;
    }
}
