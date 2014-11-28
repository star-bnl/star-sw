
//std
#include "Stiostream.h"
#include <cmath>
#include <vector>
using namespace std;

//ROOT
#include "TH1.h"
#include "TH2.h"

//SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"

//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//UpsilonAna
#include "StJetMuEvent.h"
#include "HistMaker.h"
    
ClassImp(HistMaker)
    
HistMaker::HistMaker() : mdEtaVsdPhi(0)
{
    cout <<"HistMaker::HistMaker()"<<endl;
}

HistMaker::~HistMaker()
{
    cout <<"HistMaker::~HistMaker()"<<endl;
}

void HistMaker::fill(StJetMuEvent* e)
{
    if (mCuts.verbose) {
	cout <<"HistMaker::fill(StJetMuEvent*)"<<endl;
    }
    
    if (!e) {
	cout <<"StHistMaker::fill(StUpsilonEvent*). ERROR:\t"
	     <<"null event.  Return w/o action"<<endl;
    }
    
    typedef vector<StMuTrack*> TrackVec;
    TrackVec vec;
	
    int n = e->tracks()->GetLast()+1;  // get number of primary tracks
    //cout <<"n="<<n<<"\tverbose:\t"<<mCuts.verbose<<endl;
    TClonesArray& tracks = *(e->tracks());

    StMuTrack* trigger = 0;
    double maxPt=0;

    for (int i=0; i<n; ++i) {
	StMuTrack* track = static_cast<StMuTrack*>(tracks[i]);     // get pointer to primary track
	
	if (acceptTrack(track)) {
	    //push back vector
	    if ( (track->pt()>maxPt) && (isTrigger(track)) ) {
		maxPt = track->pt();
		trigger = track;
	    }
	    
	    vec.push_back(track);
	}
    }
    
    if (!trigger) {
	//cout <<"Error, no trigger particle found.  Abort"<<endl;
	return;
    }

    if (mCuts.verbose) {
	cout <<"\t Accepted "<<vec.size()<<" tracks of "<<n<<" possible"<<endl;
    }

    //Now fill histogram
    StThreeVectorF triggerP = trigger->momentum();
    for (TrackVec::iterator it=vec.begin(); it!=vec.end(); ++it) {
	StMuTrack* track = *it;
	if (track!=trigger) {
	    StThreeVectorF p = track->momentum();
	    double dPhi = triggerP.phi() - p.phi();
	    double dEta = triggerP.pseudoRapidity() - p.pseudoRapidity();
	    mdEtaVsdPhi->Fill(dPhi, dEta);
	}
    }
}

bool HistMaker::isTrigger(StMuTrack* track)
{
    return (track->pt() >= mCuts.triggerPtCut);
}

bool HistMaker::acceptTrack(StMuTrack* track)
{
    if (track->flag() >= 0 &&
	track->momentum().perp() > mCuts.ptCut &&
	fabs(track->momentum().pseudoRapidity()) < mCuts.pseudoRapidityCutOff &&
	track->nHitsFit() >= mCuts.minNumberOfFitPoints &&
	track->nHits() >= mCuts.minNumberOfPoints) {
	return true;
    }
    else {
	return false;
    }
}
