//MuEventReader.cxx
//M.L. Miller (Yale Software)
//07/02

//std
#include <iostream>

//ROOT
#include "TObject.h"
#include "TTree.h"
#include "TChain.h"
#include "TClonesArray.h"

//SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"

//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//local
#include "MuEventReader.h"

ClassImp(MuEventReader)
    
int MuEventReader::numberOfEvents() const
{
    TTree* t = mMuDstMaker->chain();
    if (!t) {cout <<"MuEventReader::numberOfEvents(). ERROR:\t tree==0.  abort()"<<endl; abort();}
    return static_cast<int>(t->GetEntries());
}

StMuDst* MuEventReader::getEvent()
{
    StMuDst* e = mMuDstMaker->muDst();
    if (!e) {
	cout <<"MuEventReader::getEvent().  muDst==0, no action"<<endl;
	return 0;
    }
    return e;
}

StMuDst* MuEventReader::getNextEvent()
{
    int iret = mMuDstMaker->Make();
    if (iret==4) {return 0;}
    
    StMuDst* e = getEvent();
    mLcp = 0;
    double ptMax = 0.;
    
    //set the LCP
    if (e) {
	TClonesArray& particles = *(e->primaryTracks());
	int last = particles.GetLast();
	for (int i=0; i<=last; ++i) {
	    StMuTrack* p = static_cast<StMuTrack*>(particles[i]);
	    if (p->pt()>ptMax) {
		mLcp = p;
		ptMax = p->pt();
	    }
	}
    }
    
    return e;
}
