//StppEventReader.cxx
//M.L. Miller (Yale Software)
//07/02

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
#include "StppuDstMaker.h"
#include "StppEvent.h"
#include "StppEventReader.h"

ClassImp(StppEventReader)
    
int StppEventReader::numberOfEvents() const
{
    return mMuEventReader->numberOfEvents();
}

StppEvent* StppEventReader::getEvent()
{
    StMuDst* e = mMuEventReader->getEvent();
    
    if (!e) {
	cout <<"StppEventReader::getEvent().  muDst==0, no action"<<endl;
	return 0;
    }
    mStppuDstMaker->setMuDst(e);
    mStppuDstMaker->Make();
    StppEvent* event = mStppuDstMaker->event();
    return event;
}

StppEvent* StppEventReader::getNextEvent()
{
    StMuDst* dst = mMuEventReader->getNextEvent();
    if (!dst) {return 0;}
    
    StppEvent* e = getEvent();
    mLcp = 0;
    double ptMax = 0.;
    
    //set the LCP
    if (e) {
	TClonesArray& particles = *(e->tracks);
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
