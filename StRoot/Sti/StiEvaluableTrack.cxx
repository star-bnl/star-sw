//StEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//06/01

//Std
#include "Stiostream.h"

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

//Sti
#include "StiKalmanTrack.h"
#include "StiEvaluableTrack.h"

StiEvaluableTrack::StiEvaluableTrack() : mPair(0)
{
}

StiEvaluableTrack::~StiEvaluableTrack()
{
}

void StiEvaluableTrack::reset()
{
    mPair=0;
    this->StiKalmanTrack::reset();
}

/*
ostream& operator<<(ostrea& os, const StiEvaluableTrack& track)
{
    StTrackPairInfo* info = track.stTrackPairInfo();
    StTrack* stTrack = info->partnerTrack();
    StMcTrack* mcTrack = info->partnerMcTrack();
    
    os <<"\n\tFound Sti Track:"<<endl;
    os << track <<endl;

    os <<"\tFound Global Track:"<<endl;
    const StThreeVectorF& mom = stTrack->;

    os <<"\tOriginal Mc Track:"<<endl;
    os <<"q: Unknown"
       <<"pt: "<<mcTrack->momentum.perp()
       <<" eta: "<<mcTrack->momentum.pseudoRapidity()
       <<"";
    
    os <<"q: "<<track.getCharge()
       <<" pt: "<<track.getPt()
       <<" eta: "<<track.getPseudoRapidity()
       <<" tanLambda: "<<track.getTanL()
       <<" Chi2: "<<track.getChi2()
       <<" points: "<<track.getPointCount()
       <<" fitPoints: "<<track.getFitPointCount();
       
}

*/
