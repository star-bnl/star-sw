//  AnaTrackId.cxx
//  M.L. Miller
//  5/00

#include "AnaTrackId.h"

// StEvent
#include "StEventTypes.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"

AnaTrackId::AnaTrackId(  StTrack* track )
{

    //cout <<"Constructing AnaTrackId"<<endl;
    initialize();
    findPidTrait(track);
    findPid(track);
}

AnaTrackId::AnaTrackId()
{
    initialize();
    //cout <<"Constructing Default AnaTrackId"<<endl;
}

AnaTrackId::~AnaTrackId() {};

//Methods--------------------------------------
void AnaTrackId::initialize()
{
    m_dedxPidTr = 0;
    m_guess = 0;
}

//Loop over Traits, Select StDedxPidTraits found in the tpc with truncated mean
void AnaTrackId::findPidTrait(StTrack* track)
{
    StTpcDedxPidAlgorithm tpcDedx;
    StSPtrVecTrackPidTraits& traits = track->pidTraits();
    StDedxPidTraits* dedxPidTr;

    for (unsigned int itrait = 0; itrait < traits.size(); itrait++){
	dedxPidTr = 0;
	StTrackPidTraits* thisTrait = traits[itrait];
	dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
	
	if (dedxPidTr &&  dedxPidTr->method() == kTruncatedMeanId
	    && dedxPidTr->detector() == kTpcId)
	    {m_dedxPidTr = dedxPidTr;}
    }
}

//Get Oglivies PID for this track
void AnaTrackId::findPid(StTrack* track)
{
    StTpcDedxPidAlgorithm tpcDedx;
    if (track){
	const StTrack& tck = *track;
	//Pass a reference to the track and the vector of pid traits (Oglevies code is redundant)
	const StSPtrVecTrackPidTraits& traits = track->pidTraits();
	StParticleDefinition* guess = tpcDedx(tck,traits);
	if (guess) {m_guess = guess;}
    }
    else{
	m_guess = 0;
	return;
    }
    
    return;
}

//Access-----------------------------
StDedxPidTraits* AnaTrackId::getPidTraits()
{
    return m_dedxPidTr;
}

StParticleDefinition* AnaTrackId::getGuess()
{
    return m_guess;
}

const StParticleDefinition* AnaTrackId::getGuess() const
{
    return m_guess;
}
