//StEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//06/01

//Std
#include <iostream>

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

//Sti
#include "StiTrack.h"
#include "StiEvaluableTrack.h"

StiEvaluableTrack::StiEvaluableTrack() : msttrack(0), mstmctrack(0)
{
}

StiEvaluableTrack::~StiEvaluableTrack()
{
}

void StiEvaluableTrack::setStTrack(StTrack* val)
{
    msttrack = val;
}

void StiEvaluableTrack::setStMcTrack(StMcTrack* val)
{
    mstmctrack = val;
}

StTrack* StiEvaluableTrack::stTrack() const
{
    return msttrack;
}

StMcTrack* StiEvaluableTrack::stMcTrack() const
{
    return mstmctrack;
}
