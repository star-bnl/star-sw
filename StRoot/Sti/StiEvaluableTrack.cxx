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

