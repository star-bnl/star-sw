//StiTrackFilters.cxx
//M.L. Miller (Yale Software)
//3/02

#include <iostream>
#include <cmath>
#include <float.h>
using namespace std;

//Sti
#include "StiTrack.h"
#include "StiIOBroker.h"
#include "StiTrackFilters.h"

//--- StiPtFilter ---
bool StiPtFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiPtFilter::getNewState()
{
    mPtMin = mBroker->filterPtMin();
    mPtMax = mBroker->filterPtMax();
};

//--- StiEtaFilter ---
bool StiEtaFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiEtaFilter::getNewState()
{
    mEtaMin = mBroker->filterPtMin();
    mEtaMax = mBroker->filterPtMax();
}
