//StiTrackFilters.cxx
//M.L. Miller (Yale Software)
//3/02

#include <iostream>
#include <cmath>
#include <float.h>
#include <iostream>
#include <string>
#include <stdexcept>
using namespace std;

//Sti
#include "StiTrack.h"
#include "StiIOBroker.h"
#include "StiTrackFilters.h"

//--- StiPtFilter ---
bool StiPtFilter::operator()(const StiTrack* t) const
{
    double pt = t->getPt();
    return (pt>mPtMin && pt<mPtMax);
}

void StiPtFilter::getNewState()
{
    mPtMin = mBroker->filterPtMin();
    mPtMax = mBroker->filterPtMax();
};

void StiPtFilter::print() const
{
    cout <<mName<<"\tPtMin: "<<mPtMin<<"\tPtMax: "<<mPtMax;
}

//--- StiEtaFilter ---
bool StiEtaFilter::operator()(const StiTrack* t) const
{
    double eta = t->getPseudoRapidity();
    return (eta>=mEtaMin && eta<=mEtaMax);
}

void StiEtaFilter::getNewState()
{
    mEtaMin = mBroker->filterEtaMin();
    mEtaMax = mBroker->filterEtaMax();
}

void StiEtaFilter::print() const
{
    cout <<mName<<"\tEtaMin: "<<mEtaMin<<"\tEtaMax: "<<mEtaMax;
}

// --- Filter for Chi2 ---

bool StiChi2Filter::operator()(const StiTrack* t) const
{
    return true;
}

void StiChi2Filter::getNewState()
{
    mChi2Max = mBroker->filterChi2Max();
}

void StiChi2Filter::print() const
{
    cout <<mName<<"\tChi2Max: "<<mChi2Max;
}

//--- Filter for Npts ---

bool StiNptsFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiNptsFilter::getNewState()
{
    mNptsMin = mBroker->filterNptsMin();
}

void StiNptsFilter::print() const
{
    cout <<mName<<"\tNptsMin: "<<mNptsMin;
}

//--- Filter for NFitPts ---

bool StiNFitPtsFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiNFitPtsFilter::getNewState()
{
    mNFitPtsMin = mBroker->filterNFitPtsMin();
}

void StiNFitPtsFilter::print() const
{
    cout <<mName<<"\tNFitPtsMin: "<<mNFitPtsMin;
}

//--- Filter for NGaps ---

bool StiNGapsFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiNGapsFilter::getNewState()
{
    mNGapsMax = mBroker->filterNGapsMax();
}

void StiNGapsFilter::print() const
{
    cout <<mName<<"\tNGapsMax: "<<mNGapsMax;
}

//--- Filter for FitPointRatio (fit/total)

bool StiFitPointRatioFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiFitPointRatioFilter::getNewState()
{
    mFitPointRatioMin = mBroker->filterFitPointRatioMin();
}

void StiFitPointRatioFilter::print() const
{
    cout <<mName<<"\tFitPointRatioMin: "<<mFitPointRatioMin;
}

//--- Filter for Primary DCA ---

bool StiPrimaryDcaFilter::operator()(const StiTrack* t) const
{
    return true;
}

void StiPrimaryDcaFilter::getNewState()
{
    mPrimaryDcaMax = mBroker->filterPrimaryDcaMax();
}

void StiPrimaryDcaFilter::print() const
{
    cout <<mName<<"\tPrimaryDcaMax: "<<mPrimaryDcaMax;
}
