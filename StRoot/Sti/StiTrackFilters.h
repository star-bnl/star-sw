//StiTrackFilters.h
//M.L. Milelr (Yale Software)
//3/02

#ifndef StiTrackFilters_HH
#define StiTrackFilters_HH

#include "StiAbstractFilter.h"
class StiTrack;

//A typedef of general use!
typedef StiAbstractFilter<const StiTrack*> StiTrackFilter_t;

//--- Filter for Pt ---

class StiPtFilter : public StiTrackFilter_t
{
public:
    StiPtFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b) {};
    virtual ~StiPtFilter() {};
    
    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();

private:
    StiPtFilter(); //Not implemented
    double mPtMin;
    double mPtMax;
};

//--- Filter for eta ---
class StiEtaFilter : public StiTrackFilter_t
{
public:
    StiEtaFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b) {};
    virtual ~StiEtaFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();

private:
    StiEtaFilter(); //Not implemented
    double mEtaMin;
    double mEtaMax;
};

#endif
