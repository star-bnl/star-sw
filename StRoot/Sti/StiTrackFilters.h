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
    StiPtFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiPtFilter") {};
    virtual ~StiPtFilter() {};
    
    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiPtFilter(); //Not implemented
    double mPtMin;
    double mPtMax;
};

//--- Filter for eta ---

class StiEtaFilter : public StiTrackFilter_t
{
public:
    StiEtaFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiEtaFilter") {};
    virtual ~StiEtaFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiEtaFilter(); //Not implemented
    double mEtaMin;
    double mEtaMax;
};

// --- Filter for Chi2 ---

class StiChi2Filter : public StiTrackFilter_t
{
public:
    StiChi2Filter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiChi2Filter") {};
    virtual ~StiChi2Filter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiChi2Filter(); //Not implemented
    double mChi2Max;
};

//--- Filter for Npts ---

class StiNptsFilter : public StiTrackFilter_t
{
public:
    StiNptsFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiNptsFilter") {};
    virtual ~StiNptsFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiNptsFilter(); //Not implemented
    unsigned int mNptsMin;
};

//--- Filter for NFitPts ---

class StiNFitPtsFilter : public StiTrackFilter_t
{
public:
    StiNFitPtsFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiNFitPtsFilter") {};
    virtual ~StiNFitPtsFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiNFitPtsFilter(); //Not implemented
    unsigned int mNFitPtsMin;
};

//--- Filter for NGaps ---

class StiNGapsFilter : public StiTrackFilter_t
{
public:
    StiNGapsFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiNGapsFilter") {};
    virtual ~StiNGapsFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiNGapsFilter(); //Not implemented
    unsigned int mNGapsMax;
};

//--- Filter for FitPointRatio (fit/total)

class StiFitPointRatioFilter : public StiTrackFilter_t
{
public:
    StiFitPointRatioFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiFitPointRatioFilter") {};
    virtual ~StiFitPointRatioFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiFitPointRatioFilter(); //Not implemented
    double mFitPointRatioMin;
};

//--- Filter for Primary DCA ---

class StiPrimaryDcaFilter : public StiTrackFilter_t
{
public:
    StiPrimaryDcaFilter(Subject* s, StiIOBroker* b) : StiTrackFilter_t(s,b, "StiPrimaryDcaFilter") {};
    virtual ~StiPrimaryDcaFilter() {};

    virtual bool operator()(const StiTrack*) const;
    virtual void getNewState();
    virtual void print() const;

private:
    StiPrimaryDcaFilter(); //Not implemented
    double mPrimaryDcaMax;
};
#endif
