//StiTrackSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiTrackSeedFinder_HH
#define StiTrackSeedFinder_HH

#include <vector>
#include "StiSeedFinder.h"

#include "StiHit.h"
#include "StiHitContainer.h" //Include temp., so that we can use typedefs
#include "StiKalmanTrack.h"
#include "CombinationIterator.h"

class StiRootDrawableHits;
class StTrack;
class Sti2HitComboFilter;

class StiTrackSeedFinder : public StiSeedFinder
{
    typedef CombinationIterator<StiHit*> combo_iterator;
    typedef combo_iterator::tvector tvector;

public:

    StiTrackSeedFinder(StiHitContainer*, Sti2HitComboFilter*);
    StiTrackSeedFinder(const StiTrackSeedFinder&);
    StiTrackSeedFinder& operator=(const StiTrackSeedFinder&);
    
    virtual ~StiTrackSeedFinder();

    //Set Hit Combination filter type
    void setHitComboFilter(Sti2HitComboFilter* val);
    const Sti2HitComboFilter* getHitComboFilter() const;
    
    //SetFactory
    void setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val);
    
    //Enforced User interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void build();
    virtual void reset();
    
    void addLayer(double refangle, double position);
    
    virtual void print() const; //Print contents of iterator
    int numberOfLayers() const;
    
protected:
    void copyToThis(const StiTrackSeedFinder&); //deep copy
    virtual StiKalmanTrack* makeTrack(const tvector&) const;

protected:
    //Shallow members
    StiHitContainer* mhitstore;
    combo_iterator miterator;
    StiObjectFactoryInterface<StiKalmanTrack>* mtrackfactory;
    int mnlayers;
    StiRootDrawableHits* mdrawablehits;
    Sti2HitComboFilter* mhitcombofilter;

};

// Non members ---

//Helper class to filter combinations of StiHits
struct Sti2HitComboFilter
{
    virtual bool operator()(const StiHit*, const StiHit*) const = 0;
    virtual void build(const string& val="empty")=0;
};

//This is a simple test for rectangular distance in 2 dimensions
struct StiRectangular2HitComboFilter : public Sti2HitComboFilter
{
    StiRectangular2HitComboFilter() :  deltaD(-1), deltaZ(-1) {};
    virtual bool operator()(const StiHit*, const StiHit*) const;
    virtual void build(const string&);
    double deltaD;
    double deltaZ;
};

struct StiCollinear2HitComboFilter : public Sti2HitComboFilter
{
    StiCollinear2HitComboFilter() : deltaPhi(-1.), deltaTheta(-1.) {};
    virtual bool operator()(const StiHit*, const StiHit*) const;
    virtual void build(const string&);
    double deltaPhi;
    double deltaTheta;
};
//inlines


inline bool StiRectangular2HitComboFilter::operator()(const StiHit* lhs, const StiHit* rhs) const
{
    return ( (fabs(lhs->y()-rhs->y())<=deltaD) && (fabs(lhs->z()-rhs->z())<deltaZ));
}

inline void StiTrackSeedFinder::setHitComboFilter(Sti2HitComboFilter* val)
{
    mhitcombofilter = val;
}

inline const Sti2HitComboFilter* StiTrackSeedFinder::getHitComboFilter() const
{
    return mhitcombofilter;
}

inline void StiTrackSeedFinder::setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val)
{
    mtrackfactory=val;
}

#endif
