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
#include "CombinationIterator.h"

class StiRootDrawableHits;
class StiKalmanTrack;
class StTrack;
class Sti2HitComboFilter;

class StiTrackSeedFinder : public StiSeedFinder
{
    typedef CombinationIterator<StiHit*> combo_iterator;
    typedef combo_iterator::tvector tvector;

public:
    typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackFactory;

    StiTrackSeedFinder(); //Not Implemented
    StiTrackSeedFinder(const StiTrackSeedFinder&); //Not Implemented
    StiTrackSeedFinder(StiHitContainer*);
    virtual ~StiTrackSeedFinder();

    //Set Hit Combination filter type
    void setHitComboFilter(Sti2HitComboFilter* val) {mhitcombofilter = val;}
    const Sti2HitComboFilter* getHitComboFilter() const {return mhitcombofilter;}

    //SetFactory
    void setFactory(StiKalmanTrackFactory* val) {mtrackfactory=val;}
    
    //Enforced User interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    
    void addLayer(double refangle, double position);
    virtual void clear();
    virtual void init();
    
    virtual void print() const; //Print contents of iterator
    int numberOfLayers() const;
    
protected:
    virtual StiKalmanTrack* makeTrack(const tvector&) const;
    bool acceptCombination(const tvector&) const;
    
    StiHitContainer* mhitstore;
    combo_iterator* miterator;
    StiKalmanTrackFactory* mtrackfactory;
    int mnlayers;
    StiRootDrawableHits* mdrawablehits;
    Sti2HitComboFilter* mhitcombofilter;
};

// Non members ---

//Helper class to filter combinations of StiHits
struct Sti2HitComboFilter
{
    virtual bool operator()(const StiHit*, const StiHit*) const = 0;
};

//This is a simple test for rectangular distance in 2 dimensions
struct StiRectangular2HitComboFilter : public Sti2HitComboFilter
{
    virtual bool operator()(const StiHit*, const StiHit*) const;
    double deltaD;
    double deltaZ;
};

inline bool StiRectangular2HitComboFilter::operator()(const StiHit* lhs, const StiHit* rhs) const
{
    return ( (fabs(lhs->y()-rhs->y())<=deltaD) && (fabs(lhs->z()-rhs->z())<deltaZ));
}
#endif
