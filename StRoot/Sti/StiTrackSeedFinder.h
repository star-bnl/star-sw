//StiTrackSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiTrackSeedFinder_HH
#define StiTrackSeedFinder_HH

#include <vector>
#include "StiSeedFinder.h"

#include "StiHitContainer.h" //Include temp., so that we can use typedefs
#include "CombinationIterator.h"

class StiRootDrawableHits;
class StiKalmanTrack;
class StTrack;

class StiTrackSeedFinder : public StiSeedFinder
{
    typedef CombinationIterator<StiHit*> combo_iterator;
    typedef combo_iterator::tvector tvector;

public:
    StiTrackSeedFinder(); //Not Implemented
    StiTrackSeedFinder(const StiTrackSeedFinder&); //Not Implemented
    StiTrackSeedFinder(StiHitContainer*);
    virtual ~StiTrackSeedFinder();
    
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
    StiHitContainer* mhitstore;
    combo_iterator* miterator;
    int mnlayers;
    StiRootDrawableHits* mdrawablehits;
};

#endif
