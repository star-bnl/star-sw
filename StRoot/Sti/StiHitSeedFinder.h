//StiHitSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiHitSeedFinder_HH
#define StiHitSeedFinder_HH

#include <vector>
#include "StiSeedFinder.h"

#include "StiHitContainer.h" //Include temp., so that we can use typedefs
#include "CombinationIterator.h"

class StiDummyTrack;
class StTrack;

class StiHitSeedFinder : public StiSeedFinder
{
    typedef CombinationIterator<StiHit*> combo_iterator;
    typedef combo_iterator::tvector tvector;

public:
    StiHitSeedFinder(); //Not Implemented
    StiHitSeedFinder(StiHitContainer*);
    StiHitSeedFinder(const StiHitSeedFinder&); //Not Implemented
    virtual ~StiHitSeedFinder();
    
    //Enforced User interface
    virtual bool hasMore();
    virtual sti_track_pair* next();
    
    void addLayer(int sector, int padrow);
    virtual void clear();
    virtual void init();
  
    virtual void print() const; //Print contents of iterator
    int numberOfLayers() const;
    
protected:
    virtual StiDummyTrack* makeTrack(const tvector&) const;
    const StiHitContainer* mhitstore;
    combo_iterator* miterator;
    int mnlayers;
    //Temp (4/13/01)
    sti_track_pair* mtrackpair;
};

#endif
