//StiCompositeSeedFinder.h
//M.L. Miller (Yale Software)
//03/01


#ifndef StiCompositeSeedFinder_HH
#define StiCompositeSeedFinder_HH

#include <vector>
using std::vector;

#include "StiSeedFinder.h"

class StiKalmanTrack;
class StTrack;
class StiTrackSeedFinder;
class StiHitContainer;

class StiCompositeSeedFinder : public StiSeedFinder
{
public:
    StiCompositeSeedFinder(StiObjectFactoryInterface<StiKalmanTrack>*,
			   StiHitContainer*);
    virtual ~StiCompositeSeedFinder();

    //Inherited interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void reset();

protected:
    virtual void build();
    
private:
    //Not implemented
    StiCompositeSeedFinder();
    
    typedef vector<StiTrackSeedFinder*> SeedFinderVec;

    //The seedvec holds pointers to objects on the heap, owned by mSeedVec
    SeedFinderVec mSeedVec;
    SeedFinderVec::iterator mCurrent;
    
};



#endif

