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

class StiCompositeSeedFinder : public StiSeedFinder
{
public:
    StiCompositeSeedFinder();
    virtual ~StiCompositeSeedFinder();

    //Inherited interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void build();
    virtual void reset();

protected:
    
private:
    typedef vector<StiTrackSeedFinder*> SeedFinderVec;

    //The seedvec holds pointers to objects on the heap, owned by mSeedVec
    SeedFinderVec mSeedVec;
    SeedFinderVec::iterator mCurrent;
    
};



#endif

