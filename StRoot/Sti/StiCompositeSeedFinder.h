//StiCompositeSeedFinder.h
//M.L. Miller (Yale Software)
//03/01


#ifndef StiCompositeSeedFinder_HH
#define StiCompositeSeedFinder_HH
#include <vector>
using std::vector;
#include "StiSeedFinder.h"

/*! Concrete class implementing the StiSeedFinder abstract interface
  and providing a composite finder, i.e. an extensible collection
  of finders called sequentially to find track seeds.
 */
class StiCompositeSeedFinder : public StiSeedFinder
{
public:
    StiCompositeSeedFinder(const string&            name,
			   Factory<StiKalmanTrack>* trackFactory,
			   StiHitContainer*         hitContainer,
			   StiDetectorContainer*    detectorContainer);
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
    SeedFinderVec           _trackSeedFinders;
    SeedFinderVec::iterator _currentTrackSeedFinder;
};

#endif

