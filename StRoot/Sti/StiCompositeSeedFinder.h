#ifndef StiCompositeSeedFinder_H_INCLUDED
#define StiCompositeSeedFinder_H_INCLUDED
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/Base/Vectorized.h"

/// Concrete class implementing the StiSeedFinder abstract interface
/// and providing a composite finder, i.e. an extensible collection
/// of finders called sequentially to find track seeds.
/// \author M.L. Miller (Yale Software) 2001
/// \author Claude Pruneau, Wayne State 2003
class StiCompositeSeedFinder : public StiTrackSeedFinder, public Vectorized<StiTrackSeedFinder>
{
public:
  StiCompositeSeedFinder(const string&            name,
												 Factory<StiKalmanTrack>* trackFactory,
												 StiHitContainer*         hitContainer,
												 StiDetectorContainer*    detectorContainer);
  virtual ~StiCompositeSeedFinder();
	void    initialize();
  virtual bool hasMore();
  virtual StiKalmanTrack* next();
  virtual void reset();
 protected:
  vector<StiTrackSeedFinder*>::iterator _currentTrackSeedFinder;
 private:
  //Not implemented
  StiCompositeSeedFinder();
};

#endif

