#ifndef StiCompositeSeedFinder_HH
#define StiCompositeSeedFinder_HH
#include "Sti/StiSeedFinder.h"
#include "Sti/Base/Vectorized.h"

/// Concrete class implementing the StiSeedFinder abstract interface
/// and providing a composite finder, i.e. an extensible collection
/// of finders called sequentially to find track seeds.
///\author M.L. Miller (Yale Software), and Claude Pruneau, Wayne State
class StiCompositeSeedFinder : public StiSeedFinder, public Vectorized<StiSeedFinder>
{
public:
  StiCompositeSeedFinder(const string&            name,
			 Factory<StiKalmanTrack>* trackFactory,
			 StiHitContainer*         hitContainer,
			 StiDetectorContainer*    detectorContainer);
  virtual ~StiCompositeSeedFinder();
  
  virtual bool hasMore();
  virtual StiKalmanTrack* next();
  virtual void reset();
  
 protected:
  virtual void build();
  vector<StiSeedFinder*>::iterator _currentTrackSeedFinder;
  
 private:
  //Not implemented
  StiCompositeSeedFinder();
  
};

#endif

