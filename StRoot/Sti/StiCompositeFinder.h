#ifndef StiCompositeFinder_H_INCLUDED
#define StiCompositeFinder_H_INCLUDED
#include <vector>
#include "Sti/StiTrackFinder.h"

/// Class providing a composite finder, i.e. an extensible collection
/// of finders called sequentially to find track seeds.
/// \author M.L. Miller (Yale Software) 2001
/// \author Claude Pruneau, Wayne State 2003
class StiCompositeFinder : public StiTrackFinder, public vector<StiTrackFinder*>
{
public:
  StiCompositeFinder(const string& name,
		     const string& description);
  virtual ~StiCompositeFinder();
  void initialize(){};
  void reset();
  StiTrack * findTrack();
 protected:
  vector<StiTrackFinder*>::iterator _currentFinder;
 private:
  //Not implemented
  StiCompositeFinder();
};

#endif

