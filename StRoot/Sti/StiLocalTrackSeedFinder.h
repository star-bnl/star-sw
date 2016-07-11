/// \File StiLocalTrackSeedFinder.h
/// \author M.L. Miller (Yale Software) 10/01
#ifndef StiLocalTrackSeedFinder_HH
#define StiLocalTrackSeedFinder_HH
#include "Stiostream.h"
using std::ostream;
#include <vector>
using std::vector;
#include "Sti/Base/EditableParameters.h"
//VP#include "Sti/StiHelixCalculator.h"
//VP#include "Sti/StiHelixFitter.h"
#include "Sti/StiTrackFinder.h"
#include "Sti/StiSortedHitIterator.h"
#include "Sti/StiMasterDetectorBuilder.h"
#include "StDetectorDbMaker/StiLocalTrackSeedFinderParameters.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "StDetectorDbMaker/StiLocalTrackSeedFinderParameters.h"

class StiDetector;

/// \class StiLocalTrackSeedFinder
/// StiLocalTrackSeedFinder is a concrete implementation of StiTrackFinder.
/// It is built from a collection of StiDetectorObjects, which it
/// stores in a vector and orders properly, then uses each detector
/// as a layer from which a one-point seed can be generated.  It then proceeds
/// to step inwards, iteratively making a local decision at each step.
/// \author M.L. Miller (Yale Software) 10/01
/// \author Claude A Pruneau (Wayne State) Jan 2003
class StiLocalTrackSeedFinder : public StiTrackFinder
{
public:
  StiLocalTrackSeedFinder(const string& name,
			  const string& description, 
			  Factory<StiKalmanTrack> * trackFactory,
			  StiHitContainer         * hitContainer,
			  StiDetectorContainer * detectorContainer);
  virtual ~StiLocalTrackSeedFinder();
  StiTrack* findTrack(double rMin=0);
  virtual void clear(){reset();};
  virtual void initialize(){};
  virtual void print() const;
  Filter<StiTrack>    * getTrackFilter();
  virtual void reset();
  virtual void unset(){;}
  bool isReset();

  void findTracks(){}; 
  bool find(StiTrack *track, int direction,double rmin=0){return false;};

  friend ostream& operator<<(ostream& os, const StiLocalTrackSeedFinder & f);

protected:
  StiKalmanTrack *makeTrack(StiHit* hit);
  ///Extend hit looking for closest neighbor in z
  bool extendHit(StiHit & hit);
  ///Extrapolate to next layer using straight line, add hit closest in z
  bool extrapolate();
  StiKalmanTrack* initializeTrack(StiKalmanTrack*);
  //Perform helix fit, Perform helix calculation (doesn't assume any vertex)
  bool fit(StiKalmanTrack*);

  bool   _reset;
  Factory<StiKalmanTrack> * _trackFactory;
  StiHitContainer         * _hitContainer;
  StiDetectorContainer    * _detectorContainer;

  StiSortedHitIterator _hitIter;
  vector<StiHit*>        _seedHits;
  StiDefaultTrackFilter  _trackFilter;
  double fRxyMin;
 private:
  //The following are not implemented, as they are non-trivial
  //and the default compiler generated versions will be wrong.
  StiLocalTrackSeedFinder();
  StiLocalTrackSeedFinder(const StiLocalTrackSeedFinder&);
  StiLocalTrackSeedFinder operator=(const StiLocalTrackSeedFinder&);
  //Count how many hits we've skipped in extrapolation
  int    _skipped;
};


inline void StiLocalTrackSeedFinder::reset()
{
//  cout <<"StiLocalTrackSeedFinder::reset() -I- Started"<<endl;
  _reset = true;
//  cout <<"StiLocalTrackSeedFinder::reset() -I- Done"<<endl;
}

inline bool StiLocalTrackSeedFinder::isReset()
{
  bool state = _reset;
  if (_reset) _reset = false;
  return state;
}

inline Filter<StiTrack> * StiLocalTrackSeedFinder::getTrackFilter() 
{
  return &_trackFilter;
}

#endif
