/// \File StiLocalTrackSeedFinder.h
/// \author M.L. Miller (Yale Software) 10/01
#ifndef StiLocalTrackSeedFinder_HH
#define StiLocalTrackSeedFinder_HH
#include "Stiostream.h"
using std::ostream;
#include <vector>
using std::vector;
#include "StiHelixCalculator.h"
#include "StiHelixFitter.h"
#include "StiTrackSeedFinder.h"
#include "StiSortedHitIterator.h"
class StiHitContainer;
class Sti2HitComboFilter;
class StiDetector;
#include "Sti/Base/EditableParameters.h"

/// \class StiLocalTrackSeedFinder
/// StiLocalTrackSEedFinder is a concrete implementation of StiTrackSeedFinder.
/// It is built from a collection of StiDetectorObjects, which it
/// stores in a vector and orders properly, then uses each detector
/// as a layer from which a one-point seed can be generated.  It then proceeds
/// to step inwards, iteratively making a local decision at each step.
/// \author M.L. Miller (Yale Software) 10/01
/// \author Claude A Pruneau (Wayne State) Jan 2003
class StiLocalTrackSeedFinder : public StiTrackSeedFinder
{
public:
  StiLocalTrackSeedFinder(const string& name,
			  Factory<StiKalmanTrack> * trackFactory,
			  StiHitContainer         * hitContainer,
			  StiDetectorContainer    * detectorContainer);
  virtual ~StiLocalTrackSeedFinder();
  virtual bool hasMore();
  virtual StiKalmanTrack* next();
  virtual void reset();
  virtual void initialize();
  virtual void addLayer(StiDetector*);
  virtual void print() const;
  
protected:
  StiSortedHitIterator begin();
  StiSortedHitIterator end();
  ///Extend hit looking for closest neighbor in z
  bool extendHit(StiHit * hit);
  ///Extrapolate to next layer using straight line, add hit closest in z
  bool extrapolate();
  StiKalmanTrack* initializeTrack(StiKalmanTrack*);
  void calculate(StiKalmanTrack*);
  void calculateWithOrigin(StiKalmanTrack*);
  //Perform helix fit, Perform helix calculation (doesn't assume any vertex)
  bool fit(StiKalmanTrack*);
  typedef vector<StiHit*> HitVec; 
  typedef vector<StiDetector*> DetVec;
 StiSortedHitIterator _hitIter;
  virtual StiKalmanTrack* makeTrack(StiHit*);
  DetVec mDetVec;

  //define search window in the next layer when connecting two points
  double mDeltaY;
  double mDeltaZ;
  //define the number of points to connect
  int mSeedLength;
  //define search window in the next layer when extending a coonection of points
  double mExtrapDeltaY;
  double mExtrapDeltaZ;
  //Count how many hits we've skipped in extrapolation
  int mSkipped;
  //Define the max number we can skip
  int mMaxSkipped;
  //define the Min/Max number of points to extrapolate
  int mExtrapMinLength;
  int mExtrapMaxLength;
  //Use the origin to calculate helix?
  bool mUseOrigin;
  vector<StiHit*> mSeedHitVec;
  bool mDoHelixFit; //true-> fit, false-> calculate
  StiHelixCalculator mHelixCalculator;
  StiHelixFitter mHelixFitter;
  
 private:
  //The following are not implemented, as they are non-trivial
  //and the default compiler generated versions will be wrong.
  StiLocalTrackSeedFinder();
  StiLocalTrackSeedFinder(const StiLocalTrackSeedFinder&);
  StiLocalTrackSeedFinder operator=(const StiLocalTrackSeedFinder&);
};

inline bool StiLocalTrackSeedFinder::hasMore()
{
  return _hitIter!=end();
}

///Return an iterator pointing to the first hit of the current event
inline StiSortedHitIterator  StiLocalTrackSeedFinder::begin()
{
  return StiSortedHitIterator(getHitContainer(),mDetVec.begin(), mDetVec.end());
}

/// return an iterator pointing to no hit
inline StiSortedHitIterator  StiLocalTrackSeedFinder::end()
{
  return StiSortedHitIterator();
}

inline void StiLocalTrackSeedFinder::reset()
{
  _messenger <<"StiLocalTrackSeedFinder::reset() -I- Started"<<endl;
  _hitIter = begin();
  //Cleanup the base-class
  StiTrackSeedFinder::reset();
  _messenger <<"StiLocalTrackSeedFinder::reset() -I- Done"<<endl;
}

struct RPhiLessThan
{
    bool operator()(const StiDetector*, const StiDetector*);
};

struct ScaleHitError
{
    ScaleHitError(double val) : scale(val) {};
    double scale;
    void operator()(StiHit*) const;
};

#endif
