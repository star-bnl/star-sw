//StiLocalTrackSeedFinder.h
//M.L. Miller (Yale Software)
//10/01

/*! \class StiLocalTrackSeedFinder
  StiLocalTrackSEedFinder is a concrete implementation of StiTrackSeedFinder.
  It is built from a collection of StiDetectorObjects, which it
  stores in a vector and orders properly, then uses each detector
  as a layer from which a one-point seed can be generated.  It then proceeds
  to step inwards, iteratively making a local decision at each step.
  
  \author M.L. Miller
  
*/

#ifndef StiLocalTrackSeedFinder_HH
#define StiLocalTrackSeedFinder_HH

#include <vector>
using std::vector;

#include "StiTrackSeedFinder.h"

class StiHitContainer;
class Sti2HitComboFilter;
class StiDetector;
class ostream;

class StiLocalTrackSeedFinder : public StiTrackSeedFinder
{
public:
    StiLocalTrackSeedFinder(StiDetectorContainer*, StiHitContainer*, Sti2HitComboFilter*);
    virtual ~StiLocalTrackSeedFinder();

    //Inherited interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void build();
    virtual void reset();

    virtual void addLayer(StiDetector*);
    virtual void print() const;

protected:
    void increment();
    void initHitVec();
    
protected:
    typedef vector<StiDetector*> DetVec;
    typedef vector<StiHit*> HitVec;

    virtual StiKalmanTrack* makeTrack(StiHit*);
    
    DetVec mDetVec;
    DetVec::iterator mCurrentDet;
    HitVec* mHitVec;
    HitVec::iterator mCurrentHit;
    
private:
    //The following are not implemented, as they are non-trivial
    //and the default compiler generated versions will be wrong.
    StiLocalTrackSeedFinder();
    StiLocalTrackSeedFinder(const StiLocalTrackSeedFinder&);
    StiLocalTrackSeedFinder operator=(const StiLocalTrackSeedFinder&);

    
};

//Non-members

struct RPhiLessThan
{
    bool operator()(const StiDetector*, const StiDetector*);
};

#endif
