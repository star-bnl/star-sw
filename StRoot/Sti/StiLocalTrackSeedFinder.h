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

#include <iostream>
using std::ostream;

#include <vector>
using std::vector;

#include "StiHelixCalculator.h"
#include "StiHelixFitter.h"
#include "StiTrackSeedFinder.h"

class StiHitContainer;
class Sti2HitComboFilter;
class StiDetector;

class StiLocalTrackSeedFinder : public StiTrackSeedFinder
{
public:
    StiLocalTrackSeedFinder(StiDetectorContainer*, StiHitContainer*);
    virtual ~StiLocalTrackSeedFinder();

    //Implementation of Observer pattern
    virtual void getNewState();
    
    //Inherited interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void reset();

    virtual void addLayer(StiDetector*);
    virtual void print() const;

private:
    void increment();
    void initHitVec();

    ///Extend hit looking for closest neighbor in z
    bool extendHit(StiHit* hit);

    ///Extrapolate to next layer using straight line, add hit closest in z
    bool extrapolate();

    StiKalmanTrack* initializeTrack(StiKalmanTrack*);
    void calculate(StiKalmanTrack*);
    void calculateWithOrigin(StiKalmanTrack*);
    
    //Perform helix fit, Perform helix calculation (doesn't assume any vertex)
    bool fit(StiKalmanTrack*);
    
    //This is just for testing
    void triggerPartition();


protected:
    typedef vector<StiDetector*> DetVec;
    typedef vector<StiHit*> HitVec;

    virtual StiKalmanTrack* makeTrack(StiHit*);
    
    DetVec mDetVec;
    DetVec::iterator mCurrentDet;

    //Trigger hit-container partition on change in start radius
    double mCurrentRadius;

    //Store iterators to the hits for a given starting detector
    HitVec::iterator mHitsBegin;
    HitVec::iterator mHitsEnd;
    HitVec::iterator mCurrentHit;

    //define search window in the next layer when connecting two points
    double mDeltaY;
    double mDeltaZ;
    //define the number of points to connect
    unsigned int mSeedLength;

    //define search window in the next layer when extending a coonection of points
    double mExtrapDeltaY;
    double mExtrapDeltaZ;
    //Count how many hits we've skipped in extrapolation
    unsigned int mSkipped;
    //Define the max number we can skip
    unsigned int mMaxSkipped;
    //define the Min/Max number of points to extrapolate
    unsigned int mExtrapMinLength;
    unsigned int mExtrapMaxLength;

    //Use the origin to calculate helix?
    bool mUseOrigin;

    HitVec mSeedHitVec;
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

//Non-members

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
