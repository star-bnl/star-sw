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
    virtual void update(Subject* changedSubject);
    virtual void forgetSubject(Subject* theObsoleteSubject);
    
    //Inherited interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void reset();

    virtual void addLayer(StiDetector*);
    virtual void print() const;

protected:
    void increment();
    void initHitVec();
    bool extendHit(StiHit* hit);
    void initializeTrack(StiKalmanTrack*);
    void calculate(StiKalmanTrack*);
    void calculateWithOrigin(StiKalmanTrack*);
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

    //Subject
    Subject* mSubject;
    
    //define search window in the next layer
    double mDeltaY;
    double mDeltaZ;
    unsigned int mSeedLength;
    bool mUseOrigin;

    HitVec mSeedHitVec;
    StiHelixCalculator mHelixCalculator;
    
private:
    //The following are not implemented, as they are non-trivial
    //and the default compiler generated versions will be wrong.
    StiLocalTrackSeedFinder();
    StiLocalTrackSeedFinder(const StiLocalTrackSeedFinder&);
    StiLocalTrackSeedFinder operator=(const StiLocalTrackSeedFinder&);

    
};

//inlines
inline void StiLocalTrackSeedFinder::update(Subject* changedSubject)
{
    cout <<"StiLocalTrackSeedFinder::update(Subject*)"<<endl;
    if (changedSubject!=mSubject) {
	cout <<"StiLocalTrackSeedFinder::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	cout <<"getting new values"<<endl;
	getNewState();
	cout <<"\tdone getting new values"<<endl;
    }   
}

inline void StiLocalTrackSeedFinder::forgetSubject(Subject* obsolete)
{
    cout <<"StiLocalTrackSeedFinder::forgetSubject(Subject*)"<<endl;
    if (obsolete==mSubject) {
	mSubject=0;
    }
    else {
	cout <<"StiLocalTrackSeedFinder::forgetSubject(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
}


//Non-members

struct RPhiLessThan
{
    bool operator()(const StiDetector*, const StiDetector*);
};

#endif
