//StiSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiSeedFinder_HH
#define StiSeedFinder_HH

#include <string>
using std::string;

#include "Messenger.h"
#include "StiObjectFactoryInterface.h"

class StiKalmanTrack;
class StTrack;
class Messenger;
class StiHitContainer;

class StiSeedFinder
{
public:
    StiSeedFinder(StiHitContainer*);
    virtual ~StiSeedFinder();

    //Inherited interface
    virtual bool hasMore() = 0;
    virtual StiKalmanTrack* next() = 0;
    virtual void reset() =0;

    ///Set factory
    void setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val);

    ///Set hit container
    void setHitContainer(StiHitContainer*);
    
protected:
    StiObjectFactoryInterface<StiKalmanTrack>* mFactory;
    Messenger& mMessenger;
    StiHitContainer* mHitStore;
    
private:
    StiSeedFinder(); //Not implemented
};

//inlines

inline void StiSeedFinder::setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val)
{
    mFactory=val;
}

inline void StiSeedFinder::setHitContainer(StiHitContainer* val)
{
    mHitStore=val;
}

#endif

