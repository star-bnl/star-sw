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

class StiSeedFinder
{
public:
    StiSeedFinder();
    virtual ~StiSeedFinder();

    //Inherited interface
    virtual bool hasMore() = 0;
    virtual StiKalmanTrack* next() = 0;
    virtual void reset() =0;

    //SetFactory
    void setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val);
    
protected:
    StiObjectFactoryInterface<StiKalmanTrack>* mFactory;
    Messenger& mMessenger;
    
private:
};

//inlines

inline void StiSeedFinder::setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val)
{
    mFactory=val;
}



#endif

