//StiSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiSeedFinder_HH
#define StiSeedFinder_HH

#include <string>
using std::string;

#include "StiObjectFactoryInterface.h"

class StiKalmanTrack;
class StTrack;

class StiSeedFinder
{
public:
    StiSeedFinder();
    virtual ~StiSeedFinder();

    //Inherited interface
    virtual bool hasMore() = 0;
    virtual StiKalmanTrack* next() = 0;
    virtual void build() = 0;
    virtual void reset() =0;

    ///Set the path of the file from which to build the seed-finder.
    void setBuildPath(const string&);

    //SetFactory
    void setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val);
    
protected:
    StiObjectFactoryInterface<StiKalmanTrack>* mFactory;
    bool mBuilt;
    string mBuildPath;
    
private:
};

//inlines

inline void StiSeedFinder::setBuildPath(const string& val)
{
    mBuildPath=val;
}

inline void StiSeedFinder::setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val)
{
    mFactory=val;
}



#endif

