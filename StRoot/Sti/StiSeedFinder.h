//StiSeedFinder.h
//M.L. Miller (Yale Software)
//03/01

//Abstract base class for track seed finding for the tracker

#ifndef StiSeedFinder_HH
#define StiSeedFinder_HH

#include <utility>

class StiKalmanTrack;
class StTrack;

class StiSeedFinder
{
public:
    StiSeedFinder();
    StiSeedFinder(const StiSeedFinder&);
    virtual ~StiSeedFinder();

    //User interface
    virtual bool hasMore() = 0;
    virtual StiKalmanTrack* next() = 0;
    
protected:
private:
};

#endif

