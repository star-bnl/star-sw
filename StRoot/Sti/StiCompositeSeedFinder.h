//StiCompositeSeedFinder.h
//M.L. Miller (Yale Software)
//03/01


#ifndef StiCompositeSeedFinder_HH
#define StiCompositeSeedFinder_HH

#include "StiSeedFinder.h"

class StiKalmanTrack;
class StTrack;

class StiCompositeSeedFinder : public StiSeedFinder
{
public:
    StiCompositeSeedFinder();
    virtual ~StiCompositeSeedFinder();

    //Inherited interface
    virtual bool hasMore();
    virtual StiKalmanTrack* next();
    virtual void build();
    virtual void reset();

protected:
    
private:
};



#endif

