//StiEvaluableTrackSeedFinder.h
//M.L. Miller (Yale Software)
//04/01

//Class to make StiKalmanTrack seeds from any type of track in the StEvent track node.
//Used access to StiKalmanTracks via the enforced interface of hasMore() and next()

#ifndef StiEvaluableTrackSeedFinder_HH
#define StiEvaluableTrackSeedFinder_HH

#include <vector>
#include "StiSeedFinder.h"
#include "StEvent/StEnumerations.h"

#include "StiFactoryTypedefs.h"
//#include "StiEvaluableTrack.h"
//#include "StiObjectFactory.h"

class StEvent;
class StiKalmanTrack;
class StTrack;
class StiStTrackFilter;
class StMcEvent;

class StiEvaluableTrackSeedFinder : public StiSeedFinder
{
public:
    typedef vector<StiStTrackFilter*> st_trackfilter_vector;
    //typedef StiObjectFactory<StiEvaluableTrack> StiEvaluableTrackFactory;
    
    StiEvaluableTrackSeedFinder();
    virtual ~StiEvaluableTrackSeedFinder();
    
    //Sets
    void setEvent(StEvent*, StMcEvent* mcevt=0);
    void setFactory(StiEvaluableTrackFactory* val);
    
    void addStTrackFilter(StiStTrackFilter*);
    void setStTrackType(StTrackType);
    
    //User query interface to StiKalmanTracks
    virtual bool hasMore();
    virtual StiKalmanTrack* next();

    //Utilites
    void printStTracks() const;

protected:
    StiEvaluableTrack* makeTrack(StTrack*);
    
private:
    StEvent* mevent; //cache pointer
    StMcEvent* mmcevent;
    StiEvaluableTrackFactory* mfactory;
    
    StTrackType mtype; //enumeration to the type of track we get from StEvent
    st_trackfilter_vector mfiltervector;
    
    StSPtrVecTrackNodeIterator mcurrent;
    StSPtrVecTrackNodeIterator mbegin; 
    StSPtrVecTrackNodeIterator mend;
};

inline void StiEvaluableTrackSeedFinder::setFactory(StiEvaluableTrackFactory* val)
{
    mfactory=val;
}

#endif



