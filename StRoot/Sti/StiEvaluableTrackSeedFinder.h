//StiEvaluableTrackSeedFinder.h
//M.L. Miller (Yale Software)
//04/01

//Class to make StiTrack seeds from any type of track in the StEvent track node.
//Used access to StiTracks via the enforced interface of hasMore() and next()

#ifndef StiEvaluableTrackSeedFinder_HH
#define StiEvaluableTrackSeedFinder_HH

#include <vector>
#include "StiSeedFinder.h"
#include "StEvent/StEnumerations.h"

class StEvent;
class StiTrack;
class StTrack;
class StiStTrackFilter;
class StMcEvent;

class StiEvaluableTrackSeedFinder : public StiSeedFinder
{
    typedef vector<StiStTrackFilter*> st_trackfilter_vector;
    
public:
    StiEvaluableTrackSeedFinder();
    virtual ~StiEvaluableTrackSeedFinder();
    
    //Sets
    void setEvent(StEvent*, StMcEvent* mcevt=0);
    
    void addStTrackFilter(StiStTrackFilter*);
    void setStTrackType(StTrackType);
    
    //User query interface to StiTracks
    virtual bool hasMore();
    virtual StiTrack* next();

    //Utilites
    void printStTracks() const;
    
private:
    StEvent* mevent; //cache pointer
    StMcEvent* mmcevent;
    
    StTrackType mtype; //enumeration to the type of track we get from StEvent
    st_trackfilter_vector mfiltervector;
    
    StSPtrVecTrackNodeIterator mcurrent;
    StSPtrVecTrackNodeIterator mbegin; 
    StSPtrVecTrackNodeIterator mend;
};

#endif



