//StiStEventTrackSeedFinder.h
//M.L. Miller (Yale Software)
//04/01

//Class to make StiTrack seeds from any type of track in the StEvent track node.
//Used access to StiTracks via the enforced interface of hasMore() and next()

#ifndef StiStEventTrackSeedFinder_HH
#define StiStEventTrackSeedFinder_HH

#include <vector>
#include "StiSeedFinder.h"
#include "StEvent/StEnumerations.h"

class StEvent;
class StiDummyTrack;
class StTrack;
class StiStTrackFilter;

class StiStEventTrackSeedFinder : public StiSeedFinder
{
    typedef vector<StiStTrackFilter*> st_trackfilter_vector;
    
public:
    StiStEventTrackSeedFinder();
    virtual ~StiStEventTrackSeedFinder();
    
    //Sets
    void setEvent(StEvent*);
    void addStTrackFilter(StiStTrackFilter*);
    void setStTrackType(StTrackType);
    
    //User query interface to StiTracks
    virtual bool hasMore();
    virtual sti_track_pair* next();

    //Utilites
    void printStTracks() const;
    
private:
    StEvent* mevent; //cache pointer
    StTrackType mtype; //enumeration to the type of track we get from StEvent
    st_trackfilter_vector mfiltervector;
    
    sti_track_pair* mtrackpair;
    StSPtrVecTrackNodeIterator mcurrent;
    StSPtrVecTrackNodeIterator mbegin; 
    StSPtrVecTrackNodeIterator mend;
};

#endif



