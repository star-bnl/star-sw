//StiEvaluableTrackSeedFinder.h
//M.L. Miller (Yale Software)
//04/01

//Class to make StiKalmanTrack seeds from any type of track in the StEvent track node.
//Used access to StiKalmanTracks via the enforced interface of hasMore() and next()

#ifndef StiEvaluableTrackSeedFinder_HH
#define StiEvaluableTrackSeedFinder_HH

#include <vector>
#include <map>

#include "StiSeedFinder.h"
#include "StEvent/StEnumerations.h"

#include "StiGui/StiRootDrawableStiEvaluableTrack.h" //For factory

class StEvent;
class StiDetector;
class StiTrack;
class StiKalmanTrack;
class StTrack;
class StiStTrackFilter;
class StMcEvent;
class StMcTrack;
class StAssociationMaker;
class StTrackPairInfo;
class StiEvaluableTrackSeedFinder : public StiSeedFinder
{
public:
    typedef vector<StiStTrackFilter*> st_trackfilter_vector;
    //typedef StiObjectFactory<StiEvaluableTrack> StiEvaluableTrackFactory;
    
    StiEvaluableTrackSeedFinder(StAssociationMaker*);
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
    StiEvaluableTrack* makeTrack(StMcTrack*);
    
private:
    StiEvaluableTrackSeedFinder(); //Not implemented
    StAssociationMaker* mAssociationMaker;
    StEvent* mevent;
    StMcEvent* mMcEvent;
    StiEvaluableTrackFactory* mfactory;
    StTrackType mtype; //enumeration to the type of track we get from StEvent
    st_trackfilter_vector mfiltervector;

    vector<StMcTrack*>::iterator mCurrentMc;
    vector<StMcTrack*>::iterator mBeginMc;
    vector<StMcTrack*>::iterator mEndMc;
};

//Stl utility functors
class BestCommonHits
{
public:
    typedef pair<StMcTrack*, StTrackPairInfo*> McToStPair_t;
    
    BestCommonHits();

    void operator()(const McToStPair_t& rhs);
    
    StTrackPairInfo* pair() const {return mPair;}
    
private:
    unsigned int mMostCommon;
    StTrackPairInfo* mPair;
};
    
inline void StiEvaluableTrackSeedFinder::setFactory(StiEvaluableTrackFactory* val)
{
    mfactory=val;
}

#endif


