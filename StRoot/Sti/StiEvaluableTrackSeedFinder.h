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
#include "StiGui/StiRootDrawableStiEvaluableTrack.h" //For factory

class StiKalmanTrack;
class StMcEvent;
class StMcTrack;
class StAssociationMaker;
class StTrackPairInfo;

class StiEvaluableTrackSeedFinder : public StiSeedFinder
{
public:
    StiEvaluableTrackSeedFinder(StAssociationMaker*);
    virtual ~StiEvaluableTrackSeedFinder();
    
    //Sets
    void setEvent(StMcEvent* mcevt=0);
    void setFactory(StiEvaluableTrackFactory* val);
    
    //User query interface to StiKalmanTracks
    virtual bool hasMore();
    virtual StiKalmanTrack* next();

protected:
    StiEvaluableTrack* makeTrack(StMcTrack*);
    
private:
    StiEvaluableTrackSeedFinder(); //Not implemented, gotta have association maker
    
    StAssociationMaker* mAssociationMaker;
    StMcEvent* mMcEvent;
    StiEvaluableTrackFactory* mfactory;

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


