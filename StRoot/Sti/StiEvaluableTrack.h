//StiEvaluableTrack.h
//M.L. Miller (Yale Software)
//06/01

#ifndef StiEvaluableTrack_HH
#define StiEvaluableTrack_HH

#include "StiKalmanTrack.h"

class StTrackPairInfo;

class StiEvaluableTrack : public StiKalmanTrack
{
public:
    StiEvaluableTrack();
    virtual ~StiEvaluableTrack();

    //set
    void setStTrackPairInfo(StTrackPairInfo*);

    //get
    StTrackPairInfo* stTrackPairInfo() const;

    //action
    virtual void reset();
    
protected:
    StTrackPairInfo* mPair;
    
private:
};

inline void StiEvaluableTrack::setStTrackPairInfo(StTrackPairInfo* val)
{
    mPair = val;
}

inline StTrackPairInfo* StiEvaluableTrack::stTrackPairInfo() const
{
    return mPair;
}


#endif
