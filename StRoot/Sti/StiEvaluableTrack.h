//StiEvaluableTrack.h
//M.L. Miller (Yale Software)
//06/01

#ifndef StiEvaluableTrack_HH
#define StiEvaluableTrack_HH

#include "StiKalmanTrack.h"

class StTrack;
class StMcTrack;

class StiEvaluableTrack : public StiKalmanTrack
{
public:
    StiEvaluableTrack();
    virtual ~StiEvaluableTrack();

    void setStTrack(StTrack*);
    void setStMcTrack(StMcTrack*);
    
    StTrack* stTrack() const;
    StMcTrack* stMcTrack() const;

    virtual void reset();
    
protected:
    StTrack* msttrack;
    StMcTrack* mstmctrack;
    
private:
};

#endif
