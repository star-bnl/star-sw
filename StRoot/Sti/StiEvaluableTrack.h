//StiEvaluableTrack.h
//M.L. Miller (Yale Software)
//06/01

#ifndef StiEvaluableTrack_HH
#define StiEvaluableTrack_HH

#include "StiTrack.h"

class StTrack;
class StMcTrack;

class StiEvaluableTrack : public StiTrack
{
public:
    StiEvaluableTrack();
    virtual ~StiEvaluableTrack();

    void setStTrack(StTrack*);
    void setStMcTrack(StMcTrack*);
    
    StTrack* stTrack() const;
    StMcTrack* stMcTrack() const;
    
protected:
    StTrack* msttrack;
    StMcTrack* mstmctrack;
    
private:
};

#endif
