//StiRootDrawableStiEvaluableTrack.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiRootDrawableStiEvaluableTrack_HH
#define StiRootDrawableStiEvaluableTrack_HH

#include "Sti/StiEvaluableTrack.h"

#include "StiRootDrawableLine.h"

class StiRootDrawableStiEvaluableTrack : public StiEvaluableTrack, public StiRootDrawableLine
{
public:
    StiRootDrawableStiEvaluableTrack();
    virtual ~StiRootDrawableStiEvaluableTrack();

    virtual void update();
    virtual void fillHitsForDrawing();
    virtual void reset();
    
protected:

private:
    
};

#endif
