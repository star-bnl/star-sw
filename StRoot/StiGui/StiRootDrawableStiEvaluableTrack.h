//StiRootDrawableStiEvaluableTrack.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiRootDrawableStiEvaluableTrack_HH
#define StiRootDrawableStiEvaluableTrack_HH

#include "Sti/StiEvaluableTrack.h"

//#include "StiRootDrawableLine.h"
class StiRootDrawableLine;
class StiRootDrawableHits;

class StiRootDrawableStiEvaluableTrack : public StiEvaluableTrack
{
public:
    StiRootDrawableStiEvaluableTrack();
    virtual ~StiRootDrawableStiEvaluableTrack();

    virtual void update();
    virtual void fillHitsForDrawing();
    virtual void reset();
    
protected:
    StiRootDrawableLine* mLine;
    StiRootDrawableHits* mHits;
    
private:
    void setLineInfo();    
};

#endif
