#ifndef StiRootDrawableStiEvaluableTrack_HH
#define StiRootDrawableStiEvaluableTrack_HH
#include "Sti/StiEvaluableTrack.h"
#include "StiGui/StiRootDrawableTrack.h"
/*!
 Work class used to display StiEvaluable tracks with ROOT
 \author M. Miller
*/
class StiRootDrawableStiEvaluableTrack : public StiEvaluableTrack, public StiRootDrawableTrack
{
public:
    StiRootDrawableStiEvaluableTrack();
    virtual ~StiRootDrawableStiEvaluableTrack();
    virtual void fillHitsForDrawing(); 
    virtual void reset();
protected:
    void setLineInfo();
};

#endif
