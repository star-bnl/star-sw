#ifndef StiRootDrawableMcTrack_HH
#define StiRootDrawableMcTrack_HH
#include "StiGui/StiRootDrawableTrack.h"
class StMcTrack;

/*! \class StiRootDrawableMcTrack
  Work class used to display Monte Carlo tracks with ROOT.
  Instances of this class hold a pointer to the actual track.
  \author Claude A Pruneau
*/
class StiRootDrawableMcTrack : public StiRootDrawableTrack
{
public:
    StiRootDrawableMcTrack();
    virtual ~StiRootDrawableMcTrack();
    virtual void fillHitsForDrawing();
    void getNewState();
protected:
    StMcTrack * mcTrack;
};

#endif
