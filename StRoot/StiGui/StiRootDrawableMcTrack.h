#ifndef StiRootDrawableMcTrack_HH
#define StiRootDrawableMcTrack_HH 1

#include "Sti/StiMcTrack.h"
#include "StiGui/StiRootDrawableTrack.h"

/*! \class StiRootDrawableMcTrack
  Work class used to display Monte Carlo tracks with ROOT.
  Instances of this class hold a pointer to the actual track.
  \author Claude A Pruneau
*/
class StiRootDrawableMcTrack : public StiMcTrack, public StiRootDrawableTrack
{
public:
    StiRootDrawableMcTrack();
    virtual ~StiRootDrawableMcTrack();
    virtual void setStMcTrack(const StMcTrack * mcTrack);
    virtual void reset();
};


#endif
