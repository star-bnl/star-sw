#ifndef StiRootDrawableKalmanTrack_HH
#define StiRootDrawableKalmanTrack_HH
#include "Sti/StiKalmanTrack.h"
#include "StiGui/StiRootDrawableTrack.h"

/*! \class StiRootDrawableKalmanTrack
  Work class used to display Kalman tracks with ROOT
  \author M.L. Miller (Yale Software)
*/
class StiRootDrawableKalmanTrack : public StiKalmanTrack, public StiRootDrawableTrack
{
public:
    StiRootDrawableKalmanTrack();
    virtual ~StiRootDrawableKalmanTrack();
    virtual void fillHitsForDrawing();
    virtual void reset();

protected:
    void getNewState();
};

#endif
