#ifndef StiDrawableTrack_HH
#define StiDrawableTrack_HH 1
#include "Sti/SubjectObserver.h"

class StiGuiIOBroker;

/*! \class StiDrawableTrack
  Abstract base class used to define the interface to drawable tracks
  \author Claude A Pruneau
*/
class StiDrawableTrack : public Observer
{
public:
    StiDrawableTrack();
    virtual ~StiDrawableTrack();
    virtual void getNewState()=0;
    virtual void fillHitsForDrawing()=0;
    virtual void update();
    virtual void reset()=0;
protected:
    //  StiGuiIOBroker* mBroker;
};

inline void StiDrawableTrack::update()
{
    cout <<"StiDrawableTrack::update() - INFO - Starting."<<endl;
    fillHitsForDrawing();
    cout <<"StiDrawableTrack::update() - INFO - Done."<<endl;
}
#endif
