#ifndef StiRootDrawableTrack_HH
#define StiRootDrawableTrack_HH 1
#include "StiGui/StiGuiIOBroker.h"
#include "StiGui/StiRootDrawableLine.h"
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiTPolyLine3D.h"
#include "StiGui/StiRootDisplayManager.h"
#include "Sti/StiDrawableTrack.h"

/*! \class StiRootDrawableMcTrack
  Abstract base class used to define the interface to drawable tracks
  \author Claude A Pruneau
*/
class StiRootDrawableTrack : public StiDrawableTrack
{
public:
    StiRootDrawableTrack();
    virtual ~StiRootDrawableTrack();
    //virtual void getNewState()=0;
    //virtual void fillHitsForDrawing()=0;
    virtual void update();
    virtual void reset();
protected:
    StiRootDrawableLine* _line;
    StiRootDrawableHits* _hits;
    StiGuiIOBroker* mBroker;
};

inline void StiRootDrawableTrack::update()
{
  //cout <<"StiRootDrawableTrack::update() - INFO - Starting."<<endl;
  // clear the hits and lines of this track
  this->StiRootDrawableTrack::reset();
  // fill the hits
  fillHitsForDrawing();
  //cout <<"StiRootDrawableTrack::update() - INFO - Done."<<endl;
}
#endif
