//StiDrawableTrack.h
/***********************************************************************
 *
 * $Id: StiDrawableTrack.h,v 2.1 2002/12/18 16:15:00 andrewar Exp $
 *
 * \class StiDrawableTrack
 *   Abstract base class used to define the interface to drawable tracks
 *
 * \author Claude A Pruneau
 *
 ***********************************************************************
 *
 * $Log: StiDrawableTrack.h,v $
 * Revision 2.1  2002/12/18 16:15:00  andrewar
 * Fixed reference to SubjectObserver.h to adhere to new directory structure.
 *
 *
 */

#ifndef StiDrawableTrack_HH
#define StiDrawableTrack_HH 1
#include "Sti/Base/SubjectObserver.h"

class StiGuiIOBroker;

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
