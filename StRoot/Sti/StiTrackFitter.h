#ifndef StiTrackFitter_H
#define StiTrackFitter_H 1
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/Loadable.h"

class StiTrack;
class EditableParameters;

class StiTrackFitter //: public Named, public Described, public Loadable
{
public:
    StiTrackFitter() {	}
    virtual ~StiTrackFitter() {}
    virtual void fit(StiTrack * track, int direction)=0;
    virtual EditableParameters & getParameters()=0;
};

#endif
