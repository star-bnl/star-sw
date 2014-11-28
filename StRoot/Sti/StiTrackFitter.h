#ifndef StiTrackFitter_H
#define StiTrackFitter_H 1
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"

class StiTrack;
class EditableParameters;

class StiTrackFitter {
public:
    StiTrackFitter() {	}
    virtual ~StiTrackFitter() {}
    virtual int fit(StiTrack * track, int direction)=0;
};

#endif
