#ifndef StiTrackFitter_H
#define StiTrackFitter_H 1

class StiTrack;
class EditableParameters;

class StiTrackFitter 
{
public:
    StiTrackFitter()
      {	}
    virtual ~StiTrackFitter()
      {}
    virtual void fit(StiTrack * track, int direction)=0;
    virtual EditableParameters * getParameters()=0;
};

#endif
