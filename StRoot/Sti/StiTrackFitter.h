#ifndef StiTrackFitter_H
#define StiTrackFitter_H 1

class StiTrack;

class StiTrackFitter 
{
public:
    
    //_c-tor/d-tor__________________________________________________
    StiTrackFitter()
      {
			}
    virtual ~StiTrackFitter()
      {}

    //_action method__________________________________________________
    virtual void fit(StiTrack * track)=0;

};

#endif
