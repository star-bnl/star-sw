#ifndef StiTrackFitter_H
#define StiTrackFitter_H 1

#include "StiTrack.h"

class StiTrackFitter 
{
public:
    
    //_c-tor/d-tor__________________________________________________
    StiTrackFitter()
      {
				unitCharge = 1;
			}
    virtual ~StiTrackFitter()
      {}

    //_action method__________________________________________________
    virtual void fit(StiTrack * track)=0;

		double unitCharge;

protected:

};

#endif
