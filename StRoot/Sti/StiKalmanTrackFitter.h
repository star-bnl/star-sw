#ifndef StiKalmanTrackFitter_H
#define StiKalmanTrackFitter_H 1

#include "StiTrackFitter.h"
#include "StiKalmanTrackNode.h"

class StiKalmanTrackFitter : public StiTrackFitter
{
public:
    
    //_c-tor/d-tor__________________________________________________
    StiKalmanTrackFitter()
			{}
    virtual ~StiKalmanTrackFitter()
			{}
    
    //_action method
    virtual void fit(StiTrack * track); //throw (Exception);
    
};

#endif
