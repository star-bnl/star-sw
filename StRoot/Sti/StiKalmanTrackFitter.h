#ifndef StiKalmanTrackFitter_H
#define StiKalmanTrackFitter_H 1

#include "StiTrackFitter.h"
#include "StiKalmanTrackNode.h"

enum StiFitMethod {Inward=1, Outward=2};

class StiKalmanTrackFitter : public StiTrackFitter
{
public:
    
    //_c-tor/d-tor__________________________________________________
    StiKalmanTrackFitter();
    virtual ~StiKalmanTrackFitter()
    {}
    
    //_action method
    virtual void fit(StiTrack * track); //throw (Exception);
    
    virtual void fitInward(StiKalmanTrackNode * node); //throw (Exception) ;
    virtual void fitOutward(StiKalmanTrackNode * node); //throw (Exception) ;
    
    //_set/get_ methods
    
    virtual void setFitMethod(StiFitMethod fitMethod);
    virtual StiFitMethod getFitMethod() const;
    
    
protected:
    
    StiFitMethod fitMethod;
};

#endif
