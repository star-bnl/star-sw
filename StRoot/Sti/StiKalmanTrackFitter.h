#ifndef StiKalmanTrackFitter_H
#define StiKalmanTrackFitter_H
#include "StiTrackFitter.h"
#include "StiKalmanTrackFitterParameters.h"
class StiTrack;
class EditableParameters;

///Class implements a kalman track fitter 
///Based on the abstract interface StiTrackFitter
///Uses the fitting parameters carried by StiKalmanTrackFitterParameters
class StiKalmanTrackFitter : public StiTrackFitter
{
 public:
  
  StiKalmanTrackFitter();
  virtual ~StiKalmanTrackFitter();
  virtual void fit(StiTrack * track, int direction);
  void setParameters(const StiKalmanTrackFitterParameters & par);
  virtual EditableParameters & getParameters();
 
 protected:
  StiKalmanTrackFitterParameters  _pars;
  
};

#endif
