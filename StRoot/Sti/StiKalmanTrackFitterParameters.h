#ifndef StiKalmanTrackFitterParameters_H
#define StiKalmanTrackFitterParameters_H
#include "Sti/Base/EditableParameters.h"

class StiKalmanTrackFitterParameters : public EditableParameters
{
public: 
  StiKalmanTrackFitterParameters();
  StiKalmanTrackFitterParameters(const string & name, const string & description);
  StiKalmanTrackFitterParameters(const StiKalmanTrackFitterParameters & pars);
  virtual ~StiKalmanTrackFitterParameters();
  const StiKalmanTrackFitterParameters & operator=(const StiKalmanTrackFitterParameters & p);
  void setMaxChi2(double maxChi2);
  double getMaxChi2() const;
  void initialize(); 
 protected:
  double _maxChi2; 
};

inline void StiKalmanTrackFitterParameters::setMaxChi2(double maxChi2)
{
  _maxChi2 = maxChi2;
}

inline  double StiKalmanTrackFitterParameters::getMaxChi2() const
{
  return _maxChi2; 
}


#endif
