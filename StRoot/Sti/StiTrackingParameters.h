#ifndef StiTrackingParameters_H
#define StiTrackingParameters_H 1

#include "Sti/Base/EditableParameters.h"
class StiTracking;
class StiKalmanTrackNode;
class StiKalmanTrack;

class StiTrackingParameters : public EditableParameters
{
public: 
  StiTrackingParameters(const string & name,
			const string & description);
  StiTrackingParameters(const string & name,
			const string & description,
			double minSearchWindow,
			double maxSearchWindow,
			double searchWindowScaling,
			double maxChi2ForSelection);
  StiTrackingParameters(const StiTrackingParameters & pars);
  ~StiTrackingParameters();
  const StiTrackingParameters & operator=(const StiTrackingParameters & p);

  virtual void initialize();

  void setMaxChi2ForSelection(double chi);
  void setMinSearchWindow(double val);
  void setMaxSearchWindow(double val);
  void setSearchWindowScaling(double val);

  double getMinSearchWindow() const;
  double getMaxSearchWindow() const;
  double getSearchWindowScale() const;
  double getMaxChi2ForSelection() const;

 protected:
  double _minSearchWindow;
  double _maxSearchWindow;
  double _searchWindowScaling;
  double _maxChi2ForSelection;
};

inline void StiTrackingParameters::setMaxChi2ForSelection(double chi)
{
  _maxChi2ForSelection = chi;
}

inline void StiTrackingParameters::setMinSearchWindow(double val)
{
  _minSearchWindow = val;
}

inline void StiTrackingParameters::setMaxSearchWindow(double val)
{
  _maxSearchWindow = val;
}

inline void StiTrackingParameters::setSearchWindowScaling(double val)
{
  _searchWindowScaling = val;
}


inline double StiTrackingParameters::getMinSearchWindow() const
{
  return _minSearchWindow;
}

inline double StiTrackingParameters::getMaxSearchWindow() const
{
  return _maxSearchWindow;
}

inline double StiTrackingParameters::getSearchWindowScale() const
{
  return _searchWindowScaling;
}

inline double StiTrackingParameters::getMaxChi2ForSelection() const
{
  return _maxChi2ForSelection;
}


#endif
