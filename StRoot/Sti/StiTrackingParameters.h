/*!
 * $Id: StiTrackingParameters.h,v 2.3 2003/10/28 15:55:42 andrewar Exp $  
 *
 * $Log: StiTrackingParameters.h,v $
 * Revision 2.3  2003/10/28 15:55:42  andrewar
 * Added set method for parameters from input txt file.
 *
 * Revision 2.2  2003/07/30 17:04:20  andrewar
 * Added log and version id bars.
 *
 *
 */

#ifndef StiTrackingParameters_H
#define StiTrackingParameters_H 1

#include "Sti/Base/EditableParameters.h"
#include "Stiostream.h"
class StiTracking;
class StiKalmanTrackNode;
class StiKalmanTrack;

using std::iostream;
using std::ifstream;


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
  void setPar(ifstream& inFile);
  
  friend ostream& operator<<(ostream& os, const StiTrackingParameters& par);

  bool   active() const;
  double getMinSearchWindow() const;
  double getMaxSearchWindow() const;
  double getSearchWindowScale() const;
  double getMaxChi2ForSelection() const;

 protected:
  bool   _active;
  bool   _used;
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

inline bool StiTrackingParameters::active() const
{
  return _active;
}


#endif
