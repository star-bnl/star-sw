#ifndef StiKalmanTrackFinderParameters_H
#define StiKalmanTrackFinderParameters_H 1

#include "Sti/Base/EditableParameters.h"
#include "TDataSet.h"

class StiKalmanTrackFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;
class KalmanTrackFinderParameters_st;

class StiKalmanTrackFinderParameters : public EditableParameters
{
public: 
  StiKalmanTrackFinderParameters();
  StiKalmanTrackFinderParameters(const StiKalmanTrackFinderParameters & pars);
  ~StiKalmanTrackFinderParameters();
  const StiKalmanTrackFinderParameters & operator=(const StiKalmanTrackFinderParameters & p);
  const StiKalmanTrackFinderParameters & operator=(const KalmanTrackFinderParameters_st & p);

  void setUseTrackFilter(bool option);
  void setElossCalculated(bool option);
  void setMCSCalculated(bool option);
  void setField(double f);
  void setMassHypothesis(double m);
  void setMinContiguousHitCount(int count);
  void setMaxNullCount(int count);
  void setMaxContiguousNullCount(int count);
  bool getUseTrackFilter() const; 
  double getMinSearchWindow() const;
  double getMaxSearchWindow() const;
  double getSearchWindowScale() const;
  double getMassHypothesis() const;
  void   initialize();
  friend class StiKalmanTrackFinder;
  friend class StiKalmanTrack;
  friend class StiKalmanTrackNode;
	void load(TDataSet*);
  
 protected:
  bool   useMcAsRec;
  bool   useTrackFilter;
  bool   elossCalculated;
  bool   mcsCalculated; 
  double field; 
  int    maxNullCount;
  int    maxContiguousNullCount; 
  int    minContiguousHitCountForNullReset;
  double maxChi2Vertex;
  double massHypothesis;
};

//inline  double StiKalmanTrackFinderParameters::getOuterScaling() const
//{
//  return outerScaling;
//}

//inline double StiKalmanTrackFinderParameters::getInnerScaling() const
//{
//  return innerScaling;
//}
  
inline   void StiKalmanTrackFinderParameters::setUseTrackFilter(bool option)
{
  useTrackFilter = option;
}

inline   void StiKalmanTrackFinderParameters::setElossCalculated(bool option)
{
  elossCalculated = option;
}

inline   void StiKalmanTrackFinderParameters::setMCSCalculated(bool option)
{
  mcsCalculated = option;
}

inline   void StiKalmanTrackFinderParameters::setField(double f)
{
  field = f;
}

inline   void StiKalmanTrackFinderParameters::setMassHypothesis(double m)
{
  massHypothesis = m;
}

inline   void   StiKalmanTrackFinderParameters::setMinContiguousHitCount(int count)
{
  minContiguousHitCountForNullReset = count;
}

inline   void   StiKalmanTrackFinderParameters::setMaxNullCount(int count)
{
  maxNullCount = count;
}

inline   void   StiKalmanTrackFinderParameters::setMaxContiguousNullCount(int count)
{
  maxContiguousNullCount = count;
}

//inline   void   StiKalmanTrackFinderParameters::setMaxChi2ForSelection(double chi)
//{
//  maxChi2ForSelection = chi;
//}

//inline   void   StiKalmanTrackFinderParameters::setMinSearchWindow(double val)
//{
//  minSearchWindow = val;
//}

//inline   void   StiKalmanTrackFinderParameters::setMaxSearchWindow(double val)
//{
//  maxSearchWindow = val;
//}

//inline   void   StiKalmanTrackFinderParameters::setSearchWindowScale(double val)
//{
//  searchWindowScale = val;
//}

inline   bool     StiKalmanTrackFinderParameters::getUseTrackFilter() const
{
  return useTrackFilter;
}

//inline   double   StiKalmanTrackFinderParameters::getMinSearchWindow() const
//{
//  return minSearchWindow;
//}

//inline   double   StiKalmanTrackFinderParameters::getMaxSearchWindow() const
//{
//  return maxSearchWindow;
//}

//inline   double   StiKalmanTrackFinderParameters::getSearchWindowScale() const
//{
//  return searchWindowScale;
//}

inline  double   StiKalmanTrackFinderParameters::getMassHypothesis() const
{
  return massHypothesis;
}
#endif
