#ifndef StiKalmanTrackFinderParameters_H
#define StiKalmanTrackFinderParameters_H 1

#include "Sti/Base/EditableParameters.h"
#include "TDataSet.h"

class StiKalmanTrackFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;

class StiKalmanTrackFinderParameters : public EditableParameters
{
  friend class StiKalmanTrackFinder;
  friend class StiKalmanTrack;
  friend class StiKalmanTrackNode;
public: 
  StiKalmanTrackFinderParameters();
  StiKalmanTrackFinderParameters(const StiKalmanTrackFinderParameters & pars);
  ~StiKalmanTrackFinderParameters();
  const StiKalmanTrackFinderParameters & operator=(const StiKalmanTrackFinderParameters & p);

  void setElossCalculated(bool option);
  void setMCSCalculated(bool option);
  void setField(double f);
  void setMassHypothesis(double m);
  void setMinContiguousHitCount(int count);
  void setMaxNullCount(int count);
  void setMaxContiguousNullCount(int count);
  double getMassHypothesis() const;
  void   initialize();
  virtual void loadDS(TDataSet&);
  virtual void loadFS(ifstream& inFile);
  friend ostream& operator<<(ostream& os, const StiKalmanTrackFinderParameters& par);

 protected:
  bool   useMcAsRec;
  bool   elossCalculated;
  bool   mcsCalculated; 
  double field; 
  int    maxNullCount;
  int    maxContiguousNullCount; 
  int    minContiguousHitCountForNullReset;
  double maxChi2Vertex;
  double massHypothesis;
  double maxDca2dZeroXY;
  double maxDca3dVertex;
};

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

inline  double   StiKalmanTrackFinderParameters::getMassHypothesis() const
{
  return massHypothesis;
}
#endif
