#ifndef __STESTRUCTAcceptance__H
#define __STESTRUCTAcceptance__H

#include <cmath>

#include "TROOT.h"

class StEStructEventCuts;
class StEStructTrackCuts;


class StEStructAcceptance {

  double  mSigZVertex;
  double  mMaxZVertex;
  double  mMinDetectableRadius;

  bool mgRand2Good;
  double mgRand2;

 public:

  StEStructAcceptance();
  StEStructAcceptance( double sigZVertex,
                       double maxZVertex,
                       double minDetectableRadius );
  virtual ~StEStructAcceptance(){};
  void SetSeed(int iseed);
  void SetSigZVertex(double sigZVertex);
  void SetMaxZVertex(double maxZVertex);
  void SetMinDetectableRadius(double minDetectableRadius);

  double GetNewZVertex();
  bool   isTrackInAcceptance( double VertZ, double pt, double eta );
  double maxRadius(double eta, double pt, double VertZ);

  double  gRand48();

  ClassDef(StEStructAcceptance,1)
};


inline void StEStructAcceptance::SetSigZVertex(double sigZVertex) {
    mSigZVertex = sigZVertex;
};
inline void StEStructAcceptance::SetMaxZVertex(double maxZVertex) {
    mMaxZVertex = maxZVertex;
};
inline void StEStructAcceptance::SetMinDetectableRadius(double minDetectableRadius) {
    mMinDetectableRadius = minDetectableRadius;
};


#endif

