#ifndef STL3NUCLEI_HH
#define STL3NUCLEI_HH

#include "StL3RareTrackCut.h"
#include <Stiostream.h>

class StGlobalTrack;

class StL3Nuclei : public StL3RareTrackCut {

 public:
  StL3Nuclei(float pminin = 0.2, float pmaxin=10.0,int charge = 0, int mass = 2);
  ~StL3Nuclei(){};
  int  Accept(StGlobalTrack* track);
  void Report();

 private:
  float pcut[2];
  int chargeToAccept;  //<0 = negatives, 0 = all, >0 = positives
  int minMass;

  ClassDef(StL3Nuclei,1)

};

#endif
