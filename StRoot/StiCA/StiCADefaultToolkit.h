#ifndef StiCADefaultToolkit_H
#define StiCADefaultToolkit_H 1
#include "Sti/StiDefaultToolkit.h"


class StiCADefaultToolkit : public StiDefaultToolkit
{
public:
  
  StiCADefaultToolkit() : StiDefaultToolkit() {}
  virtual Factory<StiKalmanTrack>  *getTrackFactory();

  virtual StiTrackFinder         *getTrackFinder();
};

#endif


