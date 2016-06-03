#ifndef StiCAKalmanTrack_H
#define StiCAKalmanTrack_H 1

#include "Sti/StiKalmanTrack.h"



class StiCAKalmanTrack : public StiKalmanTrack 
{
 public:
  
  StiCAKalmanTrack() : StiKalmanTrack()
    {  /* nops */ }
  
   StiKalmanTrackNode * getInnerMostTPCHitNode(int qua=0)   const;

  virtual int initialize(const vector<StiHit*> &);
  int initialize0(const std::vector<StiHit*> &hits, StiNodePars *firstPars = 0, StiNodePars *lastPars = 0, StiNodeErrs *firstErrs = 0, StiNodeErrs *lastErrs = 0);
  virtual int  refit();

};

#endif
