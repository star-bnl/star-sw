/**********************************************************************
 * Author: Mike Daugherity 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for Gevsim event generator
 *
 **********************************************************************/
#ifndef __STESTRUCTGEVSIM__H
#define __STESTRUCTGEVSIM__H

#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"

#include "TROOT.h"
#include "StEStructPool/Gevsim/TGeVSim.h"

class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructGevsim : public StEStructEventReader {

  TGeVSim *mgevsim;
  int meventCount;
  int meventsToDo;
  bool mAmDone;
  int mrefMult;

  TClonesArray *mtrackArray;

  void fillTracks(StEStructEvent* estructEvent);

 public:

  StEStructGevsim();
  StEStructGevsim(int nevents, TGeVSim* gevsim, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts);

  virtual ~StEStructGevsim(){};
  bool hasGenerator();

  virtual StEStructEvent* next();
  virtual bool         done();
  virtual StEStructEvent* generateEvent();

  ClassDef(StEStructGevsim,1)
};


inline bool StEStructGevsim::done(){ return mAmDone; };


#endif

