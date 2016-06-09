// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 Jan 2011
//

#ifndef ST_FMS_SIMULATOR_MAKER_H
#define ST_FMS_SIMULATOR_MAKER_H

class StMcCalorimeterHit;
class StMcEvent;
class StFmsHit;
class StEvent;
class StFmsDbMaker;

#include "StMaker.h"

class StFmsSimulatorMaker : public StMaker {
public:
  StFmsSimulatorMaker(const char* name = "fmsSim") : StMaker(name) {}
  int Make();

private:
  int getDetectorId(int ew, int nstb) const;
  StFmsHit* makeFmsHit(const StMcCalorimeterHit*) const;
  void fillStEvent(const StMcEvent*, StEvent*);
  void printStEventSummary(const StEvent*);

  StFmsDbMaker*      mFmsDbMaker=0;  //!

  ClassDef(StFmsSimulatorMaker,0);
};

#endif	// ST_FMS_SIMULATOR_MAKER_H
