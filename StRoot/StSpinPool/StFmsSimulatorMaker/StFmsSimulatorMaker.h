// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 Jan 2011
//

#ifndef ST_FMS_SIMULATOR_MAKER_H
#define ST_FMS_SIMULATOR_MAKER_H

class St_g2t_emc_hit;
class g2t_emc_hit_st;
class StFmsHit;
class StEvent;

#include "StMaker.h"

class StFmsSimulatorMaker : public StMaker {
public:
  StFmsSimulatorMaker(const char* name = "fmsSim") : StMaker(name) {}
  int Make();

private:
  void decodeVolumeId(int volumeId, int& ew, int& nstb, int& channel) const;
  int getDetectorId(int ew, int nstb) const;
  StFmsHit* makeFmsHit(const g2t_emc_hit_st&) const;
  void fillStEvent(const St_g2t_emc_hit*, StEvent*);
  void printStEventSummary(const StEvent*);

  ClassDef(StFmsSimulatorMaker,0);
};

#endif	// ST_FMS_SIMULATOR_MAKER_H
