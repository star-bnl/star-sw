// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 Jan 2011
//

#ifndef ST_FMS_FAST_MAKER_H
#define ST_FMS_FAST_MAKER_H

class St_g2t_emc_hit;
class g2t_emc_hit_st;
class StFmsHit;
class StEvent;

#include "StMaker.h"

class StFmsFastMaker : public StMaker {
public:
  StFmsFastMaker(const char* name = "fmsfastsimu") : StMaker(name) {}
  int Make();

private:
  int getDetectorId(int ew, int nstb) const;
  StFmsHit* makeFmsHit(const g2t_emc_hit_st&) const;
  void fillStEvent(const St_g2t_emc_hit*, StEvent*);

  ClassDef(StFmsFastMaker,0);
};

#endif	// ST_FMS_FAST_MAKER_H
