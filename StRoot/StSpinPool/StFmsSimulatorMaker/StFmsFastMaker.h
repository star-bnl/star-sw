// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 Jan 2011
//

#ifndef ST_FMS_FAST_MAKER_H
#define ST_FMS_FAST_MAKER_H

class StMcEvent;
class StEvent;
class StMcCalorimeterHit;
class StFmsHit;

#include "StMaker.h"

class StFmsFastMaker : public StMaker {
public:
  StFmsFastMaker(const char* name = "fmsfastsimu") : StMaker(name) {}

  int Make();

private:
  StFmsHit* makeFmsHit(const StMcCalorimeterHit* mcHit) const;
  int getDetectorId(const StMcCalorimeterHit* mcHit) const;
  void fillStEvent(StMcEvent*, StEvent*);

  ClassDef(StFmsFastMaker,0);
};

#endif	// ST_FMS_FAST_MAKER_H
