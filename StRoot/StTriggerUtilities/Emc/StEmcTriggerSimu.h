// -*- mode: C++ -*-
//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 12 Jan 2009
//

#ifndef ST_EMC_TRIGGER_SIMU_H
#define ST_EMC_TRIGGER_SIMU_H

class StBemcTriggerSimu;
class StEemcTriggerSimu;
struct DSMLayer_EM201_2009;
struct DSMLayer_LD301_2009;
class TCU;

#include "StTriggerUtilities/StVirtualTriggerSimu.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

class StEmcTriggerSimu : public StVirtualTriggerSimu {
public:
  StEmcTriggerSimu();
  virtual ~StEmcTriggerSimu();

  bool isTrigger(int trigId);
  set<int> triggerIds() const;
  StTriggerSimuDecision triggerDecision(int trigId);

  void setHeadMaker(StMaker*) { /* dummy */ }

  void InitRun(int runNumber);
  void Make();

  void setBemc(StBemcTriggerSimu* bemc);
  void setEemc(StEemcTriggerSimu* eemc);

private:
  int get2009_DSMRegisters(int runNumber);
  int defineTriggers(int runNumber);

  TDatime mDBTime;
  StBemcTriggerSimu* mBemc;
  StEemcTriggerSimu* mEemc;
  DSMLayer_EM201_2009* mEM201;
  DSMLayer_LD301_2009* mLD301;
  TCU* mTcu;

  ClassDef(StEmcTriggerSimu,1);
};

#endif // ST_EMC_TRIGGER_SIMU_H
