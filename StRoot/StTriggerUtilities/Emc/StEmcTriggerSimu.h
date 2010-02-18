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

#include <set>
using std::set;

#include <map>
using std::map;

#include "StTriggerUtilities/StVirtualTriggerSimu.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

class StEmcTriggerSimu : public StVirtualTriggerSimu {
public:
  StEmcTriggerSimu();
  virtual ~StEmcTriggerSimu();

  bool isTrigger(int trigId);
  set<int> triggerIds() const;
  StTriggerSimuDecision triggerDecision(int trigId);

  // 16-bit output of EM201 DSM
  int EM201output() const;

  // Access to individual bits of EM201 DSM ouput
  int  BHT() const;		// (Bit 0:3) Barrel HT bits (4 bits)
  int  EHT() const;		// (Bit 4:5) Endcap HT bits (2 bits)
  int  JP1() const;		// (Bit 6) JP1, unified over the BEMC+EEMC (1 bit)
  int  JP2() const;		// (Bit 7) JP2, unified over the BEMC+EEMC (1 bit)
  int BJP1() const;		// (Bit 8) BJP1 for the 18 BEMC-only patches (1 bit)
  int BJP2() const;		// (Bit 9) BJP2 for the 18 BEMC-only patches (1 bit)
  int EJP1() const;		// (Bit 10) EJP1 for the 6 EEMC-only patches (1 bit)
  int EJP2() const;		// (Bit 11) EJP2 for the 6 EEMC-only patches (1 bit)
  int  AJP() const;		// (Bit 12) AJP for BEMC and EEMC but NOT the boundary (1 bit)
  int BAJP() const;		// (Bit 13) BAJP for the BEMC-only patches (1 bit)
  int EAJP() const;		// (Bit 14) EAJP for the EEMC-only patches (1 bit)

  void setHeadMaker(StMaker*) { /* dummy */ }

  void InitRun(int runNumber);
  void Make();

  void setBemc(StBemcTriggerSimu* bemc);
  void setEemc(StEemcTriggerSimu* eemc);

  // Use these setters to overwrite thresholds from the database (2009)
  void setOverlapJetPatchTh0(int value);
  void setOverlapJetPatchTh1(int value);
  void setOverlapJetPatchTh2(int value);

  int getOverlapJetPatchTh0() const;
  int getOverlapJetPatchTh1() const;
  int getOverlapJetPatchTh2() const;

  int getOverlapJetPatchThreshold(int trigId) const;
  void getOverlapJetPatchAdc(int i, int& jp, int& adc) const;
  map<int,int> getOverlapJetPatchesAboveThreshold(int trigId) const;

private:
  int get2009_DSMRegisters(int runNumber);
  int defineTriggers(int runNumber);

  TDatime mDBTime;
  StBemcTriggerSimu* mBemc;
  StEemcTriggerSimu* mEemc;
  DSMLayer_EM201_2009* mEM201;
  DSMLayer_LD301_2009* mLD301;
  TCU* mTcu;
  int mOverlapJetPatchTh[3];

  ClassDef(StEmcTriggerSimu,1);
};

inline int  StEmcTriggerSimu::BHT() const { return EM201output()       & 0xf; }
inline int  StEmcTriggerSimu::EHT() const { return EM201output() >>  4 & 0x3; }
inline int  StEmcTriggerSimu::JP1() const { return EM201output() >>  6 & 0x1; }
inline int  StEmcTriggerSimu::JP2() const { return EM201output() >>  7 & 0x1; }
inline int StEmcTriggerSimu::BJP1() const { return EM201output() >>  8 & 0x1; }
inline int StEmcTriggerSimu::BJP2() const { return EM201output() >>  9 & 0x1; }
inline int StEmcTriggerSimu::EJP1() const { return EM201output() >> 10 & 0x1; }
inline int StEmcTriggerSimu::EJP2() const { return EM201output() >> 11 & 0x1; }
inline int  StEmcTriggerSimu::AJP() const { return EM201output() >> 12 & 0x1; }
inline int StEmcTriggerSimu::BAJP() const { return EM201output() >> 13 & 0x1; }
inline int StEmcTriggerSimu::EAJP() const { return EM201output() >> 14 & 0x1; }

inline void StEmcTriggerSimu::setOverlapJetPatchTh0(int value) { mOverlapJetPatchTh[0] = value; }
inline void StEmcTriggerSimu::setOverlapJetPatchTh1(int value) { mOverlapJetPatchTh[1] = value; }
inline void StEmcTriggerSimu::setOverlapJetPatchTh2(int value) { mOverlapJetPatchTh[2] = value; }

#endif // ST_EMC_TRIGGER_SIMU_H
