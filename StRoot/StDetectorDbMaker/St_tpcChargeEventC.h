#ifndef St_tpcChargeEventC_h
#define St_tpcChargeEventC_h
#include "TChair.h"
#include "tables/St_tpcChargeEvent_Table.h"
#include "TArrayD.h"
#include "TArrayF.h"


class St_tpcChargeEventC : public TChair {
 public:
  static St_tpcChargeEventC* 	instance();

  tpcChargeEvent_st *Struct(Int_t i = 0)         {return ((St_tpcChargeEvent*) Table())->GetTable()+i;}
  int nChargeEvents()                            {return Struct()->nChargeEvents;}
  unsigned int* eventBunchCrossingsLow()         {return Struct()->eventBunchCrossingsLow;}
  unsigned int* eventBunchCrossingsHigh()        {return Struct()->eventBunchCrossingsHigh;}
  float* eventCharges()                          {return Struct()->eventCharges;}
  int badBunch()                                 {return Struct()->badBunch;}

  unsigned int eventBunchCrossingLow(int idx)    {return eventBunchCrossingsLow()[idx]; }
  unsigned int eventBunchCrossingHigh(int idx)   {return eventBunchCrossingsHigh()[idx]; }
  unsigned long long eventBunchCrossing(int idx) {return (((unsigned long long) (eventBunchCrossingHigh(idx))) << 32)
                                                        + ((unsigned long long) (eventBunchCrossingLow(idx))); }
  float eventCharge(int idx)                     {return eventCharges()[idx];}

  // user functions for getting the charge and time since charge

  void lastChargeTime(unsigned long long bunchCrossingNumber, float& charge, double& timeSinceCharge) {
    int idx = indexBeforeBunchCrossing(bunchCrossingNumber);
    charge = eventCharge(idx);
    timeSinceCharge = timeDifference(bunchCrossingNumber,idx);
  }

  // must call findLastChargeTime() before getLastChargeTime()
  void findLastchargeTime(unsigned long long bunchCrossingNumber) {
    lastChargeTime(bunchCrossingNumber, localStoreCharge, localStoreTimeSinceCharge);
  }
  void getLastChargeTime(float& charge, double& timeSinceCharge) {
    charge = localStoreCharge;
    timeSinceCharge = localStoreTimeSinceCharge;
  }

  // must call findChargeTimes() before getCharges() and getTimes()
  int findChargeTimes(unsigned long long bunchCrossingNumber, unsigned long long bunchCrossingWindow);
  int findChargeTimes(unsigned long long bunchCrossingNumber, double timeWindow=1.9);
  TArrayF* getCharges() { return &localStoreCharges; }
  TArrayD* getTimes() { return &localStoreTimesSinceCharges; }

 protected:
  St_tpcChargeEventC(St_tpcChargeEvent *table=0) : TChair(table) {}
  virtual ~St_tpcChargeEventC() {fgInstance = 0;}
  double timeDifference(unsigned long long bunchCrossingNumber, int idx);
  int indexBeforeBunchCrossing(unsigned long long bunchCrossingNumber);
 private:
  int localSearchLowerIndex = 0;
  int localSearchUpperIndex = -1;
  float localStoreCharge = 0; //!
  double localStoreTimeSinceCharge = 0; //!
  TArrayF localStoreCharges; //!
  TArrayD localStoreTimesSinceCharges; //!
  static St_tpcChargeEventC* fgInstance;
  ClassDef(St_tpcChargeEventC,1)
};
#endif
