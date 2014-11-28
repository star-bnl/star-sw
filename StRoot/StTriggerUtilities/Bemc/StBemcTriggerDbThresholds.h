//
// $Id: StBemcTriggerDbThresholds.h,v 0.01
//
#ifndef STAR_StBemcTriggerDbThresholds
#define STAR_StBemcTriggerDbThresholds

#include "StMessMgr.h"
#include "TDatime.h"

class St_db_Maker;

class StBemcTriggerDbThresholds {
 private:
  
  TDatime start_2006[6];
  TDatime start_2007[4];
  TDatime start_2008[6];
  TDatime start_2009[2];	
  TDatime end_2006[6];
  TDatime end_2007[4];
  TDatime end_2008[6];
  TDatime end_2009[2];

 public:

  StBemcTriggerDbThresholds();
  virtual     ~StBemcTriggerDbThresholds();

  void LoadTimeStamps();
  Int_t GetHtFEEbitOffset(Int_t);
  Int_t GetHT_DSM0_threshold(Int_t, UInt_t, Int_t);
  Int_t GetJP_DSM1_threshold(Int_t, UInt_t, Int_t);
  Int_t GetTP_DSM0_threshold(Int_t, UInt_t, Int_t);


  ClassDef(StBemcTriggerDbThresholds, 1)
 };


#endif
