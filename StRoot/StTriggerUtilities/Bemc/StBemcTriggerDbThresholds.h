//
//  StBemcTriggerDbThresholds.h,v 0.01
//
#ifndef STAR_StBemcTriggerDbThresholds
#define STAR_StBemcTriggerDbThresholds

#include "StMessMgr.h"
#include "TDatime.h"

class St_db_Maker;

class StBemcTriggerDbThresholds {
 private:
  
  TDatime start_2006[6];
  TDatime end_2006[6];
  //Int_t HTW0_TH_2006[6], HTW1_TH_2006[6], HTW2_TH_2006[6];
  //Int_t HTE0_TH_2006[6], HTE1_TH_2006[6], HTE2_TH_2006[6];
  //Int_t HTTP0_TH_2006[6], HTTP1_TH_2006[6], HTTP2_TH_2006[6];
  //Int_t JP0_TH_2006[6], JP1_TH_2006[6], JP2_TH_2006[6];
  //Int_t ETOT_2006[6];

 public:

  StBemcTriggerDbThresholds();
  virtual     ~StBemcTriggerDbThresholds();

  Int_t GetHtFEEbitOffset(Int_t);
  Int_t GetHT_DSM0_threshold(Int_t, UInt_t, Int_t);
  Int_t GetJP_DSM0_threshold(Int_t, UInt_t, Int_t);
  Int_t GetHTTP_DSM0_threshold(Int_t, UInt_t, Int_t);


  ClassDef(StBemcTriggerDbThresholds, 1)
 };


#endif
