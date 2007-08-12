#include "StBemcTriggerDbThresholds.h"

//General
#include <StMessMgr.h>

//Db
#include "St_db_Maker/St_db_Maker.h"
  

ClassImp(StBemcTriggerDbThresholds);

//==================================================

StBemcTriggerDbThresholds::StBemcTriggerDbThresholds() {
  
  LOG_INFO<<"StBemcTriggerDbThresholds::constructor"<<endm; 
  
}

//==================================================
StBemcTriggerDbThresholds::~StBemcTriggerDbThresholds(){ 

  LOG_INFO<<"StBemcTriggerDbThresholds::deconstructor"<<endl;

}


//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetHtFEEbitOffset(int year){

  LOG_INFO <<"StBemcTriggerDbThresholds::Init()"<<endm;
  
  int bitOffset=2;

  (year>2005) ? bitOffset=2 : bitOffset=3;
 
  return bitOffset;

}




