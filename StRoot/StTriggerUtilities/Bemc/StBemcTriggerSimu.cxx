//
//  StBemcTriggerSimu.cxx,v 0.01
//

//General
#include <TH2.h>
#include <StMessMgr.h>

//Bemc
#include "StBemcTriggerSimu.h"

//StEvent
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StEmcTriggerDetector.h"
#include "StEvent/StL0Trigger.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//Db
#include "St_db_Maker/St_db_Maker.h"
  

ClassImp(StBemcTriggerSimu);

//==================================================

StBemcTriggerSimu::StBemcTriggerSimu() {
  
  LOG_INFO<<"StBemcTriggerSimu::constructor"<<endm;  
  mHList=0;
  
}

//==================================================
StBemcTriggerSimu::~StBemcTriggerSimu(){ 

  LOG_INFO<<"StBemcTriggerSimu::deconstructor"<<endl;

}


//==================================================
//==================================================
void  
StBemcTriggerSimu::Init(){
  
  LOG_INFO <<"StBemcTriggerSimu::Init()"<<endm;
}


 
//==================================================
//==================================================
void  
StBemcTriggerSimu::InitRun(){
 
  LOG_INFO<<"StBemcTriggerSimu::InitRun()"<<endm;
}


//==================================================
//==================================================
void  
StBemcTriggerSimu::Clear(){
  
  LOG_INFO <<"StBemcTriggerSimu::Clear()"<<endm;
}
  
//==================================================
//==================================================
void  
StBemcTriggerSimu::addTriggerList( void * adr){

}


     
//==================================================
//==================================================
void 
StBemcTriggerSimu::Make(){

  
}


