//Author ::JB
// interface to L2EmulatorMaker

#include "StChain.h"
#include "StMaker.h"

//STAR
#include "StMessMgr.h"

#include "StL2TriggerSimu.h"
#include "StL2EmulatorMaker.h"

ClassImp(StL2TriggerSimu)

//==================================================
//==================================================
void  
StL2TriggerSimu::Init(){
  LOG_INFO <<Form("L2::Init()" )<<endm;
 
  mL2maker= (StL2EmulatorMaker*) StMaker::GetChain()->GetMaker("StarL2Emul");
  assert(mL2maker);
  
}

//==================================================
//==================================================
 
short   
StL2TriggerSimu::isTrigger(int trigId){
  return mL2maker->isTrigger(trigId);
}


//
// $Log: StL2TriggerSimu.cxx,v $
// Revision 1.1  2007/10/11 21:22:57  balewski
// added L2-->L0 interface class
//
// Revision 1.3  2007/07/23 02:59:56  balewski
// cleanup, bbc for M-C still not working
//










