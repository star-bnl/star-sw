//Author ::JB
// interface to L2EmulatorMaker

#include <StChain.h>
#include <StMaker.h>

//STAR
#include <StMessMgr.h>
#include <St_db_Maker/St_db_Maker.h>

#include "StL2TriggerSimu.h"
#include "StL2_2006EmulatorMaker.h" // tmp

ClassImp(StL2TriggerSimu)

//==================================================
//==================================================
void  
StL2TriggerSimu::InitRun(int runnumber){
  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  assert(mydb);
  mYear=mydb->GetDateTime().GetYear();


  LOG_INFO <<Form("L2TriggerSimu::InitRun() year=%d",mYear )<<endm;
  if(mYear==2006)
    mL2maker= (StGenericL2Emulator*) StMaker::GetChain()->GetMaker("L2Emul2006");
  else if(mYear==2008)
    mL2maker= (StGenericL2Emulator*) StMaker::GetChain()->GetMaker("L2Emul2008");
  else
    assert(1==2); // wrong year, not implemented

  assert(mL2maker);
}
  


//==================================================
//==================================================
void  
StL2TriggerSimu::Init(){

    LOG_INFO <<Form("L2TriggerSimu::Init() " )<<endm;
}

//==================================================
//==================================================
 
short   
StL2TriggerSimu::isTrigger(int trigId){
  // return mL2maker->isTrigger(trigId);// tmp
  return 0;
}


//
// $Log: StL2TriggerSimu.cxx,v $
// Revision 1.4  2007/10/23 13:26:40  balewski
// more cleanup
//
// Revision 1.3  2007/10/23 03:43:06  balewski
// clenup
//
// Revision 1.2  2007/10/22 23:09:59  balewski
// split L2 to generic and year specific, not finished
//
// Revision 1.1  2007/10/11 21:22:57  balewski
// added L2-->L0 interface class
//
// Revision 1.3  2007/07/23 02:59:56  balewski
// cleanup, bbc for M-C still not working
//










