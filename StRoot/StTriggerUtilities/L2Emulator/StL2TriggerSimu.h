//
//  StL2TriggerSimu.h,v 0.01
// very light intervace from L2Emulator to StTriggerSimuMaker
//
#ifndef STAR_StL2TriggerSimu
#define STAR_StL2TriggerSimu

#include <TObject.h> 
#include <vector>

#include "StTriggerSimu.h"
class StL2EmulatorMaker;

class StL2TriggerSimu : public StTriggerSimu {
 private:
  StL2EmulatorMaker *mL2maker;
  public:
  StL2TriggerSimu(){mL2maker=0;};
  void Init();
  //void InitRun(int runnumber){};
  // void Clear(){};
  void Make(){};
  short isTrigger(int trigId);


  ClassDef(StL2TriggerSimu, 1)
 };


#endif

//
// $Log: StL2TriggerSimu.h,v $
// Revision 1.1  2007/10/11 21:22:57  balewski
// added L2-->L0 interface class
//
