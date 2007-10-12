//////////////////////////////////////////////////////////////////////////
//
//
// StTriggerSimuMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: generate trigger response based on ADC
// implemented BEMC,EEMC,....
// >StTriggerSimu/*SUB*/St*SUB*TriggerSimu.h
// >where *SUB* are the subsystems: Eemc, Bemc, Bbc, L2,.... 
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StTriggerSimuMaker
#define STAR_StTriggerSimuMaker

#include <vector>

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEvent;
class StBemcTables;
class St_db_Maker;

class StVirtualTriggerSimu;
class StBbcTriggerSimu;
class StBemcTriggerSimu;
class StEemcTriggerSimu;
class StL2TriggerSimu;

class StTriggerSimuMaker : public StMaker {
private:
  TString mConfig;
  int mYear,mMCflag; // set mcFlag=0 for real data
  
  /// useful pointers
  St_db_Maker *mDbMk;
    
  /// collection of subdetector trigger simulators, individual pointers also available publicly below
  std::vector<StVirtualTriggerSimu*> mSimulators;
  
public:
  StTriggerSimuMaker(const char *name="StarTrigSimu");
  virtual           ~StTriggerSimuMaker();
  
  void    useEemc(int flag=0);  //0:just process ADC, 1:compare w/ trigData, see enum in Eemc class
  void    useBbc();
  void    useBemc();
  void    useL2();
  void    setMC(int x) {mMCflag=x;}
  
  virtual Int_t     Init();
  virtual Int_t     Make();
  virtual Int_t     Finish();
  virtual void      Clear(const Option_t* = "");
  virtual Int_t     InitRun  (int runumber);
  
  TObjArray  *mHList; // output histo access point
  void setHList(TObjArray * x){mHList=x;}
  bool isTrigger(int trigId);
  vector <int> mTriggerList;
  void setConfig(const char *config) {mConfig=config;}
  
  //hang all activated trigger detectors below
  StBbcTriggerSimu  *bbc;
  StBemcTriggerSimu *bemc;
  StEemcTriggerSimu *eemc;
  StL2TriggerSimu   *lTwo;
  
  ClassDef(StTriggerSimuMaker,0)
};

#endif

// $Id: StTriggerSimuMaker.h,v 1.12 2007/10/12 17:19:17 kocolosk Exp $
//
// $Log: StTriggerSimuMaker.h,v $
// Revision 1.12  2007/10/12 17:19:17  kocolosk
// move BEMC-specific code to StBemcTriggerSimu
// replace some config methods like setDbMaker with code that finds the Maker automatically
//
// Revision 1.11  2007/10/12 14:36:01  balewski
// added L2 interface
//
// Revision 1.10  2007/10/11 00:32:56  balewski
// L2algo added
//
// Revision 1.9  2007/09/25 18:19:35  rfatemi
// Update for TP work
//
// Revision 1.8  2007/09/24 18:08:11  kocolosk
// some code restructuring
//
// Revision 1.7  2007/09/21 18:45:51  rfatemi
// End of week update
//
// Revision 1.6  2007/08/12 01:03:22  rfatemi
// Added flag for offline/online/expert settings
//
// Revision 1.5  2007/08/07 15:48:38  rfatemi
// Added BEMC access methods
//
// Revision 1.4  2007/07/23 03:03:39  balewski
// fix
//
// Revision 1.3  2007/07/22 23:09:51  rfatemi
// added access to Bbc
//
// Revision 1.2  2007/07/21 23:35:24  balewski
// works for M-C
//
// Revision 1.1  2007/07/20 21:01:41  balewski
// start
//
