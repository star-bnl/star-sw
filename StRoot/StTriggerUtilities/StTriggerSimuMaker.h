// -*- mode:c++ -*-

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
#include <map>

using namespace std;

#include "StTriggerSimuResult.h"

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
class StEmcTriggerSimu;
class StL2TriggerSimu;
class StGenericL2Emulator;
class StGenericL2Emulator2009;
//class StTriggerSimuResult;
const int numSimulators=5;

class StTriggerSimuMaker : public StMaker {
private:
  int mYear;
  int mMCflag;			// 0=data, 1=simulation, 2=embedding
  
  /// collection of subdetector trigger simulators, individual pointers also available publicly below
  //vector<StVirtualTriggerSimu*> mSimulators;
  StVirtualTriggerSimu* mSimulators[numSimulators];

  /// Used to overwrite thresholds from the database
  //int mBarrelJetPatchTh[3];
  int mBarrelJetPatchTh[4];
  int mBarrelHighTowerTh[4];
  //int mEndcapJetPatchTh[3];
  int mEndcapJetPatchTh[4];
  int mEndcapHighTowerTh[2];
  int mOverlapJetPatchTh[3];
  void overwrite2009DsmRegisters();

  int mChangeJPThresh;
  void changeJetPatchTh();

  /// Choose DB to access trigger definitions and thresholds
  int mUseOnlineDB;
  int mUseOfflineDB;

  /// detailed results for individual trigger simulations
  map<int,StTriggerSimuResult> mResults;

  void buildDetailedResult(int trigId);
  bool get2009DsmRegistersFromOfflineDatabase(int runNumber);
  bool get2009DsmRegistersFromOnlineDatabase(int runNumber);
  bool getTriggerDefinitions(int runNumber);
  bool getTriggerThresholds(int runNumber);

public:
  StTriggerSimuMaker(const char *name="StarTrigSimu");
  
  void    useEemc(int flag=0);  //0:just process ADC, 1:compare w/ trigData, see enum in Eemc class
  void    useBbc();
  void    useBemc();
  void    useEmc();
  void    useL2(StGenericL2Emulator* );
  void    useL2(StGenericL2Emulator2009* );
  void    setMC(int x) {mMCflag=x;} // 0=data, 1=simulation, 2=embedding
  
  virtual Int_t     Init();
  virtual Int_t     Make();
  virtual Int_t     Finish();
  virtual void      Clear(const Option_t* = "");
  virtual Int_t     InitRun  (int runumber);
  
  TObjArray  *mHList; // output histo access point
  void setHList(TObjArray * x){mHList=x;}
  bool isTrigger(int trigId);
  bool isTriggerDefined(int trigId);
  vector<int> triggerIds() const;
  
  /// returns object containing detailed information about simulation of given trigger
  const StTriggerSimuResult& detailedResult(int trigId) { return mResults[trigId]; }
  
  //hang all activated trigger detectors below
  StBbcTriggerSimu  *bbc;
  StBemcTriggerSimu *bemc;
  StEemcTriggerSimu *eemc;
  StEmcTriggerSimu  *emc;
  StL2TriggerSimu   *lTwo;
  
  /// Use these setters to overwrite thresholds from the database (2009)
  void setBarrelJetPatchTh(int i, int value) { mBarrelJetPatchTh[i] = value; }
  void setBarrelHighTowerTh(int i, int value) { mBarrelHighTowerTh[i] = value; }

  void setEndcapJetPatchTh(int i, int value) { mEndcapJetPatchTh[i] = value; }
  void setEndcapHighTowerTh(int i, int value) { mEndcapHighTowerTh[i] = value; }

  void setOverlapJetPatchTh(int i, int value) { mOverlapJetPatchTh[i] = value; }

  void changeJPThresh(int value) { mChangeJPThresh = value; }

  void setLastDsmRegister(int reg, int value);

  /// Choose DB to access trigger definitions and thresholds
  void useOnlineDB() { mUseOnlineDB = 1; }
  void useOfflineDB() { mUseOfflineDB = 1; }


  //virtual const char *GetCVS() const
  //{static const char cvs[]="Tag $Name:  $ $Id: StTriggerSimuMaker.h,v 1.34 2017/01/02 15:31:39 rfatemi Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StTriggerSimuMaker,0)
};

#endif

// $Id: StTriggerSimuMaker.h,v 1.34 2017/01/02 15:31:39 rfatemi Exp $
//
// $Log: StTriggerSimuMaker.h,v $
// Revision 1.34  2017/01/02 15:31:39  rfatemi
// Updated by Danny OLVITT for 2013 dijet analysiss
//
// Revision 1.33  2016/03/18 22:49:38  zchang
// updating trigger simulator for run12 analysis
//
// Revision 1.32  2013/10/08 23:13:13  zchang
// Add member mTrigName and function askTrigger() to request trigger definition
//
// Revision 1.31  2012/07/13 16:47:26  pibero
// Users must now specify database to use for trigger definitions and thresholds
//
// Revision 1.30  2011/12/11 17:16:43  pibero
// Add function to set LD301 registers
//
// Revision 1.29  2011/10/04 18:29:16  pibero
// *** empty log message ***
//
// Revision 1.28  2011/06/10 18:56:18  pibero
// Updated meaning of mMCflag variable: 0=data, 1=simulation, 2=embedding
//
// Revision 1.27  2010/10/05 15:49:23  rfatemi
// Include function to test if trigger is defined in trigger code
//
// Revision 1.26  2010/08/13 22:21:10  pibero
// Move from online to offline DB
//
// Revision 1.25  2010/06/24 07:51:14  pibero
// Added hooks to overwrite DSM thresholds from the database.
//
// Revision 1.24  2010/04/17 17:43:40  pibero
// *** empty log message ***
//
// Revision 1.23  2010/03/01 18:48:36  pibero
// More updates for Run 9
//
// Revision 1.22  2010/02/18 20:07:03  pibero
// Run 9 updates
//
// Revision 1.21  2009/11/16 07:51:20  pibero
// Added LOG_DEBUG messages and triggerIds()
//
// Revision 1.20  2009/09/26 18:46:28  pibero
// Migration from ROOT MySQL to STAR DB API
//
// Revision 1.19  2009/09/23 22:35:30  pibero
// Removed dependencies on ROOT MySQL
//
// Revision 1.18  2009/09/20 06:46:29  pibero
// Updates for Run 9
//
// Revision 1.17  2009/02/03 15:40:55  rfatemi
// Update mSimulators structure for 2009 EMC simulator update
//
// Revision 1.16  2009/01/17 13:08:44  pibero
// Initial version of EMC DSM algorithms for 2009
//
// Revision 1.15  2008/01/17 01:58:25  kocolosk
// StTriggerSimuResult makes detailed emulation results persistent
//
// Revision 1.14  2007/11/18 21:58:50  balewski
// L2algos triggerId list fixed
//
// Revision 1.13  2007/10/12 20:10:23  balewski
// cleanup
//
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
