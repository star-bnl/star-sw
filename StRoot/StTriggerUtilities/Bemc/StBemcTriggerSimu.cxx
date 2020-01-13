#include "BEMC_DSM_decoder.h"
#include "StBemcTriggerSimu.h"

//General
#include "TList.h"
#include "TH2.h"
#include "StMessMgr.h"
#include "StTriggerSimuMaker.h"

//Bemc
#include "StEmcUtil/database/StEmcDecoder.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StBemcTriggerDbThresholds.h"

//StEvent
#include "St_DataSetIter.h"
#include "StEvent/StEventTypes.h"

//Db
#include "St_db_Maker/St_db_Maker.h"

// MuDst
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

// DSM 2009 Utilities
#include "StTriggerUtilities/StDSMUtilities/StDSM2009Utilities.hh"

ClassImp(StBemcTriggerSimu)
//==================================================
//==================================================
StBemcTriggerSimu::StBemcTriggerSimu()
{
  mEvent    = NULL;
  mDecoder  = new StEmcDecoder();
  mDbThres  = new StBemcTriggerDbThresholds();
  mGeo      = StEmcGeom::getEmcGeom("bemc");
  mAdc2e    = 0;
  starDb    = NULL;
  mTables   = NULL;
  mHList    = NULL;
  mConfig   = 0;

}
//==================================================
//==================================================
StBemcTriggerSimu::~StBemcTriggerSimu(){ 

  delete mDecoder;
  delete mDbThres;

}
//==================================================
//==================================================
void StBemcTriggerSimu::Init(){
 
  LOG_INFO <<Form("Bemc::Init() MC_flag=%d, config: flag=%d",mMCflag, mConfig)<<endm;
  assert(mConfig>=kOnline);
  assert(mConfig<=kExpert);


  starDb = static_cast<St_db_Maker*> ( mHeadMaker->GetMakerInheritsFrom("St_db_Maker") );
  if(!starDb) {
    LOG_WARN << "StBemcTriggerSimu couldn't get a pointer to St_db_maker -- this means trouble" << endm;
  }
  
  if(mMCflag == 1) {
    StEmcSimulatorMaker *emcSim = static_cast<StEmcSimulatorMaker*> ( mHeadMaker->GetMakerInheritsFrom("StEmcSimulatorMaker") );
    if(!emcSim) {
      LOG_FATAL << "StBemcTriggerSimu couldn't find StEmcSimulatorMaker in chain" << endm;
      assert(0);
    }
    mTables = emcSim->getTables();
  }
  else {
    mAdc2e = static_cast<StEmcADCtoEMaker*> ( mHeadMaker->GetMakerInheritsFrom("StEmcADCtoEMaker") );
    if(!mAdc2e) {
      LOG_FATAL << "StBemcTriggerSimu couldn't find StEmcADCtoEMaker in chain" << endm;
      assert(0);
    }
    mTables = mAdc2e->getBemcData()->getTables();
  }

  mDbThres->LoadTimeStamps();
  
  //2005
  mAllTriggers.insert(96201);   //bemc-ht1-mb
  mAllTriggers.insert(96211);   //bemc-ht2-mb
  mAllTriggers.insert(96221);   //bemc-jp1-mb
  mAllTriggers.insert(96233);   //bemc-jp2-mb-b

  //2006
  mAllTriggers.insert(117201);  //bemc-http-mb  
  mAllTriggers.insert(117211);  //bemc-ht2-mb
  mAllTriggers.insert(117212);  //bemc-ht2-mb-emul
  mAllTriggers.insert(127212);  //bemc-ht2-mb-emul
  mAllTriggers.insert(127213);  //bemc-ht2-mb-emul
  mAllTriggers.insert(137213);  //bemc-ht2-mb-emul
  mAllTriggers.insert(117221);  //bemc-jp1-mb
  mAllTriggers.insert(127221);  //bemc-jp1-mb
  mAllTriggers.insert(137221);  //bemc-jp1-mb
  mAllTriggers.insert(137222);  //bemc-jp1-mb
  mAllTriggers.insert(117501);  //bemc-jp0-mb
  mAllTriggers.insert(127501);  //bemc-jp0-mb
  mAllTriggers.insert(137501);  //bemc-jp0-mb
  mAllTriggers.insert(117571);  //bemc-jp1
  mAllTriggers.insert(127571);  //bemc-jp1
  mAllTriggers.insert(137571);  //bemc-jp1
  mAllTriggers.insert(117575);  //bemc-jp0-etot
  mAllTriggers.insert(127575);  //bemc-jp0-etot
  mAllTriggers.insert(137575);  //bemc-jp0-etot
  mAllTriggers.insert(117585);  //bemc-jp2
  mAllTriggers.insert(127585);  //bemc-jp2
  mAllTriggers.insert(137585);  //bemc-jp2
  mAllTriggers.insert(117601);  //Upsilon
  mAllTriggers.insert(117602);  //Upsilon
  mAllTriggers.insert(137602);  //Upsilon
  mAllTriggers.insert(137603);  //Upsilon
  mAllTriggers.insert(117611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(127611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(5);       //bemc-http-mb-l2gamma
  mAllTriggers.insert(137611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(117621);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(117622);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(127622);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(137622);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(117705);  //jpsi-mb
  mAllTriggers.insert(137705);  //jpsi-mb
  mAllTriggers.insert(117821);  //bemc-http-mb-fast
  mAllTriggers.insert(127821);  //bemc-http-mb-fast
  mAllTriggers.insert(137821);  //bemc-http-mb-fast
  mAllTriggers.insert(137822);  //bemc-http-mb-fast
  mAllTriggers.insert(147570);  //bemc-jp0
  mAllTriggers.insert(147585);  //bemc-http
  mAllTriggers.insert(147611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(147621);  //bemc-jp0-mb-l2jet
  mAllTriggers.insert(147705);  //jpsi-mb
  
  //2007
  mAllTriggers.insert(200211);  //bht2-mb
  mAllTriggers.insert(200212);  //bht2-mb
  mAllTriggers.insert(200220);  //bht2-mb
  mAllTriggers.insert(200221);  //bht2-mb
  mAllTriggers.insert(200222);  //bht2-mb
  mAllTriggers.insert(200213);  //btag
  mAllTriggers.insert(200214);  //btag
  mAllTriggers.insert(200585);  //bht2
  mAllTriggers.insert(200586);  //bht2
  mAllTriggers.insert(200601);  //L2-upsilon
  mAllTriggers.insert(200602);  //L2-upsilon  
  mAllTriggers.insert(200620);  //L2-gamma
  mAllTriggers.insert(200621);  //L2-gamma
  //not implemented
  mAllTriggers.insert(200400); //upc
  mAllTriggers.insert(200401); //upc
  mAllTriggers.insert(200402); //upc
  mAllTriggers.insert(200410); //upc-jpsi
  mAllTriggers.insert(200411); //upc-jpsi


  //2008dAu
  mAllTriggers.insert(210500);//BEMC-HT0
  mAllTriggers.insert(210510);//BEMC-HT1
  mAllTriggers.insert(210520);//BEMC-HT2
  mAllTriggers.insert(210501);//BEMC-HT0
  mAllTriggers.insert(210511);//BEMC-HT1
  mAllTriggers.insert(210521);//BEMC-HT2
  mAllTriggers.insert(210541);//BEMC-HT4
  //not implemented
  mAllTriggers.insert(210601);//upsilon 
  mAllTriggers.insert(210710);//UPCjpsi
  mAllTriggers.insert(210800);//BEMC-HT4-fast

  //2008pp
  mAllTriggers.insert(220500);//BEMC-HT0-mb
  mAllTriggers.insert(220510);//BEMC-HT1-mb
  mAllTriggers.insert(220520);//BEMC-HT2-mb-slow

  //2009pp
  //
  // All the registers are now retrieved from the database in StTriggerSimuMaker.cxx
  // (look for the function get2009DsmRegistersFromOnlineDatabase).
  // This way, only one call to the database is made to get
  // thresholds for bemc, eemc, and emc. The old way, each
  // subdetector made a separate call to the database to
  // get its thresholds via the noew-defunct function setDsmRegisters().
  // This, of course, puts a lot of unecessary strain on the database.
  // Hence, all the register retrievals are now done in one shot
  // in StTriggerSimuMaker.cxx.
  //
  // The 2009 trigger IDs are handled by emc which has a TCU
  // emulator. Basically, trigger definitions are retrieved
  // from the database. Those definitions include which bits
  // must be on at the output of the last DSM. Triggers are
  // then satisfied accordingly.

  Clear();

#ifdef DEBUG
  mBEMCLayer0HT6bitDiff=new TH2F("BEMCLayer0HT6bitDiff","BEMC Layer 0 HT6bit Difference",kNPatches,0,kNPatches,128,-64,64);
  mBEMCLayer0TP6bitDiff=new TH2F("BEMCLayer0TP6bitDiff","BEMC Layer 0 TP6bit Difference",kNPatches,0,kNPatches,128,-64,64);
  mBEMCLayer0HT6bit=new TH2F("BEMCLayer0HT6bit","BEMC Layer 0 HT6bit",kNPatches,0,kNPatches,32,0,32);
  mBEMCLayer0TP6bit=new TH2F("BEMCLayer0TP6bit","BEMC Layer 0 TP6bit",kNPatches,0,kNPatches,32,0,32);
  mBEMCLayer1HTBits = new TH2F("BEMCLayer1HTBits","BEMC Layer1 HT Threshold Bits", 36, 0, 36, 10, 0, 10);
  mBEMCLayer1HTBitsDiff = new TH2F("BEMCLayer1HTBitsDiff","BEMC Layer1 HT Threshold Bits - Simulated",36, 0, 36, 20, -10, 10);
  mBEMCLayer1TPBits = new TH2F("BEMCLayer1TPBits","BEMC Layer1 TP Threshold Bits", 36, 0, 36, 10, 0, 10);
  mBEMCLayer1TPBitsDiff = new TH2F("BEMCLayer1TPBitsDiff","BEMC Layer1 TP Threshold Bits - Simulated",36, 0, 36, 20, -10, 10);
  mBEMCLayer1HTTPBits = new TH2F("BEMCLayer1HTTPBits","BEMC Layer1 HT.TP Threshold Bits", 36, 0, 36, 10, 0, 10);
  mBEMCLayer1HTTPBitsDiff = new TH2F("BEMCLayer1HTTPBitsDiff","BEMC Layer1 HT.TP Threshold Bits - Simulated",36, 0, 36, 20, -10, 10);
  mBEMCLayer1PatchSum = new TH2F("BEMCLayer1PatchSum", "BEMC Layer1 Patch Sum", 36, 0, 36, 100, 0, 100);
  mBEMCLayer1PatchSumDiff = new TH2F("BEMCLayer1PatchSumDiff", "BEMC Layer1   Patch Sum Diff", 36, 0, 36, 200, -100, 100);
  mBEMCLayer1HTmaskBits = new TH2F("BEMCLayer1HTmaskBits", "BEMC Layer1 HT Mask Bits", 36, 0, 36, 8, -4, 4);
  mBEMCLayer1HTmaskDiff = new TH2F("BEMCLayer1HTmaskDiff", "BEMC Layer1 HT Mask Diff", 36, 0, 36, 8, -4, 4);
  mBEMCLayer1HTthr3Bits = new TH2F("BEMCLayer1HTthr3Bits", "BEMC Layer1 HT Thr3 Bits", 36, 0, 36, 8, -4, 4);
  mBEMCLayer1HTthr3Diff = new TH2F("BEMCLayer1HTthr3Diff", "BEMC Layer1 HT Thr3 Diff", 36, 0, 36, 8, -4, 4);
  mBEMCLayer2PatchSum = new TH2F("BEMCLayer2PatchSum", "BEMC Layer2 Patch Sum", 6, 0, 6, 100, 0, 100);
  mBEMCLayer2PatchSumDiff = new TH2F("BEMCLayer2PatchSumDiff", "BEMC Layer2   Patch Sum Diff", 6, 0, 6, 200, -100, 100);
  mBEMCLayer2HT3Bits = new TH2F("BEMCLayer2HT3Bits", "BEMC Layer2 HT3 Bits", 6, 0, 6, 4, 0, 4);
  mBEMCLayer2HT3BitsDiff = new TH2F("BEMCLayer2HT3BitsDiff", "BEMC Layer2 HT3 Bits", 6, 0, 6, 8, -4, 4);
  mBEMCLayer2HTTPBits = new TH2F("BEMCLayer2HTTPBits", "BEMC Layer2  HTTP Bits", 6, 0, 6, 4, 0, 4);
  mBEMCLayer2HTTPBitsDiff = new TH2F("BEMCLayer2HTTPBitsDiff", "BEMC Layer2   HTTP Bits", 6, 0, 6, 8, -4, 4);
  mBEMCLayer2TPBits = new TH2F("BEMCLayer2TPBits", "BEMC Layer2  TP Bits", 6, 0, 6, 4, 0, 4);
  mBEMCLayer2TPBitsDiff = new TH2F("BEMCLayer2TPBitsDiff", "BEMC Layer2 TP Bits", 6, 0, 6, 8, -4, 4);
  mBEMCLayer2JPBits = new TH2F("BEMCLayer2JPBits", "BEMC Layer2 JP Bits", 6, 0, 6, 4, 0, 4);
  mBEMCLayer2JPBitsDiff = new TH2F("BEMCLayer2JPBitsDiff", "BEMC Layer2 JP Bits", 6, 0, 6, 8, -4, 4);
  mBEMCLayer2HTj0Bits = new TH2F("BEMCLayer2HTj0Bits", "BEMC Layer2 HT_j0 Bits", 6, 0, 6, 4, 0, 4);
  mBEMCLayer2HTj0BitsDiff = new TH2F("BEMCLayer2HTj0BitsDiff", "BEMC Layer2 HT_j0 Patch Bits", 6, 0, 6, 8, -4, 4);
  mBEMCLayer2HTj1Bits = new TH2F("BEMCLayer2HTj1Bits", "BEMC Layer2 HT_j1 Bits", 6, 0, 6, 4, 0, 4);
  mBEMCLayer2HTj1BitsDiff = new TH2F("BEMCLayer2HTj1BitsDiff", "BEMC Layer2 HT_j1 Bits", 6, 0, 6, 8, -4, 4);

  mHList->Add(mBEMCLayer0HT6bit);
  mHList->Add(mBEMCLayer0TP6bit);
  mHList->Add(mBEMCLayer0HT6bitDiff);
  mHList->Add(mBEMCLayer0TP6bitDiff);
  mHList->Add(mBEMCLayer1HTBits);
  mHList->Add(mBEMCLayer1HTBitsDiff);
  mHList->Add(mBEMCLayer1TPBits);
  mHList->Add(mBEMCLayer1TPBitsDiff);
  mHList->Add(mBEMCLayer1HTTPBits);
  mHList->Add(mBEMCLayer1HTTPBitsDiff);
  mHList->Add(mBEMCLayer1PatchSum);
  mHList->Add(mBEMCLayer2PatchSum);
  mHList->Add(mBEMCLayer2HTTPBits);
  mHList->Add(mBEMCLayer2TPBits);
  mHList->Add(mBEMCLayer2HT3Bits);
  mHList->Add(mBEMCLayer2JPBits);
  mHList->Add(mBEMCLayer2HTj0Bits);
  mHList->Add(mBEMCLayer2HTj1Bits);
  mHList->Add(mBEMCLayer1PatchSumDiff);
  mHList->Add(mBEMCLayer1HTthr3Bits);
  mHList->Add(mBEMCLayer1HTthr3Diff);
  mHList->Add(mBEMCLayer1HTmaskBits);
  mHList->Add(mBEMCLayer1HTmaskDiff);
  mHList->Add(mBEMCLayer2PatchSumDiff);
  mHList->Add(mBEMCLayer2HTTPBitsDiff);
  mHList->Add(mBEMCLayer2TPBitsDiff);
  mHList->Add(mBEMCLayer2HT3BitsDiff);
  mHList->Add(mBEMCLayer2JPBitsDiff);
  mHList->Add(mBEMCLayer2HTj0BitsDiff);
  mHList->Add(mBEMCLayer2HTj1BitsDiff);

#endif

}
//==================================================
//==================================================
void StBemcTriggerSimu::InitRun(int runnumber){
  LOG_DEBUG<<"StBemcTriggerSimu::InitRun() -- " << runnumber << '\t' << mHeadMaker->GetDate() << '\t' << mHeadMaker->GetTime() << endm;
  
  assert(starDb);

  const TDatime& dbTime = starDb->GetDBTime();

  timestamp = dbTime.Get();
  year      = dbTime.GetYear();
  yyyymmdd  = dbTime.GetDate(); //form of 19971224 (i.e. 24/12/1997)
  hhmmss    = dbTime.GetTime(); //form of 123623 (i.e. 12:36:23)

  mDecoder->SetDateTime(dbTime);

  if (year < 2009) {
    getTowerStatus();
    getDSM_TPStatus();
    getDSM_HTStatus();
    getLUT();
    getPed();

    //Get FEE window for HT from support class for offline operation
    //online replaced this with Db call in getPed()
    HT_FEE_Offset=mDbThres->GetHtFEEbitOffset(year);
  }
  else {
    //Initialize DSM layers based on run number
    //b001
    if(year == 2015){
      mB001 = new DSMLayer_B001_2015;
    }else if(year == 2016){
      mB001 = new DSMLayer_B001_2014_B;
    }else
      mB001     = new DSMLayer_B001_2009;
    //b101
    if(year == 2013 && runnumber >= 14081067){
      mB101 = new DSMLayer_B101_2013;
    }else if(year == 2015){
      mB101 = new DSMLayer_B101_2015;
    }else if(year == 2016){
      mB101 = new DSMLayer_B101_2014_B;
    }else
      mB101 = new DSMLayer_B101_2009;

    if (mBemcStatus != "") {
      FEEini2009();
    }
    else {
      FEEini2009(runnumber);
    }
    mAllTriggers.clear();
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::Clear(){
  

  for (int did=1; did<=kNTowers; did++){
    adc08[did-1]=0;
    adc10[did-1]=0;
    adc12[did-1]=0;
    HT6bit_adc_holder[did-1]=0;
  }
  
  for (int tpid=0;tpid<kNPatches; tpid++){
    L0_HT_ADC[tpid]=0;
    L0_TP_ADC[tpid]=0;
    L0_TP_PED[tpid]=0;
    HTadc06[tpid]=0;
    TP6bit_adc_holder[tpid]=0;
  }
  
  mFiredTriggers.clear();
  mJpsiCandidates.clear();
}
//==================================================
//==================================================
bool StBemcTriggerSimu::isCorrupted() const {
  return mMCflag != 1 && mAdc2e->isCorrupted();
}
//==================================================
//==================================================
StTriggerSimuDecision StBemcTriggerSimu::triggerDecision(int trigId) {
  //first check if it fired
  for(unsigned i=0; i<mFiredTriggers.size(); i++) {
    if(trigId == mFiredTriggers[i]) return kYes;
  }
  
  //now check if we care
  if(mAllTriggers.find(trigId) == mAllTriggers.end()) {
    return kDoNotCare;
  }
  else {
    return kNo;
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::getTowerStatus(){
  
  fill(TowerStatus,TowerStatus+kNTowers,1);
  fill(numMaskTow,numMaskTow+kNPatches,0);
  
  if (mConfig==kOnline) {
    for (Int_t cr=1; cr <= kNCrates; cr++){
      for (Int_t ch=0; ch < kNChannels; ch++){
        Int_t did,tpid;
        mDecoder->GetTowerIdFromCrate(cr,ch,did);
        TowerStatus[did-1]=mTables->triggerTowerStatus(cr,ch);
	mDecoder->GetTriggerPatchFromTowerId(did,tpid);
	if (TowerStatus[did-1]!=1) numMaskTow[tpid]++;
      }
    }
  }
  

  if (mConfig==kOffline){
    for (int did=1; did<=kNTowers; did++){
      Int_t tpid;
      mTables->getStatus(BTOW, did, TowerStatus[did-1]);
      if (mTables->status(BTOW,did,"calib")!=1) TowerStatus[did-1]=0; 
      mDecoder->GetTriggerPatchFromTowerId(did,tpid);
      if (TowerStatus[did-1]!=1) numMaskTow[tpid]++;
    }
  }
  
  if (mConfig==kExpert){
    for (int did=1; did<=kNTowers; did++){
      Int_t tpid;
      TowerStatus[did-1]=1;
      mDecoder->GetTriggerPatchFromTowerId(did,tpid);
      if (TowerStatus[did-1]!=1) numMaskTow[tpid]++;
    }
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::getDSM_TPStatus(){

  for (int tpid=0;tpid<kNPatches;tpid++) DSM_TPStatus[tpid]=1;
   
  //for online config TP status is set by DB
  if (mConfig==kOnline){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=mTables->triggerPatchStatus(tpid);
    }
  }
 
 //for offline config all TP status is good by definition. 
  if (mConfig==kOffline) {
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=1;
    }
  }
 
  //experts do as you will but set good by definition
  if (mConfig==kExpert){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=1;
    }
  }

}
//==================================================
//==================================================
void StBemcTriggerSimu::getDSM_HTStatus(){
  
  for (int tpid=0;tpid<kNPatches;tpid++) DSM_HTStatus[tpid]=1;

  //Online get DSM HT status from db
  if (mConfig==kOnline){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_HTStatus[tpid]=mTables->triggerHighTowerStatus(tpid);
    }
  }
  
 //Offline all DSM HT status are good
  if (mConfig==kOffline){
    for (int tpid=0; tpid<kNPatches; tpid++){
      DSM_HTStatus[tpid]=1;
    }
  }
 
  if (mConfig==kExpert){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_HTStatus[tpid]=1;
    }
  } 
}
//==================================================
//==================================================
void StBemcTriggerSimu::getPed() {

  for (int i=1;i<=kNTowers;i++) {ped12[i-1]=0;}
  
  for (int did=1;did<=kNTowers;did++){
    bitConvValue[did-1]=mTables->triggerBitConversionByID(did);
  }

  //get Target Pedestal value from DB
  pedTargetValue=mTables->triggerPedestalShift();

  //online 12 bit peds stored as Float_t
  if (mConfig==kOnline){
    for (int cr=1; cr <= kNCrates; cr++){
      for (int ch=0; ch < kNChannels; ch++){
        int did;
        mDecoder->GetTowerIdFromCrate(cr,ch,did);
        ped12[did-1] = mTables->triggerPedestal(cr,ch);
      }
    }
  }
  
 //offline 12 bit peds stored as Float_t
  if (mConfig==kOffline){
    for (int did=1; did<=kNTowers; did++){
      ped12[did-1]=mTables->pedestal(BTOW,did);
    }
  }

  //Experts set ped to your favorite values
  if (mConfig==kExpert){
    for (int did=1; did<=kNTowers; did++){
      ped12[did-1]=24;
    }
  } 
}
//==================================================
//==================================================
void StBemcTriggerSimu::getLUT(){

  /*formula param
    Is the trigger mask used for LUT calculation (to account for masked towers in TP)
    0 = no, the LUT does not depend on which towers in a patch are masked out
    1 = yes, the trigger mask is used for LUT scale modification
    2 = yes, the trigger mask is used for LUT pedestal modificationLUTUseMask 2
  */

  Int_t f,param[6];
  for (int cr=1;cr<=kNCrates;cr++){
    for (int seq=0; seq<kNSeq; seq++){
      mTables->getTriggerFormulaTag(cr,seq,f);
      mTables->getTriggerFormulaParameters(cr,seq,param);
      formula[cr-1][seq]=f;
      LUTscale[cr-1][seq]=param[0];
      LUTped[cr-1][seq]=param[1];
      LUTsig[cr-1][seq]=param[2];
      LUTpow[cr-1][seq]=param[3];
    }
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::Make(){
  
  mEvent = static_cast<StEvent*> ( mHeadMaker->GetDataSet("StEvent") );

  //add test mode by zchang
  if(!mMCflag && mTestMode){
    StEmcTriggerDetector emc = StMuDst::event()->emcTriggerDetector();
    for(int ip = 0; ip < 300; ip++){
      int ht = emc.highTower(ip);
      int pa = emc.patch(ip);
      L0_TP_ADC[ip] = pa;
      L0_HT_ADC[ip] = ht;
    }
  }else{
    if (year < 2009)
      FEEout();
    else {
      FEEout2009();
      simulateFEEfailure();
    }
  }

  //pp
  if (year==2006){
    get2006_DSMLayer0();
    get2006_DSMLayer1();
    get2006_DSMLayer2();
  }

  //AuAu
  if ((year==2007)&&(yyyymmdd<20071205)){
    get2007_DSMLayer0();
    get2007_DSMLayer1();
    get2007_DSMLayer2();
  }
  
  //dAu
  if ((yyyymmdd>20071205)&&(yyyymmdd<20080129)){
    get2008dAu_DSMLayer0();
    get2008dAu_DSMLayer1();
    get2008dAu_DSMLayer2();
  }

  //pp
  if ((year==2008)&&(yyyymmdd>20080129)){
    get2008pp_DSMLayer0();
    get2008pp_DSMLayer1();
    get2008pp_DSMLayer2();
  }

  //pp
  if ((year>=2009)&&(yyyymmdd>20090101)) {
    get2009_DSMLayer0();
    get2009_DSMLayer1();
  }

  if (mMCflag) fillStEmcTriggerDetector();
}
//==================================================
//==================================================
void StBemcTriggerSimu::FEEout() {
  //many parts copied directly from Oleksandr's BEMC_DSM_decoder.cxx
  //which is a C++ translation of the FEE code
  //ped1 == ped12Diff value2 == ped10Diff value1 == ped10DiffI
   
   
  //  static int commonLUT[] = {0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
  static int commonLUT[] = { 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9,
			    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
			    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
			    60, 61, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 
			    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 
			    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62}; 
  
  if(!mEvent) {LOG_WARN << "StBemcTriggerSimu -- no StEvent!" << endm;}
  
  StEmcCollection *emc = mEvent->emcCollection();
  if(!emc)    {LOG_WARN << "StBemcTriggerSimu -- no StEmcCollection!" << endm;}
  
  StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
  if(!detector) {LOG_WARN << "StBemcTriggerSimu -- no StEmcDetector!" << endm;}

  //loop through BEMC hits 
  //Store 8,10,12 bit pedestal adjusted ADC for hits
  //for online case online tower masks are applied
  //for offline case offline tower masks are applied
  //DSM TP/HT masks are applied for online case
  //DSM TP/HT masks are perfect for offline case
  if(detector) {
    for(Int_t m = 1; m <= 120; ++m) {
      StEmcModule* module = detector->module(m);
      if(module) {
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k = 0; k < rawHit.size(); ++k) {
          if(rawHit[k]) {
            int did, tpid;
            
            Int_t m=rawHit[k]->module();
            Int_t e=rawHit[k]->eta();
            Int_t s=abs(rawHit[k]->sub());
            Int_t adc=rawHit[k]->adc();
            //Float_t energy=rawHit[k]->energy();

            //Get software tower id and trigger patch id
            mGeo->getId(m,e,s,did);	  
            mDecoder->GetTriggerPatchFromTowerId(did,tpid);
	    
            //apply tower masks
	    if (TowerStatus[did-1]==1) {
  
	      //need to shift ADC and ped to pedTargetValue 
              //goal is to ultimately place DSM channel at 1
              ped12Diff=ped12[did-1]-pedTargetValue;
	      
	      //12 bit ADC enters FEE and drop 2 bits immediately
              adc12[did-1]=adc;
              adc10[did-1]=adc12[did-1] >> 2;
  
              //determine if pedestal is > or < pedTargetValue
              int operation=1;
              if(ped12Diff < 0) {
                ped12Diff = -ped12Diff;
                operation = 0;
              }
              ped10Diff = ped12Diff/4;

              //Rounds ped10Diff up/down to an Int_t
              sprintf(buffer,"%3.0f",ped10Diff);
              ped10DiffI=atoi(buffer);
        
              //effective rounding up for binary numbers with 1xx,10xx,11xx,100xx,101xx,110xx,111xx etc
              //so that carrying out ADC12 - PED12 + 24 in 12 bit land is the same exercise as in 10 bit land
              if (ped12Diff - ped10DiffI*4 > 2) {
                ped10Diff+=1;
                sprintf(buffer,"%3.0f",ped10Diff);
                ped10DiffI=atoi(buffer);
              }

              // can't subtract/add more than 15 on 10-bit level
              if(ped10DiffI > 15) {
                sprintf(buffer,"%3.0f",double(ped10DiffI - 11)/4);
                int holder = atoi(buffer);
                ped10Diff = ped10DiffI - 4*holder;
                sprintf(buffer,"%3.0f",ped10Diff);
                ped10DiffI = atoi(buffer);
              }

              //adjust pedestal of tower adc to 24(6) in 12(10) bit
              if (operation==1) adc10[did-1] -= ped10DiffI;
              if (operation==0) adc10[did-1] += ped10DiffI;
	      // if in offline mode and pedestal is not matched well to adc spectrum so
	      // adc -ped < 0 then set adc = 0 so it cannot cause a trigger.
	      if ((mConfig==kOffline)&&(adc10[did-1] < 0)) adc10[did-1]=0;

              //now adc10 and adc08 are the 10 and 8 bit pedestal shift adcs
              adc08[did-1]=adc10[did-1] >> 2;
    
              //subject all towers to HT algorithm and transform adc10 into adc06
              int HTholder=-1;

              if (mConfig==kOnline) HTholder = adc10[did-1] >> bitConvValue[did-1];//drop lowest bits   
              if (mConfig==kOffline) HTholder = adc10[did-1] >> HT_FEE_Offset;//drop lowest bits

              int HTL = HTholder & 0x1F;// AND HTholder with 00011111 to grab 5 lowest bits
              int HTH = HTholder >> 5;//Remove lowest 5 bits
              int B5  = 0;
              if(HTH>0) B5 = 1;
              HTadc06[did-1] = HTL+(B5<<5);

              //Fill DSM L0 with 6bit HT/TP in each TP
	      HT6bit_adc_holder[did-1]=HTadc06[did-1]; 
              if (DSM_HTStatus[tpid]==1){
                if (HTadc06[did-1]>L0_HT_ADC[tpid]) L0_HT_ADC[tpid]=HTadc06[did-1];
              }
              if (DSM_TPStatus[tpid]==1) {
                L0_TP_ADC[tpid]+=adc08[did-1];
		TP6bit_adc_holder[tpid]+=adc08[did-1]; 
		//Calculate LUT ped for OFFLINE
                L0_TP_PED[tpid]++;
              }
	      
              //Mask out 6 bit adc if that DSM HT/TP bit was masked out
              if (DSM_HTStatus[tpid]==0) L0_HT_ADC[tpid]=0;
              if (DSM_TPStatus[tpid]==0) L0_TP_ADC[tpid]=0;
              if (DSM_TPStatus[tpid]==0) TP6bit_adc_holder[tpid]=0; 
	      
	      if  (0)
		{
		  cout<<"Tow#="<<did<<" TP#="<<tpid<<" adc12="<<adc12[did-1]<<" adc10="<<adc10[did-1]<<" adc08="<<adc08[did-1]	   
		      <<" HTadc06="<<HTadc06[did-1]<<" ped12="<<ped12[did-1]<<" ped12diff="<<ped12Diff<<" ped10Diff="
		      <<ped10Diff<<"HTholder="<<HTholder<<" HTL="<<HTL<<" HTH="<<HTH<<" B5="<<B5<<" BitConverValue="<<bitConvValue[did-1]
		      <<" HT_FEE_Offset="<<HT_FEE_Offset<<" L0_TP_ADC="<<L0_TP_ADC[tpid]<<" PedTargetValue="<<pedTargetValue<<endl;
		}
            }
          }
        }
      }
    }
  }

  //Find LUT
  for (int tpid=0;tpid<kNPatches;tpid++){ 
   

    Int_t cr, seq, chan, LUTindex;
    mDecoder->GetCrateAndSequenceFromTriggerPatch(tpid,cr,seq); 
    chan=seq/16;
    
    if (mConfig==kOnline)
      {

	if ( ((L0_TP_ADC[tpid]+LUTped[cr-1][chan]+2)>=0) && (formula[cr-1][chan]==2) && (LUTscale[cr-1][chan]==1) && 
	     (LUTpow[cr-1][chan]!=0) && (LUTsig[cr-1][chan]==0) && (pedTargetValue==24))
	  {
	    LUTindex=L0_TP_ADC[tpid] + LUTped[cr-1][chan] + numMaskTow[tpid];
	    LUT[tpid] = commonLUT[LUTindex];
	  }  
	else
	  {
	    /*    cout<<"Someting NOT right with LUT!"<<
	      " LUTped="<<LUTped[cr-1][chan]+2<<" formula="<<formula[cr-1][chan]<<
	      " LUTscale="<<LUTscale[cr-1][chan]<<" LUTpow="<<LUTpow[cr-1][chan]<<
	      " LUTsig="<<LUTsig[cr-1][chan]<<" pedTargetValue="<<pedTargetValue<<endl;
	      assert(1);
	    */
	  }
	L0_TP_ADC[tpid]=LUT[tpid];    
	TP6bit_adc_holder[tpid]=LUT[tpid]; 
      }
    
   
    if (mConfig==kOffline) {
      // MOCK up LUT table for Offline case
      if ((L0_TP_PED[tpid]-1)>=L0_TP_ADC[tpid]) { 
	L0_TP_ADC[tpid]=1;
	TP6bit_adc_holder[tpid]=1; 
      }
      if ((L0_TP_PED[tpid]-1)< L0_TP_ADC[tpid]) {
	L0_TP_ADC[tpid]-=(L0_TP_PED[tpid]-1);
	TP6bit_adc_holder[tpid]-=(L0_TP_PED[tpid]-1);
      }
      if (L0_TP_ADC[tpid] > 62) {
	L0_TP_ADC[tpid]=62;
	TP6bit_adc_holder[tpid]=62;
      }
    }
        

    if (0) 
      {
	cout<<" tpid="<<tpid<<" cr="<<cr<<" ch="<<chan<<" formula="<<formula[cr-1][chan]<<
	  " TPadc="<<L0_TP_ADC[tpid]<<" OfflinePed="<<L0_TP_PED[tpid]<<" LUTped="<<LUTped[cr-1][chan]<<
	  " numMaskTow="<<numMaskTow[tpid]<<" LUTindex="<<LUTindex<<" OnlineLUT="<<LUT[tpid]<<
	  " diff="<<(L0_TP_ADC[tpid] - (L0_TP_PED[tpid]-1)) - (LUT[tpid])<<endl;   
      }  
  }

  // LOG_DEBUG messages
  LOG_DEBUG << "BEMC trigger patch format is HT/TPsum" << endm;
  for (int dsm = 0; dsm < 30; ++dsm) {
    TString line = Form("TP%d-%d: ",dsm*10,dsm*10+9);
    for (int ch = 0; ch < 10; ++ch) {
      int triggerPatch = dsm*10+ch;
      line += Form("%d/%d ",L0_HT_ADC[triggerPatch],L0_TP_ADC[triggerPatch]);
    }
    LOG_DEBUG << line << endm;
  }
}

void StBemcTriggerSimu::GetTriggerPatchFromCrate(int crate, int seq, int& triggerPatch) const
{
  triggerPatch = (340-10*crate)%150+seq;
  if (crate <= 15) triggerPatch += 150;
}

void StBemcTriggerSimu::FEEini2009()
{
  const bool debug = false;

  fill(numMaskTow,numMaskTow+kNPatches,0);

  ifstream bemcStatus(mBemcStatus);

  assert(bemcStatus);

  LOG_INFO << mBemcStatus << endm;

  TString line;

  while (line.ReadLine(bemcStatus)) {
    if (line.BeginsWith("#")) {
      LOG_INFO << line << endm;
      continue;
    }
    else if (line.BeginsWith("SoftId")) {
      int towerId, crate, crateSeq, towerStatus, highTowerStatus, patchSumStatus, triggerPatch;
      float pedestal;
      sscanf(line,"SoftId %d %d %d %d %d %d %f %d",&towerId,&crate,&crateSeq,&towerStatus,&highTowerStatus,&patchSumStatus,&pedestal,&triggerPatch);
      LOG_INFO << "SoftId " << towerId << '\t' << crate << '\t' << crateSeq << '\t' << towerStatus << '\t' << highTowerStatus << '\t' << patchSumStatus << '\t' << pedestal << '\t' << triggerPatch << endm;
      TriggerPatchFromTowerId[towerId-1] = triggerPatch;
      TowerStatus[towerId-1] = towerStatus;
      ped12[towerId-1] = pedestal;
      if (towerStatus == 0) ++numMaskTow[triggerPatch];
    }
    else if (line.BeginsWith("TriggerPedestalShift")) {
      sscanf(line,"TriggerPedestalShift %lu",&pedTargetValue);
      LOG_INFO << "TriggerPedestalShift " << pedTargetValue << endm;
    }
    else if (line.BeginsWith("triggerPatch")) {
      int triggerPatch, crate, cratePatch, highTowerStatus, patchSumStatus, bitConvMode, LUTformula;
      int p0, p1, p2, p3, p4, p5;
      sscanf(line,"triggerPatch %d %d %d %d %d %d %d %d %d %d %d %d %d",&triggerPatch,&crate,&cratePatch,&highTowerStatus,&patchSumStatus,&bitConvMode,&LUTformula,&p0,&p1,&p2,&p3,&p4,&p5);
      LOG_INFO << "triggerPatch " << triggerPatch << '\t' << crate << '\t' << cratePatch << '\t' << highTowerStatus << '\t' << patchSumStatus << '\t' << bitConvMode << '\t' << LUTformula << '\t' << p0 << '\t' << p1 << '\t' << p2 << '\t' << p3 << '\t' << p4 << '\t' << p5 << endm;
      TriggerPatchFromCrate[crate-1][cratePatch] = triggerPatch;
      DSM_HTStatus[triggerPatch] = highTowerStatus;
      DSM_TPStatus[triggerPatch] = patchSumStatus;
      bitConvValue[triggerPatch] = bitConvMode;
      formula[crate-1][cratePatch] = LUTformula;
      LUTscale[crate-1][cratePatch] = p0;
      LUTped  [crate-1][cratePatch] = p1;
      LUTsig  [crate-1][cratePatch] = p2;
      LUTpow  [crate-1][cratePatch] = p3;
    }
  }

  bemcStatus.close();

  for (int towerId = 1; towerId <= kNTowers; ++towerId) {
    FEEped[towerId-1] = getFEEpedestal(ped12[towerId-1],pedTargetValue,debug);
  }
}

void StBemcTriggerSimu::FEEini2009(int runNumber)
{
  const bool debug = false;

  mTables->getTriggerPedestalShift((int&)pedTargetValue);
  LOG_INFO << "pedTargetValue\t" << pedTargetValue << endm;
  fill(numMaskTow,numMaskTow+kNPatches,0);
  LOG_INFO << "towerId\ttriggerPatch\tcrate\tseq\tstatus\tpedestal\tFEEpedestal\tgain" << endm;
  for (int towerId = 1; towerId <= kNTowers; ++towerId) {
    int crate, seq, status, triggerPatch;
    float rms;
    mDecoder->GetCrateFromTowerId(towerId,crate,seq);
    mDecoder->GetTriggerPatchFromTowerId(towerId,triggerPatch);
    TriggerPatchFromTowerId[towerId-1] = triggerPatch;
    switch (mConfig) {
    case kOnline:
      mTables->getTriggerTowerStatus(crate,seq,status);
      mTables->getTriggerPedestal(crate,seq,ped12[towerId-1]);
      break;
    case kOffline:
      mTables->getStatus(BTOW,towerId,status);
      mTables->getPedestal(BTOW,towerId,0,ped12[towerId-1],rms);
      break;
    default:
      assert(mConfig == kOnline || mConfig == kOffline);
      break;
    } // switch mConfig

    TowerStatus[towerId-1] = status == 1 || status == 18;
    // Mod by Danny    
    if(runNumber > 14000000 && runNumber < 15000000) TowerStatus[towerId-1] = status == 1;

    if (!TowerStatus[towerId-1]) ++numMaskTow[triggerPatch];
    FEEped[towerId-1] = getFEEpedestal(ped12[towerId-1],pedTargetValue,debug);
    mTables->getCalib(BTOW,towerId,1,TowerGain[towerId-1]);
    LOG_INFO << Form("%d\t%d\t%d\t%d\t%d\t%.2f\t%d\t%.5f",towerId,triggerPatch,crate,seq,TowerStatus[towerId-1],ped12[towerId-1],FEEped[towerId-1],TowerGain[towerId-1]) << endm;
  } // for towerId

  // These towers are swapped for Run 9
  // and cross trigger patch boundaries:
  // 4054 <---> 4014 (TP227 <---> TP236)
  // 4055 <---> 4015 (TP227 <---> TP236)
  // 4056 <---> 4016 (TP227 <---> TP236)
  // 4057 <---> 4017 (TP229 <---> TP238)
  swap(TriggerPatchFromTowerId[4054-1],TriggerPatchFromTowerId[4014-1]);
  swap(TriggerPatchFromTowerId[4055-1],TriggerPatchFromTowerId[4015-1]);
  swap(TriggerPatchFromTowerId[4056-1],TriggerPatchFromTowerId[4016-1]);
  swap(TriggerPatchFromTowerId[4057-1],TriggerPatchFromTowerId[4017-1]);

  // Tower 1907 (TP13) has huge pedestal (2068.39) starting with run 10154060
  // and time stamp 2009-06-03 20:38:58 GMT. Mask it off.
  //if (timestamp >= TDatime("2009-06-03 20:38:58").Get()) TowerStatus[1907-1] = 0;
  if (runNumber >= 10154060 && runNumber <= 10180034) TowerStatus[1907-1] = 0;

  // Tower 1233 has zero gain
  if (runNumber >= 10141025 && runNumber <= 10155022) TowerStatus[1233-1] = 0;

  // Tower 4355 has huge pedestal (42949672.00). Mask it out.
  if (runNumber >= 10136035 && runNumber <= 10136070) TowerStatus[4355-1] = 0;

  // Tower 1433 has zero gain in pp500
  if (runNumber >= 10078075 && runNumber <= 10103042) TowerStatus[1433-1] = 0;

  //--zchang
  LOG_INFO <<"LUT pedestal will be calculated by LUTped = LUTped (from database) + NumberOfMaskedOutTowers. --zchang"<<endm;
  LOG_INFO <<"will set all trigger patch formula and bit conversion to 2 --zchang"<<endm;

  LOG_INFO << "triggerPatch\tcrate\tseq\tHTsta\tTPsta\tbitConv\tformula\tLUTscale\tLUTped\tLUTsig\tLUTpow\tpar4\tpar5\tnumMaskTow" << endm;  
  for (int crate = 1; crate <= kNCrates; ++crate) {
    for (int seq = 0; seq < kNSeq; ++seq) {
      int triggerPatch;
      GetTriggerPatchFromCrate(crate,seq,triggerPatch);
      TriggerPatchFromCrate[crate-1][seq] = triggerPatch;
      mTables->getTriggerHighTowerStatus(triggerPatch,DSM_HTStatus[triggerPatch]);
      mTables->getTriggerPatchStatus(triggerPatch,DSM_TPStatus[triggerPatch]);
      mTables->getTriggerBitConv(crate,seq,(int&)bitConvValue[triggerPatch]);
      mTables->getTriggerFormulaTag(crate,seq,formula[crate-1][seq]);

      int parameters[6];
      mTables->getTriggerFormulaParameters(crate,seq,parameters);
      
      LUTscale[crate-1][seq] = parameters[0];
      LUTped  [crate-1][seq] = parameters[1];
      LUTsig  [crate-1][seq] = parameters[2];
      LUTpow  [crate-1][seq] = parameters[3];
      //run12 modify trigger patch pedestal due to masked out towers --zchang
      //if(runNumber > 13000000 && runNumber < 14000000){
      //Mod by Danny
      if(runNumber > 13000000 && runNumber < 15000000){
	//test zchang set all trigger patch formula and bit conversion to 2 --zchang
	bitConvValue[triggerPatch] = 2;
	formula[crate-1][seq] = 2;
	LUTped[crate-1][seq] += numMaskTow[triggerPatch];
      }
      //end --zchang
      LOG_INFO << Form("%d\t%d\t%d\t%d\t%d\t%ld\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d",triggerPatch,crate,seq,DSM_HTStatus[triggerPatch],DSM_TPStatus[triggerPatch],bitConvValue[triggerPatch],formula[crate-1][seq],LUTscale[crate-1][seq],LUTped[crate-1][seq],LUTsig[crate-1][seq],LUTpow[crate-1][seq],parameters[4],parameters[5],numMaskTow[triggerPatch]) << endm;
    } // for seq
  } // for crate

  // Special cases:

  // TP90-99 don't let the BEMC configuration software overwrite their power-up default values
  // with settings from the database. Furthermore, the bitConv bits for TP92-93 are flaky;
  // most of the time bitConv=2, but sometimes bitConv=0.
  fill(bitConvValue+90,bitConvValue+100,2);

  // TP277 TPsum output is always 0
  DSM_TPStatus[277] = 0;

  // TP50-59 have wrong config for run 10128030 (Start 2009-05-08 08:52:14 GMT) to 10129011 (Stop 2009-05-09 07:38:42 GMT)
  //if (TDatime("2009-05-08 08:52:14").Get() <= timestamp && timestamp <= TDatime("2009-05-09 07:38:42").Get()) {
  if (runNumber >= 10128030 && runNumber <= 10129011) {
    bitConvValue[50] = 2;
    bitConvValue[51] = 2;
    bitConvValue[52] = 3;
    bitConvValue[53] = 3;
    bitConvValue[54] = 2;
    bitConvValue[55] = 2;
    bitConvValue[56] = 2;
    bitConvValue[57] = 2;
    bitConvValue[58] = 3;
    bitConvValue[59] = 3;
  }

  // Copycat channels: The last 3 towers of the first trigger patch and all 16 towers of the second trigger patch
  // have duplicate ADCs. A solution is to mask out both the high tower and patch sum output of the affected tower
  // front end electronics (FEEs) in both the simulation and data for consistency. In addition, all 32 towers from
  // both trigger patches (1 FEE board) should be masked out in the offline BEMC status tables.
  // It is not clear what causes this problem but it seems to be a failure mode of either the tower crate multiplexer
  // or the tower data collector (TDC). See the BEMC Technical Design Report, pp. 133-134:
  // http://drupal.star.bnl.gov/STAR/system/files/BEMC%20TDR.pdf
  // This problem has been identified in the BEMC data as far back as 2006 and as recently as 2011.
  // See analysis:
  // http://cyclotron.tamu.edu/pibero/jets/2011.10.18/JetMeeting-2011.10.18.pdf
  if (runNumber >= 10120032 && runNumber <= 10121053) {
    DSM_HTStatus[228] = 0;
    DSM_TPStatus[228] = 0;
    DSM_HTStatus[229] = 0;
    DSM_TPStatus[229] = 0;
  }

  if (runNumber >= 10150051 && runNumber <= 10156058) {
    DSM_HTStatus[122] = 0;
    DSM_TPStatus[122] = 0;
    DSM_HTStatus[123] = 0;
    DSM_TPStatus[123] = 0;
  }

  if (runNumber >= 10156086 && runNumber <= 10160017) {
    DSM_HTStatus[22] = 0;
    DSM_TPStatus[22] = 0;
    DSM_HTStatus[23] = 0;
    DSM_TPStatus[23] = 0;
  }

  if (runNumber >= 10163048 && runNumber <= 10180034) {
    DSM_HTStatus[220] = 0;
    DSM_TPStatus[220] = 0;
    DSM_HTStatus[221] = 0;
    DSM_TPStatus[221] = 0;
  }
}

void StBemcTriggerSimu::FEEout2009()
{
  const bool debug = false;

  StEmcCollection* emc = mEvent->emcCollection();
  if (!emc) {
    LOG_WARN << "No StEmcCollection" << endm;
    return;
  }

  StEmcDetector* det = emc->detector(kBarrelEmcTowerId);
  if (!det) {
    LOG_WARN << "No StEmcDetector" << endm;
    return;
  }

  // FEE action
  int nhits = 0;

  for (size_t m = 1; m <= det->numberOfModules(); ++m) {
    const StSPtrVecEmcRawHit& hits = det->module(m)->hits();
    nhits += hits.size();
    for (size_t i = 0; i < hits.size(); ++i) {
      const StEmcRawHit* hit = hits[i];
      int towerId = hit->softId(BTOW);
      if (TowerStatus[towerId-1]) {
	int triggerPatch = TriggerPatchFromTowerId[towerId-1];
	int ht, pa;
	simulateFEEaction(hit->adc(),FEEped[towerId-1],bitConvValue[triggerPatch],ht,pa,debug);
	HT6bit_adc_holder[towerId-1] = ht;
	if (ht > L0_HT_ADC[triggerPatch]) L0_HT_ADC[triggerPatch] = ht;
	L0_TP_ADC[triggerPatch] += pa;
      }	// if TowerStatus
    } // for i
  } // for m

  // Make sure BTOW has 4800 hits. Towers with no physics signal are just
  // set to pedestal. This is achieved in real data and embedding macros with:
  //
  // StEmcADCtoEMaker* bemcAdc = new StEmcADCtoEMaker;
  // bemcAdc->saveAllStEvent(true);
  //
  // and IN ADDITION simulation macros need:
  //
  // StEmcSimulatorMaker* emcSim = (StEmcSimulatorMaker*)chain->GetMaker("EmcSimulator");
  // emcSim->setCheckStatus(kBarrelEmcTowerId,false);
  // emcSim->setMakeFullDetector(kBarrelEmcTowerId,true);
  // emcSim->setDoZeroSuppression(kBarrelEmcTowerId,false);
  assert(nhits == kNTowers);

  // FEE LUT
  for (int crate = 1; crate <= kNCrates; ++crate) {
    for (int seq = 0; seq < kNSeq; ++seq) {
      int triggerPatch = TriggerPatchFromCrate[crate-1][seq];
      if (!DSM_HTStatus[triggerPatch]) L0_HT_ADC[triggerPatch] = 0;
      if (!DSM_TPStatus[triggerPatch]) L0_TP_ADC[triggerPatch] = 0;
      else {
	int lut;
	simulateFEELUT(L0_TP_ADC[triggerPatch],formula[crate-1][seq],LUTscale[crate-1][seq],LUTped[crate-1][seq],LUTsig[crate-1][seq],LUTpow[crate-1][seq],0,0,numMaskTow[triggerPatch],pedTargetValue,lut,debug);
	L0_TP_ADC[triggerPatch] = TP6bit_adc_holder[triggerPatch] = lut;
      }
    } // for seq
  } // for crate
}

void StBemcTriggerSimu::simulateFEEfailure()
{
  switchon(L0_HT_ADC[166],1);
  switchon(L0_HT_ADC[199],0);
  switchoff(++L0_HT_ADC[277],0);
  switchoff(L0_HT_ADC[292],1);

  int runNumber = StMaker::GetChain()->GetRunNumber();

  if (runNumber >= 10169005 && runNumber <= 10180034) switchon(L0_HT_ADC[196],2);
  if (runNumber >= 10156086 && runNumber <= 10158021) switchoff(++L0_HT_ADC[288],0);

  if (runNumber >= 10131039 && runNumber <= 10139018) switchoff(L0_TP_ADC[239],0);
  if (runNumber >= 10160069 && runNumber <= 10162040) switchoff(L0_TP_ADC[161],1);
  if (runNumber >= 10121092 && runNumber <= 10136031) switchon (L0_TP_ADC[137],0);
  if (runNumber >= 10156031 && runNumber <= 10180034) switchon (L0_TP_ADC[137],0);
  if (runNumber >= 10154060 && runNumber <= 10155022) switchon (L0_TP_ADC[13 ],0);
  //lut to 1 for bad trigger patch 75 --zchang
  if(runNumber >= 13077001 && runNumber <= 14000000){
    L0_TP_ADC[75] = 1;
    LOG_INFO<<Form("patch 75 trigger patch output set to %d", L0_TP_ADC[75])<<endm;
  }
  //end -zchang
}

//==================================================
//==================================================
void StBemcTriggerSimu::get2006_DSMLayer0() {

  //0-(8)9 ADC sum Trigger Patches
  //10-11  HT threshold bits
  //12-13  TP threshold bits
  //14-15  HT&&TP threshold bits

  //SWITCH MISMATCHED tpid HERE for 2006
  int placeholder;
  placeholder=L0_HT_ADC[291];
  L0_HT_ADC[291]=L0_HT_ADC[294];
  L0_HT_ADC[294]=placeholder;
  placeholder=L0_HT_ADC[250];
  L0_HT_ADC[250]=L0_HT_ADC[251];
  L0_HT_ADC[251]=placeholder;
  placeholder=L0_HT_ADC[263];
  L0_HT_ADC[263]=L0_HT_ADC[267];
  L0_HT_ADC[267]=placeholder;

  placeholder=L0_TP_ADC[291];
  L0_TP_ADC[291]=L0_TP_ADC[294];
  L0_TP_ADC[294]=placeholder;
  placeholder=L0_TP_ADC[250];
  L0_TP_ADC[250]=L0_TP_ADC[251];
  L0_TP_ADC[251]=placeholder;
  placeholder=L0_TP_ADC[263];
  L0_TP_ADC[263]=L0_TP_ADC[267];
  L0_TP_ADC[267]=placeholder;

  //Loop over modules
  int k=0;
  int DSM_TP[kL0DsmInputs];
  for (int i=0;i<kL0DsmModule;i++){

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each module
    DSM0_TP_SUM[i]=0; 
    DSM0_HT_Bit[i]=0;
    DSM0_TP_Bit[i]=0;
    DSM0_HTTP_Bit[i]=0;

    DSM0_TP_SUM_J1[i]=0;
    DSM0_HT_Bit_J1[i]=0;
    DSM0_TP_Bit_J1[i]=0;
    DSM0_HTTP_Bit_J1[i]=0;

    DSM0_TP_SUM_J3[i]=0;
    DSM0_HT_Bit_J3[i]=0;
    DSM0_TP_Bit_J3[i]=0;
    DSM0_HTTP_Bit_J3[i]=0;

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each L0 input
    for (int j=0;j<kL0DsmInputs;j++){ 
      DSM0_HT_tp_Bit[j]=0;
      DSM0_TP_tp_Bit[j]=0;
      DSM0_HTTP_tp_Bit[j]=0; 
      DSM0_HT_tp_Bit_J1[j]=0;
      DSM0_TP_tp_Bit_J1[j]=0;
      DSM0_HTTP_tp_Bit_J1[j]=0; 
      DSM0_HT_tp_Bit_J3[j]=0;
      DSM0_TP_tp_Bit_J3[j]=0;
      DSM0_HTTP_tp_Bit_J3[j]=0;      
    }
  
    //Get array of TPid# from DSM module#
    mDecoder->GetTriggerPatchesFromDSM(i,DSM_TP);

#ifdef DEBUG
    // Overwrite input to BEMC layer 0 DSMs (output of BEMC FEEs)
    // with content of trigger bank from MuDst (data only).
    // First fill the Layer0 histograms with results from FEEout()
    if (mHeadMaker->GetDataSet("MuDst")) {
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int triggerPatch = 0; triggerPatch < kNPatches; ++triggerPatch) {
        mBEMCLayer0HT6bit->Fill(triggerPatch,emcTrig.highTower(triggerPatch));
        mBEMCLayer0TP6bit->Fill(triggerPatch,emcTrig.patch(triggerPatch));
        mBEMCLayer0HT6bitDiff->Fill(triggerPatch,emcTrig.highTower(triggerPatch)-L0_HT_ADC[triggerPatch]);
        mBEMCLayer0TP6bitDiff->Fill(triggerPatch,emcTrig.patch(triggerPatch)-L0_TP_ADC[triggerPatch]);
	L0_HT_ADC[triggerPatch] = emcTrig.highTower(triggerPatch);
	L0_TP_ADC[triggerPatch] = emcTrig.patch(triggerPatch); 
      }
    }
#endif
    

    //Loop over 10 inputs to each module 
    for (int j=0;j<kL0DsmInputs;j++){
      
      int tpid=DSM_TP[j];
      int jpid=-1;
      int seq=-1;
      mDecoder->GetJetPatchAndSequenceFromTriggerPatch(tpid, jpid, seq); 

      //Skip modules 2,7,12,17,22,27 
      if (i%5!=2) {
	
	//apply HT thresholds to each HT adc in each TP
	if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit[j]=0;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit[j]=1;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit[j]=2;
	if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit[j]=3;
	
	if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit[j]=0;
	if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit[j]=1;
	if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit[j]=2;
	if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit[j]=3;

	//apply HTTP condition - TP&&HT
	if (DSM0_TP_tp_Bit[j] >= DSM0_HT_tp_Bit[j]) DSM0_HTTP_tp_Bit[j]=DSM0_HT_tp_Bit[j];
	if (DSM0_HT_tp_Bit[j] >= DSM0_TP_tp_Bit[j]) DSM0_HTTP_tp_Bit[j]=DSM0_TP_tp_Bit[j];
	//then || each input
	if (DSM0_HTTP_tp_Bit[j] > DSM0_HTTP_Bit[i]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];

	//add up TP adc for 2/5 of JP
	DSM0_TP_SUM[i]+=L0_TP_ADC[tpid];
	
	if (DSM0_HT_Bit[i]< DSM0_HT_tp_Bit[j]) DSM0_HT_Bit[i]=DSM0_HT_tp_Bit[j];
	if (DSM0_TP_Bit[i]< DSM0_TP_tp_Bit[j]) DSM0_TP_Bit[i]=DSM0_TP_tp_Bit[j];
	if (DSM0_HTTP_Bit[i]< DSM0_HTTP_tp_Bit[j]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];
      }
      
      //Loop over 2x5 inputs(TP) for modules 2,7,12,17,22,29
      if (i%5==2){

	if (j%2)
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J3[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J3[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J3[j]=2;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit_J3[j]=3;
	  }
	else
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J1[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J1[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J1[j]=2;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit_J1[j]=3;
	  }
	
	//apply TP thresholds to each TP adc in each TP
	if (j%2)
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J3[j]=0;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit_J3[j]=1;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit_J3[j]=2;
	    if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit_J3[j]=3;
	  }                       
	else
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J1[j]=0;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit_J1[j]=1;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit_J1[j]=2;
	    if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit_J1[j]=3;
	  }
	
	
	//apply HTTP condition - TP&&HT
	if (j%2) 
	  {
	    if (DSM0_TP_tp_Bit_J3[j] >= DSM0_HT_tp_Bit_J3[j]) DSM0_HTTP_tp_Bit_J3[j]=DSM0_HT_tp_Bit_J3[j];
	    if (DSM0_HT_tp_Bit_J3[j] >= DSM0_TP_tp_Bit_J3[j]) DSM0_HTTP_tp_Bit_J3[j]=DSM0_TP_tp_Bit_J3[j];
	    if (DSM0_HTTP_tp_Bit_J3[j] > DSM0_HTTP_Bit_J3[j]) DSM0_HTTP_Bit_J3[j]=DSM0_HTTP_tp_Bit_J3[j];
	  }
	else
	  {
	    if (DSM0_TP_tp_Bit_J1[j] >= DSM0_HT_tp_Bit_J1[j]) DSM0_HTTP_tp_Bit_J1[j]=DSM0_HT_tp_Bit_J1[j];
	    if (DSM0_HT_tp_Bit_J1[j] >= DSM0_TP_tp_Bit_J1[j]) DSM0_HTTP_tp_Bit_J1[j]=DSM0_TP_tp_Bit_J1[j];
	    if (DSM0_HTTP_tp_Bit_J1[j] > DSM0_HTTP_Bit_J1[j]) DSM0_HTTP_Bit_J1[j]=DSM0_HTTP_tp_Bit_J1[j];
	  }	
	
	
	//add up TP adc for 1/5 of JP
	if (j%2)
	  DSM0_TP_SUM_J3[i]+=L0_TP_ADC[tpid];
	else
	  DSM0_TP_SUM_J1[i]+=L0_TP_ADC[tpid];
	
	//apply HT/TP/HTTP thresholds to bits
	if (DSM0_HT_Bit_J1[i]< DSM0_HT_tp_Bit_J1[j]) DSM0_HT_Bit_J1[i]=DSM0_HT_tp_Bit_J1[j];
	if (DSM0_TP_Bit_J1[i]< DSM0_TP_tp_Bit_J1[j]) DSM0_TP_Bit_J1[i]=DSM0_TP_tp_Bit_J1[j];
	if (DSM0_HTTP_Bit_J1[i]< DSM0_HTTP_tp_Bit_J1[j]) DSM0_HTTP_Bit_J1[i]=DSM0_HTTP_tp_Bit_J1[j];
	if (DSM0_HT_Bit_J3[i]< DSM0_HT_tp_Bit_J3[j]) DSM0_HT_Bit_J3[i]=DSM0_HT_tp_Bit_J3[j];
	if (DSM0_TP_Bit_J3[i]< DSM0_TP_tp_Bit_J3[j]) DSM0_TP_Bit_J3[i]=DSM0_TP_tp_Bit_J3[j];
	if (DSM0_HTTP_Bit_J3[i]< DSM0_HTTP_tp_Bit_J3[j]) DSM0_HTTP_Bit_J3[i]=DSM0_HTTP_tp_Bit_J3[j];
	
      } 
    }
    
    //k==8 is the problem
    if (i%5!=2)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k++]=DSM0_TP_SUM[i]+(DSM0_HT_Bit[i]<<10)+(DSM0_TP_Bit[i]<<12)+(DSM0_HTTP_Bit[i]<<14);
      }
    if (i%5==2)
      {
	L0_16bit_Out[k]=0;
	//cout<<k<<" DSM0_HT_Bit_J3="<<DSM0_HT_Bit_J3[i]<<endl;
	L0_16bit_Out[k++]=DSM0_TP_SUM_J3[i]+(DSM0_HT_Bit_J3[i]<<10)+(DSM0_TP_Bit_J3[i]<<12)+(DSM0_HTTP_Bit_J3[i]<<14);
	//cout<<k<<" DSM0_HT_Bit_J1="<<DSM0_HT_Bit_J1[i]<<endl;
	L0_16bit_Out[k++]=DSM0_TP_SUM_J1[i]+(DSM0_HT_Bit_J1[i]<<10)+(DSM0_TP_Bit_J1[i]<<12)+(DSM0_HTTP_Bit_J1[i]<<14);

      }

  }

#ifdef DEBUG

  // Fill diagnostic histograms
  if (mHeadMaker->GetDataSet("MuDst")) {
    // BEMC layer 1 DSMs are stored in this order in the trigger bank:
    // BE101, BE102, BE103, BW101, BW102, BW103
    // DSM channels are read out in this order:
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    // Trigger bank <-> Emulator ==> 0, 1, 2, 3, 4, 5 <-> 3, 4, 5, 0, 1, 2
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();

    // Loop over BEMC layer 1 DSMs
    for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {
      // Loop over layer 1 input channels
      for (int ch = 0; ch < kL1DsmInputs; ++ch) {

	Int_t idx = dsm*8+dsm_read_map[ch];
	Int_t TrigBankOut = emcTrig.bemcLayer1(idx);
        Int_t TPSumout = (TrigBankOut & 0x3ff);
        Int_t TPSumbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x3ff);
        Int_t HTout = (TrigBankOut & 0xc00)/0x400;
	Int_t HTbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0xc00)/0x400; 
 	Int_t TPout = (TrigBankOut & 0x3000)/0x1000;
	Int_t TPbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x3000)/0x1000;
	Int_t HTTPout = (TrigBankOut & 0xc000)/0x4000;
	Int_t HTTPbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0xc000)/0x4000;
	int TPSumdiff = (TPSumbits)-(TPSumout);
	int HTdiff = (HTbits) - (HTout);
	int TPdiff = (TPbits) - (TPout);
	int HTTPdiff = (HTTPbits) - (HTTPout);
	mBEMCLayer1PatchSum->Fill(dsm*6+ch, TPSumout);
	mBEMCLayer1HTBits->Fill(dsm*6+ch, HTout);	
	mBEMCLayer1TPBits->Fill(dsm*6+ch, TPout);
	mBEMCLayer1HTTPBits->Fill(dsm*6+ch, HTTPout);
	mBEMCLayer1PatchSumDiff->Fill(dsm*6+ch, TPSumdiff);
	mBEMCLayer1HTBitsDiff->Fill(dsm*6+ch, HTdiff);
	mBEMCLayer1TPBitsDiff->Fill(dsm*6+ch, TPdiff);
	mBEMCLayer1HTTPBitsDiff->Fill(dsm*6+ch, HTTPdiff);  
      }
    }
  }
#endif

}


//==================================================
//==================================================
void StBemcTriggerSimu::get2006_DSMLayer1(){


  //DSM_Layer0 is passed to DSM_Layer1 in 8 UShort blocks (16 bits)
  //There are 6 DSM_Layer1 boards and each can take 120 bits total
  //So DSM_Layer0 passes 8 shorts (16*8=128) or 128 bits to each DSM_Layer1

  //Zero out the DSMLayer1 Bits passed to DSMLayer2
  for (int i=0;i<kL1DsmModule;i++){
    DSM1_JP_Bit[i]=0;
    DSM1_HTj0_Bit[i]=0;
    DSM1_HTj1_Bit[i]=0;
    DSM1_TP_Bit[i]=0;
    DSM1_HTTP_Bit[i]=0;
    DSM1_ETOT_ADC[i]=0;
  }


#ifdef DEBUG
    // Overwrite input to BEMC layer 1 DSMs (output of BEMC layer 0 DSMs)
    // with content of trigger bank from MuDst (data only).
    if (mHeadMaker->GetDataSet("MuDst")) {
      static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
      static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int dsm = 0; dsm < 6; ++dsm) {
	int offset = TriggerBankToSimuMap[dsm]*5;
	DSM0_TP_SUM   [offset+0] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[0]) & 0x3ff;
	DSM0_TP_SUM   [offset+1] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[1]) & 0x3ff;
	DSM0_TP_SUM_J3[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[2]) & 0x1ff;
	DSM0_TP_SUM_J1[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[3]) & 0x1ff;
	DSM0_TP_SUM   [offset+3] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[4]) & 0x3ff;
	DSM0_TP_SUM   [offset+4] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[5]) & 0x3ff;
      }
    }
#endif
    
  //Sum TP ADC into JP's
  // West
  DSM1_JP_ADC[0]=DSM0_TP_SUM[0]+DSM0_TP_SUM[1]+DSM0_TP_SUM_J3[2];
  DSM1_JP_ADC[1]=DSM0_TP_SUM[3]+DSM0_TP_SUM[4]+DSM0_TP_SUM_J1[2];
  DSM1_JP_ADC[2]=DSM0_TP_SUM[5]+DSM0_TP_SUM[6]+DSM0_TP_SUM_J3[7];
  DSM1_JP_ADC[3]=DSM0_TP_SUM[8]+DSM0_TP_SUM[9]+DSM0_TP_SUM_J1[7];
  DSM1_JP_ADC[4]=DSM0_TP_SUM[10]+DSM0_TP_SUM[11]+DSM0_TP_SUM_J3[12];
  DSM1_JP_ADC[5]=DSM0_TP_SUM[13]+DSM0_TP_SUM[14]+DSM0_TP_SUM_J1[12];
  
  // East
  DSM1_JP_ADC[6]=DSM0_TP_SUM[15]+DSM0_TP_SUM[16]+DSM0_TP_SUM_J1[17];
  DSM1_JP_ADC[7]=DSM0_TP_SUM[18]+DSM0_TP_SUM[19]+DSM0_TP_SUM_J3[17];
  DSM1_JP_ADC[8]=DSM0_TP_SUM[20]+DSM0_TP_SUM[21]+DSM0_TP_SUM_J1[22];
  DSM1_JP_ADC[9]=DSM0_TP_SUM[23]+DSM0_TP_SUM[24]+DSM0_TP_SUM_J3[22];
  DSM1_JP_ADC[10]=DSM0_TP_SUM[25]+DSM0_TP_SUM[26]+DSM0_TP_SUM_J1[27];
  DSM1_JP_ADC[11]=DSM0_TP_SUM[28]+DSM0_TP_SUM[29]+DSM0_TP_SUM_J3[27];
 
  for (int hh=0;hh<12;hh++) JP_adc_holder[hh]=DSM1_JP_ADC[hh];
  
  //Test each JP and see if it passed
  for (int i=0;i<kNJet;i++)
    {
      DSM1_JP_jp_Bit[i]=0;
      if ( DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,0)) DSM1_JP_jp_Bit[i]=0;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,0))) DSM1_JP_jp_Bit[i]=1;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,1))) DSM1_JP_jp_Bit[i]=2;
      if ( DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) DSM1_JP_jp_Bit[i]=3;
    }  


  int mod;
  //Translate JP's into 2 bits to pass to DSMLayer2
  for (int i=0;i<kNJet;i++){
    if (i < (kNJet/2)) mod = 0;
    else mod = 1;
    DSM1_ETOT_ADC[mod]+=DSM1_JP_ADC[i];
    if ( DSM1_JP_Bit[i/2] < DSM1_JP_jp_Bit[i]) DSM1_JP_Bit[i/2]=DSM1_JP_jp_Bit[i];   
  }


  //HTTP and TP bits
  for (int i=0; i<kL1DsmModule; i++){
    for (int j=0; j<5; j++){
      int k= i*5 + j;
      int kk=i*5 + 2;
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit[k]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit[k];   
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit_J3[kk]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit_J3[kk];   
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit_J1[kk]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit_J1[kk];
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit[k]) DSM1_TP_Bit[i]=DSM0_TP_Bit[k];      
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit_J3[kk]) DSM1_TP_Bit[i]=DSM0_TP_Bit_J3[kk];   
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit_J1[kk]) DSM1_TP_Bit[i]=DSM0_TP_Bit_J1[kk];         
    }

    if (DSM1_HTTP_Bit[i]>=2) DSM1_HTTP_Bit[i]=1;
    else if (DSM1_HTTP_Bit[i]<2) DSM1_HTTP_Bit[i]=0;

    if (DSM1_TP_Bit[i]>=2) DSM1_TP_Bit[i]=1;
    else if (DSM1_TP_Bit[i]<2) DSM1_TP_Bit[i]=0;
  }


  //WEST  HT bits
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[0]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[0];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[1]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[1];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit_J3[2]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit_J3[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit_J1[2]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit_J1[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[3]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[3];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[4]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[4];

  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[5]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[5];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[6]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[6];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit_J3[7]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit_J3[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit_J1[7]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit_J1[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[8]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[8];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[9]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[9];

  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[10]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[10];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[11]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[11];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit_J3[12]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit_J3[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit_J1[12]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit_J1[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[13]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[13];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[14]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[14];

  //EAST HT bits
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[15]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[15];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[16]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[16];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit_J1[17]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit_J1[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit_J3[17]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit_J3[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[18]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[18];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[19]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[19];

  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[20]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[20];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[21]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[21];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit_J1[22]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit_J1[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit_J3[22]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit_J3[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[23]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[23];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[24]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[24];

  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[25]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[25];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[26]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[26];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit_J1[27]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit_J1[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit_J3[27]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit_J3[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[28]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[28];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[29]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[29];
  


  
  
  //Drop two lowest bits for ETOT and OR Bits>6 with 6
  for (int i=0;i<kL1DsmModule;i++) {
    DSM1_ETOT_ADC[i]/=4;
    if (DSM1_ETOT_ADC[i]>31) DSM1_ETOT_ADC[i]=31;
  }


#ifdef DEBUG

  if (mHeadMaker->GetDataSet("MuDst")) {
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    for (int dsm = 0; dsm < kL2DsmModule; ++dsm) {
      for (int ch = 0; ch < 6; ++ch) {
	int idx = dsm_read_map[ch];
	int TrigBankOut = emcTrig.emcLayer2(idx);
	int jetPatch = 2 * TriggerBankToSimuMap[ch];
	int sum = DSM1_JP_ADC[jetPatch] + DSM1_JP_ADC[jetPatch+1];
	sum = (sum >> 7) ? 31 : (sum >> 2 & 0x1f);
	int diff = (TrigBankOut & 0x1f) - (sum & 0x1f);
	mBEMCLayer2PatchSum->Fill(ch, TrigBankOut & 0x1f);
	mBEMCLayer2PatchSumDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 7 & 0x1) - (DSM1_HTTP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTTPBits->Fill(ch, TrigBankOut >> 7 & 0x1);
	mBEMCLayer2HTTPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 9 & 0x1) - (DSM1_TP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2TPBits->Fill(ch, TrigBankOut >> 9 & 0x1);
	mBEMCLayer2TPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 10 & 0x3) - (DSM1_JP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2JPBits->Fill(ch, TrigBankOut >> 10 & 0x3);
	mBEMCLayer2JPBitsDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 12 & 0x3) - (DSM1_HTj0_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj0Bits->Fill(ch, TrigBankOut >> 12 & 0x3);
	mBEMCLayer2HTj0BitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 14 & 0x3) - (DSM1_HTj1_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj1Bits->Fill(ch, TrigBankOut >> 14 & 0x3);
	mBEMCLayer2HTj1BitsDiff->Fill(ch,diff);
 
      }
    }
  }
#endif

}

void StBemcTriggerSimu::get2006_DSMLayer2()
{

  // In hardware the final trigger decisions are made in the TCU 
  // It is not possible to compare the emulator with the TCU input
  // so all final trigger decisions for the BEMC are made at Layer2 
  // in this code

  Int_t DSM2_JP_Bit=0;
  Int_t DSM2_HT_Bit=0;
  //Int_t DSM2_Esum_Bit=0;
  //Int_t DSM2_Topo_Bit=0;
  Int_t DSM2_HTTP_Bit=0;
  Int_t DSM2_TP_Bit=0;

    
  for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {

    if (DSM2_JP_Bit<DSM1_JP_Bit[dsm]) DSM2_JP_Bit=DSM1_JP_Bit[dsm];
    if (DSM2_HTTP_Bit<DSM1_HTTP_Bit[dsm]) DSM2_HTTP_Bit=DSM1_HTTP_Bit[dsm];
    if (DSM2_HT_Bit<DSM1_HTj0_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj0_Bit[dsm];
    if (DSM2_HT_Bit<DSM1_HTj1_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj1_Bit[dsm];
    if (DSM2_TP_Bit<DSM1_TP_Bit[dsm]) DSM2_TP_Bit=DSM1_TP_Bit[dsm];
  }
  
  //HT
  if (DSM2_HT_Bit > 2){
    mFiredTriggers.push_back(127212);
    mFiredTriggers.push_back(137213);
  }
  
  //HTTP trigger
  if (DSM2_HTTP_Bit==1) {
      mFiredTriggers.push_back(127611);
      mFiredTriggers.push_back(127821);
      mFiredTriggers.push_back(137821);
      mFiredTriggers.push_back(137822);
      mFiredTriggers.push_back(137611);
      mFiredTriggers.push_back(5);

      // Upsilon triggers
      mFiredTriggers.push_back(117601);
      mFiredTriggers.push_back(117602);
      mFiredTriggers.push_back(137602);
      mFiredTriggers.push_back(137603);
    } 


  //JP Trigger
  if (DSM2_JP_Bit >= 1) {  
    mFiredTriggers.push_back(127501);
    mFiredTriggers.push_back(137501);
    mFiredTriggers.push_back(127622);
    mFiredTriggers.push_back(137622);
  }
  
  if (DSM2_JP_Bit >= 2) {
    mFiredTriggers.push_back(127221);
    mFiredTriggers.push_back(137221);
    mFiredTriggers.push_back(137222);
  }

  // J/psi topology trigger
  //
  // See http://www.star.bnl.gov/public/trg/TSL/Software/EMC_2006.pdf
  //
  // The J/psi trigger uses just the high tower threshold bits of the Barrel,
  // not the Endcap. R2 selects ONE of the high-tower thresholds for this trigger.
  // The J/psi trigger fires if two opposite jet patches have high towers
  // above the selected threshold.
  //
  // +-----------+----------+------------+------------+
  // | DSM index | DSM name | j0         | j1         |
  // +-----------+----------+------------+------------+
  // | 0         | BE101    | 10' (JP6)  | 12' (JP7)  |
  // | 1         | BE102    | 2'  (JP8)  | 4'  (JP9)  |
  // | 2         | BE103    | 6'  (JP10) | 8'  (JP11) |
  // | 3         | BW101    | 10' (JP0)  | 12' (JP1)  |
  // | 4         | BW102    | 2'  (JP2)  | 4'  (JP3)  |
  // | 5         | BW103    | 6'  (JP4)  | 8'  (JP5)  |
  // +-----------+----------+------------+------------+
  //
  // The J/psi topology trigger uses register R2 to select the
  // high tower threshold used for each decay electron of the J/psi.
  // In Run 6, R2 was always set to th0. So it is hardcoded to 1
  // in the code below. Possible values are:
  // 0 - th0; 1 - th1; 2 - th2; 3 - J/psi logic OFF.
  //
  // Vector bits (0-5) correspond to positions 2,4,6,8,10,12 o'clock.

  int barrel_east_ht_jpsi[6];

  barrel_east_ht_jpsi[0] = DSM1_HTj0_Bit[1] >= 1; // 2 o'clock
  barrel_east_ht_jpsi[1] = DSM1_HTj1_Bit[1] >= 1; // 4 o'clock
  barrel_east_ht_jpsi[2] = DSM1_HTj0_Bit[2] >= 1; // 6 o'clock
  barrel_east_ht_jpsi[3] = DSM1_HTj1_Bit[2] >= 1; // 8 o'clock
  barrel_east_ht_jpsi[4] = DSM1_HTj0_Bit[0] >= 1; // 10 o'clock
  barrel_east_ht_jpsi[5] = DSM1_HTj1_Bit[0] >= 1; // 12 o'clock

  int barrel_west_ht_jpsi[6];

  barrel_west_ht_jpsi[0] = DSM1_HTj0_Bit[4] >= 1; // 2 o'clock
  barrel_west_ht_jpsi[1] = DSM1_HTj1_Bit[4] >= 1; // 4 o'clock
  barrel_west_ht_jpsi[2] = DSM1_HTj0_Bit[5] >= 1; // 6 o'clock
  barrel_west_ht_jpsi[3] = DSM1_HTj1_Bit[5] >= 1; // 8 o'clock
  barrel_west_ht_jpsi[4] = DSM1_HTj0_Bit[3] >= 1; // 10 o'clock
  barrel_west_ht_jpsi[5] = DSM1_HTj1_Bit[3] >= 1; // 12 o'clock

  int ht_jpsi[6];

  for (int i = 0; i < 6; ++i) ht_jpsi[i] = barrel_east_ht_jpsi[i] || barrel_west_ht_jpsi[i];

  int jpsiTrigger = ((ht_jpsi[0] && (ht_jpsi[2] || ht_jpsi[3] || ht_jpsi[4])) ||
		     (ht_jpsi[1] && (ht_jpsi[3] || ht_jpsi[4] || ht_jpsi[5])) ||
		     (ht_jpsi[2] && (ht_jpsi[4] || ht_jpsi[5])) ||
		     (ht_jpsi[3] && ht_jpsi[5]));

  // jpsi-mb trigger
  if (jpsiTrigger) {
    mFiredTriggers.push_back(117705);
    mFiredTriggers.push_back(137705);

    // Collect all towers above threshold and group them
    // by phi position, i.e. 2, 4, ..., 12 o'clock.
    const int phipos[] = { 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3 }; // JP0-JP11
    vector<int> ht_jpsi_ids[6];

    for (int towerId = 1; towerId <= kNTowers; ++towerId) {
      int triggerPatch, dsm;
      mDecoder->GetTriggerPatchFromTowerId(towerId,triggerPatch);
      mDecoder->GetDSMFromTriggerPatch(triggerPatch,dsm);

      if (HT6bit_adc_holder[towerId-1] > mDbThres->GetHT_DSM0_threshold(dsm,timestamp,0)) {
	int jetPatch;
	mDecoder->GetJetPatchFromTowerId(towerId,jetPatch);
	ht_jpsi_ids[phipos[jetPatch]].push_back(towerId);
      }
    }

    // Make J/psi candidates
    get2006_JpsiCandidates(ht_jpsi_ids[0],ht_jpsi_ids[2]); // 2 o'clock and 6 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[0],ht_jpsi_ids[3]); // 2 o'clock and 8 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[0],ht_jpsi_ids[4]); // 2 o'clock and 10 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[1],ht_jpsi_ids[3]); // 4 o'clock and 8 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[1],ht_jpsi_ids[4]); // 4 o'clock and 10 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[1],ht_jpsi_ids[5]); // 4 o'clock and 12 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[2],ht_jpsi_ids[4]); // 6 o'clock and 10 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[2],ht_jpsi_ids[5]); // 6 o'clock and 12 o'clock
    get2006_JpsiCandidates(ht_jpsi_ids[3],ht_jpsi_ids[5]); // 8 o'clock and 12 o'clock

#if 0
    LOG_INFO << "nJpsiCandidates = " << mJpsiCandidates.size() << endm;

    for (size_t i = 0; i < mJpsiCandidates.size(); ++i) {
      const pair<int,int>& towers = mJpsiCandidates[i];
      LOG_INFO << towers.first << " " << towers.second << endm;
    }
#endif
  }
}

void StBemcTriggerSimu::get2006_JpsiCandidates(const vector<int>& towerIds1, const vector<int>& towerIds2)
{
  for (size_t i = 0; i < towerIds1.size(); ++i)
    for (size_t j = 0; j < towerIds2.size(); ++j)
      mJpsiCandidates.push_back(make_pair(towerIds1[i],towerIds2[j]));
}

//==================================================
//==================================================
void StBemcTriggerSimu::get2007_DSMLayer0() {

  //0-(8)9 ADC sum Trigger Patches
  //10-11  HT threshold bits
  //12-13  TP threshold bits
  //14-15  HT&&TP threshold bits

  //Loop over modules
  int k=0;
  int DSM_TP[kL0DsmInputs];
  for (int i=0;i<kL0DsmModule;i++){

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each module
    DSM0_TP_SUM[i]=0; 
    DSM0_HT_Bit[i]=0;
    DSM0_TP_Bit[i]=0;
    DSM0_HTTP_Bit[i]=0;

    DSM0_TP_SUM_J1[i]=0;
    DSM0_HT_Bit_J1[i]=0;
    DSM0_TP_Bit_J1[i]=0;
    DSM0_HTTP_Bit_J1[i]=0;

    DSM0_TP_SUM_J3[i]=0;
    DSM0_HT_Bit_J3[i]=0;
    DSM0_TP_Bit_J3[i]=0;
    DSM0_HTTP_Bit_J3[i]=0;

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each L0 input
    for (int j=0;j<kL0DsmInputs;j++){ 
      DSM0_HT_tp_Bit[j]=0;
      DSM0_TP_tp_Bit[j]=0;
      DSM0_HTTP_tp_Bit[j]=0; 
      DSM0_HT_tp_Bit_J1[j]=0;
      DSM0_TP_tp_Bit_J1[j]=0;
      DSM0_HTTP_tp_Bit_J1[j]=0; 
      DSM0_HT_tp_Bit_J3[j]=0;
      DSM0_TP_tp_Bit_J3[j]=0;
      DSM0_HTTP_tp_Bit_J3[j]=0;      
    }
  
    //Get array of TPid# from DSM module#
    mDecoder->GetTriggerPatchesFromDSM(i,DSM_TP);

#ifdef DEBUG
    // Overwrite input to BEMC layer 0 DSMs (output of BEMC FEEs)
    // with content of trigger bank from MuDst (data only).
    // First fill the Layer0 histograms with results from FEEout()
    if (mHeadMaker->GetDataSet("MuDst")) {
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int triggerPatch = 0; triggerPatch < kNPatches; ++triggerPatch) {
        mBEMCLayer0HT6bit->Fill(triggerPatch,emcTrig.highTower(triggerPatch));
        mBEMCLayer0TP6bit->Fill(triggerPatch,emcTrig.patch(triggerPatch));
        mBEMCLayer0HT6bitDiff->Fill(triggerPatch,emcTrig.highTower(triggerPatch)-L0_HT_ADC[triggerPatch]);
        mBEMCLayer0TP6bitDiff->Fill(triggerPatch,emcTrig.patch(triggerPatch)-L0_TP_ADC[triggerPatch]);
	L0_HT_ADC[triggerPatch] = emcTrig.highTower(triggerPatch);
	L0_TP_ADC[triggerPatch] = emcTrig.patch(triggerPatch); 
      }
    }
#endif
    

    //Loop over 10 inputs to each module 
    for (int j=0;j<kL0DsmInputs;j++){
      
      int tpid=DSM_TP[j];
      int jpid=-1;
      int seq=-1;
      mDecoder->GetJetPatchAndSequenceFromTriggerPatch(tpid, jpid, seq); 

      //Skip modules 2,7,12,17,22,27 
      if (i%5!=2) {
	
	//apply HT thresholds to each HT adc in each TP
	if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit[j]=0;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit[j]=1;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit[j]=2;
	if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit[j]=3;
	
	if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit[j]=0;
	if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit[j]=1;
	if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit[j]=2;
	if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit[j]=3;

	//apply HTTP condition - TP&&HT
	if (DSM0_TP_tp_Bit[j] >= DSM0_HT_tp_Bit[j]) DSM0_HTTP_tp_Bit[j]=DSM0_HT_tp_Bit[j];
	if (DSM0_HT_tp_Bit[j] >= DSM0_TP_tp_Bit[j]) DSM0_HTTP_tp_Bit[j]=DSM0_TP_tp_Bit[j];
	//then || each input
	if (DSM0_HTTP_tp_Bit[j] > DSM0_HTTP_Bit[i]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];

	//add up TP adc for 2/5 of JP
	DSM0_TP_SUM[i]+=L0_TP_ADC[tpid];
	
	if (DSM0_HT_Bit[i]< DSM0_HT_tp_Bit[j]) DSM0_HT_Bit[i]=DSM0_HT_tp_Bit[j];
	if (DSM0_TP_Bit[i]< DSM0_TP_tp_Bit[j]) DSM0_TP_Bit[i]=DSM0_TP_tp_Bit[j];
	if (DSM0_HTTP_Bit[i]< DSM0_HTTP_tp_Bit[j]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];
      }
      
      //Loop over 2x5 inputs(TP) for modules 2,7,12,17,22,29
      if (i%5==2){

	if (j%2)
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J3[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J3[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J3[j]=2;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit_J3[j]=3;
	  }
	else
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J1[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J1[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J1[j]=2;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit_J1[j]=3;
	  }
	
	//apply TP thresholds to each TP adc in each TP
	if (j%2)
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J3[j]=0;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit_J3[j]=1;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit_J3[j]=2;
	    if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit_J3[j]=3;
	  }                       
	else
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J1[j]=0;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit_J1[j]=1;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit_J1[j]=2;
	    if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit_J1[j]=3;
	  }
	
	
	//apply HTTP condition - TP&&HT
	if (j%2) 
	  {
	    if (DSM0_TP_tp_Bit_J3[j] >= DSM0_HT_tp_Bit_J3[j]) DSM0_HTTP_tp_Bit_J3[j]=DSM0_HT_tp_Bit_J3[j];
	    if (DSM0_HT_tp_Bit_J3[j] >= DSM0_TP_tp_Bit_J3[j]) DSM0_HTTP_tp_Bit_J3[j]=DSM0_TP_tp_Bit_J3[j];
	    if (DSM0_HTTP_tp_Bit_J3[j] > DSM0_HTTP_Bit_J3[j]) DSM0_HTTP_Bit_J3[j]=DSM0_HTTP_tp_Bit_J3[j];
	  }
	else
	  {
	    if (DSM0_TP_tp_Bit_J1[j] >= DSM0_HT_tp_Bit_J1[j]) DSM0_HTTP_tp_Bit_J1[j]=DSM0_HT_tp_Bit_J1[j];
	    if (DSM0_HT_tp_Bit_J1[j] >= DSM0_TP_tp_Bit_J1[j]) DSM0_HTTP_tp_Bit_J1[j]=DSM0_TP_tp_Bit_J1[j];
	    if (DSM0_HTTP_tp_Bit_J1[j] > DSM0_HTTP_Bit_J1[j]) DSM0_HTTP_Bit_J1[j]=DSM0_HTTP_tp_Bit_J1[j];
	  }	
	
	
	//add up TP adc for 1/5 of JP
	if (j%2)
	  DSM0_TP_SUM_J3[i]+=L0_TP_ADC[tpid];
	else
	  DSM0_TP_SUM_J1[i]+=L0_TP_ADC[tpid];
	
	//apply HT/TP/HTTP thresholds to bits
	if (DSM0_HT_Bit_J1[i]< DSM0_HT_tp_Bit_J1[j]) DSM0_HT_Bit_J1[i]=DSM0_HT_tp_Bit_J1[j];
	if (DSM0_TP_Bit_J1[i]< DSM0_TP_tp_Bit_J1[j]) DSM0_TP_Bit_J1[i]=DSM0_TP_tp_Bit_J1[j];
	if (DSM0_HTTP_Bit_J1[i]< DSM0_HTTP_tp_Bit_J1[j]) DSM0_HTTP_Bit_J1[i]=DSM0_HTTP_tp_Bit_J1[j];
	if (DSM0_HT_Bit_J3[i]< DSM0_HT_tp_Bit_J3[j]) DSM0_HT_Bit_J3[i]=DSM0_HT_tp_Bit_J3[j];
	if (DSM0_TP_Bit_J3[i]< DSM0_TP_tp_Bit_J3[j]) DSM0_TP_Bit_J3[i]=DSM0_TP_tp_Bit_J3[j];
	if (DSM0_HTTP_Bit_J3[i]< DSM0_HTTP_tp_Bit_J3[j]) DSM0_HTTP_Bit_J3[i]=DSM0_HTTP_tp_Bit_J3[j];
	
      } 
    }
    
    //k==8 is the problem
    if (i%5!=2)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k++]=DSM0_TP_SUM[i]+(DSM0_HT_Bit[i]<<10)+(DSM0_TP_Bit[i]<<12)+(DSM0_HTTP_Bit[i]<<14);
      }
    if (i%5==2)
      {
	L0_16bit_Out[k]=0;
	//cout<<k<<" DSM0_HT_Bit_J3="<<DSM0_HT_Bit_J3[i]<<endl;
	L0_16bit_Out[k++]=DSM0_TP_SUM_J3[i]+(DSM0_HT_Bit_J3[i]<<10)+(DSM0_TP_Bit_J3[i]<<12)+(DSM0_HTTP_Bit_J3[i]<<14);
	//cout<<k<<" DSM0_HT_Bit_J1="<<DSM0_HT_Bit_J1[i]<<endl;
	L0_16bit_Out[k++]=DSM0_TP_SUM_J1[i]+(DSM0_HT_Bit_J1[i]<<10)+(DSM0_TP_Bit_J1[i]<<12)+(DSM0_HTTP_Bit_J1[i]<<14);

      }

  }

#ifdef DEBUG

  // Fill diagnostic histograms
  if (mHeadMaker->GetDataSet("MuDst")) {
    // BEMC layer 1 DSMs are stored in this order in the trigger bank:
    // BE101, BE102, BE103, BW101, BW102, BW103
    // DSM channels are read out in this order:
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    // Trigger bank <-> Emulator ==> 0, 1, 2, 3, 4, 5 <-> 3, 4, 5, 0, 1, 2
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();

    // Loop over BEMC layer 1 DSMs
    for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {
      // Loop over layer 1 input channels
      for (int ch = 0; ch < kL1DsmInputs; ++ch) {

	Int_t idx = dsm*8+dsm_read_map[ch];
	Int_t TrigBankOut = emcTrig.bemcLayer1(idx);
        Int_t TPSumout = (TrigBankOut & 0x3ff);
        Int_t TPSumbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x3ff);
        Int_t HTout = (TrigBankOut & 0xc00)/0x400;
	Int_t HTbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0xc00)/0x400; 
 	Int_t TPout = (TrigBankOut & 0x3000)/0x1000;
	Int_t TPbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x3000)/0x1000;
	Int_t HTTPout = (TrigBankOut & 0xc000)/0x4000;
	Int_t HTTPbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0xc000)/0x4000;
	int TPSumdiff = (TPSumbits)-(TPSumout);
	int HTdiff = (HTbits) - (HTout);
	int TPdiff = (TPbits) - (TPout);
	int HTTPdiff = (HTTPbits) - (HTTPout);
	mBEMCLayer1PatchSum->Fill(dsm*6+ch, TPSumout);
	mBEMCLayer1HTBits->Fill(dsm*6+ch, HTout);	
	mBEMCLayer1TPBits->Fill(dsm*6+ch, TPout);
	mBEMCLayer1HTTPBits->Fill(dsm*6+ch, HTTPout);
	mBEMCLayer1PatchSumDiff->Fill(dsm*6+ch, TPSumdiff);
	mBEMCLayer1HTBitsDiff->Fill(dsm*6+ch, HTdiff);
	mBEMCLayer1TPBitsDiff->Fill(dsm*6+ch, TPdiff);
	mBEMCLayer1HTTPBitsDiff->Fill(dsm*6+ch, HTTPdiff);  

      }
    }
  }
#endif

}


//==================================================
//==================================================
void StBemcTriggerSimu::get2007_DSMLayer1(){


  //DSM_Layer0 is passed to DSM_Layer1 in 8 UShort blocks (16 bits)
  //There are 6 DSM_Layer1 boards and each can take 120 bits total
  //So DSM_Layer0 passes 8 shorts (16*8=128) or 128 bits to each DSM_Layer1

  //Zero out the DSMLayer1 Bits passed to DSMLayer2
  for (int i=0;i<kL1DsmModule;i++){
    DSM1_JP_Bit[i]=0;
    DSM1_HTj0_Bit[i]=0;
    DSM1_HTj1_Bit[i]=0;
    DSM1_TP_Bit[i]=0;
    DSM1_HTTP_Bit[i]=0;
    DSM1_ETOT_ADC[i]=0;
  }


#ifdef DEBUG
    // Overwrite input to BEMC layer 1 DSMs (output of BEMC layer 0 DSMs)
    // with content of trigger bank from MuDst (data only).
    if (mHeadMaker->GetDataSet("MuDst")) {
      static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
      static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int dsm = 0; dsm < 6; ++dsm) {
	int offset = TriggerBankToSimuMap[dsm]*5;
	DSM0_TP_SUM   [offset+0] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[0]) & 0x3ff;
	DSM0_TP_SUM   [offset+1] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[1]) & 0x3ff;
	DSM0_TP_SUM_J3[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[2]) & 0x1ff;
	DSM0_TP_SUM_J1[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[3]) & 0x1ff;
	DSM0_TP_SUM   [offset+3] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[4]) & 0x3ff;
	DSM0_TP_SUM   [offset+4] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[5]) & 0x3ff;
      }
    }
#endif
    
  //Sum TP ADC into JP's
  // West
  DSM1_JP_ADC[0]=DSM0_TP_SUM[0]+DSM0_TP_SUM[1]+DSM0_TP_SUM_J3[2];
  DSM1_JP_ADC[1]=DSM0_TP_SUM[3]+DSM0_TP_SUM[4]+DSM0_TP_SUM_J1[2];
  DSM1_JP_ADC[2]=DSM0_TP_SUM[5]+DSM0_TP_SUM[6]+DSM0_TP_SUM_J3[7];
  DSM1_JP_ADC[3]=DSM0_TP_SUM[8]+DSM0_TP_SUM[9]+DSM0_TP_SUM_J1[7];
  DSM1_JP_ADC[4]=DSM0_TP_SUM[10]+DSM0_TP_SUM[11]+DSM0_TP_SUM_J3[12];
  DSM1_JP_ADC[5]=DSM0_TP_SUM[13]+DSM0_TP_SUM[14]+DSM0_TP_SUM_J1[12];
  
  // East
  DSM1_JP_ADC[6]=DSM0_TP_SUM[15]+DSM0_TP_SUM[16]+DSM0_TP_SUM_J1[17];
  DSM1_JP_ADC[7]=DSM0_TP_SUM[18]+DSM0_TP_SUM[19]+DSM0_TP_SUM_J3[17];
  DSM1_JP_ADC[8]=DSM0_TP_SUM[20]+DSM0_TP_SUM[21]+DSM0_TP_SUM_J1[22];
  DSM1_JP_ADC[9]=DSM0_TP_SUM[23]+DSM0_TP_SUM[24]+DSM0_TP_SUM_J3[22];
  DSM1_JP_ADC[10]=DSM0_TP_SUM[25]+DSM0_TP_SUM[26]+DSM0_TP_SUM_J1[27];
  DSM1_JP_ADC[11]=DSM0_TP_SUM[28]+DSM0_TP_SUM[29]+DSM0_TP_SUM_J3[27];
 
  for (int hh=0;hh<12;hh++) JP_adc_holder[hh]=DSM1_JP_ADC[hh];
  
  //Test each JP and see if it passed
  for (int i=0;i<kNJet;i++)
    {
      DSM1_JP_jp_Bit[i]=0;
      if ( DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,0)) DSM1_JP_jp_Bit[i]=0;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,0))) DSM1_JP_jp_Bit[i]=1;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,1))) DSM1_JP_jp_Bit[i]=2;
      if ( DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) DSM1_JP_jp_Bit[i]=3;
    }  


  int mod;
  //Translate JP's into 2 bits to pass to DSMLayer2
  for (int i=0;i<kNJet;i++){
    if (i < (kNJet/2)) mod = 0;
    else mod = 1;
    DSM1_ETOT_ADC[mod]+=DSM1_JP_ADC[i];
    if ( DSM1_JP_Bit[i/2] < DSM1_JP_jp_Bit[i]) DSM1_JP_Bit[i/2]=DSM1_JP_jp_Bit[i];   
  }


  //HTTP and TP bits
  for (int i=0; i<kL1DsmModule; i++){
    for (int j=0; j<5; j++){
      int k= i*5 + j;
      int kk=i*5 + 2;
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit[k]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit[k];   
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit_J3[kk]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit_J3[kk];   
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit_J1[kk]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit_J1[kk];
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit[k]) DSM1_TP_Bit[i]=DSM0_TP_Bit[k];      
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit_J3[kk]) DSM1_TP_Bit[i]=DSM0_TP_Bit_J3[kk];   
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit_J1[kk]) DSM1_TP_Bit[i]=DSM0_TP_Bit_J1[kk];         
    }

    if (DSM1_HTTP_Bit[i]>=2) DSM1_HTTP_Bit[i]=1;
    else if (DSM1_HTTP_Bit[i]<2) DSM1_HTTP_Bit[i]=0;

    if (DSM1_TP_Bit[i]>=2) DSM1_TP_Bit[i]=1;
    else if (DSM1_TP_Bit[i]<2) DSM1_TP_Bit[i]=0;
  }


  //WEST  HT bits
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[0]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[0];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[1]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[1];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit_J3[2]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit_J3[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit_J1[2]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit_J1[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[3]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[3];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[4]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[4];

  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[5]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[5];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[6]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[6];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit_J3[7]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit_J3[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit_J1[7]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit_J1[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[8]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[8];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[9]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[9];

  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[10]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[10];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[11]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[11];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit_J3[12]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit_J3[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit_J1[12]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit_J1[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[13]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[13];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[14]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[14];

  //EAST HT bits
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[15]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[15];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[16]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[16];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit_J1[17]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit_J1[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit_J3[17]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit_J3[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[18]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[18];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[19]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[19];

  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[20]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[20];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[21]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[21];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit_J1[22]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit_J1[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit_J3[22]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit_J3[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[23]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[23];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[24]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[24];

  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[25]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[25];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[26]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[26];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit_J1[27]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit_J1[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit_J3[27]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit_J3[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[28]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[28];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[29]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[29];
  


  
  
  //Drop two lowest bits for ETOT and OR Bits>6 with 6
  for (int i=0;i<kL1DsmModule;i++) {
    DSM1_ETOT_ADC[i]/=4;
    if (DSM1_ETOT_ADC[i]>31) DSM1_ETOT_ADC[i]=31;
  }


#ifdef DEBUG

  if (mHeadMaker->GetDataSet("MuDst")) {
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    for (int dsm = 0; dsm < kL2DsmModule; ++dsm) {
      for (int ch = 0; ch < 6; ++ch) {
	int idx = dsm_read_map[ch];
	int TrigBankOut = emcTrig.emcLayer2(idx);
	int jetPatch = 2 * TriggerBankToSimuMap[ch];
	int sum = DSM1_JP_ADC[jetPatch] + DSM1_JP_ADC[jetPatch+1];
	sum = (sum >> 7) ? 31 : (sum >> 2 & 0x1f);
	int diff = (TrigBankOut & 0x1f) - (sum & 0x1f);
	mBEMCLayer2PatchSum->Fill(ch, TrigBankOut & 0x1f);
	mBEMCLayer2PatchSumDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 7 & 0x1) - (DSM1_HTTP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTTPBits->Fill(ch, TrigBankOut >> 7 & 0x1);
	mBEMCLayer2HTTPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 9 & 0x1) - (DSM1_TP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2TPBits->Fill(ch, TrigBankOut >> 9 & 0x1);
	mBEMCLayer2TPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 10 & 0x3) - (DSM1_JP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2JPBits->Fill(ch, TrigBankOut >> 10 & 0x3);
	mBEMCLayer2JPBitsDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 12 & 0x3) - (DSM1_HTj0_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj0Bits->Fill(ch, TrigBankOut >> 12 & 0x3);
	mBEMCLayer2HTj0BitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 14 & 0x3) - (DSM1_HTj1_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj1Bits->Fill(ch, TrigBankOut >> 14 & 0x3);
	mBEMCLayer2HTj1BitsDiff->Fill(ch,diff);
 
      }
    }
  }
#endif

}

void StBemcTriggerSimu::get2007_DSMLayer2()
{

  // In hardware the final trigger decisions are made in the TCU 
  // It is not possible to compare the emulator with the TCU input
  // so all final trigger decisions for the BEMC are made at Layer2 
  // in this code

  Int_t DSM2_JP_Bit=0;
  Int_t DSM2_HT_Bit=0;
  Int_t DSM2_HTTP_Bit=0;
  Int_t DSM2_TP_Bit=0;

    
  for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {

    if (DSM2_JP_Bit<DSM1_JP_Bit[dsm]) DSM2_JP_Bit=DSM1_JP_Bit[dsm];
    if (DSM2_HTTP_Bit<DSM1_HTTP_Bit[dsm]) DSM2_HTTP_Bit=DSM1_HTTP_Bit[dsm];
    if (DSM2_HT_Bit<DSM1_HTj0_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj0_Bit[dsm];
    if (DSM2_HT_Bit<DSM1_HTj1_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj1_Bit[dsm];
    if (DSM2_TP_Bit<DSM1_TP_Bit[dsm]) DSM2_TP_Bit=DSM1_TP_Bit[dsm];
  }
  
  //HT
  if (DSM2_HT_Bit > 1){
    mFiredTriggers.push_back(200601);
    mFiredTriggers.push_back(200602);
    mFiredTriggers.push_back(200213);
    mFiredTriggers.push_back(200214);
    
  }

  if (DSM2_HT_Bit > 2){
    mFiredTriggers.push_back(200211);
    mFiredTriggers.push_back(200212);
    mFiredTriggers.push_back(200220);
    mFiredTriggers.push_back(200221);
    mFiredTriggers.push_back(200222);
    mFiredTriggers.push_back(200620);
    mFiredTriggers.push_back(200621);
  }
  
}



//==================================================
//==================================================
void StBemcTriggerSimu::get2008dAu_DSMLayer0() {
  
  //0-(8)9 Unused
  //10-11  First 2 HT threshold bits encoded into 2 bits
  //12     High Tower bit for threshold 3
  //13     Masked high tower bit for threshold 4
  //14-15  Unused
 
  //Loop over modules
  int k=0;
  int DSM_TP[kL0DsmInputs];
  for (int i=0;i<kL0DsmModule;i++){

    //Zero out 16 bit L0 HT outputs for each module
    DSM0_HT_Bit[i]=0;
    DSM0_HT_2Bit[i]=0;
    DSM0_HT_Thr3_Bit[i]=0;
    DSM0_HT_Masked_Bit[i]=0;
    DSM0_HT_Bit_J1[i]=0;
    DSM0_HT_2Bit_J1[i]=0;
    DSM0_HT_Thr3_Bit_J1[i]=0;
    DSM0_HT_Masked_Bit_J1[i]=0;
    DSM0_HT_Bit_J3[i]=0;  
    DSM0_HT_2Bit_J3[i]=0;
    DSM0_HT_Thr3_Bit_J3[i]=0;
    DSM0_HT_Masked_Bit_J3[i]=0;

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each L0 input
    for (int j=0;j<kL0DsmInputs;j++){ 
      DSM0_HT_tp_Bit[j]=0;
      DSM0_HT_tp_Bit_J1[j]=0;
      DSM0_HT_tp_Bit_J3[j]=0;
    }
  
    //Get array of TPid# from DSM module#
    mDecoder->GetTriggerPatchesFromDSM(i,DSM_TP);
    
#ifdef DEBUG
    // Overwrite input to BEMC layer 0 DSMs (output of BEMC FEEs)
    // with content of trigger bank from MuDst (data only).
    // First fill the Layer0 histograms with results from FEEout()
    if (mHeadMaker->GetDataSet("MuDst")) {
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int triggerPatch = 0; triggerPatch < kNPatches; ++triggerPatch) {
        mBEMCLayer0HT6bit->Fill(triggerPatch,emcTrig.highTower(triggerPatch));
        mBEMCLayer0TP6bit->Fill(triggerPatch,emcTrig.patch(triggerPatch));
        mBEMCLayer0HT6bitDiff->Fill(triggerPatch,emcTrig.highTower(triggerPatch)-L0_HT_ADC[triggerPatch]);
        mBEMCLayer0TP6bitDiff->Fill(triggerPatch,emcTrig.patch(triggerPatch)-L0_TP_ADC[triggerPatch]);
	L0_HT_ADC[triggerPatch] = emcTrig.highTower(triggerPatch);
	L0_TP_ADC[triggerPatch] = emcTrig.patch(triggerPatch); 
      }
    }
#endif
    

    //Loop over 10 inputs to each module 
    for (int j=0;j<kL0DsmInputs;j++){
      
      int tpid=DSM_TP[j];
      int jpid=-1;
      int seq=-1;
      mDecoder->GetJetPatchAndSequenceFromTriggerPatch(tpid, jpid, seq); 

      //Skip modules 2,7,12,17,22,27 
      if (i%5!=2) {
	
	//apply 5 HT thresholds to each HT adc in each TP
	if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit[j]=0;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit[j]=1;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit[j]=2;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,3)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2))) DSM0_HT_tp_Bit[j]=3;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,4)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,3))) DSM0_HT_tp_Bit[j]=4;
	if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,4)) DSM0_HT_tp_Bit[j]=5;
	
	//if all 6 bits are high then input is ignored
	if (L0_HT_ADC[tpid]==63) DSM0_HT_tp_Bit[j]=0;
		
	//Or threshold bits for all 10 high-towers
	if (DSM0_HT_Bit[i]< DSM0_HT_tp_Bit[j]) DSM0_HT_Bit[i]=DSM0_HT_tp_Bit[j];

      }
      
      //Loop over 2x5 inputs(TP) for modules 2,7,12,17,22,29
      if (i%5==2){

	if (j%2)
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J3[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J3[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J3[j]=2;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,3)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2))) DSM0_HT_tp_Bit_J3[j]=3;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,4)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,3))) DSM0_HT_tp_Bit_J3[j]=4;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,4)) DSM0_HT_tp_Bit_J3[j]=5;
	  }
	else
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J1[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J1[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J1[j]=2;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,3)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2))) DSM0_HT_tp_Bit_J1[j]=3;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,4)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,3))) DSM0_HT_tp_Bit_J1[j]=4;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,4)) DSM0_HT_tp_Bit_J1[j]=5;
	  }
	
	//if all 6 bits are high then input is ignored
	if (L0_HT_ADC[tpid]==63) DSM0_HT_tp_Bit_J1[j]=0;
	if (L0_HT_ADC[tpid]==63) DSM0_HT_tp_Bit_J3[j]=0;
	
	//Or threshold bits for all 10 high-towers
	if (DSM0_HT_Bit_J1[i]< DSM0_HT_tp_Bit_J1[j]) DSM0_HT_Bit_J1[i]=DSM0_HT_tp_Bit_J1[j];
	if (DSM0_HT_Bit_J3[i]< DSM0_HT_tp_Bit_J3[j]) DSM0_HT_Bit_J3[i]=DSM0_HT_tp_Bit_J3[j];     
      }
    }
    
    
    //Code first three thresholds (0-2) into 2 bits
    DSM0_HT_2Bit[i]=DSM0_HT_Bit[i];
    DSM0_HT_2Bit_J1[i]=DSM0_HT_Bit_J1[i];
    DSM0_HT_2Bit_J3[i]=DSM0_HT_Bit_J3[i];
    if (DSM0_HT_Bit[i]>3) DSM0_HT_2Bit[i]=3;
    if (DSM0_HT_Bit_J1[i]>3) DSM0_HT_2Bit_J1[i]=3;
    if (DSM0_HT_Bit_J3[i]>3) DSM0_HT_2Bit_J3[i]=3;

    //HT bit for threshold 3
    if (DSM0_HT_Bit[i]>=4) DSM0_HT_Thr3_Bit[i]=1;
    if (DSM0_HT_Bit_J1[i]>=4) DSM0_HT_Thr3_Bit_J1[i]=1;
    if (DSM0_HT_Bit_J3[i]>=4) DSM0_HT_Thr3_Bit_J3[i]=1;
    
    //Or between highest threshold and R5
    if (DSM0_HT_Bit[i]==5) DSM0_HT_Masked_Bit[i]=1;
    if (DSM0_HT_Bit_J1[i]==5) DSM0_HT_Masked_Bit_J1[i]=1;
    if (DSM0_HT_Bit_J3[i]==5) DSM0_HT_Masked_Bit_J3[i]=1;
 
    if (i%5!=2)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k++]=(DSM0_HT_2Bit[i]<<10)+(DSM0_HT_Thr3_Bit[i]<<12)+(DSM0_HT_Masked_Bit[i]<<13);
      }
    if (i%5==2)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k++]=(DSM0_HT_2Bit_J3[i]<<10)+(DSM0_HT_Thr3_Bit_J3[i]<<12)+(DSM0_HT_Masked_Bit_J3[i]<<13);
	L0_16bit_Out[k++]=(DSM0_HT_2Bit_J1[i]<<10)+(DSM0_HT_Thr3_Bit_J1[i]<<12)+(DSM0_HT_Masked_Bit_J1[i]<<13);
      }

  }

#ifdef DEBUG

  // Fill diagnostic histograms
  if (mHeadMaker->GetDataSet("MuDst")) {
    // BEMC layer 1 DSMs are stored in this order in the trigger bank:
    // BE101, BE102, BE103, BW101, BW102, BW103
    // DSM channels are read out in this order:
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    // Trigger bank <-> Emulator ==> 0, 1, 2, 3, 4, 5 <-> 3, 4, 5, 0, 1, 2
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();

    // Loop over BEMC layer 1 DSMs
    for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {
      // Loop over layer 1 input channels
      for (int ch = 0; ch < kL1DsmInputs; ++ch) {

	Int_t idx = dsm*8+dsm_read_map[ch];
	Int_t TrigBankOut = emcTrig.bemcLayer1(idx);
	Int_t HTout = (TrigBankOut & 0xc00)/0x400;
	Int_t HTbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0xc00)/0x400; 
 	Int_t HTthr3out = (TrigBankOut & 0x1000)/0x1000;
	Int_t HTthr3bit = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x1000)/0x1000;
	Int_t HTmaskout = (TrigBankOut & 0x2000)/0x2000;
	Int_t HTmaskbit = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x2000)/0x2000;
	int HTdiff = (HTbits) - (HTout);
	int HTthr3diff = (HTthr3out) - (HTthr3bit);
	int HTmaskdiff = (HTmaskout) - (HTmaskbit);
	mBEMCLayer1HTBits->Fill(dsm*6+ch, HTout);	
	mBEMCLayer1HTthr3Bits->Fill(dsm*6+ch, HTthr3out);
	mBEMCLayer1HTmaskBits->Fill(dsm*6+ch, HTmaskout);
	mBEMCLayer1HTBitsDiff->Fill(dsm*6+ch, HTdiff);
	mBEMCLayer1HTthr3Diff->Fill(dsm*6+ch, HTthr3diff);
	mBEMCLayer1HTmaskDiff->Fill(dsm*6+ch, HTmaskdiff);  
      }
    }
  }
#endif


}


//==================================================
//==================================================
void StBemcTriggerSimu::get2008dAu_DSMLayer1(){

  //DSM_Layer0 is passed to DSM_Layer1 in 8 UShort blocks (16 bits)
  //There are 6 DSM_Layer1 boards and each can take 120 bits total
  //So DSM_Layer0 passes 8 shorts (16*8=128) or 128 bits to each DSM_Layer1

  //Zero out the DSMLayer1 Bits passed to DSMLayer2
  for (int i=0;i<kL1DsmModule;i++){
    DSM1_HTj0_Bit[i]=0;
    DSM1_HTj1_Bit[i]=0;
  }


#ifdef DEBUG
    // Overwrite input to BEMC layer 1 DSMs (output of BEMC layer 0 DSMs)
    // with content of trigger bank from MuDst (data only).
    if (mHeadMaker->GetDataSet("MuDst")) {
      static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
      static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int dsm = 0; dsm < 6; ++dsm) {
	int offset = TriggerBankToSimuMap[dsm]*5;
	DSM0_TP_SUM   [offset+0] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[0]) & 0x3ff;
	DSM0_TP_SUM   [offset+1] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[1]) & 0x3ff;
	DSM0_TP_SUM_J3[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[2]) & 0x1ff;
	DSM0_TP_SUM_J1[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[3]) & 0x1ff;
	DSM0_TP_SUM   [offset+3] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[4]) & 0x3ff;
	DSM0_TP_SUM   [offset+4] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[5]) & 0x3ff;
      }
    }
#endif
    

  //WEST  HT bits
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[0]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[0];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[1]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[1];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit_J3[2]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit_J3[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit_J1[2]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit_J1[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[3]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[3];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[4]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[4];

  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[5]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[5];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[6]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[6];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit_J3[7]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit_J3[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit_J1[7]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit_J1[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[8]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[8];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[9]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[9];

  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[10]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[10];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[11]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[11];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit_J3[12]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit_J3[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit_J1[12]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit_J1[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[13]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[13];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[14]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[14];

  //EAST HT bits
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[15]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[15];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[16]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[16];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit_J1[17]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit_J1[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit_J3[17]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit_J3[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[18]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[18];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[19]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[19];

  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[20]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[20];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[21]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[21];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit_J1[22]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit_J1[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit_J3[22]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit_J3[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[23]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[23];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[24]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[24];

  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[25]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[25];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[26]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[26];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit_J1[27]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit_J1[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit_J3[27]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit_J3[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[28]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[28];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[29]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[29];
    

#ifdef DEBUG

  if (mHeadMaker->GetDataSet("MuDst")) {
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    for (int dsm = 0; dsm < kL2DsmModule; ++dsm) {
      for (int ch = 0; ch < 6; ++ch) {
	int idx = dsm_read_map[ch];
	int TrigBankOut = emcTrig.emcLayer2(idx);
	int jetPatch = 2 * TriggerBankToSimuMap[ch];
	int sum = DSM1_JP_ADC[jetPatch] + DSM1_JP_ADC[jetPatch+1];
	sum = (sum >> 7) ? 31 : (sum >> 2 & 0x1f);
	int diff = (TrigBankOut & 0x1f) - (sum & 0x1f);
	mBEMCLayer2PatchSum->Fill(ch, TrigBankOut & 0x1f);
	mBEMCLayer2PatchSumDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 7 & 0x1) - (DSM1_HTTP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTTPBits->Fill(ch, TrigBankOut >> 7 & 0x1);
	mBEMCLayer2HTTPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 9 & 0x1) - (DSM1_TP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2TPBits->Fill(ch, TrigBankOut >> 9 & 0x1);
	mBEMCLayer2TPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 10 & 0x3) - (DSM1_JP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2JPBits->Fill(ch, TrigBankOut >> 10 & 0x3);
	mBEMCLayer2JPBitsDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 12 & 0x3) - (DSM1_HTj0_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj0Bits->Fill(ch, TrigBankOut >> 12 & 0x3);
	mBEMCLayer2HTj0BitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 14 & 0x3) - (DSM1_HTj1_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj1Bits->Fill(ch, TrigBankOut >> 14 & 0x3);
	mBEMCLayer2HTj1BitsDiff->Fill(ch,diff);
 
      }
    }
  }
#endif

}

//==================================================
//==================================================
void StBemcTriggerSimu::get2008dAu_DSMLayer2(){

  // In hardware the final trigger decisions are made in the TCU 
  // It is not possible to compare the emulator with the TCU input
  // so all final trigger decisions for the BEMC are made at Layer2 
  // in this code

  Int_t DSM2_HT_Bit=0;
    
  for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {
    if (DSM2_HT_Bit<DSM1_HTj0_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj0_Bit[dsm];
    if (DSM2_HT_Bit<DSM1_HTj1_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj1_Bit[dsm];
  }
  
  //HT0
  if (DSM2_HT_Bit > 0){
    mFiredTriggers.push_back(210500);
    mFiredTriggers.push_back(210501);
  }
  //HT1
  if (DSM2_HT_Bit > 1){
    mFiredTriggers.push_back(210510);
    mFiredTriggers.push_back(210511);
  }
  //HT2
  if (DSM2_HT_Bit > 2){
    mFiredTriggers.push_back(210520);
    mFiredTriggers.push_back(210521);
  }
  //HT4
  if (DSM2_HT_Bit > 4){
    mFiredTriggers.push_back(210541);
  }

}


//==================================================
//==================================================
void StBemcTriggerSimu::get2008pp_DSMLayer0() {

  //0-(8)9 ADC sum Trigger Patches
  //10-11  HT threshold bits
  //12-13  TP threshold bits
  //14-15  HT&&TP threshold bits

  //Loop over modules
  int k=0;
  int DSM_TP[kL0DsmInputs];
  for (int i=0;i<kL0DsmModule;i++){

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each module
    DSM0_TP_SUM[i]=0; 
    DSM0_HT_Thr3_Bit[i]=0;
    DSM0_HT_Bit[i]=0;
    DSM0_TP_Bit[i]=0;
    DSM0_HTTP_Bit[i]=0;

    DSM0_TP_SUM_J1[i]=0;
    DSM0_HT_Thr3_Bit_J1[i]=0;
    DSM0_HT_Bit_J1[i]=0;
    DSM0_TP_Bit_J1[i]=0;
    DSM0_HTTP_Bit_J1[i]=0;

    DSM0_TP_SUM_J3[i]=0;
    DSM0_HT_Thr3_Bit_J3[i]=0;
    DSM0_HT_Bit_J3[i]=0;
    DSM0_TP_Bit_J3[i]=0;
    DSM0_HTTP_Bit_J3[i]=0;
   

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each L0 input
    for (int j=0;j<kL0DsmInputs;j++){ 
      DSM0_HT_tp_Bit[j]=0;
      DSM0_HT_Thr3_tp_Bit[j]=0;
      DSM0_TP_tp_Bit[j]=0;
      DSM0_HTTP_tp_Bit[j]=0; 
      DSM0_HT_tp_Bit_J1[j]=0;
      DSM0_HT_Thr3_tp_Bit_J1[j]=0;
      DSM0_TP_tp_Bit_J1[j]=0;
      DSM0_HTTP_tp_Bit_J1[j]=0; 
      DSM0_HT_tp_Bit_J3[j]=0;
      DSM0_HT_Thr3_tp_Bit_J3[j]=0;
      DSM0_TP_tp_Bit_J3[j]=0;
      DSM0_HTTP_tp_Bit_J3[j]=0;      
    }
  
    //Get array of TPid# from DSM module#
    mDecoder->GetTriggerPatchesFromDSM(i,DSM_TP);

#ifdef DEBUG
    // Overwrite input to BEMC layer 0 DSMs (output of BEMC FEEs)
    // with content of trigger bank from MuDst (data only).
    // First fill the Layer0 histograms with results from FEEout()
    if (mHeadMaker->GetDataSet("MuDst")) {
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int triggerPatch = 0; triggerPatch < kNPatches; ++triggerPatch) {
        mBEMCLayer0HT6bit->Fill(triggerPatch,emcTrig.highTower(triggerPatch));
        mBEMCLayer0TP6bit->Fill(triggerPatch,emcTrig.patch(triggerPatch));
        mBEMCLayer0HT6bitDiff->Fill(triggerPatch,emcTrig.highTower(triggerPatch)-L0_HT_ADC[triggerPatch]);
        mBEMCLayer0TP6bitDiff->Fill(triggerPatch,emcTrig.patch(triggerPatch)-L0_TP_ADC[triggerPatch]);
	L0_HT_ADC[triggerPatch] = emcTrig.highTower(triggerPatch);
	L0_TP_ADC[triggerPatch] = emcTrig.patch(triggerPatch); 
      }
    }
#endif
    
    
    //Loop over 10 inputs to each module 
    for (int j=0;j<kL0DsmInputs;j++){
      
      int tpid=DSM_TP[j];
      int jpid=-1;
      int seq=-1;
      mDecoder->GetJetPatchAndSequenceFromTriggerPatch(tpid, jpid, seq); 

      //Skip modules 2,7,12,17,22,27 
      if (i%5!=2) {
	
	//apply HT thresholds to each HT adc in each TP
	if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit[j]=0;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit[j]=1;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit[j]=2;
	if ((L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2))) DSM0_HT_tp_Bit[j]=3;
	if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,3)) DSM0_HT_Thr3_tp_Bit[j]=1;
	if ( L0_HT_ADC[tpid]==63 ) DSM0_HT_tp_Bit[j]=0;

	//apply TP threshold to each TP adc in each TP
	if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit[j]=0;
	if ( L0_TP_ADC[tpid] >  mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit[j]=1;
	if ( L0_TP_ADC[tpid]==63 ) DSM0_TP_tp_Bit[j]=0;

	//add up TP adc for 2/5 of JP
	DSM0_TP_SUM[i]+=L0_TP_ADC[tpid];

	// AND HT#3 with TP bit
        DSM0_HTTP_tp_Bit[j]=(DSM0_TP_tp_Bit[j]&&DSM0_HT_Thr3_tp_Bit[j]);
	// Set HT#3 bit
	if (DSM0_HT_Thr3_Bit[i]<DSM0_HT_Thr3_tp_Bit[j]) DSM0_HT_Thr3_Bit[i]=DSM0_HT_Thr3_tp_Bit[j];
	// OR bits for HT thresholds #0,#1,#2
	if (DSM0_HT_Bit[i]<DSM0_HT_tp_Bit[j]) DSM0_HT_Bit[i]=DSM0_HT_tp_Bit[j];
	// OR bits for TP for all trigger patches						     
	if (DSM0_TP_Bit[i]<DSM0_TP_tp_Bit[j]) DSM0_TP_Bit[i]=DSM0_TP_tp_Bit[j];
	//OR bits for HTTP for all trigger patches
	if (DSM0_HTTP_Bit[i]<DSM0_HTTP_tp_Bit[j]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];
      }
      
      //Loop over 2x5 inputs(TP) for modules 2,7,12,17,22,29
      if (i%5==2){

	if (j%2)
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J3[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J3[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J3[j]=2;
	    if ((L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2))) DSM0_HT_tp_Bit_J3[j]=3;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,3)) DSM0_HT_Thr3_tp_Bit_J3[j]=1;
	    if ( L0_HT_ADC[tpid]==63 ) DSM0_HT_tp_Bit_J3[j]=0;
	  }
	else
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J1[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J1[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J1[j]=2;
	    if ((L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2))) DSM0_HT_tp_Bit_J1[j]=3;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,3)) DSM0_HT_Thr3_tp_Bit_J1[j]=1;
	    if ( L0_HT_ADC[tpid]==63 ) DSM0_HT_tp_Bit_J1[j]=0;
	  }
	
	//apply TP thresholds to each TP adc in each TP
	if (j%2)
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J3[j]=0;
	    if ( L0_TP_ADC[tpid] >  mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J3[j]=1;
	    if ( L0_TP_ADC[tpid] == 63 ) DSM0_TP_tp_Bit_J3[j]=0;
	  }                       
	else
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J1[j]=0;
	    if ( L0_TP_ADC[tpid] >  mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J1[j]=1;
	    if ( L0_TP_ADC[tpid] == 63 ) DSM0_TP_tp_Bit_J1[j]=0;
	  }

	//add up TP adc for 1/5 of JP
	if (j%2)
	  DSM0_TP_SUM_J3[i]+=L0_TP_ADC[tpid];
	else
	  DSM0_TP_SUM_J1[i]+=L0_TP_ADC[tpid];
		
	
	//AND HT#3 with TP bit
	if (j%2) 
	{
	  DSM0_HTTP_tp_Bit_J3[j]=(DSM0_TP_tp_Bit_J3[j] && DSM0_HT_Thr3_tp_Bit_J3[j]);
	}
	else
	{
	  DSM0_HTTP_tp_Bit_J1[j]=(DSM0_TP_tp_Bit_J1[j] && DSM0_HT_Thr3_tp_Bit_J1[j]);
	}

	// OR bits for HT, TP, and HTTP for all trigger patches
	if (DSM0_HT_Bit_J3[i]<DSM0_HT_tp_Bit_J3[j]) DSM0_HT_Bit_J3[i]=DSM0_HT_tp_Bit_J3[j];
	if (DSM0_TP_Bit_J3[i]<DSM0_TP_tp_Bit_J3[j]) DSM0_TP_Bit_J3[i]=DSM0_TP_tp_Bit_J3[j];
	if (DSM0_HTTP_Bit_J3[i]<DSM0_HTTP_tp_Bit_J3[j]) DSM0_HTTP_Bit_J3[i]=DSM0_HTTP_tp_Bit_J3[j];
	if (DSM0_HT_Thr3_Bit_J3[i]<DSM0_HT_Thr3_tp_Bit_J3[j]) DSM0_HT_Thr3_Bit_J3[i]=DSM0_HT_Thr3_tp_Bit_J3[j];

	if (DSM0_HT_Bit_J1[i]<DSM0_HT_tp_Bit_J1[j]) DSM0_HT_Bit_J1[i]=DSM0_HT_tp_Bit_J1[j];
	if (DSM0_TP_Bit_J1[i]<DSM0_TP_tp_Bit_J1[j]) DSM0_TP_Bit_J1[i]=DSM0_TP_tp_Bit_J1[j];
	if (DSM0_HTTP_Bit_J1[i]<DSM0_HTTP_tp_Bit_J1[j]) DSM0_HTTP_Bit_J1[i]=DSM0_HTTP_tp_Bit_J1[j];
	if (DSM0_HT_Thr3_Bit_J1[i]<DSM0_HT_Thr3_tp_Bit_J1[j]) DSM0_HT_Thr3_Bit_J1[i]=DSM0_HT_Thr3_tp_Bit_J1[j];

      } 
    }
    

    if (i%5!=2)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k++]=DSM0_TP_SUM[i]+(DSM0_HT_Bit[i]<<10)+(DSM0_HT_Thr3_Bit[i]<<12)+(DSM0_TP_Bit[i]<<13)+(DSM0_HTTP_Bit[i]<<14);
      }
    if (i%5==2)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k++]=DSM0_TP_SUM_J3[i]+(DSM0_HT_Bit_J3[i]<<10)+(DSM0_HT_Thr3_Bit_J3[i]<<12)+(DSM0_TP_Bit_J3[i]<<13)+(DSM0_HTTP_Bit_J3[i]<<14);
	L0_16bit_Out[k++]=DSM0_TP_SUM_J1[i]+(DSM0_HT_Bit_J1[i]<<10)+(DSM0_HT_Thr3_Bit_J1[i]<<12)+(DSM0_TP_Bit_J1[i]<<13)+(DSM0_HTTP_Bit_J1[i]<<14);
      }
  }

#ifdef DEBUG

  // Fill diagnostic histograms
  if (mHeadMaker->GetDataSet("MuDst")) {
    // BEMC layer 1 DSMs are stored in this order in the trigger bank:
    // BE101, BE102, BE103, BW101, BW102, BW103
    // DSM channels are read out in this order:
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    // Trigger bank <-> Emulator ==> 0, 1, 2, 3, 4, 5 <-> 3, 4, 5, 0, 1, 2
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();

    // Loop over BEMC layer 1 DSMs
    for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {
      // Loop over layer 1 input channels
      for (int ch = 0; ch < kL1DsmInputs; ++ch) {

	Int_t idx = dsm*8+dsm_read_map[ch];
	Int_t TrigBankOut = emcTrig.bemcLayer1(idx);
	//bits 0-9
        Int_t TPSumout = (TrigBankOut & 0x3ff);
        Int_t TPSumbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x3ff);
	//bits 10-11
        Int_t HTout = (TrigBankOut & 0xc00)/0x400;
	Int_t HTbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0xc00)/0x400; 
	//bit 12
	Int_t HT3out = (TrigBankOut & 0x1000/0x800);
	Int_t HT3bits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x1000/0x800);
	//bit 13
 	Int_t TPout = (TrigBankOut & 0x2000)/0x1000;
	Int_t TPbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x2000)/0x1000;
	//bit 14
	Int_t HTTPout = (TrigBankOut & 0x4000)/0x2000;
	Int_t HTTPbits = (L0_16bit_Out[TriggerBankToSimuMap[dsm]*6+ch] & 0x4000)/0x2000;

	int TPSumdiff = (TPSumbits)-(TPSumout);
	int HTdiff = (HTbits) - (HTout);
	int HT3diff = (HT3bits) - (HT3out);
	int TPdiff = (TPbits) - (TPout);
	int HTTPdiff = (HTTPbits) - (HTTPout);
	mBEMCLayer1PatchSum->Fill(dsm*6+ch, TPSumout);
	mBEMCLayer1HTBits->Fill(dsm*6+ch, HTout);
	mBEMCLayer1HTthr3Bits->Fill(dsm*6+ch,HT3out);
	mBEMCLayer1TPBits->Fill(dsm*6+ch, TPout);
	mBEMCLayer1HTTPBits->Fill(dsm*6+ch, HTTPout);
	mBEMCLayer1PatchSumDiff->Fill(dsm*6+ch, TPSumdiff);
	mBEMCLayer1HTBitsDiff->Fill(dsm*6+ch, HTdiff);
	mBEMCLayer1HTthr3Diff->Fill(dsm*6+ch, HT3diff);
	mBEMCLayer1TPBitsDiff->Fill(dsm*6+ch, TPdiff);
	mBEMCLayer1HTTPBitsDiff->Fill(dsm*6+ch, HTTPdiff);  
      }
    }
  }
#endif

}


//==================================================
//==================================================
void StBemcTriggerSimu::get2008pp_DSMLayer1(){


  //DSM_Layer0 is passed to DSM_Layer1 in 8 UShort blocks (16 bits)
  //There are 6 DSM_Layer1 boards and each can take 120 bits total
  //So DSM_Layer0 passes 8 shorts (16*8=128) or 128 bits to each DSM_Layer1

  //Zero out the DSMLayer1 Bits passed to DSMLayer2
  for (int i=0;i<kL1DsmModule;i++){
    DSM1_JP_Bit[i]=0;
    DSM1_HTj0_Bit[i]=0;
    DSM1_HTj1_Bit[i]=0;
    DSM1_HT3_Bit[i]=0;
    DSM1_TP_Bit[i]=0;
    DSM1_HTTP_Bit[i]=0;
    DSM1_ETOT_ADC[i]=0;
  }


#ifdef DEBUG
    // Overwrite input to BEMC layer 1 DSMs (output of BEMC layer 0 DSMs)
    // with content of trigger bank from MuDst (data only).
    if (mHeadMaker->GetDataSet("MuDst")) {
      static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
      static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
      StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
      for (int dsm = 0; dsm < 6; ++dsm) {
	int offset = TriggerBankToSimuMap[dsm]*5;
	DSM0_TP_SUM   [offset+0] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[0]) & 0x3ff;
	DSM0_TP_SUM   [offset+1] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[1]) & 0x3ff;
	DSM0_TP_SUM_J3[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[2]) & 0x1ff;
	DSM0_TP_SUM_J1[offset+2] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[3]) & 0x1ff;
	DSM0_TP_SUM   [offset+3] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[4]) & 0x3ff;
	DSM0_TP_SUM   [offset+4] = emcTrig.bemcLayer1(dsm*8+dsm_read_map[5]) & 0x3ff;
      }
    }
#endif
    
  //Sum TP ADC into JP's
  // West
  DSM1_JP_ADC[0]=DSM0_TP_SUM[0]+DSM0_TP_SUM[1]+DSM0_TP_SUM_J3[2];
  DSM1_JP_ADC[1]=DSM0_TP_SUM[3]+DSM0_TP_SUM[4]+DSM0_TP_SUM_J1[2];
  DSM1_JP_ADC[2]=DSM0_TP_SUM[5]+DSM0_TP_SUM[6]+DSM0_TP_SUM_J3[7];
  DSM1_JP_ADC[3]=DSM0_TP_SUM[8]+DSM0_TP_SUM[9]+DSM0_TP_SUM_J1[7];
  DSM1_JP_ADC[4]=DSM0_TP_SUM[10]+DSM0_TP_SUM[11]+DSM0_TP_SUM_J3[12];
  DSM1_JP_ADC[5]=DSM0_TP_SUM[13]+DSM0_TP_SUM[14]+DSM0_TP_SUM_J1[12];
  
  // East
  DSM1_JP_ADC[6]=DSM0_TP_SUM[15]+DSM0_TP_SUM[16]+DSM0_TP_SUM_J1[17];
  DSM1_JP_ADC[7]=DSM0_TP_SUM[18]+DSM0_TP_SUM[19]+DSM0_TP_SUM_J3[17];
  DSM1_JP_ADC[8]=DSM0_TP_SUM[20]+DSM0_TP_SUM[21]+DSM0_TP_SUM_J1[22];
  DSM1_JP_ADC[9]=DSM0_TP_SUM[23]+DSM0_TP_SUM[24]+DSM0_TP_SUM_J3[22];
  DSM1_JP_ADC[10]=DSM0_TP_SUM[25]+DSM0_TP_SUM[26]+DSM0_TP_SUM_J1[27];
  DSM1_JP_ADC[11]=DSM0_TP_SUM[28]+DSM0_TP_SUM[29]+DSM0_TP_SUM_J3[27];
 
  for (int hh=0;hh<12;hh++) JP_adc_holder[hh]=DSM1_JP_ADC[hh];
  
  //Test each JP and see if it passed
  for (int i=0;i<kNJet;i++)
    {
      DSM1_JP_jp_Bit[i]=0;
      if ( DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,0)) DSM1_JP_jp_Bit[i]=0;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,0))) DSM1_JP_jp_Bit[i]=1;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,1))) DSM1_JP_jp_Bit[i]=2;
      if ( DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) DSM1_JP_jp_Bit[i]=3;
    }  

  
  int mod;
  //Translate JP's into 2 bits to pass to DSMLayer2
  for (int i=0;i<kNJet;i++){
    if (i < (kNJet/2)) mod = 0;
    else mod = 1;
    DSM1_ETOT_ADC[mod]+=DSM1_JP_ADC[i];
    if ( DSM1_JP_Bit[i/2] < DSM1_JP_jp_Bit[i]) DSM1_JP_Bit[i/2]=DSM1_JP_jp_Bit[i];   
  }


  //COMBINE HT3,TP0 and HTTP bits from all channels
  for (int i=0; i<kL1DsmModule; i++){
    for (int j=0; j<5; j++){
      int k= i*5 + j;
      int kk=i*5 + 2;

      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit[k]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit[k];   
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit_J3[kk]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit_J3[kk];   
      if ( DSM1_HTTP_Bit[i] < DSM0_HTTP_Bit_J1[kk]) DSM1_HTTP_Bit[i]=DSM0_HTTP_Bit_J1[kk];

      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit[k]) DSM1_TP_Bit[i]=DSM0_TP_Bit[k];      
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit_J3[kk]) DSM1_TP_Bit[i]=DSM0_TP_Bit_J3[kk];   
      if ( DSM1_TP_Bit[i] < DSM0_TP_Bit_J1[kk]) DSM1_TP_Bit[i]=DSM0_TP_Bit_J1[kk];         

      if ( DSM1_HT3_Bit[i] < DSM0_HT_Thr3_Bit[k]) DSM1_HT3_Bit[i]=DSM0_HT_Thr3_Bit[k];      
      if ( DSM1_HT3_Bit[i] < DSM0_HT_Thr3_Bit_J3[kk]) DSM1_HT3_Bit[i]=DSM0_HT_Thr3_Bit_J3[kk];   
      if ( DSM1_HT3_Bit[i] < DSM0_HT_Thr3_Bit_J1[kk]) DSM1_HT3_Bit[i]=DSM0_HT_Thr3_Bit_J1[kk];         

    }
  }


  //WEST  HT bits
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[0]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[0];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit[1]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit[1];
  if (DSM1_HTj0_Bit[0]<DSM0_HT_Bit_J3[2]) DSM1_HTj0_Bit[0]=DSM0_HT_Bit_J3[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit_J1[2]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit_J1[2];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[3]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[3];
  if (DSM1_HTj1_Bit[0]<DSM0_HT_Bit[4]) DSM1_HTj1_Bit[0]=DSM0_HT_Bit[4];

  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[5]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[5];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit[6]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit[6];
  if (DSM1_HTj0_Bit[1]<DSM0_HT_Bit_J3[7]) DSM1_HTj0_Bit[1]=DSM0_HT_Bit_J3[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit_J1[7]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit_J1[7];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[8]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[8];
  if (DSM1_HTj1_Bit[1]<DSM0_HT_Bit[9]) DSM1_HTj1_Bit[1]=DSM0_HT_Bit[9];

  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[10]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[10];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit[11]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit[11];
  if (DSM1_HTj0_Bit[2]<DSM0_HT_Bit_J3[12]) DSM1_HTj0_Bit[2]=DSM0_HT_Bit_J3[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit_J1[12]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit_J1[12];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[13]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[13];
  if (DSM1_HTj1_Bit[2]<DSM0_HT_Bit[14]) DSM1_HTj1_Bit[2]=DSM0_HT_Bit[14];

  //EAST HT bits
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[15]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[15];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit[16]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit[16];
  if (DSM1_HTj0_Bit[3]<DSM0_HT_Bit_J1[17]) DSM1_HTj0_Bit[3]=DSM0_HT_Bit_J1[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit_J3[17]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit_J3[17];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[18]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[18];
  if (DSM1_HTj1_Bit[3]<DSM0_HT_Bit[19]) DSM1_HTj1_Bit[3]=DSM0_HT_Bit[19];

  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[20]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[20];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit[21]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit[21];
  if (DSM1_HTj0_Bit[4]<DSM0_HT_Bit_J1[22]) DSM1_HTj0_Bit[4]=DSM0_HT_Bit_J1[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit_J3[22]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit_J3[22];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[23]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[23];
  if (DSM1_HTj1_Bit[4]<DSM0_HT_Bit[24]) DSM1_HTj1_Bit[4]=DSM0_HT_Bit[24];

  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[25]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[25];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit[26]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit[26];
  if (DSM1_HTj0_Bit[5]<DSM0_HT_Bit_J1[27]) DSM1_HTj0_Bit[5]=DSM0_HT_Bit_J1[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit_J3[27]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit_J3[27];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[28]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[28];
  if (DSM1_HTj1_Bit[5]<DSM0_HT_Bit[29]) DSM1_HTj1_Bit[5]=DSM0_HT_Bit[29];
  
  
  //Drop two lowest bits for ETOT and OR Bits>6 with 6
  for (int i=0;i<kL1DsmModule;i++) {
    DSM1_ETOT_ADC[i]/=4;
    if (DSM1_ETOT_ADC[i]>31) DSM1_ETOT_ADC[i]=31;
  }


#ifdef DEBUG

  if (mHeadMaker->GetDataSet("MuDst")) {
    StEmcTriggerDetector& emcTrig = StMuDst::event()->emcTriggerDetector();
    static const int dsm_read_map[] = { 3, 2, 1, 0, 7, 6, 5, 4 };
    static const int TriggerBankToSimuMap[] = { 3, 4, 5, 0, 1, 2 };
    for (int dsm = 0; dsm < kL2DsmModule; ++dsm) {
      for (int ch = 0; ch < 6; ++ch) {
	int idx = dsm_read_map[ch];
	int TrigBankOut = emcTrig.emcLayer2(idx);
	int jetPatch = 2 * TriggerBankToSimuMap[ch];
	int sum = DSM1_JP_ADC[jetPatch] + DSM1_JP_ADC[jetPatch+1];
	sum = (sum >> 7) ? 31 : (sum >> 2 & 0x1f);
	int diff = (TrigBankOut & 0x1f) - (sum & 0x1f);
	mBEMCLayer2PatchSum->Fill(ch, TrigBankOut & 0x1f);
	mBEMCLayer2PatchSumDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 7 & 0x1) - (DSM1_HT3_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HT3Bits->Fill(ch, TrigBankOut >> 7 & 0x1);
	mBEMCLayer2HT3BitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 8 & 0x1) - (DSM1_TP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2TPBits->Fill(ch, TrigBankOut >> 8 & 0x1);
	mBEMCLayer2TPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 9 & 0x1) - (DSM1_HTTP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTTPBits->Fill(ch, TrigBankOut >> 9 & 0x1);
	mBEMCLayer2HTTPBitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 10 & 0x3) - (DSM1_JP_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2JPBits->Fill(ch, TrigBankOut >> 10 & 0x3);
	mBEMCLayer2JPBitsDiff->Fill(ch, diff);
	diff = (TrigBankOut >> 12 & 0x3) - (DSM1_HTj0_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj0Bits->Fill(ch, TrigBankOut >> 12 & 0x3);
	mBEMCLayer2HTj0BitsDiff->Fill(ch,diff);
	diff = (TrigBankOut >> 14 & 0x3) - (DSM1_HTj1_Bit[TriggerBankToSimuMap[ch]]);
	mBEMCLayer2HTj1Bits->Fill(ch, TrigBankOut >> 14 & 0x3);
	mBEMCLayer2HTj1BitsDiff->Fill(ch,diff);
 
      }
    }
  }
#endif

}

void StBemcTriggerSimu::get2008pp_DSMLayer2()
{

  // In hardware the final trigger decisions are made in the TCU 
  // It is not possible to compare the emulator with the TCU input
  // so all final trigger decisions for the BEMC are made at Layer2 
  // in this code

  Int_t DSM2_HT_Bit=0;
    
  for (int dsm = 0; dsm < kL1DsmModule; ++dsm) {

    if (DSM2_HT_Bit<DSM1_HTj0_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj0_Bit[dsm];
    if (DSM2_HT_Bit<DSM1_HTj1_Bit[dsm]) DSM2_HT_Bit=DSM1_HTj1_Bit[dsm];

  }
  
  //HT0
  if (DSM2_HT_Bit > 0) mFiredTriggers.push_back(220500);
  //HT1
  if (DSM2_HT_Bit > 1) mFiredTriggers.push_back(220510);
  //HT2
  if (DSM2_HT_Bit > 2) mFiredTriggers.push_back(220520);

  
}

//==================================================

void StBemcTriggerSimu::get2009_DSMLayer0()
{
  // Loop over 30 modules
  for (int dsm = 0; dsm < kL0DsmModule; ++dsm) {
    TString line = (*mB001)[dsm].name + ": ";
    // Loop over 10 input channels to each module 
    for (int ch = 0 ; ch < kL0DsmInputs; ++ch) {
      int tpid = dsm*kL0DsmInputs+ch;
      (*mB001)[dsm].channels[ch] = L0_HT_ADC[tpid] | L0_TP_ADC[tpid] << 6;
      line += Form("%04x ",(*mB001)[dsm].channels[ch]);
    } // End loop over channels
    LOG_DEBUG << line << endm;
  } // End loop over modules

  // Emulate BEMC layer 0
  mB001->run();
}

//==================================================

void StBemcTriggerSimu::get2009_DSMLayer1()
{
  // Get input from BEMC layer 0
  mB001->write(*mB101);

  // LOG_DEBUG messages
  for (size_t dsm = 0; dsm < mB101->size(); ++dsm) {
    TString line = (*mB101)[dsm].name + ": ";
    for (int ch = 0; ch < 8; ++ch) line += Form("%04x ",(*mB101)[dsm].channels[ch]);
    LOG_DEBUG << line << endm;
  }

  // Emulate BEMC layer 1
  mB101->run();
}

//==================================================


const vector< pair<int,int> > StBemcTriggerSimu::getTowersAboveThreshold(int trigId) const {  
  vector< pair<int,int> > towers;
  
  for (int i=0;i<kNTowers;i++)
    {
      int tpid = -1;
      int dsmid = -1;
      
      mDecoder->GetTriggerPatchFromTowerId(i,tpid);
      mDecoder->GetDSMFromTriggerPatch(tpid,dsmid);

      if (trigId==210500 || trigId==210501 || trigId==220500) {
	if (HT6bit_adc_holder[i] > mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,0)) {
	  towers.push_back( make_pair(i+1,HT6bit_adc_holder[i]) );
	}
      }
      if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5 ||
	  trigId==200601 || trigId==200602 || trigId==200213 || trigId==200214 || trigId==210510 || trigId==210511 ||
	  trigId==220510) {
	if (HT6bit_adc_holder[i] > mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,1)) {
	  towers.push_back( make_pair(i+1,HT6bit_adc_holder[i]) );
	}
      }
      if (trigId==127212 || trigId==137213 || trigId==200211 || trigId==200212 || trigId==200220 || trigId==200221 || 
	  trigId==200222 || trigId==200620 || trigId==200621 || trigId==210520 || trigId==210521 || trigId==220520) {
	if (HT6bit_adc_holder[i] > mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,2)) {
	  towers.push_back( make_pair(i+1,HT6bit_adc_holder[i]) );
	}
      }
      if (trigId==210541){
	if (HT6bit_adc_holder[i] > mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,4)) {
	  towers.push_back( make_pair(i+1,HT6bit_adc_holder[i]) );
	}
      }
    }
  return towers;
}


const vector< pair<int,int> > StBemcTriggerSimu::getTriggerPatchesAboveThreshold(int trigId) const {  
  vector< pair<int,int> > patches;

  for (int i=0;i<kNPatches;i++)
    {
      int dsmid;
      mDecoder->GetDSMFromTriggerPatch(i,dsmid);

      if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
	if (TP6bit_adc_holder[i] > mDbThres->GetTP_DSM0_threshold(dsmid,timestamp,1)) {	
	  //cout << "In getTPAboveThreshold: " << i+1 << "\tTP: " << TP6bit_adc_holder[i] <<" dsmid"<<dsmid<<" tpid="<<i<< endl;
	  patches.push_back( make_pair(i,TP6bit_adc_holder[i]) );
	}
      }
    }
  return patches;
}


const vector< pair<int,int> > StBemcTriggerSimu::getJetPatchesAboveThreshold(int trigId) const {  
  vector< pair<int,int> > patches;
  
  for (int i=0;i<kNJet;i++)
    {
      if ((JP_adc_holder[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) && (JP_adc_holder[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,0))) {
	if (trigId==127501 || trigId==137501 || trigId==127622 || trigId==137622) {
	  patches.push_back( make_pair(i,JP_adc_holder[i]) );
	}
      }
      if (JP_adc_holder[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) {
	if (trigId==127221 || trigId==137221 || trigId==137222 || trigId==127501 || trigId==137501 || trigId==127622 || trigId==137622) {
	  patches.push_back( make_pair(i,JP_adc_holder[i]) );
	}
      }
    }
  return patches;
}


int StBemcTriggerSimu::getTowerThreshold(int trigId, int dsmid) const {  
  int threshold =-1;

  if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
    threshold = mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,1);
  }
  if (trigId==127212 || trigId==137213) {
    threshold = mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,2);
  }

  return threshold;
}


int StBemcTriggerSimu::getTriggerPatchThreshold(int trigId, int dsmid) const {
  int threshold = -1;

  if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
    threshold = mDbThres->GetTP_DSM0_threshold(dsmid,timestamp,1);
  }
  return threshold;
}


int StBemcTriggerSimu::getJetPatchThreshold(int trigId, int dsmid) const {  
  int threshold = -1;

  if (trigId==127501 || trigId==137501 || trigId==127622 || trigId==137622) {  
    threshold = mDbThres->GetJP_DSM1_threshold(dsmid,timestamp,0);
  }
  if (trigId==127221 || trigId==137221 || trigId==137222) {
    threshold = mDbThres->GetJP_DSM1_threshold(dsmid,timestamp,1);
  }
  
  return threshold;
}


int StBemcTriggerSimu::barrelJetPatchTh(int i) const { return mB101->getRegister(i); }
int StBemcTriggerSimu::barrelHighTowerTh(int i) const { return mB001->getRegister(i); }
int StBemcTriggerSimu::barrelJetPatchAdc(int jp) const { return (*mB101)[jp%6].info[(jp/6+2)%3]; }

void StBemcTriggerSimu::fillStEmcTriggerDetector()
{
  if (mEvent && mEvent->triggerDetectorCollection()) {
    StEmcTriggerDetector& emc = mEvent->triggerDetectorCollection()->emc();
    for (int triggerPatch = 0; triggerPatch < kNPatches; ++triggerPatch) {
      emc.setHighTower(triggerPatch,getBEMC_FEE_HT_ADC()[triggerPatch]);
      emc.setPatch(triggerPatch,getBEMC_FEE_TP_ADC()[triggerPatch]);
    }
  }
  if (StMuDst::event()) {
    StEmcTriggerDetector& emc = StMuDst::event()->emcTriggerDetector();
    for (int triggerPatch = 0; triggerPatch < kNPatches; ++triggerPatch) {
      emc.setHighTower(triggerPatch,getBEMC_FEE_HT_ADC()[triggerPatch]);
      emc.setPatch(triggerPatch,getBEMC_FEE_TP_ADC()[triggerPatch]);
    }
  }
}
