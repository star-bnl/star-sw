//////////////////////////////////////////////////////////////////////////
//
// StL2_2006EmulatorMaker  Jan Balewski  (Fall, 2007)
//
// Goal: 
//
//
//////////////////////////////////////////////////////////////////////////
// StEvent
#include "StEventTypes.h"
#include "StEvent/StEvent.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//trg stuff
#include "StTriggerData2005.h"
#include "StDaqLib/TRG/L2jetResults2006.h"
#include "StDaqLib/TRG/trgStructures2005.h"
#include "StDaqLib/TRG/trgStructures.h"

//L2 stuff
#include "L2algoUtil/L2EmcDb.h"
#include "L2jetAlgo/L2jetAlgo.h"
#include "L2pedAlgo/L2pedAlgo.h"
//#include "L2gammaAlgo/L2gammaAlgo.h"

#include "StL2_2006EmulatorMaker.h"

ClassImp(StL2_2006EmulatorMaker)

StL2_2006EmulatorMaker::StL2_2006EmulatorMaker(const char *name):StMaker(name) {
  mL2pedAlgo=0;
  mL2jetAlgo=0;

  //  mHList=0;

}

//========================================
StL2_2006EmulatorMaker::~StL2_2006EmulatorMaker(){
}


//========================================
//========================================
Int_t  
StL2_2006EmulatorMaker::InitRun(int runNo){
  //WARN: do NOT use  runNo for any setup - it woul dberak for M-C
  
  initRun();


  LOG_INFO << Form("::setupL2Algos2006(), dbDate=%d  ", mYearMonthDay)<<endm;

  mL2algoN=2; // total # of L2 algos
  mL2algo =new L2VirtualAlgo *[mL2algoN]; // not cleared memeory leak
  memset(mL2algo,0,mL2algoN*sizeof(void*));

  //setup evry algo one by one, params may be time dependent
  enum {mxPar=10};
  int ints[mxPar]; // params passed from run control gui
  float floats[mxPar]; // 
  int L2ResOff=0;

  /* Temporary solutions, needs fixing later 
     - ints[] and floats[] should not be hardcoded but passed from some sort of DB or input file

     Jan
  */


  // ----------- L2 ped algo ----------------
  L2ResOff=L2RESULTS_OFFSET_EMC_PED;
  memset(ints,0,sizeof(ints));
  memset(floats,0,sizeof(floats));
  ints[0]=1;   // subtract ped on/off  
  ints[1]=1;   // speed factor, use 16 for 180kTics=100muSec
  ints[2]=0;   //  saveBinary on/off
  ints[3]=0;   // debug verbose on/off 

  mL2pedAlgo=new L2pedAlgo(mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  assert(mL2pedAlgo->initRun("aaa", runNo,ints,floats)==0); // zero tolerance for missing input files
  mL2algo[0]=mL2pedAlgo;

  // ----------- L2 jet algo ----------------
  memset(ints,0,sizeof(ints));
  memset(floats,0,sizeof(floats));
  L2ResOff=L2RESULTS_OFFSET_DIJET;
  ints[0] = 22;  /* cutTag, whatever value, not used for anything  */
  ints[1] =  3; /* useBtow 1=East, 2=West, 3=E+W*/
  ints[2] =  1; /* useEndcap   */
  ints[3] =  8; /* threshold for ADC-ped */
  ints[4] =  5; /* min phi dist J1-J2 in L2phiBins */
  
  floats[4] = 0 ;   // debug level
  //time dependent L2jet cuts are below 
  assert( mYearMonthDay>20060316); // before L2jet was not used
  assert( mYearMonthDay<20060620); // after L2jet was not used
  if( mYearMonthDay<20060406) { // ppLong-1 period not implementd
    assert(1==2);
  } else if (  mYearMonthDay<200605011) { // ppTrans
  floats[0] = 8.0;  /* oneJetThr , slideing  */
  floats[1] = 3.6;  /* diJet1thr , higher */
  floats[2] = 3.3;  /* diJet2thr , lower */
  floats[3] = 0.01; // rndAccProb
  } else   { // ppLong-2, 62 geV  periods not implementd
    assert(1==2);
  }
  mL2jetAlgo=new L2jetAlgo(mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  assert(mL2jetAlgo->initRun("jetA",runNo,ints,floats)==0); // zero tolerance for missing input files
  mL2algo[1]=mL2jetAlgo;

  LOG_INFO  << "StL2JetEmulMaker::InitRun() done, run=" <<runNo<<endm;
  
  return kStOK; 
} 



//_____________________________________________________________________________
Int_t 
StL2_2006EmulatorMaker::Init() {
  init();
  LOG_INFO <<Form("Init()")<<endm;
  //  initHistoA("spj_");

 return StMaker::Init();
}

//========================================
void 
StL2_2006EmulatorMaker::Clear(const Option_t*){
  clear();
  LOG_DEBUG<<"StL2_2006EmulatorMaker::Clear()"<<endm;
}


//========================================
Int_t 
StL2_2006EmulatorMaker::Make(){
  if( mMCflag==0) getTriggerData(); // for monitoring only

  make();

  addTriggerList(); // based on emulated L2Result[..]

  //  if(mConfig==100) janTest100();
  //---------------- debugging is below ------------
  int l2jetOff=-1;
  if(mYear==2006) l2jetOff=L2RESULTS_OFFSET_DIJET;
  //dump L2jet  results calculated by offline-algo
  const unsigned int *l2res=( (TrgDataType*)mTrigData)->TrgSum.L2Result;
  printf(" L2-jet off-line results below:\n");
  //  int k;  for (k=0;k<32;k++) printf("k=%2d  val=0x%04x\n",k,l2res[k]);
  L2jetResults2006 *out1= ( L2jetResults2006 *) &l2res[l2jetOff];
  
  L2jetResults2006_print(out1);
  unsigned char cSum=L2jetResults2006_doCheckSum(out1);
  assert(cSum==0);

  return kStOK;
}

//========================================
Int_t 
StL2_2006EmulatorMaker::Finish(){
  finish();
  return kStOK;
}


//========================================
//========================================
//========================================
void
StL2_2006EmulatorMaker::janTest100(){

  printf("in play:JanTest 100\n");
  // hA[1]->Fill(2);
}



//========================================
void
StL2_2006EmulatorMaker::addTriggerList() {// based on emulated L2Result[..]
  int l2jetOff=-1;
  assert(mYear=2006); // other years not implemented
  if(mYear==2006) l2jetOff=L2RESULTS_OFFSET_DIJET;
  const unsigned int *l2res=( (TrgDataType*)mTrigData)->TrgSum.L2Result;
  //  printf("aa off=%d\n",  l2jetOff);
  L2jetResults2006 *out= ( L2jetResults2006 *) &l2res[l2jetOff];
  
  if(out->int0.decision & (3<<6)) {
    //    printf(" FF  0x%0x 0x%0x \n", out->int0.decision,3<<6);
    // always both, can't distinguish
    mTriggerList.push_back(127652); // e-L2jet
    mTriggerList.push_back(127622); // b-L2jet
  }
  // printf(" FFB  %d %d \n",  isTrigger(127652) , isTrigger(127622));

}


//========================================
//========================================
bool 
StL2_2006EmulatorMaker::getTriggerData(){
  const StTriggerId *L1=0;
  //play with trigID

  int runId=0;
  int l2jetOff=0;
  if(mYear==2006) l2jetOff=L2RESULTS_OFFSET_DIJET;

  const unsigned int *l2res=0;  

  if(mUseMuDst) {
    StMuDstMaker *muMk = (StMuDstMaker*)StMaker::GetChain()-> GetMaker("MuDst");
    assert(muMk);
    // use muDst first, in JetReader StEvent also exist -but w/o trigger data
    StMuEvent *muEve = muMk -> muDst() -> event();
    assert(muEve);
    StMuTriggerIdCollection ticB = muEve -> triggerIdCollection();
    L1 = &ticB.nominal();
    StEventInfo &info=muEve->eventInfo();
    runId=info.runId();

#if 0 // read ezTree
    printf("AccessL2Decision() from ezTree:\n");    
    EztTrigBlob  *eTrig=muMk->muDst()->eztTrig();
    assert(eTrig);
    const TrgDataType2005 * trgDB=(TrgDataType2005 *)eTrig->trgd->GetArray();
    
    StTriggerData2005 trgAkio5(trgDB , runId);
    l2jetOff=trgAkio5.L2ResultsOffset(idJ); 
    l2res=trgDB->TrgSum.L2Result;
#endif
  
    
#if 1 // read regular muDst 
    TArrayI& l2Array = muMk->muDst()->event()->L2Result();
    printf("AccessL2Decision() from regular muDst: L2Ar-size=%d\n",l2Array.GetSize());    
    l2res=(unsigned int *)l2Array.GetArray();
#endif

 } else { // try StEvent  
    StEvent *mEvent = (StEvent *)StMaker::GetChain()->  GetInputDS("StEvent");
    assert(mEvent); // no other choises (except ezTree)
    StTriggerIdCollection *ticA=mEvent->triggerIdCollection();
    assert(ticA);     L1=ticA->nominal(); //was: l1();
    StEventInfo *info=mEvent->info();
    runId=info->runId();
    //?? trgD=(StTriggerData2005*) mEvent->triggerData(); assert(trgD);
    //not working 
    assert(1==2);
  }

#if 0
  if( !(L1->isTrigger(127622) || L1->isTrigger(127652)) ) {
    printf("Discard none-L2jet triggered events\n");
    return false; // discard events
  }
#endif

  printf(" L2-jet online results below:\n");
  // int k;  for (k=0;k<32;k++) printf("k=%2d  val=0x%04x\n",k,l2res[k]);
  L2jetResults2006 *out1= ( L2jetResults2006 *) &l2res[l2jetOff];
  // printf("pp=%p %d %d \n",out1,sizeof(L2jetResults2006), sizeof(L2jetOutInt0));
  
  L2jetResults2006_print(out1);
  unsigned char cSum=L2jetResults2006_doCheckSum(out1);
  assert(cSum==0);
    
#if 0
  vector<unsigned int> trgL=L1->triggerIds();
  printf("trigL len=%d totEve=%d\n",trgL.size(),mTotInpEve);
  uint ii;
  for(ii=0;ii<trgL.size();ii++){
    printf("ii=%d trigID=%d\n",ii,trgL[ii]);
  }
#endif

  // int id1=96300; printf("is zeroBias=%d -->%d\n",id1,L1->isTrigger(id1));
  // if(totInpEve>15)  assert(L1->isTrigger(id1)==0 || trgL.size()>1);
  //if(mTrigId && L1->isTrigger(mTrigId)==0) return false;
 

   //........get decision in 2006.......................
  //  bool isDijet  = (jetRes->int0.decision) & 0x80;
  // bool isMonojet= (jetRes->int0.decision) & 0x40;
  //bool isRandom = (jetRes->int0.decision) & 0x20;


  return true;

}


// $Id: StL2_2006EmulatorMaker.cxx,v 1.1 2007/10/22 23:09:59 balewski Exp $
//


#if 0
//_____________________________________________________________________________
void
StL2_2006EmulatorMaker::initHistoA(TString core) {
  LOG_INFO <<Form("initHistoA(),")<<endm;

  assert(mHList);
  memset(hA,0,sizeof(hA));

  hA[0]=0;
  hA[1]=new TH1F(core+"trgID", "event counter for Jan",10,1,11);

  //.... add histos to the list
  int i;
  for(i=0;i<mxAH;i++) {
    if( hA[i]==0) continue;
    mHList->Add( hA[i]);
  }
}
#endif
