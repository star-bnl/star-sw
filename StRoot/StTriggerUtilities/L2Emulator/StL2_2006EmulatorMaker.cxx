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
#include "L2jetAlgo/L2jetAlgo2006.h"
#include "L2pedAlgo/L2pedAlgo.h"
#include "L2gammaAlgo/L2gammaAlgo.h"
#include "L2upsilon/L2upsilon2006.hh"

#include "StL2_2006EmulatorMaker.h"

ClassImp(StL2_2006EmulatorMaker)

StL2_2006EmulatorMaker::StL2_2006EmulatorMaker(const char *name):StMaker(name) {
  mL2pedAlgo=0;
  mL2jetAlgo2006=0;
  mL2gammaEEmc=mL2gammaBEmc=0;
  mL2upsilon2006=0;
}

//========================================
StL2_2006EmulatorMaker::~StL2_2006EmulatorMaker(){
}


//========================================
//========================================
Int_t  
StL2_2006EmulatorMaker::InitRun(int runNo){
  //WARN: do NOT use  runNo for any setup - it woul dberak for M-C
  //WARN: do NOT use run# to controll setup of L2-algos
  
  initRun1();
  LOG_INFO << Form("::setupL2Algos2006(), dbDate=%d  ", mYearMonthDay)<<endm;

     
  assert(mYearMonthDay>20060000); 
  assert(mYearMonthDay<20060700);
  
  mL2algoN=5; // total # of L2 algos (ped, jet)
  mL2algo =new L2VirtualAlgo *[mL2algoN]; // not cleared memeory leak
  memset(mL2algo,0,mL2algoN*sizeof(void*));
  //setup every algo one by one, params may be time dependent
  
  // ----------- L2 ped algo ----------------  slot 0
  mL2algo[0]=mL2pedAlgo=new L2pedAlgo("ped-algo",mL2EmcDb,mL2EmcDb->logPath,L2RESULTS_OFFSET_EMC_PED);

  // ----------- L2 jet algo ---------------- slot 1
  //time dependent L2jet cuts are below 
  assert( mYearMonthDay>20060316); // before L2jet was not used
  assert( mYearMonthDay<20060620); // after L2jet was not used
  
  mL2algo[1]=mL2jetAlgo2006=new L2jetAlgo2006("jet06-algo",mL2EmcDb,mL2EmcDb->logPath,L2RESULTS_OFFSET_DIJET);
  
  // ----------- L2 gamma algo ----------------uses  slots 2 & 3
  assert( mYearMonthDay >= 20060406 ); // before ppTrans
  assert( mYearMonthDay <= 20060607 ); // after ppLong2
  
  mL2algo[2]=mL2gammaEEmc=new L2gammaAlgo("etow_gamma",mL2EmcDb,mL2EmcDb->logPath,L2RESULTS_OFFSET_PIG+2);
  mL2algo[3]=mL2gammaBEmc=new L2gammaAlgo("btow_gamma",mL2EmcDb,mL2EmcDb->logPath,L2RESULTS_OFFSET_PIG);

  // ----------- L2 Upsilon algo ----------------  slot 4
  assert( mYearMonthDay >= 20060406 ); // before ppTrans
  assert( mYearMonthDay <= 20060607 ); // after ppLong2

  mL2algo[4]=mL2upsilon2006=new L2upsilon2006("upsilon", mL2EmcDb, mL2EmcDb->logPath,L2RESULTS_OFFSET_UPS); 
  TString fullPath=Form("%sL2/%d/algos/btowXYZ.dat", mSetupPath.Data(), mYear);
  mL2upsilon2006->readGeomXYZ(fullPath.Data());

  // ----------- L2 J/Psi algo ----------------  slot 5
  // add here

  initRun2(runNo);

  LOG_INFO  << "::InitRun() done, run=" <<runNo<<" isMC="<<mMCflag<<endm;
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
  //  LOG_DEBUG<<"StL2_2006EmulatorMaker::Clear()"<<endm;
}


//========================================
Int_t 
StL2_2006EmulatorMaker::Make(){
  if( mMCflag==0) getTriggerData(); // for monitoring only

  make();


#if 0
  //---------------- ONLY debugging is below ------------
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

#endif // end of debuging

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

    // read regular muDst   
    if(mMCflag==0) {
      TArrayI& l2Array = muMk->muDst()->event()->L2Result();
      LOG_DEBUG <<Form("AccessL2Decision() from regular muDst: L2Ar-size=%d",l2Array.GetSize())<<endm;    
      l2res=(unsigned int *)l2Array.GetArray();
    }

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

#if 0 // just debugging & cross check
  if(mMCflag==0) {
    printf(" L2-jet online results below:\n");
    // int k;  for (k=0;k<32;k++) printf("k=%2d  val=0x%04x\n",k,l2res[k]);
    L2jetResults2006 *out1= ( L2jetResults2006 *) &l2res[l2jetOff];
    // printf("pp=%p %d %d \n",out1,sizeof(L2jetResults2006), sizeof(L2jetOutInt0));    
    L2jetResults2006_print(out1);
    unsigned char cSum=L2jetResults2006_doCheckSum(out1);
    assert(cSum==0);
  } // only for real data
#endif // end of debugging
    
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


// $Id: StL2_2006EmulatorMaker.cxx,v 1.13 2010/08/13 00:44:16 rfatemi Exp $
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
