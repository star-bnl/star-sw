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
#include "L2gammaAlgo/L2gammaAlgo.h"

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
  //WARN: do NOT use run# to controll setup of L2-algos
  
  initRun();
  
  LOG_INFO << Form("::setupL2Algos2006(), dbDate=%d  ", mYearMonthDay)<<endm;
  
  assert(mYearMonthDay>20060000); 
  assert(mYearMonthDay<20060700);
  
  mL2algoN=4; // total # of L2 algos (ped, jet)
  mL2algo =new L2VirtualAlgo *[mL2algoN]; // not cleared memeory leak
  memset(mL2algo,0,mL2algoN*sizeof(void*));
  //setup evry algo one by one, params may be time dependent

  char fname[1000];

  // ----------- L2 ped algo ----------------  slot 0
  int  L2ResOff=L2RESULTS_OFFSET_EMC_PED;
  sprintf(fname,"%sL2/%d/algos/algoPed.setup",mSetupPath.Data(),mYear);
  assert( L2VirtualAlgo::readParams(fname,mxPar,intsPar,floatsPar)==4);
  mL2pedAlgo=new L2pedAlgo("ped-algo",mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  assert(mL2pedAlgo->initRun( runNo,intsPar,floatsPar)==0); // zero tolerance for missing input files
  mL2algo[0]=mL2pedAlgo;

  // ----------- L2 jet algo ---------------- slot 1
  L2ResOff=L2RESULTS_OFFSET_DIJET;
  //time dependent L2jet cuts are below 
  assert( mYearMonthDay>20060316); // before L2jet was not used
  assert( mYearMonthDay<20060620); // after L2jet was not used

  if( mYearMonthDay<20060406) { // ppLong-1 period not implementd
    assert(1==2);
  } else if (  mYearMonthDay<200605011) { // ppTrans
  sprintf(fname,"%sL2/%d/algos/algoJet.setup1",mSetupPath.Data(),mYear);
  } else   { // ppLong-2, 62 geV  periods not implementd
    assert(1==2);
  }
  assert( L2VirtualAlgo::readParams(fname,mxPar,intsPar,floatsPar)==10);
  mL2jetAlgo=new L2jetAlgo("jet-algo",mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  assert(mL2jetAlgo->initRun(runNo,intsPar,floatsPar)==0); // zero tolerance for missing input files
  mL2algo[1]=mL2jetAlgo;

  // ----------- L2 gamma algo ----------------uses  slots 2 & 3
  mL2gammaEEmc=mL2gammaBEmc=0;
  addL2GammaAlgos2006(runNo); 

  // ----------- L2 J/Psi algo ----------------  slot 5
  // add here

  LOG_INFO  << "StL2JetEmulMaker::InitRun() done, run=" <<runNo<<endm;
  
  return kStOK; 
} 

//_____________________________________________________________________________
void
StL2_2006EmulatorMaker::addL2GammaAlgos2006(int runNo){
  int L2ResOff = L2RESULTS_OFFSET_PIG;
  assert( mYearMonthDay >= 20060406 ); // before ppTrans
  assert( mYearMonthDay <= 20060607 ); // after ppLong2
  
  TString l2gammaEEmcSetup=Form("%sL2/%d/algos/l2gamma_eemc.setup",mSetupPath.Data(),mYear);
  TString l2gammaBEmcSetup=Form("%sL2/%d/algos/l2gamma_bemc.setup",mSetupPath.Data(),mYear);
  
  // Note-- this breaks because physicists don't wait until midnight
  // to make the swicth!
  if ( mYearMonthDay >= 20060406 &&
       mYearMonthDay <= 20060510 ) {
    l2gammaEEmcSetup += 1;
    l2gammaBEmcSetup += 1;
  }
  else if ( mYearMonthDay >  20060510 &&
	    mYearMonthDay <= 20060513 ) {
    l2gammaEEmcSetup += 2;
    l2gammaBEmcSetup += 2;
  }
  else if ( mYearMonthDay >  20060513 &&
	    mYearMonthDay <= 20060515 ) {
    l2gammaEEmcSetup += 3;
    l2gammaBEmcSetup += 3;
  }
  else {
    l2gammaEEmcSetup += 4;
    l2gammaBEmcSetup += 4;
  }
  
  // create L2gamma 
  Int_t   eemc_ints[5],   bemc_ints[5];
  Float_t eemc_floats[5], bemc_floats[5];
  Int_t etest = L2VirtualAlgo::readParams( l2gammaEEmcSetup.Data(), 5, eemc_ints, eemc_floats );
  Int_t btest = L2VirtualAlgo::readParams( l2gammaBEmcSetup.Data(), 5, bemc_ints, bemc_floats );
  
  if ( etest != 10 ) {
    LOG_ERROR << GetName() << Form(" -- error initializing EEMC params filename=%s code=%i",l2gammaEEmcSetup.Data(),etest)<<endm;
    assert(2+2==5);
  }
  if ( btest != 10 ) {
    LOG_ERROR << GetName() << Form(" -- error initializing BEMC params filename=%s code=%i",l2gammaBEmcSetup.Data(),btest)<<endm;
  }
  
  mL2gammaEEmc = new L2gammaAlgo("etow_gamma",mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  mL2gammaBEmc = new L2gammaAlgo("btow_gamma",mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  
  
  etest=  mL2gammaEEmc->initRun("eblah",runNo,eemc_ints,eemc_floats);
  btest=  mL2gammaBEmc->initRun("bblah",runNo,bemc_ints,bemc_floats);
  
  if ( etest ) {
    LOG_ERROR << Form("Problem initializing runtime parameters for eemc run=%i",runNo) << endm;
  }
  if ( btest ) {
    LOG_ERROR << Form("Problem initializing runtime parameters for bemc run=%i",runNo) << endm;
  }
  
  // add to list of algos to execute
  mL2algo[2]=mL2gammaEEmc;
  mL2algo[3]=mL2gammaBEmc;
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


// $Id: StL2_2006EmulatorMaker.cxx,v 1.5 2007/11/02 03:03:36 balewski Exp $
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
