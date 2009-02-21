//
//  StEemcTriggerSimu.cxx,v 0.01
//
/*
  changes to be done in bbc-code, Jan
  - add TDC limits for real data
  - add histos for all PMTs
*/

#include <TH2.h>
#include <StMessMgr.h>

#include "StEemcTriggerSimu.h"
#include "EemcHttpInfo.h"

#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StEmcTriggerDetector.h"
#include "StEvent/StL0Trigger.h"


//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "St_db_Maker/St_db_Maker.h"
  
// ETOW stuff
#include <StEEmcUtil/database/StEEmcDb.h>
#include <StEEmcUtil/database/EEmcDbCrate.h>
#include <StEEmcUtil/database/EEmcDbItem.h>
#include <StEEmcUtil/database/cstructs/eemcConstDB.hh>


#include "StEEmcUtil/EEdsm/EEfeeTPTree.h" 
#include "StEEmcUtil/EEdsm/EEfeeTP.h" 
#include "StEEmcUtil/EEdsm/EEdsm0Tree.h"
#include "StEEmcUtil/EEdsm/EEdsm1Tree.h"
#include "StEEmcUtil/EEdsm/EMCdsm2Tree.h"
#include "StEEmcUtil/EEdsm/EEdsm3.h"
#include "StEEmcUtil/EEdsm/EemcTrigUtil.h"

// #### modified by Liaoyuan ####
// DSM 2009 Utilities
#include "StTriggerUtilities/StDSMUtilities/StDSM2009Utilities.hh"
// #### modified end ####


ClassImp(StEemcTriggerSimu);

//==================================================

StEemcTriggerSimu::StEemcTriggerSimu() {
  //  printf("The StEemcTriggerSimu constructor\n");
  nInpEve=0;
  mHList=0;
  mDbE=0;
  mBemcEsum5bit=0;
  mExternDsmSetup=0;
  mSetupPath="wrong1";

  feeTPTreeADC=new EEfeeTPTree("ADC",mxChan);
  dsm0TreeADC =new EEdsm0Tree("ADC");
  dsm1TreeADC =new EEdsm1Tree("ADC");
  dsm2TreeADC =new EMCdsm2Tree("ADC");

  // only for QA
  dsm0TreeTRG =new EEdsm0Tree("TRG");
  dsm1TreeTRG =new EEdsm1Tree("TRG");
  dsm2TreeTRG =new EMCdsm2Tree("TRG");
  dsm3TRG     =new EEdsm3();

  LOG_INFO <<"Eemc::constructor"<<endm;
}



//==================================================
StEemcTriggerSimu::~StEemcTriggerSimu(){ /* nop */}


//==================================================
//==================================================
void  
StEemcTriggerSimu::Clear(){
  
  // printf("This is StEemcTriggerSimu::Clear\n");
  memset(rawAdc,0,sizeof(rawAdc));
  feeTPTreeADC->clear();
  dsm0TreeADC->clear();
  dsm1TreeADC->clear();
  dsm2TreeADC->clear();

  // always clear, despite of setup
  dsm0TreeTRG->clear();
  dsm1TreeTRG->clear();
  dsm2TreeTRG->clear();
  dsm3TRG->clear();
  
}

//==================================================
//==================================================
void  
StEemcTriggerSimu::Init(){

  mYear=-888;
  //................EEMC stuff ..............
  mDbE = (StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb");
  assert(mDbE);
  //  assert( mBemcEsum5bit);
  LOG_INFO <<Form("Eemc::Init() MC_flag=%d, config: flag=%d, path=%s=",mMCflag, mConfig,mSetupPath.Data())<<endm;
  assert(mConfig>=kOnlyAdc);
  assert(mConfig<=kAdcCompareTrig);
}
  
//==================================================
//==================================================
void  
StEemcTriggerSimu::addTriggerList( void * adr){
  vector <int> *trgList=( vector <int> *)adr;

  if(mYear==2006) {
    //fix it if(   yymmdd<20060408  || yymmdd>20060414) return; 
    if(dsm2TreeADC->getOutEndcapHTTP1bit())   trgList->push_back(127580);//
    if(dsm2TreeADC->getOutEndcapJP2bit()>=1)  trgList->push_back(127551);//EJP0,add mising mb
    if(dsm2TreeADC->getOutEndcapJP2bit()>=2)  trgList->push_back(127271);
    if(dsm2TreeADC->getOutEndcapJP2bit()>=1 && dsm2TreeADC->getOutEtot1bit()) trgList->push_back(127652);
    if(dsm2TreeADC->getOutEndcapHTTP1bit()){
      trgList->push_back(127831);
      trgList->push_back(127611);
    }
  }
  // #### modified by Liaoyuan ####
  else if( mYear == 2009 ){
    ; // Triggers-Ids are Generated in StEmcTriggerSimu Instead
  }
  // #### modified end ####

}

//==================================================
//==================================================
StTriggerSimuDecision
StEemcTriggerSimu::triggerDecision(int trigId) {
  vector<int> tmpTrigList;
  addTriggerList(&tmpTrigList);
  for(unsigned i=0; i<tmpTrigList.size(); i++) {
    if(trigId == tmpTrigList[i]) return kYes;
  }
  return kDoNotCare;
}
 
//==================================================
//==================================================
void  
StEemcTriggerSimu::InitRun(int runnumber){

  
  memset(feePed,0,sizeof(feePed));
  memset(feeMask,0xff,sizeof(feeMask)); // mask everything as bad
  getEemcFeeMask();

  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  assert(mydb);
  mYear=mydb->GetDateTime().GetYear();
  int yyyymmdd=mydb->GetDateTime().GetDate(); //form of 19971224 (i.e. 24/12/1997)
  int hhmmss=mydb->GetDateTime().GetTime(); //form of 123623 (i.e. 12:36:23)

  LOG_INFO<<Form("Eemc::InitRun()  yyyymmdd=%d  hhmmss=%06d\n", yyyymmdd, hhmmss )<<endm;

  char text[1000];
  sprintf(text,"%sL0/%d/EemcFeePed/",mSetupPath.Data(),mYear);  
  EemcTrigUtil::getFeePed4(text, yyyymmdd, hhmmss, mxChan, feePed);

  if( mYear == 2006 ){ // #### modified line by Liaoyuan 
    DsmThreshold thresholds;
    if(!mExternDsmSetup) {
      EemcTrigUtil::getDsmThresholds( yyyymmdd, hhmmss, thresholds );
    }  else {
      LOG_INFO<<Form("Eemc::InitRun() use externalDSM setup")<<endm;
      int i;
      for(i=0;i<nThr;i++) thresholds.HT[i]=mExternDsmSetup[0+i];
      for(i=0;i<nThr;i++) thresholds.TP[i]=mExternDsmSetup[3+i];
      for(i=0;i<nThr;i++) thresholds.JP[i]=mExternDsmSetup[6+i];
      thresholds.TPthrSelc   =mExternDsmSetup[9];
      thresholds.HTTPselect  =mExternDsmSetup[10];
      thresholds.JPSIthrSelc =mExternDsmSetup[11];
      thresholds.BarreSide   =mExternDsmSetup[12];
      thresholds.BEsumthr    =mExternDsmSetup[13];
      thresholds.EEsumthr    =mExternDsmSetup[14];
      thresholds.EtotThr     =mExternDsmSetup[15];
    }

    LOG_INFO<<Form("Eemc::DSM setup HTthr: %d, %d, %d",thresholds.HT[0],thresholds.HT[1],thresholds.HT[2])<<endm;
    LOG_INFO<<Form("Eemc::DSM setup TPthr: %d, %d, %d",thresholds.TP[0],thresholds.TP[1],thresholds.TP[2])<<endm;
    LOG_INFO<<Form("Eemc::DSM setup JPthr: %d, %d, %d",thresholds.JP[0],thresholds.JP[1],thresholds.JP[2])<<endm;
    LOG_INFO<<Form("Eemc::DSM setup  BEsumthr=%d, EEsumthr=%d, EtotThr=%d",thresholds.BEsumthr,thresholds.EEsumthr,thresholds.EtotThr)<<endm;
    LOG_INFO<<Form("Eemc::DSM setup TPthrSelc=%d, HTTPthrSelc=%d, JPSIthrSelc=%d, BarreSide=%d",thresholds.TPthrSelc,thresholds.HTTPselect,thresholds.JPSIthrSelc,thresholds.BarreSide)<<endm;

    dsm0TreeADC->setYear(mYear,thresholds.HT,thresholds.TP); 
    dsm0TreeTRG->setYear(mYear,thresholds.HT,thresholds.TP); 

    dsm1TreeADC->setYear(mYear,thresholds.JP,thresholds.TPthrSelc,thresholds.HTTPselect);
    dsm1TreeTRG->setYear(mYear,thresholds.JP,thresholds.TPthrSelc,thresholds.HTTPselect);
 
    dsm2TreeTRG->setYear(mYear,thresholds.BEsumthr,thresholds.EEsumthr,thresholds.JPSIthrSelc,thresholds.BarreSide,thresholds.EtotThr);
    dsm2TreeADC->setYear(mYear,thresholds.BEsumthr,thresholds.EEsumthr,thresholds.JPSIthrSelc,thresholds.BarreSide,thresholds.EtotThr);
  
    dsm3TRG->setYear(mYear);

    initHisto();
  } // #### modified line by Liaoyuan 
  // #### modified by Liaoyuan ####
  else if( mYear == 2009 ){

    DsmThreshold thresholds;
    EemcTrigUtil::getDsmThresholds( yyyymmdd, hhmmss, thresholds );

    LOG_INFO<<Form("Eemc::DSM setup HTthr: %d, %d, %d",thresholds.HT[0],thresholds.HT[1])<<endm;
    LOG_INFO<<Form("Eemc::DSM setup JPthr: %d, %d, %d",thresholds.JP[0],thresholds.JP[1],thresholds.JP[2])<<endm;

    for (int i = 0; i < 2; ++i) mE001->setRegister(i,thresholds.HT[i]);
    for (int i = 0; i < 3; ++i) mE101->setRegister(i,thresholds.JP[i]);

  }
  // #### modified end ####
}

     
//==================================================
//==================================================
void 
StEemcTriggerSimu::Make(){
  nInpEve++;  
  mDumpEve=eveId%1==0;

#if 0
  assert(mMCflag==0); // not sure what will it do for MC
  StMuDstMaker* muMk = (StMuDstMaker*) StMaker::GetChain()->Maker("MuDst");
  assert(muMk);  
  StEventInfo &info=muMk->muDst()->event()->eventInfo();
  //  mEleT->info.zVertex=ver.z();
  eveId=info.id();
  StMuTriggerIdCollection *tic=&(muMk->muDst()->event()->triggerIdCollection());
  std::vector<unsigned int> trgL=(tic->nominal()).triggerIds();
  //  printf("   trigL len=%d \n",trgL.size());
  uint ii;
  for(ii=0;ii<trgL.size();ii++){ // collect all trigger ID's
    TString cID=Form("%d",trgL[ii]);
    hA[1]->Fill(cID.Data(),1.);
  }
#endif

  // ************** Emulation of trigger based on ADC ************ 
  getEemcAdc();   //  processed raw ADC
  feeTPTreeADC->compute(rawAdc,feePed,feeMask); 

  if( mYear == 2006 ){ // #### modified line by Liaoyuan 


    int i;
    //  for(i=0;i<90;i++) feeTPTreeADC->TP(i)->print(3); // prints FEE output for 720 towers
  
    // printf("...... populate inputs of dsm0TreeADC...\n");
    for(i=0;i<EEfeeTPTree::mxTP;i++) {
      dsm0TreeADC->setInp12bit(i,feeTPTreeADC->TP(i)->getOut12bit());
    }
    dsm0TreeADC->compute();
    // if(mDumpEve) dsm0TreeADC->print();

    int j;
    for(j=0;j<EEdsm0Tree::Nee0out;j++) {
      int k = EEdsm1Tree::Nee1BoardInpCha;
      int brd = j/k+1;
      int cha = j%k;
      dsm1TreeADC->setInp16bit(brd, cha, dsm0TreeADC->getOut16bit(j));
    }
    dsm1TreeADC->compute();

    // Endcap DSM2 (1 board), all information
    dsm2TreeADC->setInput16bit(0, 0, dsm1TreeADC->getOut16bit(0));
    dsm2TreeADC->setInput16bit(0, 1, dsm1TreeADC->getOut16bit(1));

#if 0 // waits for Renee's Etot from DSM
    // Barrel DSM2 (3 boards), only 5bit Esum for 6 remaining inputs
    static const  int kA[6]={3,4,5,0,1,2}; // mapping between Renee & Hank  
    for(j=0;j<6;j++) {
      ushort fakeInput=mBemcEsum5bit[kA[j]]; //DSM 5bit ADC
      // higher bits ar not provided for the Barrel
      int ibr=j/2;
      int ich=j%2;
      dsm2TreeADC->setInput16bit(ibr+1, ich, fakeInput);    
    }
#endif
    dsm2TreeADC->compute();

  
    
    // *********** END of Emulation of trigger based on ADC ************ 

    if(mMCflag==0 && mConfig>=kAdcAndTrig){ 
      // acquire true DSM values  only for real events
      //........... QA of online trigger data ...............
      getDsm0123inputs(); // this tree contains trigger data
      dsm0TreeTRG->compute();
      dsm1TreeTRG->compute(); 
      dsm2TreeTRG->compute(); 


      //if(mDumpEve)   dsm0TreeTRG->print();
      //if(mDumpEve)   dsm1TreeTRG->print();
      //if(mDumpEve)   dsm2TreeTRG->print();  
      //if(mDumpEve)   dsm3TRG->print();  
    
      //dsm2TreeADC->print(); 
      //dsm2TreeTRG->print(); 
      if(mConfig>=  kAdcCompareTrig) {
	compareADCfee_TRG0(); 
	compareADC0_TRG1();
	compareADC1_TRG2();
	compareADC2_TRG3();
      
	compareTRG0_TRG1(); 
	compareTRG1_TRG2(); 
	compareTRG2_TRG3();
      }  
    }

    DSM2EsumSpectra();



  // dsm2TreeADC->print(0); 
  
  //if(mDumpEve) printf("\nzzzzz===================================================\n\n");
  } // #### modified line by Liaoyuan 
  // #### modified by Liaoyuan ####
  else if( mYear == 2009 ){
    get2009_DSMLayer0();
    get2009_DSMLayer1();
  }
  // #### modified end ####  
}


//==================================================
//==================================================
//==================================================
bool 
StEemcTriggerSimu::getHttpInfo(int tpId, EemcHttpInfo &httpInfo){
  httpInfo.clear();
  int ith=mHTTPthrSelc-1;
  assert(ith>=0 && ith<nThr);
  if(tpId<0 or tpId>=EEfeeTPTree::mxTP) return false;
  EEfeeTP *TP=feeTPTreeADC->TP(tpId); assert(TP);
  int TPsum6b=TP->getOutTPsum();
  if(TPsum6b<mTPthr[ith]) return false;
  int HT6bit=TP->getOutHT();
  if(HT6bit<mHTthr[ith]) return false;
  httpInfo.tpId=tpId;
  httpInfo.tpSumDsmAdc=TPsum6b;
  httpInfo.htwCh=TP->getTranHTchId();
  httpInfo.htwCr=TP->getCrateID();
  httpInfo.htwDsmAdc=HT6bit; 
  return true;
 
}

//==================================================
//==================================================
//==================================================
void 
StEemcTriggerSimu::getEemcAdc(){
  
   // printf("This StEemcTriggerSimu::getting Endcap ADC values\n");

  StMuDstMaker* muMk = (StMuDstMaker*) StMaker::GetChain()->Maker("MuDst");
  assert(muMk);
  StMuEmcCollection* emc = muMk->muDst()->muEmcCollection();
  if (!emc) {
     LOG_WARN  <<"No EMC data1 for this event"<<endm;    
     return;
  }
  // printf("see %d endcap towers\n",emc->getNEndcapTowerADC());
  
//.........................  T O W E R S .....................
  int i;
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,AdcRead; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,AdcRead,sec,sub,eta);

    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=mDbE->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
    // do NOT drop not working channels
    
    assert(x->crate>0 && x->crate<7);
    assert(x->chan>=0 && x->chan<120);
    rawAdc[ (x->crate-1)*mxChan+ x->chan]=AdcRead;
 
    //if(strstr(x->name,"12TD03")) printf("i=%d, name=%s, sec=%d, crate=%d, chan=%d, ped=%.1f, rawADC=%d\n",i, x->name, sec, x->crate, x->chan, x->ped, AdcRead);
    //  if(x->crate==2) printf(" name=%s crate=%d, chan=%d  ped=%.1f rawADC=%d  stat=%d fail=%d\n", x->name, x->crate, x->chan, x->ped, AdcRead, x->stat, x->fail);
    
  }// end of loop over towers
  
}

//==================================================
void 
StEemcTriggerSimu::getDsm0123inputs(){
  /*
    >     unsigned char eemcHighTower(int patch_id, int prepost=0) const;
    >     unsigned char eemcJetPatch (int patch_id, int prepost=0) const;    
    >     unsigned char eemcHighestTowerADC(int prepost=0) const;
    >     unsigned      char * getDsm0_EEMC(int prepost=0) const;
    >     unsigned short int * getDsm1_EEMC(int prepost=0) const;
    >     unsigned short int * getDsm2_EMC()  const;
    >     unsigned short int * getDsm3()      const; // lastDSM
  */
  

  //  printf("This StEemcTriggerSimu::getting Endcap DSM0 inputs\n");

  StMuDstMaker* muMk = (StMuDstMaker*) StMaker::GetChain()->Maker("MuDst");
  assert(muMk);

  StEmcTriggerDetector &emcTrgDet=muMk->muDst()->event()->emcTriggerDetector();

  int i;
  for(i=0;i<EEfeeTPTree::mxTP;i++) {
    int HT=emcTrgDet.highTowerEndcap(i) &0x3f ;
    int TP=emcTrgDet.patchEndcap(i) &0x3f;
    dsm0TreeTRG->setInp12bit(i,((TP<<6)+HT));
  }
   

#if 0  
  int   mNEemcLayer1 = 16, mNEmcLayer2 = 8;

  for(i=0;i<mNEemcLayer1;i++) {
    unsigned short x=emcTrgDet.eemcLayer1(i);
    printf("DSM L1: i=%d val=%d \n",i,x);
  }
  for(i=0;i<mNEmcLayer2;i++) {
    unsigned short x=emcTrgDet.emcLayer2(i);
    printf("DSM L2: i=%d val=%d \n",i,x);
  }
#endif

 
  for(i=0;i<dsm1TreeTRG->getNboards();i++) { // loop over DSM1 boards
   dsm1TreeTRG->setInp16bit(i+1,0,emcTrgDet.eemcLayer1(3+i*8));
   dsm1TreeTRG->setInp16bit(i+1,1,emcTrgDet.eemcLayer1(2+i*8));
   dsm1TreeTRG->setInp16bit(i+1,2,emcTrgDet.eemcLayer1(1+i*8));
   dsm1TreeTRG->setInp16bit(i+1,3,emcTrgDet.eemcLayer1(0+i*8));
   dsm1TreeTRG->setInp16bit(i+1,4,emcTrgDet.eemcLayer1(7+i*8));
   dsm1TreeTRG->setInp16bit(i+1,5,emcTrgDet.eemcLayer1(6+i*8));
 }

  //Endcap DSM2 (1 board)
  dsm2TreeTRG->setInput16bit(0,0,emcTrgDet.emcLayer2(5));
  dsm2TreeTRG->setInput16bit(0,1,emcTrgDet.emcLayer2(4));
  //Barrel DSM2 (3 boards)
  dsm2TreeTRG->setInput16bit(1,0,emcTrgDet.emcLayer2(3));
  dsm2TreeTRG->setInput16bit(1,1,emcTrgDet.emcLayer2(2));
  dsm2TreeTRG->setInput16bit(2,0,emcTrgDet.emcLayer2(1));
  dsm2TreeTRG->setInput16bit(2,1,emcTrgDet.emcLayer2(0));
  dsm2TreeTRG->setInput16bit(3,0,emcTrgDet.emcLayer2(7));
  dsm2TreeTRG->setInput16bit(3,1,emcTrgDet.emcLayer2(6));

  //DSM3 (lastDSM)
  StL0Trigger &L0trg=muMk->muDst()->event()->l0Trigger();
  int L0Num;
  L0Num=L0trg.lastDsmArraySize();
  ushort L0word=L0trg.lastDsmArray(0);
  dsm3TRG->setWord(0, L0word);
  //printf("L0word=%d\n", L0word);


#if 0 
  printf("Dump of real DSM0 inputs, HT:");
  for(i=0;i<mxTP;i++) {
    if(i%10==0) 
      printf("\nTPch=%2d ",i);
    else     if(i%5==0) 
      printf("   ");
    else     
      printf(" ");
    printf("%3d ",dsm0inHT[i]);
  }  printf("\n");
  printf("Dump of real DSM0 inputs, TPsum:");
  for(i=0;i<mxTP;i++) {
    if(i%10==0) 
      printf("\nTPch=%2d ",i);
    else     if(i%5==0) 
      printf("   ");
    else     
      printf(" ");
    printf("%3d ",dsm0inTPsum[i]);
  }
  printf("\n");
#endif
}


//==================================================
//==================================================

void 
StEemcTriggerSimu::getEemcFeeMask(){
  assert(mDbE);
  for (int icr=0; icr<6; icr++){
    int ich;
    for(ich=0; ich<mxChan;ich++) {
      const EEmcDbItem *x=mDbE-> getByCrate(icr+1,ich);
      if(x==0) continue; // skip not mapped channels
      bool killIt=x->stat & EEMCSTAT_HOTHT;
      if(killIt)  x->print();  
      feeMask[ (x->crate-1)*mxChan   + ich]=killIt;
    } // end of chan loop
  }// end of crate loop
}

//==================================================
//==================================================

// #### modified by Liaoyuan ####

void
StEemcTriggerSimu::get2009_DSMLayer0(){
  for( size_t dsm = 0; dsm < mE001->size(); dsm++ ){
    for( int ch = 0; ch < 10; ch++ ){
      Int_t tpid = dsm * 10 + ch;
      (*mE001)[dsm].channels[ch] = feeTPTreeADC->TP(tpid)->getOut12bit();
    }
  }
  
  mE001->run();
}

void
StEemcTriggerSimu::get2009_DSMLayer1(){
  mE001->write(*mE101);
  mE101->run();
}

// #### modified end ####

//
// $Log: StEemcTriggerSimu.cxx,v $
// Revision 1.13  2009/02/21 19:21:02  pibero
// Updates to match changes in EemcTrigUtil
//
// Revision 1.12  2009/02/20 23:40:17  pibero
// Updates for Run 9 by Liaoyuan
//
// Revision 1.11  2009/02/04 20:01:28  rfatemi
// Change includes for StEmcDecoder
//
// Revision 1.10  2007/11/08 20:59:52  kocolosk
// subdet isTrigger returns a bool
// triggerDecision returns enumerator including kDoNotCare
//
// Revision 1.9  2007/10/12 20:11:33  balewski
// cleanup of setup path, now at inst/iucf
//
// Revision 1.8  2007/10/11 00:33:03  balewski
// L2algo added
//
// Revision 1.7  2007/09/24 18:08:42  kocolosk
// added inheritance from ABC clss StTriggerSimu
//
// Revision 1.6  2007/07/24 01:32:59  balewski
// added HTTP id for the endcap
//
// Revision 1.5  2007/07/23 03:00:00  balewski
// cleanup, bbc for M-C still not working
//

