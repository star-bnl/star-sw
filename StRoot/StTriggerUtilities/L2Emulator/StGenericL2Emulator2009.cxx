// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 16 April 2010
// Original StGenericL2Emulator by Jan Balewski <balewski@mit.edu> and Renee Fatemi <rfatemi@pa.uky.edu>
// Interfaces L2 algos to the STAR ofl software

#include "StChain.h"
#include "St_DataSetIter.h"

#include <Stiostream.h>
#include <math.h>

#include "TFile.h"
#include "TArrayF.h"

#include <StMessMgr.h>


// ETOW stuff
#include <StEEmcUtil/database/StEEmcDb.h>
#include <StEEmcUtil/database/EEmcDbItem.h>
#include <StEEmcUtil/database/EEmcDbCrate.h>


// BTOW stuff
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/database/StEmcDecoder.h"

// StEvent
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StTriggerId.h"
#include "StEvent/StTriggerIdCollection.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//L2 stuff
#include "L2algoUtil/L2EmcDb.h"
#include "L2algoUtil/L2EmcGeom.h"
#include "L2algoUtil/L2DbConfig.h"  // time-dep config
#include "L2algoUtil/L2DbTime.h"  // time-dep config
#include "L2algoUtil/L2btowCalAlgo09.h"
#include "L2algoUtil/L2etowCalAlgo09.h"
#include "L2pedAlgo/L2pedAlgo09.h"

//trg-data from ezTree, to read on-line decision, tmp
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h" // to access DB time stamp
#include "St_db_Maker/St_db_Maker.h"

// L0-trigSimu
#include "StTriggerSimuMaker.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/EMCdsm2Tree.h"

#include "StGenericL2Emulator2009.h"

  /* usefull dimensions */
#define MaxBtowRdo (L2EmcDb::BTOW_MAXFEE*L2EmcDb::BTOW_DATSIZE)
#define MaxEtowRdo (L2EmcDb::ETOW_MAXFEE*L2EmcDb::ETOW_DATSIZE)

ClassImp(StGenericL2Emulator2009)

StGenericL2Emulator2009::StGenericL2Emulator2009(){
  mBTOW_BANK =new  unsigned short [MaxBtowRdo];
  mETOW_BANK =new  unsigned short [MaxEtowRdo];
  mUseMuDst=true;
  setMC(false);

  mSetupPath="wrong2";
  mOutPath="wrong3";
  mYear=-1;
  mYearMonthDay=-2;
  mHourMinSec=-3;
}

//________________________________________________________
//________________________________________________________

StGenericL2Emulator2009::~StGenericL2Emulator2009(){
  delete [] mBTOW_BANK;
  delete [] mETOW_BANK;

}




//________________________________________________________
//________________________________________________________

void StGenericL2Emulator2009::init(){
  mTotInpEve=0;   
  //................EEMC stuff ..............
  mDbE = (StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb");
  assert(mDbE);
  mGeomB = StEmcGeom::instance("bemc");
  //....
  mL2EmcDb=0; // will be instantiated in InitRun
  mL2EmcGeom=0;
 
  LOG_INFO << Form("generic:init() , use: MuDst=1 (StEvent=0)=%d isMC=%d",mUseMuDst,mMCflag) <<endm;
}

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void
StGenericL2Emulator2009::make(){
#if 0 // filter some events base on L0-trigger decision, if you want  
  int L0trgSwitch=1; // flag passed to L2-algos, derived from L0 decision
  StTriggerSimuMaker *L0trgSim=(StTriggerSimuMaker *)StMaker::GetChain()->GetMaker("StarTrigSimu");
  assert(L0trgSim);
  //  L0trgSim->eemc->dsm3TRG->print();
  EMCdsm2Tree *dsm2tree=L0trgSim->eemc->dsm2TreeADC; // use response based on ADC 
  LOG_INFO<<Form("sim L0,dsm2,EEMC: EJP2bit=%d;   EEtot1bit=%d , val=%d",
		 dsm2tree->getOutEndcapJP2bit(), dsm2tree->getOutEndcapSum1bit(),dsm2tree->getIntEndcapSum())<<endm;

  if(dsm2tree->getOutEndcapJP2bit()==0)  return;

  //....... processing only events w/ EJP0 ....., just an example
  L0trgSwitch=1; // can assigne here a different value depending on L0 sim
#endif


  mTotInpEve++;

 
  if(mUseMuDst) {// pick one source of ADCs
    doBanksFromMuDst(); 
  } else {
    doBanksFromStRawData(); 
  }

  const int fakeToken1 = 1;
  const int fakeToken2 = 2;
  L2btowCalAlgo09* l2btowCal09 = dynamic_cast<L2btowCalAlgo09*>(mL2algo[0]);
  L2etowCalAlgo09* l2etowCal09 = dynamic_cast<L2etowCalAlgo09*>(mL2algo[1]);
  L2pedAlgo09* l2ped = dynamic_cast<L2pedAlgo09*>(mL2algo[2]);
  l2btowCal09->calibrateBtow(fakeToken2,mBTOW_in,mBTOW_BANK);
  l2etowCal09->calibrateEtow(fakeToken2,mETOW_in,mETOW_BANK);
  int nInpTrg = StMaker::GetChain()->GetIventNumber();
  l2ped->doPedestals(nInpTrg,(int*)mL2Result,mBTOW_in,mBTOW_BANK,mETOW_in,mETOW_BANK);
  for(size_t ia=3;ia<mL2algo.size();ia++) {//execute all instantiated L2algos 
    if(mL2algo[ia]==0) continue;
    mL2algo[ia]->compute(fakeToken2);
    mL2algo[ia]->decision(fakeToken2,mBTOW_in,mETOW_in,(int*)mL2Result);
  } // tmp, accept should be filled in internaly, in next iteration, Jan
  l2btowCal09->clear(fakeToken1);
  l2etowCal09->clear(fakeToken1);
  //  printf("L2Generic::make   BB=%d EE=%d \n",mBTOW_in,mETOW_in);

  addTriggerList();
}


//========================================
//========================================
void
StGenericL2Emulator2009::initRun1(){
  //WARN: do NOT use  runNo for any setup - it would berak for M-C

  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  assert(mydb);
  mYear=mydb->GetDateTime().GetYear();
  mYearMonthDay=mydb->GetDateTime().GetDate();
  mHourMinSec=mydb->GetDateTime().GetTime();


  //define path for L2 setup files & output
  mSetPath=Form("%sL2/%d/db/",mSetupPath.Data(),mYear);
  LOG_INFO <<"initRun1()  "<<"DB setPath="<<mSetPath.Data()<<" outPath="<<mOutPath.Data()<<endm;
  
  // read in time-dependent L2 configuration
  L2DbConfig confDB1(mSetPath+"/L2DbTime.dat");
  L2DbTime * confL2 = confDB1.getConfiguration( mYearMonthDay, mHourMinSec );
  assert( confL2 ); // trigger code has not been setup properly


  // create new L2Db instance , new per run
  if(mL2EmcDb) delete mL2EmcDb;
  mL2EmcDb=new L2EmcDb((char*)mSetPath.Data(),(char*)mOutPath.Data());
  // override default ped and mask files
  mL2EmcDb->setPedFile ( confL2->getPedFile() );
  mL2EmcDb->setMaskFile( confL2->getMaskFile() );

  // Create L2 geometry interface
  mL2EmcGeom = new L2EmcGeom;

  // access BTOW DB only re-map ADC back to rdo indexing
  StBemcTables *myTable=new StBemcTables;  
  StMaker* maker= StMaker::GetChain()->GetMaker("StarDb");
  myTable->loadTables(maker );
  // this is how BTOW mapping is accesible
  mMappB = new StEmcDecoder(mydb->GetDateTime().GetDate(),mydb->GetDateTime().GetTime());
  LOG_INFO  << "initRun1() done"<<endm;

  
} 

//========================================
//========================================
void
StGenericL2Emulator2009::initRun2(int runNo){
  assert(mL2EmcDb->initRun(runNo) == 0);
  //WARN: do NOT use  runNo for any setup - it would berak for M-C
  // read in time-dependent L2 offline trigger ID's
  enum {mxPar=10}; // maximuma for any algo, separate ints & floats
  int intsPar[mxPar]; // params passed from run control gui
  float floatsPar[mxPar]; 

  LOG_INFO  << Form("initRun2() run#=%d  begin",runNo)<<endm;
  L2DbConfig confDB2(mSetPath+"/L2TriggerIds.dat");

  for(size_t ia=0;ia<mL2algo.size();ia++) { //initialize trigger ID for given L2-algo
    // printf("uu i=%d, =%s=\n",ia,mL2algo[ia]->getName());
    if (mL2algo[ia]==0) continue;
    TString algoName=mL2algo[ia]->getName();
    L2DbTime *config = confDB2.getConfiguration(mYearMonthDay,mHourMinSec,algoName);
    if(config==0) {
      LOG_ERROR  << Form("\n************\ninitRun2() failed L2-%s  configuration for yyyy=%d hhmmss=%d,\n On explicit request from Renee, the L2 emulator will continue with disabled this particulra L2-algo.\nThis will result with false positives - ignore emulated trigger results for this algo,\nIt would be much better to fix the setup and provide missing record.\nYou have been warned,  Jan B.\n***********\n",algoName.Data(),mYearMonthDay,mHourMinSec)<<endm;
      mL2algo[ia]=0; continue;
      // assert(config); disabed to let the code go dispite missing setup, Jan
    }
    TString aa1 = config->getBuf1(); // setup name
    TString aa2 = config->getBuf2();
    Int_t trgId = atoi(aa2.Data());
    LOG_INFO<<Form("L2algo=%s=initRun2(), trigID=%d  setup=%s= ",algoName.Data(),trgId,aa1.Data())<<endm;
    
    TString fullPath=Form("%sL2/%d/algos/%s", mSetupPath.Data(), mYear,aa1.Data());
    L2VirtualAlgo2009::readParams(fullPath, mxPar, intsPar, floatsPar);// tmp, no check of # of params
    mL2algo[ia]->initRun(runNo,intsPar,floatsPar);
    mL2algo[ia]->setOflTrigID(trgId);
  }
  LOG_INFO  << "initRun2() done"<<endm;
}

//========================================

StTriggerSimuDecision
StGenericL2Emulator2009::isTrigger(int trigId) {
  if (mAcceptTriggerList.find(trigId) != mAcceptTriggerList.end()) return kYes;
  if (mVetoTriggerList.find(trigId) != mVetoTriggerList.end()) return kNo;
  return kDoNotCare;
}

//========================================
//========================================
void
StGenericL2Emulator2009::finish() {

  LOG_INFO <<"Finish()=======\n totEveSeen="<< mTotInpEve<<endm;
  
  for(size_t ia=0;ia<mL2algo.size();ia++) //execute all instantiated L2algos 
    if(mL2algo[ia]) mL2algo[ia]->finishRun(); 
  LOG_INFO <<"Finish()======= end"<<endm;

}
//========================================
//========================================
void 
StGenericL2Emulator2009::clear( ){
  mBTOW_in=mETOW_in=0;
  memset(mBTOW_BANK,0,MaxBtowRdo*sizeof(unsigned short));
  memset(mETOW_BANK,0,MaxEtowRdo*sizeof(unsigned short));
  memset(mL2Result,0,sizeof(mL2Result));
  mAcceptTriggerList.clear();
  mVetoTriggerList.clear();
}
 

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void StGenericL2Emulator2009::printBEtowers(){
  StEvent* mEvent = (StEvent*)StMaker::GetChain()-> GetInputDS("StEvent");
  assert(mEvent); // fix your chain
  StEmcCollection* emcCollection = mEvent->emcCollection();

  StEmcDetector* twB = emcCollection->detector(kBarrelEmcTowerId);
  StEmcDetector* twE = emcCollection->detector(kEndcapEmcTowerId);
  if(twE==0) {
    printf(" StGenericL2Emulator2009 found no E-EMC tower data in StEvent, skip event\n");
    return ;
  }

  int i;

  if(twB) {
    printf(" StGenericL2Emulator2009:: B_EMC Tower HITS ...\n");
    for ( i = 1; i <= (int)twB->numberOfModules(); i++) { // The B-EMC modules
      StSPtrVecEmcRawHit& emcTowerHits = twB->module(i)->hits();
      uint j;
      for ( j = 0; j < emcTowerHits.size(); j++) { 
	int adc= emcTowerHits[j]->adc();
	int mod= emcTowerHits[j]->module();
	int sub= emcTowerHits[j]->sub();
	int eta= emcTowerHits[j]->eta();
	float energy= emcTowerHits[j]->energy();
	printf("j=%d, mod=%d, sub=%d, eta=%d adc=%d ener=%f\n",j,mod,sub,eta,adc,energy);
      }
    }
  } else {
    printf("StGenericL2Emulator2009 found no B-EMC tower data in StEvent, skip event\n");
  }

  if(twE) {
    printf("StGenericL2Emulator2009:: E_EMC Tower HITS ... %d\n",twE->numberOfModules());
    for ( i = 0; i < (int)twE->numberOfModules(); i++) { // The E-EMC modules
      // printf("AAA %d\n",i);
      StEmcModule* stmod =   twE->module(i);
      if(stmod==0)	continue;
      StSPtrVecEmcRawHit& emcTowerHits = stmod->hits();
      uint j;
      for ( j = 0; j < emcTowerHits.size(); j++) { 
	int adc= emcTowerHits[j]->adc();
	int sec= emcTowerHits[j]->module()+1;
	int sub= emcTowerHits[j]->sub()+'A';
	int eta= emcTowerHits[j]->eta()+1;
	float energy= emcTowerHits[j]->energy();
	printf("j=%d, sec=%d, sub=%c, eta=%d adc=%d ener=%f\n",j,sec,sub,eta,adc,energy);
      }
    } 
  } else {
    printf("StGenericL2Emulator2009 found no E-EMC tower data in StEvent, skip event\n");
  }
  
}


//========================================
//========================================
void 
StGenericL2Emulator2009::doBanksFromStRawData(){
  assert(mUseMuDst==false);
  return; // tmp disabled, see below
  assert(1==2); // define  E/BTOW_in=1 somehow before use, JB
  
  StEvent *mEvent = (StEvent *)StMaker::GetChain()->  GetInputDS("StEvent");
  if (!mEvent) {
    LOG_ERROR<< "StGenericL2Emulator2009::getStEmcDetector() -- no StEvent found" << endm;    return ;
  }
  StEmcCollection *emcColl =  mEvent->emcCollection();
  if (!emcColl) {
    return ;
  }

  int icr;
 
 //.................BTOW  is simple ........  
  StEmcRawData *rawB = mEvent->emcCollection()->bemcRawData();
  assert(rawB);
  icr=0;
  printf(" BTOW   size=%d\n",rawB->sizeData(icr));
  assert(rawB->sizeData(icr) <=MaxBtowRdo);
  unsigned short* adc=rawB->data(icr);
  int i;
  // copy BTOW ADCs 1:1
  for(i=0;i<rawB->sizeData(icr);i++) mBTOW_BANK[i]=adc[i];

  //...........ETOW : transpose crates & channals back to oryginal daq format
  StEmcRawData *rawE = mEvent->emcCollection()->eemcRawData();
  assert(rawE);
  assert(rawE->sizeData(icr) <=MaxEtowRdo);

  for(icr=0;icr<mDbE->getNFiber();icr++) {
    const EEmcDbCrate *fiber=mDbE->getFiber(icr);
    if(fiber->crID>6) continue; // drop non-tower crates
    if(rawE->sizeHeader(icr)<=0) continue; // unused fibers
    assert(fiber->useIt); // code not ready to handle masked crates
    printf(" ETOW  crID=%d type=%c size=%d\n",fiber->crID,fiber->type,rawE->sizeData(icr));
    assert(fiber->crID==icr+1);
    unsigned short* adc=rawE->data(icr);
    int i;
    for(i=0;i<rawE->sizeData(icr);i++) {
      int rdo=icr + i*L2EmcDb::ETOW_MAXFEE;
      assert(rdo>=0 && rdo<MaxEtowRdo); // siher ist siher
      mETOW_BANK[rdo]=adc[i];
    }
    //  for(i=0;i<30;i++) printf("ch=%d adc=%d\n",i,adc[i]);//test
  }

}






//========================================
//========================================
void 
StGenericL2Emulator2009::doBanksFromMuDst(){

  assert(mUseMuDst==true);

  StEvent *mEvent = (StEvent*)StMaker::GetChain()-> GetInputDS("StEvent");
  assert(mEvent);
     
  StMuDstMaker *muMk = (StMuDstMaker*)StMaker::GetChain()->GetMaker("MuDst");
  assert(muMk);  

  StMuEmcCollection* muEmc = muMk->muDst()->muEmcCollection();
  
  //.........................  E T O W   ....................
  int nE=0;
  int i;
  for (i=0; i < muEmc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,rawAdc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    muEmc->getEndcapTowerADC(i,rawAdc,sec,sub,eta);
    assert(sec>0 && sec<=MaxSectors);// total corruption of muDst
    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=mDbE->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // DB mapping must be provided for all channels
    assert(x->crate>0);
    assert(x->crate<=6);
    int rdo=x->crate-1 + x->chan*L2EmcDb::ETOW_MAXFEE;
    mETOW_BANK[rdo]=rawAdc;
    nE++;
  }
  mETOW_in=1; 
  LOG_DEBUG << Form("doBanksFromMuDst() , ETOW nAdc=%d",nE)<<endm;  assert(nE==720);
  
  
  //.........................  B T O W   ....................
  //use StEvent as default  to get simulation right in BEMC
  if( mEvent) 
    {  
    StEmcCollection *emc = mEvent->emcCollection();
    if (emc)
      {
      StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
      if(detector) 
	{
	for(Int_t m = 1; m <= 120; ++m) 
	  {
	  StEmcModule* module = detector->module(m);
	  if(module) 
	    {
	    StSPtrVecEmcRawHit& rawHit=module->hits();
	    for(UInt_t k = 0; k < rawHit.size(); ++k) 
	      {
	      if(rawHit[k]) 
		{
		  int did, RDO;
		  
		  Int_t m=rawHit[k]->module();
		  Int_t e=rawHit[k]->eta();
		  Int_t s=abs(rawHit[k]->sub());
		  Int_t adc=rawHit[k]->adc();
		  
		  //Get software tower id to get DaqID
		  mGeomB->getId(m,e,s,did);
		  mMappB->GetDaqIdFromTowerId(did,RDO);
		  mBTOW_BANK[RDO]=adc;
		}
	      }
	    }
	  }
	}
      }
    mBTOW_in=1;
    }   
  else
    {
      
      int nB=0;
      int id;
      for (id=1; id <=4800 ; id++) 
	{
	  int rawAdc= muEmc->getTowerADC(id);
	  int RDO;
	  assert(mMappB->GetDaqIdFromTowerId(id,RDO)==1);// is good range
	  mBTOW_BANK[RDO]=rawAdc;
	  nB++;
	}
      
      mBTOW_in=1; // tmp, it should detectd there is no BTOW data
      /*The easiest way in muEmcCollection is
	getCrateStatus(int crate, int detector = bemc)
	This is just a placehold so there is no methods to check stuff. 
	Alex
      */
      
      LOG_INFO << Form("doBanksFromMuDst() , BTOW nAdc=%d",nB)<<endm;
      assert(nB==4800);
    
    }

}

//========================================
//========================================
void 
StGenericL2Emulator2009::printBEblocks(){
  int i;

  printf("printBEblocks(), just begin & end of each block, mBTOW_in=%d mETOW_in=%d\n",mBTOW_in,mETOW_in);

  if(mBTOW_in) {
    for(i=0;i<10;i++) printf("BTOWi=%d ADC=%d\n",i,mBTOW_BANK[i]);
    for(i=4790;i<4800;i++) printf("BTOWi=%d ADC=%d\n",i,mBTOW_BANK[i]);
  }

 if( mETOW_in){
   for(i=0;i<10;i++) printf("ETOWi=%d ADC=%d\n",i,mETOW_BANK[i]);
   for(i=710;i<720;i++) printf("ETOWi=%d ADC=%d\n",i,mETOW_BANK[i]);
 }
}



//========================================
void
StGenericL2Emulator2009::addTriggerList() {
 
  for(size_t ia=0;ia<mL2algo.size();ia++) {
    if (mL2algo[ia]==0) continue;
    if (mL2algo[ia]->getOflTrigID()==0) continue; // undefined triggerID
    if (mL2algo[ia]->isAccepted()) {
      mAcceptTriggerList.insert(mL2algo[ia]->getOflTrigID());
    }
    else {
      mVetoTriggerList.insert(mL2algo[ia]->getOflTrigID());
    }
  }

  LOG_DEBUG  << Form("addTriggerList() yesSize=%d vetoSize=%d",mAcceptTriggerList.size(),mVetoTriggerList.size())<<endm;
}
