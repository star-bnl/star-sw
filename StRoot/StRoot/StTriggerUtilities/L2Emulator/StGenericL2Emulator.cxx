// *-- Author : J.Balewski, R.Fatemi
// 
// $Id: StGenericL2Emulator.cxx,v 1.18 2009/10/12 18:04:36 pibero Exp $

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

//trg stuff
#include "StDaqLib/TRG/trgStructures.h"

//L2 stuff
#include "L2algoUtil/L2EmcDb.h"
#include "L2algoUtil/L2DbConfig.h"  // time-dep config
#include "L2algoUtil/L2DbTime.h"  // time-dep config

//trg-data from ezTree, to read on-line decision, tmp
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h" // to access DB time stamp
#include "St_db_Maker/St_db_Maker.h"

// L0-trigSimu
#include "StTriggerSimuMaker.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StTriggerUtilities/Eemc/EMCdsm2Tree.h"

#include "StGenericL2Emulator.h"

  /* usefull dimensions */
#define MaxBtowRdo (L2EmcDb::BTOW_MAXFEE*L2EmcDb::BTOW_DATSIZE)
#define MaxEtowRdo (L2EmcDb::ETOW_MAXFEE*L2EmcDb::ETOW_DATSIZE)

ClassImp(StGenericL2Emulator)

StGenericL2Emulator::StGenericL2Emulator(){
  mBTOW_BANK =new  unsigned short [MaxBtowRdo];
  mETOW_BANK =new  unsigned short [MaxEtowRdo];
  mTrigData = new  TrgDataType; //note it is _local_ container to store L2Results - it has nothing in common with the same type container filled during data taking - do not mix them up -JB
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

StGenericL2Emulator::~StGenericL2Emulator(){
  delete [] mBTOW_BANK;
  delete [] mETOW_BANK;

}




//________________________________________________________
//________________________________________________________

void StGenericL2Emulator::init(){
  mTotInpEve=0;   
  //................EEMC stuff ..............
  mDbE = (StEEmcDb*)StMaker::GetChain()->GetDataSet("StEEmcDb");
  assert(mDbE);
  mGeomB = StEmcGeom::instance("bemc");
  //....
  mL2EmcDb=0; // will be instantiated in InitRun
 
  LOG_INFO << Form("generic:init() , use: MuDst=1 (StEvent=0)=%d isMC=%d",mUseMuDst,mMCflag) <<endm;
}

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void
StGenericL2Emulator::make(){
  int L0trgSwitch=1; // flag passed to L2-algos, derived from L0 decision
#if 0 // filter some events base on L0-trigger decision, if you want  
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

  
  int ia;
  for(ia=0;ia<mL2algoN;ia++) {//execute all instantiated L2algos 
    if(mL2algo[ia]==0) continue;
    mL2algo[ia]-> doEvent(L0trgSwitch, mTotInpEve, (TrgDataType*)mTrigData,mBTOW_in, mBTOW_BANK, mETOW_in, mETOW_BANK);
  } // tmp, accept should be filled in internaly, in next iteration, Jan
  
  //  printf("L2Generic::make   BB=%d EE=%d \n",mBTOW_in,mETOW_in);

  addTriggerList(); 

 return;
}


//========================================
//========================================
void
StGenericL2Emulator::initRun1(){
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
StGenericL2Emulator::initRun2(int runNo){
  //WARN: do NOT use  runNo for any setup - it would berak for M-C
  // read in time-dependent L2 offline trigger ID's
  enum {mxPar=10}; // maximuma for any algo, separate ints & floats
  int intsPar[mxPar]; // params passed from run control gui
  float floatsPar[mxPar]; 

  LOG_INFO  << Form("initRun2() run#=%d  begin",runNo)<<endm;
  L2DbConfig confDB2(mSetPath+"/L2TriggerIds.dat");

  int ia;
  for(ia=0;ia<mL2algoN;ia++) { //initialize trigger ID for given L2-algo
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
    L2VirtualAlgo::readParams(fullPath, mxPar, intsPar, floatsPar);// tmp, no check of # of params
    assert(mL2algo[ia]->initRun( runNo,intsPar,floatsPar)==0);
    mL2algo[ia]->setOflTrigID(trgId);
  }
  LOG_INFO  << "initRun2() done"<<endm;
}

//========================================

StTriggerSimuDecision
StGenericL2Emulator::isTrigger(int trigId) {
  uint j;
  for(j=0; j<mAcceptTriggerList.size();j++) {
    if(trigId==mAcceptTriggerList[j]) return kYes; 
  }
  for(j=0; j<mVetoTriggerList.size();j++) {
    if(trigId==mVetoTriggerList[j]) return kNo; 
  }
  return kDoNotCare;
}

//========================================
//========================================
void
StGenericL2Emulator::finish() {

  LOG_INFO <<"Finish()=======\n totEveSeen="<< mTotInpEve<<endm;
  
  int ia;
  for(ia=0;ia<mL2algoN;ia++) //execute all instantiated L2algos 
    if(mL2algo[ia]) mL2algo[ia]->finishRun(); 
  LOG_INFO <<"Finish()======= end"<<endm;

}
//========================================
//========================================
void 
StGenericL2Emulator::clear( ){
  mBTOW_in=mETOW_in=0;
  memset(mBTOW_BANK,0,MaxBtowRdo*sizeof(unsigned short));
  memset(mETOW_BANK,0,MaxEtowRdo*sizeof(unsigned short));
  memset(mTrigData,0,sizeof(TrgDataType));
  mAcceptTriggerList.clear();
  mVetoTriggerList.clear();
}
 

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void StGenericL2Emulator::printBEtowers(){
  StEvent* mEvent = (StEvent*)StMaker::GetChain()-> GetInputDS("StEvent");
  assert(mEvent); // fix your chain
  StEmcCollection* emcCollection = mEvent->emcCollection();

  StEmcDetector* twB = emcCollection->detector(kBarrelEmcTowerId);
  StEmcDetector* twE = emcCollection->detector(kEndcapEmcTowerId);
  if(twE==0) {
    printf(" StGenericL2Emulator found no E-EMC tower data in StEvent, skip event\n");
    return ;
  }

  int i;

  if(twB) {
    printf(" StGenericL2Emulator:: B_EMC Tower HITS ...\n");
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
    printf("StGenericL2Emulator found no B-EMC tower data in StEvent, skip event\n");
  }

  if(twE) {
    printf("StGenericL2Emulator:: E_EMC Tower HITS ... %d\n",twE->numberOfModules());
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
    printf("StGenericL2Emulator found no E-EMC tower data in StEvent, skip event\n");
  }
  
}


//========================================
//========================================
void 
StGenericL2Emulator::doBanksFromStRawData(){
  assert(mUseMuDst==false);
  return; // tmp disabled, see below
  assert(1==2); // define  E/BTOW_in=1 somehow before use, JB
  
  StEvent *mEvent = (StEvent *)StMaker::GetChain()->  GetInputDS("StEvent");
  if (!mEvent) {
    LOG_ERROR<< "StGenericL2Emulator::getStEmcDetector() -- no StEvent found" << endm;    return ;
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
StGenericL2Emulator::doBanksFromMuDst(){

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
StGenericL2Emulator::printBEblocks(){
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
StGenericL2Emulator::addTriggerList() {
 
 int ia;
  for(ia=0;ia<mL2algoN;ia++) {
    if (mL2algo[ia]==0) continue;
    if (mL2algo[ia]->getOflTrigID()==0) continue; // undefined triggerID
    if (mL2algo[ia]->accepted()) 
      mAcceptTriggerList.push_back(mL2algo[ia]->getOflTrigID());
    else
      mVetoTriggerList.push_back(mL2algo[ia]->getOflTrigID());
  }

  LOG_DEBUG  << Form("addTriggerList() yesSize=%d vetoSize=%d",mAcceptTriggerList.size(),mVetoTriggerList.size())<<endm;
}

const unsigned int * StGenericL2Emulator::result() const {
    return ( (TrgDataType*)mTrigData)->TrgSum.L2Result;
}

// $Log: StGenericL2Emulator.cxx,v $
// Revision 1.18  2009/10/12 18:04:36  pibero
// Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
//
// Revision 1.17  2009/02/04 20:26:22  rfatemi
// Update include for StEmcDecoder as well as access to StEEmc
//
// Revision 1.16  2008/10/14 00:53:12  balewski
// allow production of bin.eve files from 2008 pp data to be used by L2-algos
//
// Revision 1.15  2008/01/23 16:22:26  balewski
// make event loop silent
//
// Revision 1.14  2008/01/23 02:52:23  balewski
// allow L2-algo to be disabled in initRun if setup time stamp is wrong.
//
// Revision 1.13  2008/01/17 01:56:52  kocolosk
// export 128-byte emulated L2Result
//
// Revision 1.12  2007/12/15 00:59:06  balewski
// protect against unforeseen time stamp
//
// Revision 1.11  2007/12/11 16:39:40  rfatemi
// Fixed Bug in StGenericL2Emulator
//
// Revision 1.10  2007/12/09 15:56:28  rfatemi
// Allow BEMC to get ADC from StEvent instead of MuDst
//
// Revision 1.9  2007/11/19 22:18:16  balewski
// most L2algos provide triggerID's
//
// Revision 1.8  2007/11/18 21:58:53  balewski
// L2algos triggerId list fixed
//
// Revision 1.7  2007/11/13 23:05:59  balewski
// toward more unified L2-algos
//
// Revision 1.6  2007/11/13 00:12:26  balewski
// added offline triggerID, take1
//
// Revision 1.5  2007/11/08 21:29:09  balewski
// now L2emu runs on M-C
//
// Revision 1.4  2007/11/06 22:07:20  balewski
// added timeStamp controlled L2 setup from Jason
//
// Revision 1.3  2007/10/25 02:06:54  balewski
// added L2upsilon & binary event dump
//
// Revision 1.2  2007/10/23 02:47:10  balewski
// cleanup
//
// Revision 1.1  2007/10/22 23:09:58  balewski
// split L2 to generic and year specific, not finished
//
// Revision 1.2  2007/10/12 20:11:50  balewski
// cleanu setup , output path
//
// Revision 1.1  2007/10/11 00:33:09  balewski
// L2algo added
//
// Revision 1.5  2003/09/02 17:57:55  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/07/18 18:31:46  perev
// test for nonexistance of XXXReader added
//
// Revision 1.3  2003/04/30 20:36:37  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.2  2003/02/14 00:04:31  balewski
// remove few printouts
//
// Revision 1.1  2003/01/28 23:13:00  balewski
// star
//






