// *-- Author : J.Balewski, R.Fatemi
// 
// $Id: StGenericL2Emulator.cxx,v 1.5 2007/11/08 21:29:09 balewski Exp $

#include "StChain.h"
#include "St_DataSetIter.h"

#include <Stiostream.h>
#include <math.h>

#include "TFile.h"
#include "TArrayF.h"

#include <StMessMgr.h>

#include "StGenericL2Emulator.h"

// ETOW stuff
#include <StEEmcDbMaker/StEEmcDbMaker.h>
#include <StEEmcDbMaker/EEmcDbItem.h>
#include <StEEmcDbMaker/EEmcDbCrate.h>


// BTOW stuff
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StDaqLib/EMC/StEmcDecoder.h"

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
#include "L2algoUtil/L2VirtualAlgo.h"
#include "L2algoUtil/L2DbConfig.h"  // time-dep config
#include "L2algoUtil/L2DbTime.h"  // time-dep config

//trg-data from ezTree, to read on-line decision, tmp
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h" // to access DB time stamp
#include "St_db_Maker/St_db_Maker.h"

// L0-trigSimu
#include "StTriggerSimuMaker.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StEEmcUtil/EEdsm/EMCdsm2Tree.h"

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
  mDbE= (StEEmcDbMaker*) StMaker::GetChain()-> GetMaker("eemcDb");
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
  for(ia=0;ia<mL2algoN;ia++) //execute all instantiated L2algos 
    if(mL2algo[ia]) mL2algo[ia]-> doEvent(L0trgSwitch, mTotInpEve, (TrgDataType*)mTrigData,mBTOW_in, mBTOW_BANK, mETOW_in, mETOW_BANK);
  

  printf("gen i   BB=%d EE=%d \n",mBTOW_in,mETOW_in);
  
 return;
}


//========================================
//========================================
void
StGenericL2Emulator::initRun(){
  //WARN: do NOT use  runNo for any setup - it woul dberak for M-C

  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  assert(mydb);
  mYear=mydb->GetDateTime().GetYear();
  mYearMonthDay=mydb->GetDateTime().GetDate();
  mHourMinSec=mydb->GetDateTime().GetTime();

  assert(mYearMonthDay>=20060410);
  // add other reference runs for later time stamps as appropriate
  assert(mYearMonthDay<20060700);


  //define path for L2 setup files & output

  char setPath[1000];
  sprintf(setPath,"%sL2/%d/db/",mSetupPath.Data(),mYear);
  LOG_INFO <<"InitRun()  "<<"DB setPath="<<setPath<<" outPath="<<mOutPath<<endm;
 
  // read in time-dependent configuration
  L2DbConfig config(setPath);
  L2DbTime * myconfig = config.getConfiguration( mYearMonthDay, mHourMinSec );
  assert( myconfig ); // trigger code has not been setup properly


  // create new L2Db instance , new per run
  if(mL2EmcDb) delete mL2EmcDb;
  mL2EmcDb=new L2EmcDb(setPath,(char*)mOutPath.Data());
  // override default ped and mask files
  mL2EmcDb->setPedFile( myconfig->getPedFile() );
  mL2EmcDb->setMaskFile( myconfig->getMaskFile() );



  // access BTOW DB only re-map ADC back to rdo indexing
  StBemcTables *myTable=new StBemcTables;  
  StMaker* maker= StMaker::GetChain()->GetMaker("StarDb");
  myTable->loadTables(maker );
  // this is how BTOW mapping is accesible
  mMappB = new StEmcDecoder(mydb->GetDateTime().GetDate(),mydb->GetDateTime().GetTime());

  LOG_INFO  << "StGenericL2Emulator::InitRun() done"<<endm;

  
} 

//========================================
    
bool 
StGenericL2Emulator::isTrigger(int trigId) {
  uint j;
  for(j=0; j<mTriggerList.size();j++) {
    //  printf("aa j=%d,  %d %d  ret=%d\n",j, trigId, mTriggerList[j],trigId==mTriggerList[j]);
    if(trigId==mTriggerList[j]) return true;
  }
  return false;
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
  mTriggerList.clear();
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
	//  printf("bbb=%d\n",j);
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

  StMuDstMaker *muMk = (StMuDstMaker*)StMaker::GetChain()->GetMaker("MuDst");
  assert(muMk);

  StMuEmcCollection* emc = muMk->muDst()->muEmcCollection();
  
  //.........................  B T O W   ....................
  int nB=0;
  int id;
  for (id=1; id <=4800 ; id++) {
    int rawAdc= emc->getTowerADC(id);
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

  int nE=0;
  //.........................  E T O W   ....................
  int i;
  for (i=0; i < emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,rawAdc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,rawAdc,sec,sub,eta);
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
  LOG_INFO << Form("doBanksFromMuDst() , ETOW nAdc=%d",nE)<<endm;  assert(nE==720);
  
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


// $Log: StGenericL2Emulator.cxx,v $
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






