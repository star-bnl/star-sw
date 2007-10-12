// *-- Author : J.Balewski, R.Fatemi
// 
// $Id: StL2EmulatorMaker.cxx,v 1.2 2007/10/12 20:11:50 balewski Exp $

#include "StChain.h"
#include "St_DataSetIter.h"

#include <Stiostream.h>
#include <math.h>
#include "TFile.h"
#include "TArrayF.h"

#include <StMessMgr.h>

#include "StL2EmulatorMaker.h"

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
#include "StTriggerData2005.h"
#include "StDaqLib/TRG/L2jetResults2006.h"
#include "StDaqLib/TRG/trgStructures2005.h"
#include "StDaqLib/TRG/trgStructures.h"

//L2 stuff
#include "L2algoUtil/L2EmcDb.h"
#include "L2jetAlgo/L2jetAlgo.h"
#include "L2pedAlgo/L2pedAlgo.h"
//#include "L2gammaAlgo/L2gammaAlgo.h"

//trg-data from ezTree, to read on-line decision, tmp
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h" // to access DB time stamp
#include "St_db_Maker/St_db_Maker.h"

// L0-trigSimu
#include "StTriggerSimuMaker.h"
#include "StTriggerUtilities/Eemc/StEemcTriggerSimu.h"
#include "StEEmcUtil/EEdsm/EMCdsm2Tree.h"

ClassImp(StL2EmulatorMaker)

StL2EmulatorMaker::StL2EmulatorMaker(const char *name):StMaker(name){
  mBTOW_BANK =new  unsigned short [MaxBtowRdo];
  mETOW_BANK =new  unsigned short [MaxEtowRdo];
  mTrigData = new  TrgDataType; //note it is _local_ container to store L2Results - it has nothing in common with the same type container filled during data taking - do not mix them up -JB
  mUseMuDst=true;
  mMCflag=false;

  mL2pedAlgo=0;
  mL2jetAlgo=0;
  mSetupPath="wrong2";
  mOutPath="wrong3";
}

//________________________________________________________
//________________________________________________________

StL2EmulatorMaker::~StL2EmulatorMaker(){
  delete [] mBTOW_BANK;
  delete [] mETOW_BANK;

}


//========================================
//========================================
void 
StL2EmulatorMaker::setupL2Algos2006(int yyyymmdd, int runNo){
 
  LOG_INFO << Form(" %s::setupL2Algos2006(), dbDate=%d  ",GetName(), yyyymmdd)<<endm;

  mL2algoN=2; // total # of L2 algos
  mL2algo =new L2VirtualAlgo *[mL2algoN]; // not cleared memeory leak
  memset(mL2algo,0,mL2algoN*sizeof(void*));

  //setup evry algo one by one, params may be time dependent
  int ints[5]; // params passed from run control gui
  float floats[5]; // 
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
  assert( yyyymmdd>20060316); // before L2jet was not used
  assert( yyyymmdd<20060620); // after L2jet was not used
  if( yyyymmdd<20060406) { // ppLong-1 period not implementd
    assert(1==2);
  } else if (  yyyymmdd<200605011) { // ppTrans
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

}


//________________________________________________________
//________________________________________________________

Int_t StL2EmulatorMaker::Init(){
  mTotInpEve=0;   
  //................EEMC stuff ..............
  mDbE= (StEEmcDbMaker*) GetMaker("eemcDb");
  assert(mDbE);
  mGeomB = StEmcGeom::instance("bemc");
  //....
  mL2EmcDb=0; // will be instantiated in InitRun
  mYear=-1;
 
  // initAuxHisto();
  LOG_INFO << Form(" %s::Init() , use: MuDst=1 (StEvent=0)=%d isMC=%d",GetName(),mUseMuDst,mMCflag) <<endm;
 return StMaker::Init();
}

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
Int_t 
StL2EmulatorMaker::Make(){
  int L0trgSwitch=1; // flag passed to L2-algos, derived from L0 decision

#if 1 // filter some events base on L0-trigger decision, if you want  
  StTriggerSimuMaker *L0trgSim=(StTriggerSimuMaker *)StMaker::GetChain()->GetMaker("StarTrigSimu");
  assert(L0trgSim);
  //  L0trgSim->eemc->dsm3TRG->print();
  EMCdsm2Tree *dsm2tree=L0trgSim->eemc->dsm2TreeADC; // use response based on ADC 
  LOG_INFO<<Form("sim L0,dsm2,EEMC: EJP2bit=%d;   EEtot1bit=%d , val=%d",
		 dsm2tree->getOutEndcapJP2bit(), dsm2tree->getOutEndcapSum1bit(),dsm2tree->getIntEndcapSum())<<endm;

  if(dsm2tree->getOutEndcapJP2bit()==0)  return kStOK;

  //....... processing only events w/ EJP0 ....., just an example
  L0trgSwitch=1; // can assigne here a different value depending on L0 sim
#endif


  mTotInpEve++;

  if( mMCflag==0) getTriggerData(); // for monitoring only

  if(mUseMuDst) {// pick one source of ADCs
    doBanksFromMuDst(); 
  } else {
    doBanksFromStRawData(); 
  }

  
  int ia;
  for(ia=0;ia<mL2algoN;ia++) //execute all instantiated L2algos 
    if(mL2algo[ia]) mL2algo[ia]-> doEvent(L0trgSwitch, mTotInpEve, (TrgDataType*)mTrigData,mBTOW_in, mBTOW_BANK, mETOW_in, mETOW_BANK);
  

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
//========================================
Int_t 
StL2EmulatorMaker::InitRun(int runNo){
  //WARN: do NOT use  runNo for any setup - it woul dberak for M-C

  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  assert(mydb);
  mYear=mydb->GetDateTime().GetYear();
  int yyyymmdd=mydb->GetDateTime().GetDate();


  assert(yyyymmdd>=20060410);
  int refRun=710052;
  // add other reference runs for later time stamps as appropriate
  assert(yyyymmdd<20060700);


  //define path for L2 setup files & output

  char setPath[1000];
  sprintf(setPath,"%sL2/%d/R%d/",mSetupPath.Data(),mYear,refRun);

  LOG_INFO << GetName()<<"::InitRun()  run=" <<runNo<<" setPath="<<setPath<<" outPath="<<mOutPath<<endm;
 
  StBemcTables *myTable=new StBemcTables;
  
  StMaker* maker=this;
  myTable->loadTables(maker );

  // this is how BTOW mapping is accesible
  mMappB = new StEmcDecoder(mydb->GetDateTime().GetDate(),mydb->GetDateTime().GetTime());

  // create new L2Db instance for each run
  if(mL2EmcDb) delete mL2EmcDb;
  mL2EmcDb=new L2EmcDb(setPath,(char*)mOutPath.Data());
  
  LOG_INFO << Form(" %s::setupL2Algos(), dbDate=%d",GetName(),yyyymmdd)<<endm;
  //WARN: do NOT use run# to controll setup of L2-algos

  /* at this moment only 2006 L2 algos are implemented
     add new setupL@AlgosYYYY for 2008, use if/else clause to pick the right one
   */
  
  assert(yyyymmdd>20060000); 
  assert(yyyymmdd<20060700);
  setupL2Algos2006(yyyymmdd,runNo); 

  LOG_INFO  << "StL2JetEmulMaker::InitRun() done, run=" <<runNo<<endm;

  return kStOk;
} 

//========================================
void
StL2EmulatorMaker::addTriggerList() {// based on emulated L2Result[..]
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
    
bool 
StL2EmulatorMaker::isTrigger(int trigId) {
  uint j;
  for(j=0; j<mTriggerList.size();j++) {
    //  printf("aa j=%d,  %d %d  ret=%d\n",j, trigId, mTriggerList[j],trigId==mTriggerList[j]);
    if(trigId==mTriggerList[j]) return true;
  }
  return false;
}

//========================================
//========================================
Int_t 
StL2EmulatorMaker::Finish() {

  LOG_INFO << GetName()<<"::Finish()=======\n totEveSeen="<< mTotInpEve<<endm;
  
  int ia;
  for(ia=0;ia<mL2algoN;ia++) //execute all instantiated L2algos 
    if(mL2algo[ia]) mL2algo[ia]->finishRun(); 
  LOG_INFO << GetName()<<"::Finish()======= end"<<endm;
  return kStOk;
}
//========================================
//========================================
void 
StL2EmulatorMaker::Clear(const Option_t* ){
  mBTOW_in=mETOW_in=0;
  memset(mBTOW_BANK,0,MaxBtowRdo*sizeof(unsigned short));
  memset(mETOW_BANK,0,MaxEtowRdo*sizeof(unsigned short));
  memset(mTrigData,0,sizeof(TrgDataType));
  mTriggerList.clear();
}
 

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void StL2EmulatorMaker::printBEtowers(){
  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent); // fix your chain
  StEmcCollection* emcCollection = mEvent->emcCollection();

  StEmcDetector* twB = emcCollection->detector(kBarrelEmcTowerId);
  StEmcDetector* twE = emcCollection->detector(kEndcapEmcTowerId);
  if(twE==0) {
    printf("%s found no E-EMC tower data in StEvent, skip event\n",GetName());
    return ;
  }

  int i;

  if(twB) {
    printf("%s:: B_EMC Tower HITS ...\n",GetName());
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
    printf("%s found no B-EMC tower data in StEvent, skip event\n",GetName());
  }

  if(twE) {
    printf("%s:: E_EMC Tower HITS ... %d\n",GetName(),twE->numberOfModules());
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
    printf("%s found no E-EMC tower data in StEvent, skip event\n",GetName());
  }
  
}

//========================================
//========================================
bool 
StL2EmulatorMaker::getTriggerData(){
  const StTriggerId *L1=0;
  //play with trigID

  int runId=0;
  int l2jetOff=0;
  if(mYear==2006) l2jetOff=L2RESULTS_OFFSET_DIJET;

  const unsigned int *l2res=0;  

  if(mUseMuDst) {
    StMuDstMaker *muMk = (StMuDstMaker*)GetMaker("MuDst");
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
    StEvent *mEvent = (StEvent *) GetInputDS("StEvent");
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



//========================================
//========================================
void 
StL2EmulatorMaker::doBanksFromStRawData(){
  assert(mUseMuDst==false);
  return; // tmp disabled, see below
  assert(1==2); // define  E/BTOW_in=1 somehow before use, JB
  
  StEvent *mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) {
    cerr << "StL2JetEmulMaker::getStEmcDetector() -- no StEvent found" << endl;    return ;
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
StL2EmulatorMaker::doBanksFromMuDst(){
  assert(mUseMuDst==true);

  StMuDstMaker *muMk = (StMuDstMaker*)GetMaker("MuDst");
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
  LOG_INFO << Form(" %s::doBanksFromMuDst() , BTOW nAdc=%d",GetName(),nB)<<endm;
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
  LOG_INFO << Form(" %s::doBanksFromMuDst() , ETOW nAdc=%d",GetName(),nE)<<endm;  assert(nE==720);
  
}

//========================================
//========================================
void 
StL2EmulatorMaker::printBEblocks(){
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


// $Log: StL2EmulatorMaker.cxx,v $
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






