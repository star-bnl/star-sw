#include "StBemcTriggerSimu.h"

//General
#include <TH2.h>
#include <StMessMgr.h>

//Bemc
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/StEmcRawMaker.h"
#include "StBemcTriggerDbThresholds.h"

//StEvent
#include "St_DataSetIter.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StEmcTriggerDetector.h"
#include "StEvent/StL0Trigger.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//Db
#include "St_db_Maker/St_db_Maker.h"
  

ClassImp(StBemcTriggerSimu);

//==================================================

StBemcTriggerSimu::StBemcTriggerSimu() {
  
  LOG_INFO<<"StBemcTriggerSimu::constructor"<<endm; 

  mMuDstMaker=new StMuDstMaker();
  muDst=new StMuDst();
  mEvent=new StEvent();
  mDecoder = new StEmcDecoder();
  mDbThres = new StBemcTriggerDbThresholds();
  mGeo=StEmcGeom::getEmcGeom("bemc");
  
  mHList=0;
  
}

//==================================================
StBemcTriggerSimu::~StBemcTriggerSimu(){ 

  LOG_INFO<<"StBemcTriggerSimu::~StBemcTriggerSimu()"<<endl;

}


//==================================================
//==================================================
void StBemcTriggerSimu::Init(){
 
  LOG_INFO <<"StBemcTriggerSimu::Init()"<<endm;

}


 
//==================================================
//==================================================
void StBemcTriggerSimu::InitRun(int runnumber){
 
  LOG_INFO<<"StBemcTriggerSimu::InitRun()"<<endm;

  assert(starDb);
  getTowerStatus();
  getDSM_TPStatus();
  getDSM_HTStatus();
  getLUT();
  getPed();

  //Get FEE window for HT from support class
  //Replaced this with Db call in getPed()
  int yyyy=starDb->GetDateTime().GetYear();  
  HT_FEE_Offset=mDbThres->GetHtFEEbitOffset(yyyy);

}


//==================================================
//==================================================
void  
StBemcTriggerSimu::Clear(){
  
  LOG_INFO <<"StBemcTriggerSimu::Clear()"<<endm;

  for (did=1; did<=kNTowers; did++){
    adc08[did-1]=0;
    adc10[did-1]=0;
    adc12[did-1]=0;
  }
  
  for (tpid=0;tpid<kNPatches; tpid++){
    L0_HT_ADC[tpid]=0;
    L0_TP_ADC[tpid]=0;
    L0_TP_PED[tpid]=0;
    HTadc06[tpid]=0;
  }

}

  
//==================================================
//==================================================
void  
StBemcTriggerSimu::addTriggerList( void * adr){

}

//==================================================
//==================================================
short
StBemcTriggerSimu::isTrigger(int trigId) {
    return -1;
}
//==================================================
//==================================================
void 
StBemcTriggerSimu::getTowerStatus(){
  
  LOG_INFO<<" StBemcTriggerSimu::getTowerStatus()"<<endm;
  
  for (int i=0;i<kNTowers;i++) TowerStatus[i]=1;

  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerStatus *STATUSonline=(St_emcTriggerStatus*) dbOnline->Find("bemcTriggerStatus");
  emcTriggerStatus_st *STATUStab=STATUSonline->GetTable();
  if (config->Contains("online")) {
    for (int cr=0; cr < kNCrates; cr++){
      for (int ch=0; ch < kNSeq; ch++){
	mDecoder->GetTowerIdFromCrate(cr,ch,did);
	TowerStatus[did-1]=STATUStab->TowerStatus[cr][ch];
      }
    }
  }

  if (config->Contains("offline")){
    for (int did=1; did<=kNTowers; did++){
      mTables->getStatus(BTOW, did, TowerStatus[did-1]);
    }
  }
  
  if (config->Contains("expert")){
    for (int did=1; did<=kNTowers; did++){
      TowerStatus[did-1]=1;
    }
  }
}

//==================================================
//==================================================
void 
StBemcTriggerSimu::getDSM_TPStatus(){

  LOG_INFO<<" StBemcTriggerSimu::getDSM_TPStatus()"<<endm;

  for (int i=0;i<kNPatches;i++) DSM_TPStatus[i]=1;
   
  //for online config TP status is set by DB
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerStatus *STATUSonline=(St_emcTriggerStatus*) dbOnline->Find("bemcTriggerStatus");
  emcTriggerStatus_st *STATUStab=STATUSonline->GetTable();
  if (config->Contains("online")){
    for (int i=0;i<kNPatches;i++){
      DSM_TPStatus[i]=STATUStab->PatchStatus[i];
    }
  }
 
 //for offline config all TP status is good by definition. 
  if (config->Contains("offline")) {
    for (int i=0;i<kNPatches;i++){
      DSM_TPStatus[i]=1;
    }
  }
 
  //experts do as you will but set good by definition
  if (config->Contains("expert")){
    for (int i=0;i<kNPatches;i++){
      DSM_TPStatus[i]=1;
    }
  }

}

//==================================================
//==================================================
void 
StBemcTriggerSimu::getDSM_HTStatus(){
  
  LOG_INFO<<" StBemcTriggerSimu::getDSM_HTStatus()"<<endm;

  for (int i=0;i<kNPatches;i++) DSM_HTStatus[i]=1;

  //Online get DSM HT status from db
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerStatus *STATUSonline=(St_emcTriggerStatus*) dbOnline->Find("bemcTriggerStatus");
  emcTriggerStatus_st *STATUStab=STATUSonline->GetTable();
  if (config->Contains("online")){
    for (int i=0;i<kNPatches;i++){
      DSM_HTStatus[i]=STATUStab->HighTowerStatus[i];
    }
  }
  
 //Offline all DSM HT status are good
  if (config->Contains("offline")){
    for (int i=0; i<kNPatches; i++){
      DSM_HTStatus[i]=1;
    }
  }
 
  if (config->Contains("expert")){
    for (int i=0;i<kNPatches;i++){
      DSM_HTStatus[i]=STATUStab->HighTowerStatus[i];
    }
  } 
}
  
void StBemcTriggerSimu::getPed(){

  Float_t ped,rms;   
  LOG_INFO<<"StBemcTriggerSimu::getPed()"<<endm;

  for (int i=1;i<=kNTowers;i++) {ped12[i-1]=0;}

  //online 12 bit peds - NOTE online peds are stored as Int_t
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerPed *PEDonline=(St_emcTriggerPed*) dbOnline->Find("bemcTriggerPed");
  emcTriggerPed_st *PEDtab=PEDonline->GetTable();
  
  //Get Pedestal shift for HT which depends on calibration
  for (int cr=0;cr<kNCrates;cr++){
    for (int seq=0; seq<10; seq++){
      bitConvValue[cr][seq]=PEDtab->BitConversionMode[cr][seq];
    }
  }
  
  //get Target Pedestal value from DB
  pedTargetValue=PEDtab->PedShift/100;
  

  if (config->Contains("online")){
    for (int cr=0; cr < kNCrates; cr++){
      for (int ch=0; ch < kNChannels; ch++){
	mDecoder->GetTowerIdFromCrate(cr,ch,did);
	ped12[did-1]=(Int_t)PEDtab->Ped[cr][ch]/100;
      }
    }
  }

 //offline 12 bit peds which are stored as Float_t
  if (config->Contains("offline")){
    for (did=1; did<=kNTowers; did++){
      mTables->getPedestal(BTOW,did,0,ped,rms);
      ped12[did-1]=(Int_t)ped;
    }
  }

  //Experts set ped to your favorite values
  if (config->Contains("expert")){
    for ( did=1; did<=kNTowers; did++){
      ped12[did-1]=24;
    }
  }
  
  //copied directly from Oleksandr's BEMC_DSM_decoder.cxx
  /*  for (did=1; did<kNTowers; did++){
      
    char buffer[10];
    int scale10bits = 4;
    int operationBit = 1;

    double ped1 = ped12[did-1] - pedTargetValue;
    if (ped1 < 0) {
      ped1 = -ped1;
      operationBit = 0;
    }

    double value2 = ped1 / scale10bits;
    sprintf(buffer, "%3.0f", value2);
    int value1 = atoi(buffer);
    value2 = ped1 - value1 * scale10bits;
    if (value2 > 2) {
      value2 = value1 + 1;
      sprintf(buffer, "%3.0f", value2);
      value1 = atoi(buffer);
    }

    if (value1 > 15) {
      sprintf(buffer, "%3.0f", double(value1 - 11) / scale10bits);
      int value3 = atoi(buffer);
      value3 *= scale10bits;
      value2 = value1 - value3;
      sprintf(buffer, "%3.0f", value2);
      value1 = atoi(buffer);
    }

    int value = 0;
    if (operationBit == 1) {
      value = (value1 & 0x0F) | 0x10;
    }
    if (operationBit == 0) {
      value = (value1 & 0x0F);
    }
    ped10[did-1]=value;
 
    mTables->getPedestal(BTOW,did,0,ped,rms);
    cout << "Tower id="<< did<<" Online pedAdc = " << ped12[did-1] << ", shift = "<< 
            pedTargetValue <<" Oleg PED10 = " << value <<" offline PED="<<ped<<  endl;
    }
  */
  
}



void StBemcTriggerSimu::getLUT(){

  LOG_INFO<<" StBemcTriggerSimu::getLUT()"<<endm;

  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerLUT *LUTonline=(St_emcTriggerLUT*) dbOnline->Find("bemcTriggerLUT");
  emcTriggerLUT_st *LUTtab=LUTonline->GetTable();
  for (int cr=0;cr<kNCrates;cr++){
    for (int seq=0; seq<kNSeq; seq++){
      LUTtag[cr][seq]=LUTtab->FormulaTag[cr][seq];
      LUTbit0[cr][seq]=LUTtab->FormulaParameter0[cr][seq];
      LUTbit1[cr][seq]=LUTtab->FormulaParameter1[cr][seq];
      LUTbit2[cr][seq]=LUTtab->FormulaParameter2[cr][seq];
      LUTbit3[cr][seq]=LUTtab->FormulaParameter3[cr][seq];
      LUTbit4[cr][seq]=LUTtab->FormulaParameter4[cr][seq];
      LUTbit5[cr][seq]=LUTtab->FormulaParameter5[cr][seq];
    }
  }
  

}


//==================================================
//==================================================
void StBemcTriggerSimu::Make(){
  
  LOG_INFO<<"StBemcTriggerSimu::Make()"<<endl;
  
  Clear();

  if(!mEvent)
    {
      LOG_WARN << "StBemcTriggerSimu -- no StEvent!" << endm;
    }
  
  StEmcCollection *emc = mEvent->emcCollection();
  if(!emc)
    {
      LOG_WARN << "StBemcTriggerSimu -- no StEmcCollection!" << endm;
    }
  
  StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
  if(!detector)
    {
      LOG_WARN << "StBemcTriggerSimu -- no StEmcDetector!" << endm;
    }
 

  //loop through BEMC hits 
  //Store 8,10,12 bit pedestal adjusted ADC for hits
  //for online case online tower masks are applied
  //for offline case offline tower masks are applied
  //DSM TP/HT masks are applied for online case
  //DSM TP/HT masks are perfect for offline case
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

		      Int_t m=rawHit[k]->module();
		      Int_t e=rawHit[k]->eta();
		      Int_t s=abs(rawHit[k]->sub());
		      Int_t adc=rawHit[k]->adc();

		      //Get software tower id, trigger patch id, crate and seq
		      mGeo->getId(m,e,s,did);
		      mDecoder->GetTriggerPatchFromTowerId(did,tpid);
		      mDecoder->GetCrateFromTowerId(did,cr,seq);
		      
		      //apply tower masks
		      if (TowerStatus[did-1]==1){
			
			//12 bit ADC enters FEE and drop 2 bits immediately
			adc12[did-1]=adc;
			adc10[did-1]=adc12[did-1] >> 2;
			
			//need to shift ADC and ped to pedTargetValue 
			//goal is to ultimately place DSM channel at 1
			ped12Diff=(Double_t)ped12[did-1]-pedTargetValue;

			//determine if pedestal is > or < pedTargetValue
			int operation=1;
			if(ped12Diff < 0)
			  {
			    ped12Diff = -ped12Diff;
			    operation = 0;
			  }
			Int_t ped10Diff =(Int_t) ped12Diff/4;
			
		
			//effective rounding up for binary numbers with 1xx,10xx,11xx,100xx,101xx,110xx,111xx etc
			//so that carrying out ADC12 - PED12 + 24 in 12 bit land is the same exercise as in 10 bit land
			if (ped12Diff - ped10Diff*4 > 2)  ped10Diff+=1;

			// can't subtract/add more than 15 on 10-bit level
			if(ped10Diff > 15) ped10Diff = ped10Diff - 4*((ped10Diff-11)/4);
			
			//adjust pedestal of tower adc to 24(6) in 12(10) bit
			if(operation==1) adc10[did-1] -= ped10Diff;
			if(operation==0) adc10[did-1] += ped10Diff;
		       			
			//now adc10 and adc08 are the 10 and 8 bit pedestal shift adcs
 			adc08[did-1]=adc10[did-1] >> 2;
			
			//subject all towers to HT algorithm and transform adc10 into adc06
			int HTholder=-1;
			if (config->Contains("online")) HTholder = adc10[did-1] >> bitConvValue[cr][seq];//drop lowest bits
			if (config->Contains("offline")) HTholder = adc10[did-1] >> HT_FEE_Offset;//drop lowest bits
			int HTL = HTholder & 0x1F;//reserve 5 LSB
			int HTH = HTholder >> 5;//take 6 LSB
			int B5  = 0;
			if(HTH>0) B5 = 1;
			HTadc06[did-1] = HTL+(B5<<5);

			//Fill DSM L0 with 6bit HT/TP in each TP
			if (DSM_HTStatus[tpid]==1){
			  if (HTadc06[did-1]>L0_HT_ADC[tpid]) L0_HT_ADC[tpid]=HTadc06[did-1];
			}
			if (DSM_TPStatus[tpid]==1) {
			  L0_TP_ADC[tpid]+=adc08[did-1];
			  L0_TP_PED[tpid]++;
			}

			//Mask out 6 bit adc if that DSM HT/TP bit was masked out
			if (DSM_HTStatus[tpid]==0) L0_HT_ADC[tpid]=0;
			if (DSM_TPStatus[tpid]==0) L0_TP_ADC[tpid]=0;

			  
			//LOG_INFO<<"Tow#="<<did<<" TP#="<<tpid<<" adc12="<<adc12[did-1]<<" adc10="<<adc10[did-1]<<" adc08="<<adc08[did-1]
			//<<" HTadc06="<<HTadc06[did-1]<<" ped12="<<ped12[did-1]<<" ped10="<<ped10[did-1]<<" ped12diff="<<ped12Diff<<" ped10Diff="
			//<<ped10Diff<<"HTholder="<<HTholder<<" HTL="<<HTL<<" HTH="<<HTH<<" B5="<<B5<<" BitConverValue="<<bitConvValue[cr][seq]
			//<<" HT_FEE_Offset="<<HT_FEE_Offset<<" L0_TP_ADC="<<L0_TP_ADC[tpid]<<" L0_TP_PED"<<endm;

		      }
		    }
		}
	    }
	}
    }

  for (tpid=0;tpid<kNPatches;tpid++){ 
    mDecoder->GetCrateAndSequenceFromTriggerPatch(tpid,cr,seq);
    if (config->Contains("offline")) L0_TP_ADC[tpid]-=(L0_TP_PED[tpid]-1);
    cout<<"TPid="<<tpid<<" cr="<<cr<<" L0_TP_ADC="<<L0_TP_PED[tpid]-1<<endl;
    for (int s=seq;s<seq+16;s++){
      cout<<"    seq="<<s<<" LUT="<<LUTbit5[cr][s]<<LUTbit4[cr][s]<<LUTbit3[cr][s]<<LUTbit2[cr][s]<<LUTbit1[cr][s]<<LUTbit0[cr][s]<<" tag="<<LUTtag[cr][s]<<endl;
    }
    //if (config->Contains("online")) L0_TP_ADC[tpid]-=(
  }
}

  
