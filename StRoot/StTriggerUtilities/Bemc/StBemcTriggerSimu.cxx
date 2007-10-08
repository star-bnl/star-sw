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

  if (config->Contains("online")) {
    for (cr=1; cr <= kNCrates; cr++){
      for (ch=0; ch < kNChannels; ch++){
	mDecoder->GetTowerIdFromCrate(cr,ch,did);
	TowerStatus[did-1]=mTables->triggerTowerStatus(cr,ch);
      }
    }
  }

  if (config->Contains("offline")){
    for (did=1; did<=kNTowers; did++){
      mTables->getStatus(BTOW, did, TowerStatus[did-1]);
    }
  }
  
  if (config->Contains("expert")){
    for (did=1; did<=kNTowers; did++){
      TowerStatus[did-1]=1;
    }
  }
}

//==================================================
//==================================================
void 
StBemcTriggerSimu::getDSM_TPStatus(){

  LOG_INFO<<" StBemcTriggerSimu::getDSM_TPStatus()"<<endm;

  for (tpid=0;tpid<kNPatches;tpid++) DSM_TPStatus[tpid]=1;
   
  //for online config TP status is set by DB
  if (config->Contains("online")){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=mTables->triggerPatchStatus(tpid);
    }
  }
 
 //for offline config all TP status is good by definition. 
  if (config->Contains("offline")) {
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=1;
    }
  }
 
  //experts do as you will but set good by definition
  if (config->Contains("expert")){
    for (tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=1;
    }
  }

}

//==================================================
//==================================================
void 
StBemcTriggerSimu::getDSM_HTStatus(){
  
  LOG_INFO<<" StBemcTriggerSimu::getDSM_HTStatus()"<<endm;

  for (tpid=0;tpid<kNPatches;tpid++) DSM_HTStatus[tpid]=1;

  //Online get DSM HT status from db
  if (config->Contains("online")){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_HTStatus[tpid]=mTables->triggerHighTowerStatus(tpid);
    }
  }
  
 //Offline all DSM HT status are good
  if (config->Contains("offline")){
    for (tpid=0; tpid<kNPatches; tpid++){
      DSM_HTStatus[tpid]=1;
    }
  }
 
  if (config->Contains("expert")){
    for (tpid=0;tpid<kNPatches;tpid++){
      DSM_HTStatus[tpid]=1;
    }
  } 
}
  
void StBemcTriggerSimu::getPed(){

  Float_t ped,rms;   
  LOG_INFO<<"StBemcTriggerSimu::getPed()"<<endm;

  for (int i=1;i<=kNTowers;i++) {ped12[i-1]=0;}
  
  //Get Pedestal shift for HT which depends on calibration
  //for (cr=1;cr<=kNCrates;cr++){
  //  for (seq=0;seq<kNSeq;seq++){
  //    bitConvValue[cr][seq]=mTables->triggerBitConversion(cr,seq);
  // }
  //}

  for (did=1;did<=kNTowers;did++){
    bitConvValue[did-1]=mTables->triggerBitConversionByID(did);
  }

  //get Target Pedestal value from DB
  pedTargetValue=mTables->triggerPedestalShift();
  
  //online 12 bit peds stored as Float_t
  if (config->Contains("online")){
    for (int cr=1; cr <= kNCrates; cr++){
      for (int ch=0; ch < kNChannels; ch++){
	mDecoder->GetTowerIdFromCrate(cr,ch,did);
	ped12[did-1] = mTables->triggerPedestal(cr,ch);
      }
    }
  }
  
 //offline 12 bit peds stored as Float_t
  if (config->Contains("offline")){
    for (did=1; did<=kNTowers; did++){
      mTables->getPedestal(BTOW,did,0,ped,rms);
      ped12[did-1]=ped;
    }
  }

  //Experts set ped to your favorite values
  if (config->Contains("expert")){
    for (did=1; did<=kNTowers; did++){
      ped12[did-1]=24;
    }
  } 
}



void StBemcTriggerSimu::getLUT(){

  LOG_INFO<<" StBemcTriggerSimu::getLUT()"<<endm;

  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerLUT *LUTonline=(St_emcTriggerLUT*) dbOnline->Find("bemcTriggerLUT");
  emcTriggerLUT_st *LUTtab=LUTonline->GetTable();
  for (int cr=1;cr<=kNCrates;cr++){
    for (seq=0; seq<kNSeq; seq++){
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
  FEEout();
  //DSMLayer0();
  //DSMLayer1();
  //DSMLayer2();

}

void StBemcTriggerSimu::FEEout(){

  //many parts copied directly from Oleksandr's BEMC_DSM_decoder.cxx
  //which is a C++ translation of the FEE code
  //ped1 == ped12Diff value2 == ped10Diff value1 == ped10DiffI
   
  LOG_INFO<<"StBemcTriggerSimu::Fee()"<<endl;
  
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
		      mDecoder->GetCrateAndSequenceFromTriggerPatch(tpid,cr,seq);
		      
		      //apply tower masks
		      if (TowerStatus[did-1]==1){
			
			//12 bit ADC enters FEE and drop 2 bits immediately
			adc12[did-1]=adc;
			adc10[did-1]=adc12[did-1] >> 2;
			
			//need to shift ADC and ped to pedTargetValue 
			//goal is to ultimately place DSM channel at 1
			ped12Diff=ped12[did-1]-pedTargetValue;

			//determine if pedestal is > or < pedTargetValue
			int operation=1;
			if(ped12Diff < 0)
			  {
			    ped12Diff = -ped12Diff;
			    operation = 0;
			  }
			ped10Diff = ped12Diff/4;

			//Rounds ped10Diff up/down to an Int_t
			sprintf(buffer,"%3.0f",ped10Diff);
			ped10DiffI=atoi(buffer);
					
			//effective rounding up for binary numbers with 1xx,10xx,11xx,100xx,101xx,110xx,111xx etc
			//so that carrying out ADC12 - PED12 + 24 in 12 bit land is the same exercise as in 10 bit land
			if (ped12Diff - ped10DiffI*4 > 2)  
			  {
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
			if(operation==1) adc10[did-1] -= ped10DiffI;
			if(operation==0) adc10[did-1] += ped10DiffI;
	
			//now adc10 and adc08 are the 10 and 8 bit pedestal shift adcs
 			adc08[did-1]=adc10[did-1] >> 2;
			
			//subject all towers to HT algorithm and transform adc10 into adc06
			int HTholder=-1;

			if (config->Contains("online")) HTholder = adc10[did-1] >> bitConvValue[did-1];//drop lowest bits		
			if (config->Contains("offline")) HTholder = adc10[did-1] >> HT_FEE_Offset;//drop lowest bits

			int HTL = HTholder & 0x1F;// AND HTholder with 00011111 to grab 5 lowest bits
			int HTH = HTholder >> 5;//Remove lowest 5 bits
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
			
			/* LOG_INFO<<"Tow#="<<did<<" TP#="<<tpid<<" adc12="<<adc12[did-1]<<" adc10="<<adc10[did-1]<<" adc08="<<adc08[did-1]
				  <<" HTadc06="<<HTadc06[did-1]<<" ped12="<<ped12[did-1]<<" ped12diff="<<ped12Diff<<" ped10Diff="
				  <<ped10Diff<<"HTholder="<<HTholder<<" HTL="<<HTL<<" HTH="<<HTH<<" B5="<<B5<<" BitConverValue="<<bitConvValue[did-1]
				  <<" HT_FEE_Offset="<<HT_FEE_Offset<<" L0_TP_ADC="<<L0_TP_ADC[tpid]<<" PedTargetValue="<<pedTargetValue<<endm;
			*/
		      }
		    }
		}
	    }
	}
    }
  
  for (tpid=0;tpid<kNPatches;tpid++){ 
    if (config->Contains("offline")) L0_TP_ADC[tpid]-=(L0_TP_PED[tpid]-1);
    if (config->Contains("online")) L0_TP_ADC[tpid]-=(L0_TP_PED[tpid]-1);
  }
}

  
void DSMLayer0(){

  //output 16 bits
  //0-9 ADC sum Trigger Patches
  //10-11 HT threshold bits
  //12-13 TP threshold bits
  //14-15 HT&&TP threshold bits
  //for (int i=0;i<kL0DsmModule;i++){

    // HT_L0_DSM_threshold[i]=mDbThres->GetHT_L0_DSM_threshold(i,yyyy);
    //TP_L0_DSM_threshold[i]=mDbThres->GetTP_L0_DSM_threshold(i,yyyy);
    
    //for (int j=0;j<kL0DsmInputs;j++){
      
    // }
  //}
}

void DSMLayer1(){

}

void DSMLayer2(){

}
