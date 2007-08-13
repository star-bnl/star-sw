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

  LOG_INFO<<"StBemcTriggerSimu::deconstructor"<<endl;

}


//==================================================
//==================================================
void StBemcTriggerSimu::Init(){
 
  LOG_INFO <<"StBemcTriggerSimu::Init()"<<endm;

}


 
//==================================================
//==================================================
void StBemcTriggerSimu::InitRun(){
 
  LOG_INFO<<"StBemcTriggerSimu::InitRun()"<<endm;

  assert(starDb);
  int yyyy=starDb->GetDateTime().GetYear();  

  //Get FEE window for HT from support class
  //Replace this with Db soon
  HT_FEE_Offset=mDbThres->GetHtFEEbitOffset(yyyy);

  setTowerStatus();
  setDSM_TPStatus();
  setDSM_HTStatus();
  setLUT();
  getPed();
}


//==================================================
//==================================================
void  
StBemcTriggerSimu::Clear(){
  
  LOG_INFO <<"StBemcTriggerSimu::Clear()"<<endm;

  //set all adcs and pedestals to 0
  for (did=1; did<=kNTowers; did++){
    adc08[did-1]=0;
    adc10[did-1]=0;
    adc12[did-1]=0;
    ped10[did-1]=0;
    ped12[did-1]=0;
  }

}

  
//==================================================
//==================================================
void  
StBemcTriggerSimu::addTriggerList( void * adr){

}

//==================================================
//==================================================
void 
StBemcTriggerSimu::setTowerStatus(){
  
  
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerStatus *STATUSonline=(St_emcTriggerStatus*) dbOnline->Find("bemcTriggerStatus");
  emcTriggerStatus_st *STATUStab=STATUSonline->GetTable();
  if (config->Contains("online")) {
    for (int cr=0; cr < kNCrates; cr++){
      for (int ch=0; ch < kNChannels; ch++){
	mDecoder->GetTowerIdFromCrate(cr,ch,did);
	TowerStatus[did-1]=STATUStab->TowerStatus[cr][ch];
      }
    }
  }

  if (config->Contains("offline")){
    for (did=1; did<=kNTowers; did++){
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
StBemcTriggerSimu::setDSM_TPStatus(){

  //for offline config all TP status is good by definition. 
  if (config->Contains("offline")) {
    for (int i=0;i<kNPatches;i++){
      DSM_TPStatus[i]=1;
    }
  }

  
  //for online config TP status is set by DB
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerStatus *STATUSonline=(St_emcTriggerStatus*) dbOnline->Find("bemcTriggerStatus");
  emcTriggerStatus_st *STATUStab=STATUSonline->GetTable();
  if (config->Contains("online")){
    for (int i=0;i<kNPatches;i++){
      DSM_TPStatus[i]=STATUStab->PatchStatus[i];
    }
  }
  
  if (config->Contains("expert")){
    for (int i=0;i<kNPatches;i++){
      DSM_TPStatus[i]=1;
    }
  }
}

//==================================================
//==================================================
void 
StBemcTriggerSimu::setDSM_HTStatus(){
  
  //Offline all DSM HT status are good
  if (config->Contains("offline")){
    for (int i=0; i<kNPatches; i++){
      DSM_HTStatus[i]=1;
    }
  }

  //Online get DSM HT status from db
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerStatus *STATUSonline=(St_emcTriggerStatus*) dbOnline->Find("bemcTriggerStatus");
  emcTriggerStatus_st *STATUStab=STATUSonline->GetTable();
  if (config->Contains("online")){
    for (int i=0;i<kNPatches;i++){
      DSM_HTStatus[i]=STATUStab->HighTowerStatus[i];
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
  
  //offline 12 bit peds
  if (config->Contains("offline")){
    for (did=1; did<=kNTowers; did++){
      mTables->getPedestal(BTOW,did,0,ped,rms);
      ped12[did-1]=(Int_t)ped;
    }
  }


 //online 12 bit peds
  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerPed *PEDonline=(St_emcTriggerPed*) dbOnline->Find("bemcTriggerPed");
  emcTriggerPed_st *PEDtab=PEDonline->GetTable();
  if (config->Contains("online")){
    for (int cr=0; cr < kNCrates; cr++){
      for (int ch=0; ch < kNChannels; ch++){
	mDecoder->GetTowerIdFromCrate(cr,ch,did);
	ped12[did-1]=(Int_t)PEDtab->Ped[cr][ch];
      }
    }
  }

  //Set ped to your favorite values
  if (config->Contains("expert")){
    for ( did=1; did<=kNTowers; did++){
      ped12[did-1]=16;
    }
  }

  
  for (did=1; did<=kNTowers; did++){
    mTables->getPedestal(BTOW,did,0,ped,rms);
    cout<<" tower id="<<did<<" Online ped="<<ped12[did-1]/100<<" Offline ped="<<ped<<endl;
  }
  

  pedTargetValue=PEDtab->PedShift/100;
  // copied directly from Oleksandr's BEMC_DSM_decoder.cxx
  for (did=1; did<kNTowers; did++){
    
    ped12[did-1]/=100;

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
    cout << "Calculating FEE pedestal: pedAdc = " << ped12[did-1] << ", shift = " << pedTargetValue << "; PED = " << value << endl;
  }
}


void StBemcTriggerSimu::setLUT(){


  dbOnline = starDb->GetDataBase("Calibrations/emc/trigger"); 
  St_emcTriggerLUT *LUTonline=(St_emcTriggerLUT*) dbOnline->Find("bemcTriggerLUT");
  emcTriggerLUT_st *LUTtab=LUTonline->GetTable();

}


//==================================================
//==================================================
void StBemcTriggerSimu::Make(){
  
  LOG_INFO<<"StBemcTriggerSimu::Maker()"<<endl;
  
  // Clear all of ADC values
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
  
 //loop through BEMC hits and store 8,10,12 bit pedestal adjusted adcs for all hits
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

		      mGeo->getId(m,e,s,did);
		      if (TowerStatus[did-1]==1){
			adc12[did-1]=adc;
			adc10[did-1]=adc12[did-1] >> 2;
			//insert ped adjust here before shifting again
 			adc08[did-1]=adc10[did-1] >> 2;
		      }
		      
		    }
		}
	    }
	}
    }
  

  //Loop through Trigger Patches and find 6 bit HT and 6 bit TP FEE ADC 
  for(int tpid = 0; tpid < kNPatches; ++tpid) 
    {      
      {
        
	int crate = 0;
	int seq  = 0;
	//int HT = 0;
	//int PA = 0;
	//int HTID = -1;
	//int patchPed = 0;

            mDecoder->GetCrateAndSequenceFromTriggerPatch(tpid,crate,seq);
            
            //loop over each tower(j) in the trigger patch (tpid)
            for(int j = seq; j < seq + 16; ++j)
            {
            
	      int stat = mDecoder->GetTowerIdFromCrate(crate,j,did);

                if(stat == 1)
                {
            
		  /* if(adc10[id-1]>=HT)
                    {
		      HT = adc10[id-1];
		      HTID = id;
                    }
		  
		  patchPed += ped10[id-1] >> 2;
		  PA += adc08[id-1];
		  */
                }
		
            }
	    
            // now HT=10 bits and patch=12 bits
            // convert patch sum to 6 bits using LUT
            // during 2006 LUT's looked like this:
            // 0,0,0,...,0,1,2,3,...,63,63,63 -- total 4096 entries
            // <-- ped -->
            // the number of 0's is equal to patchPed_12bit
	    /*
            if(PA >= patchPed){
	      mTrigger.Patch[i] = PA - (patchPed - 1);
	      if(mTrigger.Patch[i] > 62)  mTrigger.Patch[i] = 62;
            }
	    
            else
	      {
                mTrigger.Patch[i] = 1;
	      }
            
	    // for HT need to:
            //1) drop lowest bits (depends on calibration)
            //2) take next 6 LSB as HT 6 bit adc
            //3) if 6 or higher bit ==1 need to set all bits high (63)
            
	    HT = HT >> mTrigger.HTBits - 1;
	    int HTL = HT & 0x1F;//5 LSB
            int HTH = HT >> 5;  //>= 6 LSB
            int B5  = 0;
            if(HTH>0) B5 = 1;
            mTrigger.HT[i] = HTL+(B5<<5);
            mTrigger.HTID[i] = HTID;
            
	    {
	      LOG_DEBUG <<"Patch number "<<i<<" Tower id = "<<mTrigger.HTID[i]<<" adc12 = "<<adc12[HTID-1]
			<<" adc10 = "<<adc10[HTID-1]<<" adc08 = "<<adc08[HTID-1]<<" HT10 = "<<HT<<" PA12 = "<<PA
			<<" HT = "<<mTrigger.HT[i]<<" PA = "<<mTrigger.Patch[i]<<endm; 
            }
            
            if(mTrigger.HT[i]>HTmax)
            {
	      HTmax=mTrigger.HT[i];
	      HTmaxID=HTID;
            }
    
	    for (int matrix=0; matrix<6; matrix++){
	      
	      if (HTID>2400) {//East
		if(mTrigger.HT[i]>HT2EAST_TH_2006[matrix])
		  {
		    HT2_2006_array[matrix][numHT2_2006[matrix]]=HTID;
		    numHT2_2006[matrix]++;
		  }
	      }
	      
	      if (HTID<=2400) {//West
		if(mTrigger.HT[i]>HT2WEST_TH_2006[matrix])
		  {
		    HT2_2006_array[matrix][numHT2_2006[matrix]]=HTID;
		    numHT2_2006[matrix]++;
		  }
	      }
	    }
	    */
	}
    }            
}

/*
//copied from Oleksandr's BEMC_DSM_decoder.cxx
//is input 12 bit ped and 12 bit adc?
void StBemcTriggerSimu::simulateFEEaction(int adc, int ped, int bitConv, int &ht, int &pa) {

  //12 bit ADC comes into FEE and first need to drop 2 lowest bits
  int adc1 = adc >> 2; 

  //if 5th bit of ped written in binary ==1(0) then operation bit == 16(0)
  //specifically  if int{ped/16} = odd(even) operationBit==16(0)
  //if ped=0-15,32-47,... operationBit==0
  //if ped=16-31,48-63,... operationBit==16
  int operationBit = ped & 0x10;  
  
  //pedestal = remainder of ped/16
  int pedestal = ped & 0x0F;
  
  //if operationBit ==0 adc2 =adc1 + pedestal
  //if operationBit ==16 adc2 = adc1 - pedestal
  int adc2 = operationBit ? (adc1 - pedestal) : (adc1 + pedestal);
  
  // drop 2 lowest bits to give 8 bits total
  int adc3 = adc2 >> 2;   
  pa = adc3;

  //ht needs a 6 bit window determined by year and calibration
  if (bitConv == 0) {
    ht = adc2;
  } else if (bitConv == 1) {
    int adc4 = ((adc2 >> 1) & 0x1F) | ((adc2 & 0x03C0) ? 0x20 : 0);
    ht = adc4;
  } else if (bitConv == 2) {
    int adc4 = ((adc2 >> 2) & 0x1F) | ((adc2 & 0x0380) ? 0x20 : 0);
    ht = adc4;
  } else if (bitConv == 3) {
    int adc4 = ((adc2 >> 3) & 0x1F) | ((adc2 & 0x0300) ? 0x20 : 0);
    ht = adc4;
  }
  cout << "Simulating FEE: adc = " << adc << ", ped = " << ped << ", bitConv = " << bitConv<<endl;
  cout << "pedestal = " << pedestal << ", adc2 = " << adc2<<endl;
  cout << "HT = " << ht << ", PA = " << pa << endl;
}
*/
