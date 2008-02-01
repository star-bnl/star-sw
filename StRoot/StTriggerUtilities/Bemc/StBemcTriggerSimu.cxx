#include "StBemcTriggerSimu.h"

//General
#include "TH2.h"
#include "StMessMgr.h"
#include "StTriggerSimuMaker.h"

//Bemc
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StBemcTriggerDbThresholds.h"

//StEvent
#include "St_DataSetIter.h"
#include "StEvent/StEventTypes.h"

//Db
#include "St_db_Maker/St_db_Maker.h"

ClassImp(StBemcTriggerSimu)
//==================================================
//==================================================
StBemcTriggerSimu::StBemcTriggerSimu() {

  mEvent    = NULL;
  mDecoder  = new StEmcDecoder();
  mDbThres  = new StBemcTriggerDbThresholds();
  mGeo      = StEmcGeom::getEmcGeom("bemc");
  starDb    = NULL;
  mTables   = NULL;
  mHList    = NULL;
  mConfig =0;
}
//==================================================
//==================================================
StBemcTriggerSimu::~StBemcTriggerSimu(){ 

  delete mDecoder;
  delete mDbThres;
}
//==================================================
//==================================================
void StBemcTriggerSimu::Init(){
 
  LOG_INFO <<Form("Bemc::Init() MC_flag=%d, config: flag=%d",mMCflag, mConfig)<<endm;
  assert(mConfig>=kOnline);
  assert(mConfig<=kExpert);


  starDb = static_cast<St_db_Maker*> ( mHeadMaker->GetMakerInheritsFrom("St_db_Maker") );
  if(!starDb) {
    LOG_WARN << "StBemcTriggerSimu couldn't get a pointer to St_db_maker -- this means trouble" << endm;
  }
  
  if(mMCflag) {
    StEmcSimulatorMaker *emcSim = static_cast<StEmcSimulatorMaker*> ( mHeadMaker->GetMakerInheritsFrom("StEmcSimulatorMaker") );
    if(!emcSim) {
      LOG_FATAL << "StBemcTriggerSimu couldn't find StEmcSimulatorMaker in chain" << endm;
      assert(0);
    }
    mTables = emcSim->getTables();
  }
  else {
    StEmcADCtoEMaker *adc2e = static_cast<StEmcADCtoEMaker*> ( mHeadMaker->GetMakerInheritsFrom("StEmcADCtoEMaker") );
    if(!adc2e) {
      LOG_FATAL << "StBemcTriggerSimu couldn't find StEmcADCtoEMaker in chain" << endm;
      assert(0);
    }
    mTables = adc2e->getBemcData()->getTables();
  }

  mDbThres->LoadTimeStamps();
  
  //2005
  mAllTriggers.insert(96201);   //bemc-ht1-mb
  mAllTriggers.insert(96211);   //bemc-ht2-mb
  mAllTriggers.insert(96221);   //bemc-jp1-mb
  mAllTriggers.insert(96233);   //bemc-jp2-mb-b

  //2006
  mAllTriggers.insert(117201);  //bemc-http-mb
  
  mAllTriggers.insert(117211);  //bemc-ht2-mb
  mAllTriggers.insert(117212);  //bemc-ht2-mb-emul
  mAllTriggers.insert(127212);  //bemc-ht2-mb-emul
  mAllTriggers.insert(127213);  //bemc-ht2-mb-emul
  mAllTriggers.insert(137213);  //bemc-ht2-mb-emul
  
  mAllTriggers.insert(117221);  //bemc-jp1-mb
  mAllTriggers.insert(127221);  //bemc-jp1-mb
  mAllTriggers.insert(137221);  //bemc-jp1-mb
  mAllTriggers.insert(137222);  //bemc-jp1-mb
  
  mAllTriggers.insert(117501);  //bemc-jp0-mb
  mAllTriggers.insert(127501);  //bemc-jp0-mb
  mAllTriggers.insert(137501);  //bemc-jp0-mb
  
  mAllTriggers.insert(117571);  //bemc-jp1
  mAllTriggers.insert(127571);  //bemc-jp1
  mAllTriggers.insert(137571);  //bemc-jp1
  
  mAllTriggers.insert(117575);  //bemc-jp0-etot
  mAllTriggers.insert(127575);  //bemc-jp0-etot
  mAllTriggers.insert(137575);  //bemc-jp0-etot
  
  mAllTriggers.insert(117585);  //bemc-jp2
  mAllTriggers.insert(127585);  //bemc-jp2
  mAllTriggers.insert(137585);  //bemc-jp2
    
  mAllTriggers.insert(117601);  //Upsilon
  mAllTriggers.insert(117602);  //Upsilon
  mAllTriggers.insert(137602);  //Upsilon
  mAllTriggers.insert(137603);  //Upsilon
  
  mAllTriggers.insert(117611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(127611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(5);       //bemc-http-mb-l2gamma
  mAllTriggers.insert(137611);  //bemc-http-mb-l2gamma
  
  mAllTriggers.insert(117621);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(117622);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(127622);  //bemc-jp0-etot-mb-l2jet
  mAllTriggers.insert(137622);  //bemc-jp0-etot-mb-l2jet
  
  mAllTriggers.insert(117705);  //jpsi-mb
  mAllTriggers.insert(137705);  //jpsi-mb
  
  mAllTriggers.insert(117821);  //bemc-http-mb-fast
  mAllTriggers.insert(127821);  //bemc-http-mb-fast
  mAllTriggers.insert(137821);  //bemc-http-mb-fast
  mAllTriggers.insert(137822);  //bemc-http-mb-fast

  mAllTriggers.insert(147570);  //bemc-jp0
  mAllTriggers.insert(147585);  //bemc-http
  mAllTriggers.insert(147611);  //bemc-http-mb-l2gamma
  mAllTriggers.insert(147621);  //bemc-jp0-mb-l2jet
  mAllTriggers.insert(147705);  //jpsi-mb
  
  //2007
  mAllTriggers.insert(200211);  //bht2-mb
  mAllTriggers.insert(200212);  //bht2-mb OR btag -- ARE YOU KIDDING ME?
  mAllTriggers.insert(200220);  //bht2-mb
  mAllTriggers.insert(200221);  //bht2-mb
  mAllTriggers.insert(200222);  //bht2-mb
  
  mAllTriggers.insert(200213);  //btag
  mAllTriggers.insert(200214);  //btag
  
  mAllTriggers.insert(200585);  //bht2
  mAllTriggers.insert(200586);  //bht2
  
  mAllTriggers.insert(200601);  //L2-upsilon
  mAllTriggers.insert(200602);  //L2-upsilon
  
  mAllTriggers.insert(200620);  //L2-gamma
  mAllTriggers.insert(200621);  //L2-gamma

  Clear();
}
//==================================================
//==================================================
void StBemcTriggerSimu::InitRun(int runnumber){
  LOG_DEBUG<<"StBemcTriggerSimu::InitRun() -- " << runnumber << '\t' << mHeadMaker->GetDate() << '\t' << mHeadMaker->GetTime() << endm;
  
  mDecoder->SetDateTime(mHeadMaker->GetDate(), mHeadMaker->GetTime());
  
  assert(starDb);
  getTowerStatus();
  getDSM_TPStatus();
  getDSM_HTStatus();
  getLUT();
  getPed();

  timestamp=starDb->GetDateTime().Get();
  year=starDb->GetDateTime().GetYear(); 

 //Get FEE window for HT from support class for offline operation
  //online replaced this with Db call in getPed()
  HT_FEE_Offset=mDbThres->GetHtFEEbitOffset(year);
  
  for ( int tpid=0;tpid<kNPatches;tpid++) numMaskTow[tpid]=0;

}
//==================================================
//==================================================
void StBemcTriggerSimu::Clear(){
  

  for (int did=1; did<=kNTowers; did++){
    adc08[did-1]=0;
    adc10[did-1]=0;
    adc12[did-1]=0;
  }
  
  for (int tpid=0;tpid<kNPatches; tpid++){
    L0_HT_ADC[tpid]=0;
    L0_TP_ADC[tpid]=0;
    L0_TP_PED[tpid]=0;
    HTadc06[tpid]=0;
    TP6bit_adc_holder[tpid]=0;
    HT6bit_adc_holder[tpid]=0;
  }
  
  mFiredTriggers.clear();
}
//==================================================
//==================================================
StTriggerSimuDecision StBemcTriggerSimu::triggerDecision(int trigId) {
  //first check if it fired
  for(unsigned i=0; i<mFiredTriggers.size(); i++) {
    if(trigId == mFiredTriggers[i]) return kYes;
  }
  
  //now check if we care
  if(mAllTriggers.find(trigId) == mAllTriggers.end()) {
    return kDoNotCare;
  }
  else {
    return kNo;
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::getTowerStatus(){
  
  for (int i=0;i<kNTowers;i++) TowerStatus[i]=1;
  
  if (mConfig==kOnline) {
    for (Int_t cr=1; cr <= kNCrates; cr++){
      for (Int_t ch=0; ch < kNChannels; ch++){
        Int_t did,tpid;
        mDecoder->GetTowerIdFromCrate(cr,ch,did);
        TowerStatus[did-1]=mTables->triggerTowerStatus(cr,ch);
	mDecoder->GetTriggerPatchFromTowerId(did,tpid);
	if (TowerStatus[did-1]!=1) numMaskTow[tpid]++;
      }
    }
  }
  

  if (mConfig==kOffline){
    for (int did=1; did<=kNTowers; did++){
      Int_t tpid;
      mTables->getStatus(BTOW, did, TowerStatus[did-1]); 
      mDecoder->GetTriggerPatchFromTowerId(did,tpid);
      if (TowerStatus[did-1]!=1) numMaskTow[tpid]++;
    }
  }
  
  if (mConfig==kExpert){
    for (int did=1; did<=kNTowers; did++){
      Int_t tpid;
      TowerStatus[did-1]=1;
      mDecoder->GetTriggerPatchFromTowerId(did,tpid);
      if (TowerStatus[did-1]!=1) numMaskTow[tpid]++;
    }
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::getDSM_TPStatus(){

  for (int tpid=0;tpid<kNPatches;tpid++) DSM_TPStatus[tpid]=1;
   
  //for online config TP status is set by DB
  if (mConfig==kOnline){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=mTables->triggerPatchStatus(tpid);
    }
  }
 
 //for offline config all TP status is good by definition. 
  if (mConfig==kOffline) {
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=1;
    }
  }
 
  //experts do as you will but set good by definition
  if (mConfig==kExpert){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_TPStatus[tpid]=1;
    }
  }

}
//==================================================
//==================================================
void StBemcTriggerSimu::getDSM_HTStatus(){
  
  for (int tpid=0;tpid<kNPatches;tpid++) DSM_HTStatus[tpid]=1;

  //Online get DSM HT status from db
  if (mConfig==kOnline){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_HTStatus[tpid]=mTables->triggerHighTowerStatus(tpid);
    }
  }
  
 //Offline all DSM HT status are good
  if (mConfig==kOffline){
    for (int tpid=0; tpid<kNPatches; tpid++){
      DSM_HTStatus[tpid]=1;
    }
  }
 
  if (mConfig==kExpert){
    for (int tpid=0;tpid<kNPatches;tpid++){
      DSM_HTStatus[tpid]=1;
    }
  } 
}
//==================================================
//==================================================
void StBemcTriggerSimu::getPed() {

  for (int i=1;i<=kNTowers;i++) {ped12[i-1]=0;}
  
  for (int did=1;did<=kNTowers;did++){
    bitConvValue[did-1]=mTables->triggerBitConversionByID(did);
  }

  //get Target Pedestal value from DB
  pedTargetValue=mTables->triggerPedestalShift();
  
  //online 12 bit peds stored as Float_t
  if (mConfig==kOnline){
    for (int cr=1; cr <= kNCrates; cr++){
      for (int ch=0; ch < kNChannels; ch++){
        int did;
        mDecoder->GetTowerIdFromCrate(cr,ch,did);
        ped12[did-1] = mTables->triggerPedestal(cr,ch);
      }
    }
  }
  
 //offline 12 bit peds stored as Float_t
  if (mConfig==kOffline){
    for (int did=1; did<=kNTowers; did++){
      ped12[did-1]=mTables->pedestal(BTOW,did);
    }
  }

  //Experts set ped to your favorite values
  if (mConfig==kExpert){
    for (int did=1; did<=kNTowers; did++){
      ped12[did-1]=24;
    }
  } 
}
//==================================================
//==================================================
void StBemcTriggerSimu::getLUT(){

  Int_t f,param[6];
  for (int cr=1;cr<=kNCrates;cr++){
    for (int seq=0; seq<kNSeq; seq++){
      mTables->getTriggerFormulaTag(cr,seq,f);
      mTables->getTriggerFormulaParameters(cr,seq,param);
      formula[cr-1][seq]=f;
      LUTscale[cr-1][seq]=param[0];
      LUTped[cr-1][seq]=param[1];
      LUTsig[cr-1][seq]=param[2];
      LUTpow[cr-1][seq]=param[3];
    }
  }
}
//==================================================
//==================================================
void StBemcTriggerSimu::Make(){
  
  mEvent = static_cast<StEvent*> ( mHeadMaker->GetDataSet("StEvent") );
  
  FEEout();
  if (year==2006){
    get2006_DSMLayer0();
    get2006_DSMLayer1();
  }

}
//==================================================
//==================================================
void StBemcTriggerSimu::FEEout() {
  //many parts copied directly from Oleksandr's BEMC_DSM_decoder.cxx
  //which is a C++ translation of the FEE code
  //ped1 == ped12Diff value2 == ped10Diff value1 == ped10DiffI
   
   
  //  static int commonLUT[] = {0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
  static int commonLUT[] = { 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9,
			    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
			    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
			    60, 61, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 
			    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 
			    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62}; 
  
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
  if(detector) {
    for(Int_t m = 1; m <= 120; ++m) {
      StEmcModule* module = detector->module(m);
      if(module) {
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k = 0; k < rawHit.size(); ++k) {
          if(rawHit[k]) {
            int did, tpid;
            
            Int_t m=rawHit[k]->module();
            Int_t e=rawHit[k]->eta();
            Int_t s=abs(rawHit[k]->sub());
            Int_t adc=rawHit[k]->adc();
            //Float_t energy=rawHit[k]->energy();

            //Get software tower id and trigger patch id
            mGeo->getId(m,e,s,did);	  
            mDecoder->GetTriggerPatchFromTowerId(did,tpid);
	    
            //apply tower masks
	    if (TowerStatus[did-1]==1) {
  
	      //need to shift ADC and ped to pedTargetValue 
              //goal is to ultimately place DSM channel at 1
              ped12Diff=ped12[did-1]-pedTargetValue;
	      
	      //12 bit ADC enters FEE and drop 2 bits immediately
              adc12[did-1]=adc;
              adc10[did-1]=adc12[did-1] >> 2;
  
              //determine if pedestal is > or < pedTargetValue
              int operation=1;
              if(ped12Diff < 0) {
                ped12Diff = -ped12Diff;
                operation = 0;
              }
              ped10Diff = ped12Diff/4;

              //Rounds ped10Diff up/down to an Int_t
              sprintf(buffer,"%3.0f",ped10Diff);
              ped10DiffI=atoi(buffer);
        
              //effective rounding up for binary numbers with 1xx,10xx,11xx,100xx,101xx,110xx,111xx etc
              //so that carrying out ADC12 - PED12 + 24 in 12 bit land is the same exercise as in 10 bit land
              if (ped12Diff - ped10DiffI*4 > 2) {
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
	      //if (adc10[did-1] < 0) adc10[did-1]=0;

              //now adc10 and adc08 are the 10 and 8 bit pedestal shift adcs
              adc08[did-1]=adc10[did-1] >> 2;
    
              //subject all towers to HT algorithm and transform adc10 into adc06
              int HTholder=-1;

              if (mConfig==kOnline) HTholder = adc10[did-1] >> bitConvValue[did-1];//drop lowest bits   
              if (mConfig==kOffline) HTholder = adc10[did-1] >> HT_FEE_Offset;//drop lowest bits

              int HTL = HTholder & 0x1F;// AND HTholder with 00011111 to grab 5 lowest bits
              int HTH = HTholder >> 5;//Remove lowest 5 bits
              int B5  = 0;
              if(HTH>0) B5 = 1;
              HTadc06[did-1] = HTL+(B5<<5);

              //Fill DSM L0 with 6bit HT/TP in each TP
	      HT6bit_adc_holder[did-1]=HTadc06[did-1]; 
              if (DSM_HTStatus[tpid]==1){
                if (HTadc06[did-1]>L0_HT_ADC[tpid]) L0_HT_ADC[tpid]=HTadc06[did-1];
              }
              if (DSM_TPStatus[tpid]==1) {
                L0_TP_ADC[tpid]+=adc08[did-1];
		TP6bit_adc_holder[tpid]+=adc08[did-1]; 
		//Calculate LUT ped for OFFLINE
                L0_TP_PED[tpid]++;
              }
	      
              //Mask out 6 bit adc if that DSM HT/TP bit was masked out
              if (DSM_HTStatus[tpid]==0) L0_HT_ADC[tpid]=0;
              if (DSM_TPStatus[tpid]==0) L0_TP_ADC[tpid]=0;
              if (DSM_TPStatus[tpid]==0) TP6bit_adc_holder[tpid]=0; 
	      
	      if  (0)
		{
		  cout<<"Tow#="<<did<<" TP#="<<tpid<<" adc12="<<adc12[did-1]<<" adc10="<<adc10[did-1]<<" adc08="<<adc08[did-1]	   
		      <<" HTadc06="<<HTadc06[did-1]<<" ped12="<<ped12[did-1]<<" ped12diff="<<ped12Diff<<" ped10Diff="
		      <<ped10Diff<<"HTholder="<<HTholder<<" HTL="<<HTL<<" HTH="<<HTH<<" B5="<<B5<<" BitConverValue="<<bitConvValue[did-1]
		      <<" HT_FEE_Offset="<<HT_FEE_Offset<<" L0_TP_ADC="<<L0_TP_ADC[tpid]<<" PedTargetValue="<<pedTargetValue<<endl;
		}
            }
          }
        }
      }
    }
  }

  //Find LUT
  for (int tpid=0;tpid<kNPatches;tpid++){ 
   

    Int_t cr, seq, chan, LUTindex;
    mDecoder->GetCrateAndSequenceFromTriggerPatch(tpid,cr,seq); 
    chan=seq/16;
    
    if (mConfig==kOnline)
      {
	if ( ((L0_TP_ADC[tpid]+LUTped[cr-1][chan]+2)>=0) && (formula[cr-1][chan]==2) && (LUTscale[cr-1][chan]==1) && 
	     (LUTpow[cr-1][chan]!=0) && (LUTsig[cr-1][chan]==0) && (pedTargetValue==24))
	  {
	    LUTindex=L0_TP_ADC[tpid] + LUTped[cr-1][chan] + numMaskTow[tpid];
	    LUT[tpid] = commonLUT[LUTindex];
	  }  
	else
	  {
	    cout<<" Something not right with LUT!"<<endl;
	    assert(1);
	  }
	L0_TP_ADC[tpid]=LUT[tpid];    
	TP6bit_adc_holder[tpid]=LUT[tpid]; 
      }
    
   
    if (mConfig==kOffline) {
      // MOCK up LUT table for Offline case
      if ((L0_TP_PED[tpid]-1)>=L0_TP_ADC[tpid]) { 
	L0_TP_ADC[tpid]=1;
	TP6bit_adc_holder[tpid]=1; 
      }
      if ((L0_TP_PED[tpid]-1)< L0_TP_ADC[tpid]) {
	L0_TP_ADC[tpid]-=(L0_TP_PED[tpid]-1);
	TP6bit_adc_holder[tpid]-=(L0_TP_PED[tpid]-1);
      }
      if (L0_TP_ADC[tpid] > 62) {
	L0_TP_ADC[tpid]=62;
	TP6bit_adc_holder[tpid]=62;
      }
    }
        

    if (0) 
      {
	cout<<" tpid="<<tpid<<" cr="<<cr<<" ch="<<chan<<" formula="<<formula[cr-1][chan]<<
	  " TPadc="<<L0_TP_ADC[tpid]<<" OfflinePed="<<L0_TP_PED[tpid]<<" LUTped="<<LUTped[cr-1][chan]<<
	  " numMaskTow="<<numMaskTow[tpid]<<" LUTindex="<<LUTindex<<" OnlineLUT="<<LUT[tpid]<<
	  " diff="<<(L0_TP_ADC[tpid] - (L0_TP_PED[tpid]-1)) - (LUT[tpid])<<endl;   
      }  
  }
  }
//==================================================
//==================================================
void StBemcTriggerSimu::get2006_DSMLayer0() {

  //0-(8)9 ADC sum Trigger Patches
  //10-11  HT threshold bits
  //12-13  TP threshold bits
  //14-15  HT&&TP threshold bits


  //Loop over modules
  int k=0;
  int DSM_TP[kL0DsmInputs];
  for (int i=0;i<kL0DsmModule;i++){

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each module
    DSM0_TP_SUM[i]=0; 
    DSM0_HT_Bit[i]=0;
    DSM0_TP_Bit[i]=0;
    DSM0_HTTP_Bit[i]=0;

    DSM0_TP_SUM_J1[i]=0;
    DSM0_HT_Bit_J1[i]=0;
    DSM0_TP_Bit_J1[i]=0;
    DSM0_HTTP_Bit_J1[i]=0;

    DSM0_TP_SUM_J3[i]=0;
    DSM0_HT_Bit_J3[i]=0;
    DSM0_TP_Bit_J3[i]=0;
    DSM0_HTTP_Bit_J3[i]=0;

    //Zero out 16 bit L0 TP/HT/HTTP outputs for each L0 input
    for (int j=0;j<kL0DsmInputs;j++){ 
      DSM0_HT_tp_Bit[j]=0;
      DSM0_TP_tp_Bit[j]=0;
      DSM0_HTTP_tp_Bit[j]=0; 
      DSM0_HT_tp_Bit_J1[j]=0;
      DSM0_TP_tp_Bit_J1[j]=0;
      DSM0_HTTP_tp_Bit_J1[j]=0; 
      DSM0_HT_tp_Bit_J3[j]=0;
      DSM0_TP_tp_Bit_J3[j]=0;
      DSM0_HTTP_tp_Bit_J3[j]=0;      
    }
  
    //Get array of TPid# from DSM module#
    mDecoder->GetTriggerPatchesFromDSM(i,DSM_TP);
    
    //Loop over 10 inputs to each module 
    for (int j=0;j<kL0DsmInputs;j++){
      
      int tpid=DSM_TP[j];
      int jpid=-1;
      int seq=-1;
      mDecoder->GetJetPatchAndSequenceFromTriggerPatch(tpid, jpid, seq); 

      //Skip modules 2,7,12,17,22,27 
      if (((i+3)%5)!=0){
	
	//apply HT thresholds to each HT adc in each TP
	if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit[j]=0;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit[j]=1;
	if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit[j]=2;
	if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit[j]=3;
	
	if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit[j]=0;
	if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit[j]=1;
	if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit[j]=2;
	if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit[j]=3;

	//apply HTTP condition - TP&&HT
	if (DSM0_TP_tp_Bit[j] >= DSM0_HT_tp_Bit[j]) DSM0_HTTP_tp_Bit[j]=DSM0_HT_tp_Bit[j];
	if (DSM0_HT_tp_Bit[j] >= DSM0_TP_tp_Bit[j]) DSM0_HTTP_tp_Bit[j]=DSM0_TP_tp_Bit[j];
	//then || each input
	if (DSM0_HTTP_tp_Bit[j] > DSM0_HTTP_Bit[i]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];

	//add up TP adc for 2/5 of JP
	DSM0_TP_SUM[i]+=L0_TP_ADC[tpid];
	
	if (DSM0_HT_Bit[i]< DSM0_HT_tp_Bit[j]) DSM0_HT_Bit[i]=DSM0_HT_tp_Bit[j];
	if (DSM0_TP_Bit[i]< DSM0_TP_tp_Bit[j]) DSM0_TP_Bit[i]=DSM0_TP_tp_Bit[j];
	if (DSM0_HTTP_Bit[i]< DSM0_HTTP_tp_Bit[j]) DSM0_HTTP_Bit[i]=DSM0_HTTP_tp_Bit[j];
      }
      
      //Loop over 2x5 inputs(TP) for modules 2,7,12,17,22,29
      if (((i+3)%5)==0){
	
	//apply HT thresholds to each HT adc in each TP
	if (j>4) 
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J1[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J1[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J1[j]=2;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit_J1[j]=3;
	  } 
	if (j<5)
	  {
	    if ( L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,0)) DSM0_HT_tp_Bit_J3[j]=0;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,1)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,0))) DSM0_HT_tp_Bit_J3[j]=1;
	    if ((L0_HT_ADC[tpid] <= mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) && (L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,1))) DSM0_HT_tp_Bit_J3[j]=2;
	    if ( L0_HT_ADC[tpid] > mDbThres->GetHT_DSM0_threshold(i,timestamp,2)) DSM0_HT_tp_Bit_J3[j]=3;
	  }
	
	//apply TP thresholds to each TP adc in each TP
	if (j>4)
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J1[j]=0;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit_J1[j]=1;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit_J1[j]=2;
	    if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit_J1[j]=3;
	  }
	if (j<5)
	  {
	    if ( L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,0)) DSM0_TP_tp_Bit_J3[j]=0;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,1)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,0))) DSM0_TP_tp_Bit_J3[j]=1;
	    if ((L0_TP_ADC[tpid] <= mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) && (L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,1))) DSM0_TP_tp_Bit_J3[j]=2;
	    if ( L0_TP_ADC[tpid] > mDbThres->GetTP_DSM0_threshold(i,timestamp,2)) DSM0_TP_tp_Bit_J3[j]=3;
	  }                       
	
	//apply HTTP condition - TP&&HT
	if (j>4) {
	  if (DSM0_TP_tp_Bit_J1[j] >= DSM0_HT_tp_Bit_J1[j]) DSM0_HTTP_tp_Bit_J1[j]=DSM0_HT_tp_Bit_J1[j];
	  if (DSM0_HT_tp_Bit_J1[j] >= DSM0_TP_tp_Bit_J1[j]) DSM0_HTTP_tp_Bit_J1[j]=DSM0_TP_tp_Bit_J1[j];
	  if (DSM0_HTTP_tp_Bit_J1[j] > DSM0_HTTP_Bit_J1[j]) DSM0_HTTP_Bit_J1[j]=DSM0_HTTP_tp_Bit_J1[j];
	}
	if (j<5){
	  if (DSM0_TP_tp_Bit_J3[j] >= DSM0_HT_tp_Bit_J3[j]) DSM0_HTTP_tp_Bit_J3[j]=DSM0_HT_tp_Bit_J3[j];
	  if (DSM0_HT_tp_Bit_J3[j] >= DSM0_TP_tp_Bit_J3[j]) DSM0_HTTP_tp_Bit_J3[j]=DSM0_TP_tp_Bit_J3[j];
	  if (DSM0_HTTP_tp_Bit_J3[j] > DSM0_HTTP_Bit_J3[j]) DSM0_HTTP_Bit_J3[j]=DSM0_HTTP_tp_Bit_J3[j];
	}
	
	//add up TP adc for 1/5 of JP
	if (j>4) DSM0_TP_SUM_J1[i]+=L0_TP_ADC[tpid];
	if (j<5) DSM0_TP_SUM_J3[i]+=L0_TP_ADC[tpid];
	
	//apply HT/TP/HTTP thresholds to bits
	if (DSM0_HT_Bit_J1[i]< DSM0_HT_tp_Bit_J1[j]) DSM0_HT_Bit_J1[i]=DSM0_HT_tp_Bit_J1[j];
	if (DSM0_TP_Bit_J1[i]< DSM0_TP_tp_Bit_J1[j]) DSM0_TP_Bit_J1[i]=DSM0_TP_tp_Bit_J1[j];
	if (DSM0_HTTP_Bit_J1[i]< DSM0_HTTP_tp_Bit_J1[j]) DSM0_HTTP_Bit_J1[i]=DSM0_HTTP_tp_Bit_J1[j];
	if (DSM0_HT_Bit_J3[i]< DSM0_HT_tp_Bit_J3[j]) DSM0_HT_Bit_J3[i]=DSM0_HT_tp_Bit_J3[j];
	if (DSM0_TP_Bit_J3[i]< DSM0_TP_tp_Bit_J3[j]) DSM0_TP_Bit_J3[i]=DSM0_TP_tp_Bit_J3[j];
	if (DSM0_HTTP_Bit_J3[i]< DSM0_HTTP_tp_Bit_J3[j]) DSM0_HTTP_Bit_J3[i]=DSM0_HTTP_tp_Bit_J3[j];
      
      }      
    }
    
    
    //Construct total output bits from DSMLayer0
    Int_t two10 = (Int_t) pow(2.0,10.0);
    Int_t two12 = (Int_t) pow(2.0,12.0);
    Int_t two14 = (Int_t) pow(2.0,14.0);

    if (((i+3)%5)!=0)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k]=DSM0_TP_SUM[i]+DSM0_HT_Bit[i]*two10+ DSM0_TP_Bit[i]*two12+DSM0_HTTP_Bit[i]*two14;
	k++;
      }
    if (((i+3)%5)==0)
      {
	L0_16bit_Out[k]=0;
	L0_16bit_Out[k]=DSM0_TP_SUM_J1[i]+DSM0_HT_Bit_J1[i]*two10+DSM0_TP_Bit_J1[i]*two12+DSM0_HTTP_Bit_J1[i]*two14;
	k++;

	L0_16bit_Out[k]=0;
	L0_16bit_Out[k]=DSM0_TP_SUM_J3[i]+DSM0_HT_Bit_J3[i]*two10+DSM0_TP_Bit_J3[i]*two12+DSM0_HTTP_Bit_J3[i]*two14;
	k++;
      }


    mDecoder->GetTriggerPatchesFromDSM(i,DSM_TP); 
    //NEED to eventually make this decision higher up but keep it here for now
    if ((DSM0_HTTP_Bit[i]>1)||(DSM0_HTTP_Bit_J3[i]>1)||(DSM0_HTTP_Bit_J1[i]>1)) {
      mFiredTriggers.push_back(127611);
      mFiredTriggers.push_back(127821);
      mFiredTriggers.push_back(137821);
      mFiredTriggers.push_back(137822);
      mFiredTriggers.push_back(137611);
      mFiredTriggers.push_back(5);
    }
    
    if ((DSM0_HT_Bit[i]> 2)||(DSM0_HT_Bit_J3[i]>2)||(DSM0_HT_Bit_J1[i]>2)) {
      mFiredTriggers.push_back(127212);
      mFiredTriggers.push_back(137213);
    }

  }
}


//==================================================
//==================================================
void StBemcTriggerSimu::get2006_DSMLayer1(){


  //DSM_Layer0 is passed to DSM_Layer1 in 8 UShort blocks (16 bits)
  //There are 6 DSM_Layer1 boards and each can take 120 bits total
  //So DSM_Layer0 passes 8 shorts (16*8=128) or 128 bits to each DSM_Layer1
  //  int nShort[8] = {3, 2, 1, 0, 7, 6, 5, 4};
  for (int i=0; i<kL1DsmModule; i++)
    {
      for (int j=0; j<6; j++) //only loop over 6 shorts 
	{
	  // Int_t L0_Output_Channel = (i * 8) + nShort[j];
	}
    }

  //zero out the DSMLayer1 Bits passed to DSMLayer2
  for (int i=0;i<kL1DsmModule;i++){
    DSM1_JP_Bit[i]=0;
    DSM1_HT_Bit[i]=0;
    DSM1_TP_Bit[i]=0;
    DSM1_HTTP_Bit[i]=0;
    DSM1_ETOT_ADC[i]=0;
  }

  //Sum TP ADC into JP's
  DSM1_JP_ADC[0]=DSM0_TP_SUM[0]+DSM0_TP_SUM[1]+DSM0_TP_SUM_J3[2];
  DSM1_JP_ADC[1]=DSM0_TP_SUM[3]+DSM0_TP_SUM[4]+DSM0_TP_SUM_J1[2];
  DSM1_JP_ADC[2]=DSM0_TP_SUM[5]+DSM0_TP_SUM[6]+DSM0_TP_SUM_J3[7];
  DSM1_JP_ADC[3]=DSM0_TP_SUM[8]+DSM0_TP_SUM[9]+DSM0_TP_SUM_J1[7];
  DSM1_JP_ADC[4]=DSM0_TP_SUM[10]+DSM0_TP_SUM[11]+DSM0_TP_SUM_J3[12];
  DSM1_JP_ADC[5]=DSM0_TP_SUM[13]+DSM0_TP_SUM[14]+DSM0_TP_SUM_J1[12];
  DSM1_JP_ADC[6]=DSM0_TP_SUM[15]+DSM0_TP_SUM[16]+DSM0_TP_SUM_J3[17];
  DSM1_JP_ADC[7]=DSM0_TP_SUM[18]+DSM0_TP_SUM[19]+DSM0_TP_SUM_J1[17];
  DSM1_JP_ADC[8]=DSM0_TP_SUM[20]+DSM0_TP_SUM[21]+DSM0_TP_SUM_J3[22];
  DSM1_JP_ADC[9]=DSM0_TP_SUM[23]+DSM0_TP_SUM[24]+DSM0_TP_SUM_J1[22];
  DSM1_JP_ADC[10]=DSM0_TP_SUM[25]+DSM0_TP_SUM[26]+DSM0_TP_SUM_J3[27];
  DSM1_JP_ADC[11]=DSM0_TP_SUM[28]+DSM0_TP_SUM[29]+DSM0_TP_SUM_J1[27];
  for (int hh=0;hh<12;hh++) {
    JP_adc_holder[hh]=DSM1_JP_ADC[hh];
  }
  
  //Test each JP and see if it passed
  for (int i=0;i<kNJet;i++)
    {
      DSM1_JP_jp_Bit[i]=0;
      if ( DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,0)) DSM1_JP_jp_Bit[i]=0;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,0))) DSM1_JP_jp_Bit[i]=1;
      if ((DSM1_JP_ADC[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) && (DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,1))) DSM1_JP_jp_Bit[i]=2;
      if ( DSM1_JP_ADC[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,2)) DSM1_JP_jp_Bit[i]=3;
    }  

  // there are 2 L2 BEMC modules
  int mod;
  //Translate JP's into 2 bits to pass to DSMLayer2
  for (int i=0;i<kNJet;i++){
    if (i < (kNJet/2)) mod = 0;
    else mod = 1;
    DSM1_ETOT_ADC[mod]+=DSM1_JP_ADC[i];
    if ( DSM1_JP_Bit[mod] < DSM1_JP_jp_Bit[i]) DSM1_JP_Bit[mod]=DSM1_JP_jp_Bit[i];   
  }


  // again we will move this up to a higher level but for now keep this here
  if ((DSM1_JP_Bit[0] >= 1)||(DSM1_JP_Bit[1] >=1 )) {
    mFiredTriggers.push_back(127501);
    mFiredTriggers.push_back(137501);
    mFiredTriggers.push_back(127622);
    mFiredTriggers.push_back(137622);
  }
  if ((DSM1_JP_Bit[0] >= 2)||(DSM1_JP_Bit[1] >=2 )) {
    mFiredTriggers.push_back(127221);
    mFiredTriggers.push_back(137221);
    mFiredTriggers.push_back(137222);
  }

  for (int i=0;i<kL0DsmModule;i++){

    if (i < (kL0DsmModule/2)) mod=0;
    else  mod=1;

    if ( DSM1_HT_Bit[mod] < DSM0_HT_Bit[i] ) DSM1_HT_Bit[mod]=DSM0_HT_Bit[i];
    if ( DSM1_HT_Bit[mod] < DSM0_HT_Bit_J3[i] ) DSM1_HT_Bit[mod]=DSM0_HT_Bit_J3[i];
    if ( DSM1_HT_Bit[mod] < DSM0_HT_Bit_J1[i] ) DSM1_HT_Bit[mod]=DSM0_HT_Bit_J1[i];
    if ( DSM1_TP_Bit[mod] < DSM0_TP_Bit[i] ) DSM1_TP_Bit[mod]=DSM0_TP_Bit[i];
    if ( DSM1_TP_Bit[mod] < DSM0_TP_Bit_J3[i] ) DSM1_TP_Bit[mod]=DSM0_TP_Bit_J3[i];
    if ( DSM1_TP_Bit[mod] < DSM0_TP_Bit_J1[i] ) DSM1_TP_Bit[mod]=DSM0_TP_Bit_J1[i];
    if ( DSM1_HTTP_Bit[mod] < DSM0_HTTP_Bit[i] ) DSM1_HTTP_Bit[mod]=DSM0_HTTP_Bit[i];
    if ( DSM1_HTTP_Bit[mod] < DSM0_HTTP_Bit_J3[i] ) DSM1_HTTP_Bit[mod]=DSM0_HTTP_Bit_J3[i];
    if ( DSM1_HTTP_Bit[mod] < DSM0_HTTP_Bit_J1[i] ) DSM1_HTTP_Bit[mod]=DSM0_HTTP_Bit_J1[i];
  
  }

  //Drop two lowest bits for ETOT and OR Bits>6 with 6
  for (int i=0;i<kL1DsmModule;i++) {
    DSM1_ETOT_ADC[i]/=4;
    if (DSM1_ETOT_ADC[i]>31) DSM1_ETOT_ADC[i]=31;
  }
}


const vector< pair<int,int> > StBemcTriggerSimu::getTowersAboveThreshold(int trigId) const {  
  vector< pair<int,int> > towers;
  
  for (int i=0;i<kNTowers;i++)
    {
      int tpid = -1;
      int dsmid = -1;
      
      mDecoder->GetTriggerPatchFromTowerId(i,tpid);
      mDecoder->GetDSMFromTriggerPatch(tpid,dsmid);

      if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
	if (HT6bit_adc_holder[i] > mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,1)) {
	  //cout << "In getTowersAboveThreshold: " << i+1 << "\tHT: " << HT6bit_adc_holder[i] << "\tThreshold: " << mDbThres->GetHT_DSM0_threshold(i,timestamp,1) << endl;
	  towers.push_back( make_pair(i+1,HT6bit_adc_holder[i]) );
	}
      }
      if (trigId==127212 || trigId==137213) {
	if (HT6bit_adc_holder[i] > mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,2)) {
	  //cout << "In getTowersAboveThreshold: " << i+1 << "\tHT: " << HT6bit_adc_holder[i] << "\tThreshold: " << mDbThres->GetHT_DSM0_threshold(i,timestamp,2) << endl;
	  towers.push_back( make_pair(i+1,HT6bit_adc_holder[i]) );
	}
      }
    }
  return towers;
}


const vector< pair<int,int> > StBemcTriggerSimu::getTriggerPatchesAboveThreshold(int trigId) const {  
  vector< pair<int,int> > patches;

  for (int i=0;i<kNPatches;i++)
    {
      int dsmid;
      mDecoder->GetDSMFromTriggerPatch(i,dsmid);

      if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
	if (TP6bit_adc_holder[i] > mDbThres->GetTP_DSM0_threshold(dsmid,timestamp,1)) {	
	  //cout << "In getTPAboveThreshold: " << i+1 << "\tTP: " << TP6bit_adc_holder[i] <<" dsmid"<<dsmid<<" tpid="<<i<< endl;
	  patches.push_back( make_pair(i,TP6bit_adc_holder[i]) );
	}
      }
    }
  return patches;
}


const vector< pair<int,int> > StBemcTriggerSimu::getJetPatchesAboveThreshold(int trigId) const {  
  vector< pair<int,int> > patches;
  
  for (int i=0;i<kNJet;i++)
    {
      if ((JP_adc_holder[i] <= mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) && (JP_adc_holder[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,0))) {
	if (trigId==127501 || trigId==137501 || trigId==127622 || trigId==137622) {
	  patches.push_back( make_pair(i,JP_adc_holder[i]) );
	}
      }
      if (JP_adc_holder[i] > mDbThres->GetJP_DSM1_threshold(i,timestamp,1)) {
	if (trigId==127221 || trigId==137221 || trigId==137222 || trigId==127501 || trigId==137501 || trigId==127622 || trigId==137622) {
	  patches.push_back( make_pair(i,JP_adc_holder[i]) );
	}
      }
    }
  return patches;
}


int StBemcTriggerSimu::getTowerThreshold(int trigId, int dsmid) const {  
  int threshold =-1;

  if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
    threshold = mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,1);
  }
  if (trigId==127212 || trigId==137213) {
    threshold = mDbThres->GetHT_DSM0_threshold(dsmid,timestamp,2);
  }

  return threshold;
}


int StBemcTriggerSimu::getTriggerPatchThreshold(int trigId, int dsmid) const {
  int threshold = -1;

  if (trigId==127611 || trigId==127821 || trigId==137821 || trigId==137822 || trigId==137611 || trigId==5) {
    threshold = mDbThres->GetTP_DSM0_threshold(dsmid,timestamp,1);
  }
  return threshold;
}


int StBemcTriggerSimu::getJetPatchThreshold(int trigId, int dsmid) const {  
  int threshold = -1;

  if (trigId==127501 || trigId==137501 || trigId==127622 || trigId==137622) {  
    threshold = mDbThres->GetJP_DSM1_threshold(dsmid,timestamp,0);
  }
  if (trigId==127221 || trigId==137221 || trigId==137222) {
    threshold = mDbThres->GetJP_DSM1_threshold(dsmid,timestamp,1);
  }
  
  return threshold;
}

