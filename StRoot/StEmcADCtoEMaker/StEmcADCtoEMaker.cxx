// 
// $Id: StEmcADCtoEMaker.cxx,v 1.40 2003/02/10 16:29:20 suaide Exp $
// $Log: StEmcADCtoEMaker.cxx,v $
// Revision 1.40  2003/02/10 16:29:20  suaide
// small modifications
//
// Revision 1.39  2003/02/06 16:34:58  suaide
// changes on data validation
//
// Revision 1.38  2003/02/04 18:33:19  suaide
// small modifications
//
// Revision 1.37  2003/01/23 03:09:05  jeromel
// Include modif
//
// Revision 1.36  2003/01/21 22:40:29  suaide
// small modification to save all tower hits
//
// Revision 1.35  2003/01/17 23:02:25  suaide
// small modification
//
// Revision 1.34  2002/12/02 21:20:12  suaide
// modifications for new DB scheme
//
// Revision 1.33  2002/09/30 14:03:00  suaide
// small bugs fixed
//
// Revision 1.32  2002/09/19 21:32:23  suaide
// Modifications to use a new internal data format
//
// Revision 1.31  2002/05/22 22:04:23  suaide
// small bug fixed to reconstruct micro DST's
//
// Revision 1.30  2002/05/15 15:05:28  suaide
// bugs fixed to recalibrate EMC after production
//
// Revision 1.29  2002/02/25 16:23:24  suaide
// reduced SMD threshold
//
// Revision 1.28  2002/02/24 21:19:21  suaide
// clean up and modifications on the settings that allow to save only hits
// above a given threshold that can be defined for each sub detector.
//
// Revision 1.27  2001/12/28 15:03:09  suaide
// fixed documentation
//
// Revision 1.26  2001/12/27 17:45:36  suaide
// removed obsolete files and updated documentation
//
// Revision 1.24  2001/12/26 19:25:34  suaide
// Added documentation and few modifications
//
// Revision 1.23  2001/12/06 17:50:08  suaide
// changes to save ADC without pedestal subtraction
//
// Revision 1.22  2001/12/05 22:31:12  suaide
// Modifications to include SMD
//
// Revision 1.21  2001/12/04 22:05:50  suaide
// new QA histogram for tower
//
// Revision 1.20  2001/11/05 17:09:11  suaide
// small changes
//
// Revision 1.19  2001/10/31 22:24:17  suaide
// modified Finish() method
//
// Revision 1.11  2001/10/24 23:06:54  suaide
// log messages included for easier debug
//
// Revision 1.8  2001/10/24 14:47:16  suaide
// type correction
//
// Revision 1.7  2001/10/24 14:41:44  suaide
// huge change on StEmcADCtoEMaker to a different software.
// The other version is kept as *.old for future debug 
//
// Revision 1.13  2000/05 16:07:01  
// Add README
//
#include "StEmcADCtoEMaker.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include <iostream.h>
#include <math.h>
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TStopwatch.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StBemcData.h"

#define STATUS_OK 1
#define MAXDET 4
#define CAP1 124
#define CAP2 125

ClassImp(StEmcADCtoEMaker)

//_____________________________________________________________________________
/* 
Default constructor. Set Initial values for some variables
*/
StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name):StMaker(name)
{
  mControlADCtoE = new controlADCtoE_st();

  Int_t   calib[]      = {1, 0, 1, 1, 0, 0, 0, 0};
  Int_t   pedSub[]     = {1, 0, 1, 1, 0, 0, 0, 0};
  Float_t energyCut[]  = {-1, -1, 0.07, 0.07, -1, -1, -1, -1};
  Int_t   onlyCal[]    = {0, 0, 0, 0, 0, 0, 0, 0};
  
  for(Int_t i=0; i<MAXDET; i++)
  {
    mControlADCtoE->DeductPedestal[i]=pedSub[i];  
    mControlADCtoE->Calibration[i]=calib[i];
    mControlADCtoE->EnergyCutOff[i]=energyCut[i];
    mControlADCtoE->OnlyCalibrated[i]=onlyCal[i];
  } 
  mEmbedd=kFALSE;
}
//_____________________________________________________________________________
/* 
Default destructor
*/
StEmcADCtoEMaker::~StEmcADCtoEMaker()
{
}
//_____________________________________________________________________________
/* 
Init function. Initializes the histograms and all other variables used by the
program
*/
Int_t StEmcADCtoEMaker::Init()
{     
  //Making QA histgrams
  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
	
	mValidEvents = new TH2F("ValidEvents","Valid events for each detector (1=good, 2= bad)",4,-0.5,3.5,8,0.5,8.5);
 
  mNhit = new TH2F("EmcNHitsVsDet" ,"Number of hit with energy > 0 .vs. Detector #",1000,0.0,18000,8,0.5,8.5);
  mEtot = new TH2F("EmcEtotVsDet" ,"Total energy(log10) .vs. Detector #",500,-4.0,15.0,8,0.5,8.5);
 
  //tower spectra for gain monitoring  
  mTower=new TH2F("TowerSpectra","Tower Spectra up to ADC = 500",4800,0.5,4800.5,500,0,500);
       
  // SMD time bin
  mSmdTimeBinHist = new TH2F("SmdTimeBin","SMD Time bin",8,-0.5,7.5,128,0.5,128.5);

  for (Int_t i=0; i<MAXDET; i++) if(mControlADCtoE->Calibration[i]==1)
  {
    TString name_h = detname[i] + "_Hits";
    TString name_e = detname[i] + "_Energy";
    TString name_a = detname[i] + "_ADC";
    TString name_a1= detname[i] + "_ADC1D";
    TString name_e1= detname[i] + "_Energy1D";
    TString title_h= detname[i] + " Hits distribution with energy > 0";
    TString title_e= detname[i] + " Energy distribution";
    TString title_a= detname[i] + " ADC distribution";
    TString title_a1= detname[i] +" ADC distribution (log10)";
    TString title_e1= detname[i] +" Energy distribution";
    Float_t rpi = M_PI + 0.00001; 
    mHits[i]   = new TH2F(name_h,title_h,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    mEnergyHist[i] = new TH2F(name_e,title_e,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    mAdc[i]    = new TH2F(name_a,title_a,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    mAdc1d[i]  = new TH1F(name_a1,title_a1,1000,0,8);   
    mEn1d[i]   = new TH1F(name_e1,title_e1,1000,-200,2000);   
  }
  for (Int_t i=0; i<MAXDET; i++) 
	{
		mGeo[i]=StEmcGeom::getEmcGeom(detname[i].Data());
    cout <<"det = "<<detname[i].Data()
         <<"  DeductPedestal = "<<mControlADCtoE->DeductPedestal[i]
         <<"  Calibration = "<<mControlADCtoE->Calibration[i]
         <<"  EnergyCutOff = "<<mControlADCtoE->EnergyCutOff[i]
         <<"  OnlyCalibrated = "<<mControlADCtoE->OnlyCalibrated[i]<<endl;
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
/*!
This method creates mean ADC and RMS histograms. It runs only in the end of the job
*/
Int_t StEmcADCtoEMaker::Finish()
{
  return kStOk;
}
//_____________________________________________________________________________
void StEmcADCtoEMaker::zeroAll()
{ 
  if(mDecoder) delete mDecoder;
  mDecoder=new StEmcDecoder(GetDate(),GetTime());
  mDb=NULL;
 	clearStEventStaf();
} 
//_____________________________________________________________________________
/*!
Process the event. Basicaly it get the status database and makes a loop over
EMC subdetectors. For each sub detector it gets the calibration tables, subtract
pedestals and apply calibration constants. In the end, update StEvent with
calibrated hits
*/
Int_t StEmcADCtoEMaker::Make()
{  
  TStopwatch clock;
  clock.Start();
  cout <<"\n\nStEmcADCtoEMaker::Make()******************************************************************\n";
  zeroAll(); 
	
	mData = new StBemcData(); 
	AddData(mData);
		
	Bool_t dataOk = getEmc();    
	
  if(dataOk) for(Int_t det=0;det<MAXDET;det++)
  {
		Bool_t present = kFALSE;
		if(det==0 && mData->TowerPresent) present = kTRUE;
		if(det==2 && mData->SMDPresent) present = kTRUE;
		if(det==3 && mData->SMDPresent) present = kTRUE;
		if(present && mControlADCtoE->Calibration[det]==1)
		{
			Bool_t ok = getStatus(det);
			if(ok)
			{
				Int_t nCh = 0;
				if(det==0) nCh = mData->NTowerHits;
				if(det==2) nCh = mData->NSmdHits;
				if(det==3) nCh = mData->NSmdHits;
				if(nCh>0) calibrate(det);				
			}
		}
  }
	fillHistograms();
	fillStEvent();
  
  clock.Stop();
  cout <<"Time to run StEmcADCtoEMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  cout <<"*******************************************************************************************\n\n\n";

  return kStOK;
}
//_____________________________________________________________________________
/*!
This method gets the status tables for a given detector and stores it in the
array mStatus[det][index].
*/
Bool_t StEmcADCtoEMaker::getStatus(Int_t det)
{
	mDb=NULL;
	TString DbName = "Calibrations/emc/y3"+detname[det];
  mDb=GetDataBase(DbName.Data());
  if(!mDb) {cout <<"StEmcADCtoEMaker::Make() - Can not get new status tables.... \n"; return kFALSE;}
  
	cout <<"Getting status table for detector "<<detname[det].Data()<<endl;
  TString TableName=detname[det]+"Status";
	
	Int_t NGOOD = 0; 
  
  if(det==0)
	{
		St_emcStatus* run = NULL;
		if(mDb) run = (St_emcStatus*)mDb->Find(TableName.Data());
		if(run)
		{
			emcStatus_st* runst=run->GetTable();
			for(Int_t i=1;i<=4800;i++) 
			{
				mData->TowerStatus[i-1] = runst[0].Status[i-1];
				if(mData->TowerStatus[i-1]==STATUS_OK) NGOOD++;
			}
		} 
	}
  
	if(det==2 || det==3)
  {
    St_smdStatus* run = NULL;
    if(mDb) run = (St_smdStatus*)mDb->Find(TableName.Data());
    if(run)
    {
      smdStatus_st* runst=run->GetTable(); 
      for(Int_t i=1;i<=18000;i++)
      {
        if(det==2) mData->SmdeStatus[i-1]=runst[0].Status[i-1];
				if(det==3) mData->SmdpStatus[i-1]=runst[0].Status[i-1];
				if(runst[0].Status[i-1]==STATUS_OK) NGOOD++;
      }
    }
  }
	cout <<"Detector: "<<detname[det].Data()<<"  STATUS_OK = "<<NGOOD<<"  channels\n";
  return kTRUE;

}
//_____________________________________________________________________________
/*!
This method gets EMC collection from DAQ dataset. It also gets the capacitor number
for SMD and saves it in the calibrationType member of StEmcRawHit. 
*/
Bool_t StEmcADCtoEMaker::getEmcFromDaq(TDataSet* daq)
{
  cout <<"***** Getting EMC event from daq file\n";

	StDAQReader* TheDataReader=(StDAQReader*)(daq->GetObject());
  if(!TheDataReader->EMCPresent()) return kFALSE;

  StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
  if(!TheEmcReader) return kFALSE;
	
	EMC_Reader* reader = TheEmcReader->getBemcReader();
	if(!reader) return kFALSE;
	 
  // tower data
	mData->TowerPresent = reader->isTowerPresent();
	if(reader->isTowerPresent())
	{
		Bank_BTOWERADCR& tower = reader->getBTOWERADCR();
		mData->EventNumber = tower.EventNumber;
		mData->TowerByteCount = tower.ReceivedByteCount;
		mData->NTowerHits = tower.NTowerHits;
		mData->TDCErrorFlag = tower.TDCErrorFlag;
		mData->NTDCChannels = tower.NTDCChannels;
		for(Int_t i=0;i<30;i++)
		{
			mData->TDCError[i] = tower.TDCError[i];
			mData->TDCToken[i] = tower.TDCToken[i];
			mData->TDCTrigger[i] = tower.TDCTrigger[i];
			mData->TDCCrateId[i] = tower.TDCCrateId[i];
			mData->TDCCount[i] = tower.TDCCount[i];
		}
		for(Int_t id=1;id<=4800;id++)
		{
			Int_t m,e,s;
			mGeo[0]->getBin(id,m,e,s);
			mData->TowerADC[id-1] = tower.TowerMatrix[m-1][e-1][s-1];
		}
	}	
	
	// smd data
	mData->SMDPresent=reader->isSmdPresent();
	if(reader->isSmdPresent())
	{
		Bank_BSMDADCR& smd =  reader->getSMD_ADCR();
		mData->SMDErrorFlag = smd.SMDErrorFlag;
		mData->SMDByteCount = smd.ReceivedByteCount;
		mData->NSmdHits = smd.NSmdHits;
		for(Int_t i=0;i<8;i++) mData->TimeBin[i]=(Char_t)smd.TimeBin[i];
		for(Int_t id=1;id<=18000;id++)
		{
			Int_t m,e,s;
			//smd-eta
			mGeo[2]->getBin(id,m,e,s);
			mData->SmdeADC[id-1] =  smd.SmdE_ADCMatrix[m-1][e-1];
			//smd-phi
			mGeo[3]->getBin(id,m,e,s);
			mData->SmdpADC[id-1] =  smd.SmdP_ADCMatrix[m-1][e-1][s-1];			
		}
	}
	mData->validateData();
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcADCtoEMaker::getEmcFromStEvent(StEmcCollection *emc)
{
	mData->NTowerHits = 0;
	mData->NSmdHits = 0;
	mData->ValidTowerEvent = kFALSE; 
	mData->TowerPresent = kFALSE;
	mData->ValidSMDEvent = kFALSE;
	mData->SMDPresent = kFALSE;
  if(!emc) return kFALSE;
  for(Int_t det=0;det<MAXDET;det++)
  {
    StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
    StEmcDetector* detector=emc->detector(id);
    if(detector)
    {
      for(UInt_t j=1;j<=120;j++)
      {
        StEmcModule* module = detector->module(j);
        if(module)
        {
          StSPtrVecEmcRawHit& rawHit=module->hits();
          for(UInt_t k=0;k<rawHit.size();k++)
          {
            Int_t m=rawHit[k]->module();
            Int_t e=rawHit[k]->eta();
            Int_t s=abs(rawHit[k]->sub());
            Short_t adc=(Short_t) rawHit[k]->adc();
       
            Int_t idh;
            mGeo[det]->getId(m,e,s,idh);
            if(det==0) 
						{
							mData->TowerADC[idh-1] = adc; 
							mData->ValidTowerEvent=kTRUE; 
							mData->TowerPresent = kTRUE;
							mData->NTowerHits++;
						}
						if(det==2) mData->SmdeADC[idh-1] = adc;
						if(det==3) mData->SmdpADC[idh-1] = adc;
            if(det==2 || det==3) 
            {
              Int_t RDO,index;
              mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
              mData->TimeBin[RDO]=rawHit[k]->calibrationType();
							mData->ValidSMDEvent=kTRUE;
							mData->SMDPresent=kTRUE;
							mData->NSmdHits++;
            }
          }
        }
      }
    } 
  }
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method gets EMC hits from different sources. First it looks for DAQ datasets.
if Not present, looks for StEvent hits to recalibrate.
*/
Bool_t StEmcADCtoEMaker::getEmc()
{  
// check if there is event from DAQ
	mData->EventDate = GetDate();
	mData->EventTime = GetTime();
	mFromDaq = kFALSE;
	
  TDataSet* TheData   = GetDataSet("StDAQReader");
  if(TheData) 
	{
  	mFromDaq = kTRUE;
		StDAQReader* TheDataReader=(StDAQReader*)(TheData->GetObject());
  	if(!TheDataReader) return kFALSE;
		mData->RunNumber = TheDataReader->getRunNumber();
		mData->TriggerWord = TheDataReader->getTrigWord();
		
		if(getEmcFromDaq(TheData)) return kTRUE; 
		return kFALSE;
	}
      
  // check if there is event from StEvent
  StEvent* event=NULL;
  if(!mEmbedd) event = (StEvent*)GetInputDS("StEvent");
  else 
  {
    StMaker *m = GetMaker("embedIO");
    if(!m) { cout<<"No embedIO maker"<<endl; return kFALSE; }
    event = (StEvent*)m->GetInputDS("StEvent");  
  }
  if(event)
	{
  	mData->RunNumber = event->runId();
		mData->TriggerWord = event->triggerMask();
		
		StEmcCollection* emctemp=event->emcCollection();
  	if(!emctemp)  return kFALSE;
  	if(getEmcFromStEvent(emctemp)) return kTRUE;
	}  
	
  return kFALSE;  
}
//_____________________________________________________________________________
/*!
This method applies the calibration constants to get the hit energy. The calibration
is applied only to the hits which tower/strip status is set to 1 (mStatus[det][index==1).
It also checks if the calibration is done for that bin
*/
Bool_t StEmcADCtoEMaker::calibrate(Int_t det)
{
  cout <<"Calibrating detector "<<detname[det].Data()<<endl;
	
	Bool_t Valid = kTRUE;
	if(det==0) Valid = mData->ValidTowerEvent;
	if(det==2 || det==3) Valid = mData->ValidSMDEvent;	
	if(!Valid) return kFALSE;
	
	mDb=NULL;
	TString DbName = "Calibrations/emc/y3"+detname[det];
  mDb=GetInputDB(DbName.Data());
  if(!mDb) return kFALSE;
  	  
	//getting tables ////////////////////////////////////////////////////
	emcCalib_st* emccalibst = NULL;
	smdCalib_st* smdcalibst = NULL;
	emcGain_st* emcgainst = NULL;
	smdGain_st* smdgainst = NULL;
	emcPed_st *emcpedst=NULL;
  smdPed_st *smdpedst=NULL;
	TString TableName;
	if(det<2)//bemc and bprs
	{
		TableName = detname[det]+"Calib";
		St_emcCalib* a = (St_emcCalib*) mDb->Find(TableName.Data());
		if(a) emccalibst = a->GetTable();
		if(!emccalibst) return kFALSE;
		
		TableName = detname[det]+"Gain";
		St_emcGain* b = (St_emcGain*) mDb->Find(TableName.Data());
		if(b) emcgainst = b->GetTable();
		
		if(mControlADCtoE->DeductPedestal[det]==1)
		{
			TableName = detname[det]+"Ped";
			St_emcPed* ped=(St_emcPed*)mDb->Find(TableName.Data());
    	if(ped) emcpedst=ped->GetTable();
    	if(!emcpedst) return kFALSE;
		}
	}
	else// SMD
	{
		TableName = detname[det]+"Calib";
		St_smdCalib* a = (St_smdCalib*) mDb->Find(TableName.Data());
		if(a) smdcalibst = a->GetTable();
		if(!smdcalibst) return kFALSE;
		
		TableName = detname[det]+"Gain";
		St_smdGain* b = (St_smdGain*) mDb->Find(TableName.Data());
		if(b) smdgainst = b->GetTable();
		
		if(mControlADCtoE->DeductPedestal[det]==1)
		{
			TableName = detname[det]+"Ped";
			St_smdPed* ped=(St_smdPed*)mDb->Find(TableName.Data());
    	if(ped) smdpedst=ped->GetTable();
    	if(!smdpedst) return kFALSE;
		}
	}
			
	//loop over all channels ///////////////////////////////////////////////
	Int_t MAX = 18000;
	if(det<2) MAX = 4800;
	Float_t TOTALE=0;
	Int_t   NHITS=0;
	Int_t cap = 0;
	for(Int_t id=1;id<=MAX;id++)
	{
		Float_t ADC = 0;
		Float_t PED = 0;
		Char_t status = 0;
		if(det==0) { ADC = (Float_t)mData->TowerADC[id-1]; status = mData->TowerStatus[id-1]; }
		if(det==2) { ADC = (Float_t)mData->SmdeADC[id-1];  status = mData->SmdeStatus[id-1];  }
		if(det==3) { ADC = (Float_t)mData->SmdpADC[id-1];  status = mData->SmdpStatus[id-1];  }
		if(mControlADCtoE->DeductPedestal[det]==1)
		{
      if(det<2) PED = ((Float_t)emcpedst[0].AdcPedestal[id-1])/100.;
			else
			{
      	Int_t RDO,index,m,e,s;
				mGeo[det]->getBin(id,m,e,s);
      	mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
      	Int_t capacitor=mData->TimeBin[RDO];
				cap = 0;
				if(capacitor == CAP1) cap = 1;
				if(capacitor == CAP2) cap = 2;
				PED = ((Float_t)smdpedst[0].AdcPedestal[id-1][cap])/100.;
		  }
		}
    if(status==STATUS_OK)
		{
			PED =(Float_t)((Int_t)(PED)); // calibration is done this way because of histograms
      Float_t ADCSUB=ADC-PED;
			Float_t EN=0;
			Float_t ADCPOWER=1;
			//calibrating ..............
      for(Int_t i=0;i<5;i++) 
      {
        Float_t c = 0;
				if(det<2) c = emccalibst[0].AdcToE[id-1][i];
				else      c = smdcalibst[0].AdcToE[id-1][i];
				//if(i==0 && det==0) c=emccalibst[0].AdcToE[id-1][0]-2.2e-4;
				EN+=c*ADCPOWER; 
        ADCPOWER*=ADCSUB;
      }
      //if(ADCSUB<0) EN*=-1;
			//correcting gain ..........
			Float_t gain = 1;
			if(det<2 && emcgainst) gain = emcgainst[0].Gain[id-1];
			if(det>2 && smdgainst) gain = smdgainst[0].Gain[id-1];
			EN*=gain;
			
			if(det==0) mData->TowerEnergy[id-1] = EN;
			if(det==2) mData->SmdeEnergy[id-1] = EN;     
			if(det==3) mData->SmdpEnergy[id-1] = EN;     
			if(saveHit(det,id)) 
			{
				TOTALE+=EN;
				NHITS++;
			  //if(det==3) cout <<"id = "<<id<<"  ADC = "<<ADC<<"  PED = "<<PED<<"  CAP = "<<cap<<"  ADCSUB = "<<ADCSUB<<"  E = "<<EN<<endl;
			}
		}
    else
    {
 			if(det==0) mData->TowerEnergy[id-1] = 0;
			if(det==2) mData->SmdeEnergy[id-1] = 0;     
			if(det==3) mData->SmdpEnergy[id-1] = 0;        
    }
	}
	cout <<"detector: "<<detname[det].Data()<<"  NHITS = "<<NHITS<<"  TOTALE = "<<TOTALE<<endl;
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method fills QA histograms
*/
Bool_t StEmcADCtoEMaker::fillHistograms()
{
  
  for(Int_t det=0;det<MAXDET;det++)
	{
		Int_t nHits = 0;
		Float_t totalE = 0;
		Float_t totalADC = 0;
		Int_t MAXCHANNEL = 18000;
		if(det<2) MAXCHANNEL = 4800;
		Bool_t valid = kTRUE;
		if(det==0) valid = mData->ValidTowerEvent;
		if(det==2) valid = mData->ValidSMDEvent;
		if(det==3) valid = mData->ValidSMDEvent;
		if(valid) mValidEvents->Fill(1,det+1); else mValidEvents->Fill(2,det+1);
		for(Int_t i=0;i<MAXCHANNEL;i++)
		{
			Char_t status = 0;
			if(det==0) status = mData->TowerStatus[i];
			if(det==2) status = mData->SmdeStatus[i];
			if(det==3) status = mData->SmdpStatus[i];
			if(status==STATUS_OK)
			{
				Float_t ADC = 0;
				Float_t E = 0;
				if(det==0) { ADC = (Float_t)mData->TowerADC[i]; E = mData->TowerEnergy[i]; mTower->Fill(i+1,ADC);}
				if(det==2) { ADC = (Float_t)mData->SmdeADC[i]; E = mData->SmdeEnergy[i]; }
				if(det==3) { ADC = (Float_t)mData->SmdpADC[i]; E = mData->SmdpEnergy[i]; }
				totalE+=E;
				totalADC+=ADC;
				nHits++;
				Float_t eta,phi;
				mGeo[det]->getEtaPhi(i+1,eta,phi);
				mHits[det]->Fill(eta,phi);
        mAdc[det]->Fill(eta,phi,ADC);
				mEnergyHist[det]->Fill(eta,phi,E);
			}
		}
		if(nHits>0)    mNhit->Fill((Float_t)nHits,(Float_t)det+1);
		if(totalE>0)   mEtot->Fill(log10(totalE),(Float_t)det+1);
		if(mEn1d[det]) mEn1d[det]->Fill(totalE);
		if(totalADC>0) mAdc1d[det]->Fill(log10(totalADC));
  	if(det==2) for(Int_t RDO=0;RDO<8;RDO++) mSmdTimeBinHist->Fill(RDO,mData->TimeBin[RDO]);
	}
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method makes a clean up of StEvent before store it in the .data
*/
Bool_t StEmcADCtoEMaker::fillStEvent()
{  
  // first need to clean hits with adc = 0
  StEvent* event=NULL;
  if(!mEmbedd) event = (StEvent*)GetInputDS("StEvent");
  else
  {
    StMaker *m = GetMaker("embedIO");
    if(!m) { cout<<"No embedIO maker"<<endl; return kFALSE; }
    event = (StEvent*)m->GetInputDS("StEvent");  
  }
  
  mEmc = NULL;	
	if(event) mEmc = event->emcCollection();
	if(mEmc) clearOldEmc();
  	
  for(Int_t det=0;det<MAXDET;det++) 
	{
		if(mControlADCtoE->Calibration[det]==1)
  	{
    	Bool_t Valid = kTRUE;
			Int_t NHITS = 0;
    	Int_t NGOOD = 0;
	  	if(det==0) Valid = mData->ValidTowerEvent;
	  	if(det==2 || det==3) Valid = mData->ValidSMDEvent;	
			if(Valid)
			{
				Int_t Max = 120*mGeo[det]->NEta()*mGeo[det]->NSub();    
    		// first check if there is at least one valid hit to save
    		Bool_t saveDet=kFALSE;
    		for(Int_t idh=1;idh<=Max;idh++) if(saveHit(det,idh)) { saveDet = kTRUE; break; }

    		if(saveDet)
    		{
      		if(!mEmc) 
					{
						mEmc =new StEmcCollection();
						if(event) event->setEmcCollection(mEmc);
					}
      		StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
      		StEmcDetector* detector=mEmc->detector(id);
					if(!detector)
					{
						detector = new StEmcDetector(id,120); 
						mEmc->setDetector(detector);
					}
    
      		for(Int_t idh=1;idh<=Max;idh++)
      		{      
        		if(saveHit(det,idh))
        		{
          		Int_t ADC = 0;
							Float_t E = 0;
							if(det==0) { ADC = mData->TowerADC[idh-1]; E = mData->TowerEnergy[idh-1]; }
							if(det==2) { ADC = mData->SmdeADC[idh-1]; E = mData->SmdeEnergy[idh-1]; }
							if(det==3) { ADC = mData->SmdpADC[idh-1]; E = mData->SmdpEnergy[idh-1]; }
          		Int_t m,e,s;
          		mGeo[det]->getBin(idh,m,e,s);
          
          		StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
          		hit->setEnergy(E);
          		if(det==2 || det==3) 
          		{
            		Int_t RDO,index;
            		mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
            		Int_t timeBin=mData->TimeBin[RDO];
            		hit->setCalibrationType(timeBin);
          		}
          		detector->addHit(hit);
							NHITS++;
            	if(E!=0) NGOOD++;
          	} 
      		}
    		}
			}
			cout <<"NHITS Saved on StEvent for detector "<<detname[det].Data()<<" = "<<NHITS<<"  GOOD = "<<NGOOD<<endl;
  	} 
	} 
  // finished clean up
  
  return kTRUE;
}
//_____________________________________________________________________________
/*!
Check if this hit is ok to be saved on StEvent
*/
Bool_t StEmcADCtoEMaker::saveHit(Int_t det,Int_t idh)
{  
  Int_t ADC = 0;
	Float_t E = 0;
	Char_t S = 0;
	if(det==0) {ADC = mData->TowerADC[idh-1]; E = mData->TowerEnergy[idh-1]; S = STATUS_OK; } // save all for towers
	if(det==2) {ADC = mData->SmdeADC[idh-1]; E = mData->SmdeEnergy[idh-1]; S = mData->SmdeStatus[idh-1]; }
	if(det==3) {ADC = mData->SmdpADC[idh-1]; E = mData->SmdpEnergy[idh-1]; S = mData->SmdpStatus[idh-1]; }
  
  Bool_t save = kTRUE;  
	if(S!=STATUS_OK) save = kFALSE;   
  if(mControlADCtoE->EnergyCutOff[det]>0) if(E<mControlADCtoE->EnergyCutOff[det]) save = kFALSE;
  
  return save;
}
//_____________________________________________________________________________
/*!
Clear old EMC stuff
*/
Bool_t StEmcADCtoEMaker::clearOldEmc()
{  
	if(!mEmc) return kFALSE;
	StSPtrVecEmcPoint& pvec = mEmc->barrelPoints();
  if(pvec.size()>0)  pvec.clear(); 
 
  for(Int_t i=0; i<4; i++)
  {
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=mEmc->detector(id);
    if(detector)
    {
      if(detector->cluster())
			{
      	StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
      	if(cluster.size()>0) cluster.clear();  
      }
			for(UInt_t j=1;j<=detector->numberOfModules() ;j++)
			{
				StEmcModule *module = detector->module(j);
				if(module)
				{
					StSPtrVecEmcRawHit&  hits=module->hits();
					hits.clear();
					//delete module;
				}
			}
			//delete detector;
    }
  }
	return kTRUE;
}


