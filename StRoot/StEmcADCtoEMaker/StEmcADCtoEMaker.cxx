// 
// $Id: StEmcADCtoEMaker.cxx,v 1.28 2002/02/24 21:19:21 suaide Exp $
// $Log: StEmcADCtoEMaker.cxx,v $
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
//#include "StChain.h"
//#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include <iostream.h>
#include <math.h>
#include "StMessMgr.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StEmcUtil/StEmcGeom.h"
#include "TStopwatch.h"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDaqLib/EMC/StEmcDecoder.h"

ClassImp(StEmcADCtoEMaker)

//_____________________________________________________________________________
/* 
Default constructor. Set Initial values for some variables
*/
StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name):StMaker(name)
{
  mControlADCtoE = new controlADCtoE_st();

  Int_t calib[]   = {1, 0, 1, 1, 0, 0, 0, 0};
  Int_t pedSub[]  = {1, 0, 1, 1, 0, 0, 0, 0};
  Int_t adcCut[]  = {-1, -1, 7, 7, -1, -1, -1, -1};
  Int_t onlyCal[] = {0, 0, 0, 0, 0, 0, 0, 0};
  
  for(Int_t i=0; i<MAXDET; i++)
  {
    mControlADCtoE->DeductPedestal[i]=pedSub[i];  
    mControlADCtoE->Calibration[i]=calib[i];
    mControlADCtoE->AdcCutOff[i]=adcCut[i];
    mControlADCtoE->OnlyCalibrated[i]=onlyCal[i];
    cout <<"det = "<<detname[i].Data()
         <<"  DeductPedestal = "<<mControlADCtoE->DeductPedestal[i]
         <<"  Calibration = "<<mControlADCtoE->Calibration[i]
         <<"  AdcCutOff = "<<mControlADCtoE->AdcCutOff[i]
         <<"  OnlyCalibrated = "<<mControlADCtoE->OnlyCalibrated[i]<<endl;
  } 

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
  
  gMessMgr->SetLimit("StEmcADCtoEMaker",1000);
  
  //Making QA histgrams
  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
 
  mNhit = new TH2F("EmcNHitsVsDet" ,"Number of hit(log) with energy > 0 .vs. Detector #",500,0.0,15.0,8,0.5,8.5);
  mEtot = new TH2F("EmcEtotVsDet" ,"Total energy(log) .vs. Detector #",500,-4.0,15.0,8,0.5,8.5);
 
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
    TString title_h= detname[i] + " Hits distribution with energy > 0";
    TString title_e= detname[i] + " Energy distribution";
    TString title_a= detname[i] + " ADC distribution";
    TString title_a1= detname[i] +" ADC distribution (log)";
    Float_t rpi = M_PI + 0.00001; 
    mHits[i]   = new TH2F(name_h,title_h,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    mEnergyHist[i] = new TH2F(name_e,title_e,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    mAdc[i]    = new TH2F(name_a,title_a,nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    mAdc1d[i]  = new TH1F(name_a1,title_a1,1000,0,8);
    
    // creating geometry ...
    mGeo[i]=StEmcGeom::getEmcGeom(detname[i].Data());
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
void StEmcADCtoEMaker::zeroAll()
{ 
  if(mDecoder) delete mDecoder;
  mDecoder=new StEmcDecoder(GetDate(),GetTime());
  mStatusDb=NULL;
  
  for(Int_t i=0;i<MAXDET;i++)
  {
    mNChannels[i]=0;
    for(Int_t j=0;j<18000;j++)
    {
      mStatus[i][j]=0;
      mADC[i][j]=0;
      mADCPedSub[i][j]=0;
      mEnergy[i][j]=0;
    }
  }
  for(Int_t i=0;i<8;i++) mSmdTimeBin[i]=0;
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

  mStatusDb=GetInputDB("Calibrations/emc/status");
  if(!mStatusDb) gMessMgr->Warning("StEmcADCtoEMaker::Make() - Can not get new status tables.... will use default ");
  for(Int_t det=0;det<MAXDET;det++) if(mControlADCtoE->Calibration[det]==1) getStatus(det);
  
  if(!getEmc())
  {
    gMessMgr->Warning("StEmcADCtoEMaker::Make() - Can not get pointer to EMC Event");
    return kStWarn;
  }
   
  // loop over detectors...
  for(Int_t det=0;det<MAXDET;det++)
  {
    if(mNChannels[det]>0)
    {
      cout <<"**** Calibrating detector "<<detname[det].Data()<<endl;
      Bool_t ok=kTRUE;
      
      cout <<"***** Getting calibration tables\n";            
      mCalibDb=NULL;
      TString DbName="Calibrations/emc/"+detname[det];      
      mCalibDb=GetInputDB(DbName.Data());
      if(!mCalibDb) 
      {
        gMessMgr->Warning("StEmcADCtoEMaker::Make() - Can not get new calib tables.... ");
        ok=kFALSE;       
      }
                              
      if(ok)
      {
        Int_t nhits=0;
        Float_t energy=0;
        Bool_t CalOk=calibrate(det,&nhits,&energy);
        if(CalOk) fillHistograms(det,nhits,energy);
        cout <<"***** NHits = "<<nhits<<"  E = "<<energy<<endl;
      }
    }
  }
    
  if(!fillStEvent()) gMessMgr->Warning("StEmcADCtoEMaker::Make() - No StEvent to save EmcCollection");
  
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
void StEmcADCtoEMaker::getStatus(Int_t det)
{
  cout <<"Getting status table for detector "<<detname[det].Data()<<endl;
  TString TableName=detname[det]+"Running";
  Int_t total=0;
  
  Int_t d = GetDate();
  //Int_t t = GetTime();
  
  if(det<2) // bemc and bprs
  {
    total = 4800;
    St_emcRunning* run = NULL;
    if(mStatusDb) run = (St_emcRunning*)mStatusDb->Find(TableName.Data());
    if(run)
    {
      cout <<"Got status table from DataBase\n";
      emcRunning_st* runst=run->GetTable(); 
      for(Int_t i=1;i<=4800;i++) 
      {
        mStatus[det][i-1]=(Int_t) runst[0].IsRunning[i-1];
        if(mStatus[det][i-1]==1) mNChannels[det]++;
      }
    } 
    else  // standard status table for bemc
    {
      if(det==0)
      {
        cout <<"Using bemc default status table\n ";
        for(Int_t i=1;i<=4800;i++)
        {
          mStatus[det][i-1]=0;
          if (i>=1861 && i<=2340) mStatus[det][i-1]=1; // initial 2001 configuration
          if (i>=2021 && i<=2100) mStatus[det][i-1]=0; // initial 2001 configuration
          if (i>=1861 && i<=2020)  // remove crate 0x12
          {
            if(d>=20011015 && d<=20011020) mStatus[det][i-1]=0;
          }
          if(mStatus[det][i-1]==1) mNChannels[det]++;

        }

        mStatus[det][2309-1]=0;
        mStatus[det][1986-1]=0;
        mStatus[det][1979-1]=0;
        mNChannels[det]-=3;
      }
    }
  }
  else
  {
    total = 18000;
    St_smdRunning* run = NULL;
    if(mStatusDb) run = (St_smdRunning*)mStatusDb->Find(TableName.Data());
    if(run)
    {
      smdRunning_st* runst=run->GetTable(); 
      for(Int_t i=1;i<=18000;i++) 
      {
        mStatus[det][i-1]=(Int_t)runst[0].IsRunning[i-1];
        if(mStatus[det][i-1]==1) mNChannels[det]++;
      }
    }
  }
  cout <<"Total number of channels = "<<total<<"  active = "<<mNChannels[det]<<endl;
  return;

}
//_____________________________________________________________________________
/*!
This method gets EMC collection from DAQ dataset. It also gets the capacitor number
for SMD and saves it in the calibrationType member of StEmcRawHit. 
*/
Bool_t StEmcADCtoEMaker::getEmcFromDaq(TDataSet* daq)
{
  cout <<"***** Getting EMC event from daq file\n";
  UInt_t eta[]={20,20,150,10};
  UInt_t sub[]={2,2,1,15};

  cout <<"***** Getting Daq Reader\n";
  StDAQReader* TheDataReader=(StDAQReader*)(daq->GetObject());
  if(!TheDataReader) return kFALSE;
  if(!TheDataReader->EMCPresent()) return kFALSE;

  cout <<"***** Getting point to EMC data bank\n";
  StEMCReader* TheEmcReader=TheDataReader->getEMCReader();
  if(!TheEmcReader) return kFALSE;
  
  cout <<"***** Loop over detectors\n";
  for(Int_t det=0;det<4;det++) if(mControlADCtoE->Calibration[det]==1) 
  {
    Float_t sum=0,validChannels=0;    
    Bool_t Ok=kTRUE;
    
    if(det==0) // check if data is Ok for bemc
    {
      cout <<"EMC VALID TOWER HITS = "<<TheEmcReader->NTowerHits()<<" OFFLINE ACTIVE CHANNELS = "<<mNChannels[det]<<endl;
      if(TheEmcReader->NTowerHits()==0) Ok=kFALSE;
    }
      
    if(det==2 || det ==3) // get time bin
    {
      unsigned int timebin;
      for(Int_t RDO=0;RDO<8;RDO++)
      {
        TheEmcReader->getSMD_TIMEBIN(RDO,timebin);
        if(timebin>255) timebin=255;
        mSmdTimeBin[RDO]=(Int_t)timebin;
      }
    }
    
    if(Ok)
    {
      for(UInt_t m=1;m<=120;m++)
        for(UInt_t e=1;e<=eta[det];e++)
          for(UInt_t s=1;s<=sub[det];s++)
          {
            unsigned short adc=0;
            if(det==0) if(!TheEmcReader->getTowerADC((int)m,(int)e,(int)s,adc)) goto next;
            if(det==2) if(!TheEmcReader->getSMDE_ADC((int)m,(int)e,adc)) goto next;
            if(det==3) if(!TheEmcReader->getSMDP_ADC((int)m,(int)e,(int)s,adc)) goto next;
            Int_t idh;
            mGeo[det]->getId(m,e,s,idh);
            //cout <<"m = "<<m<<"  e = "<<e<<"  s = "<<s<<"  adc = "<<ADC<<endl;
            if(adc>0)
            {
              sum+=(Float_t)adc;
              validChannels++;
              mADC[det][idh-1] = (Float_t)adc;
            }
            next: continue;
          }
      cout <<"Total ADC sum = "<<sum<<"  validChannels = "<<validChannels<<endl;
    }
    else
    {
      cout <<"***** BAD event for detector "<<detname[det].Data()<<endl;
      mNChannels[det]=0;
    }
  }
  return kTRUE;
}
Bool_t StEmcADCtoEMaker::getEmcFromStEvent(StEmcCollection *emc)
{
  if(!emc) return kFALSE;
  for(Int_t det=0;det<MAXDET;det++) if(mControlADCtoE->Calibration[det]==1)
  {
    StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
    StEmcDetector* detector=emc->detector(id);
    if(detector)
    {
      Float_t sum=0,validChannels=0;    
      for(UInt_t j=1;j<121;j++)
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
            Float_t adc=(Float_t) rawHit[k]->adc();
       
            Int_t idh;
            mGeo[det]->getId(m,e,s,idh);
            mADC[det][idh-1]=adc;
            sum+=adc;
            validChannels++;
            if(det==2 || det==3) 
            {
              Int_t RDO,index;
              mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
              mSmdTimeBin[RDO]=rawHit[k]->calibrationType();
            }
          }
        }
      }
      if(validChannels==0) mNChannels[det]=0;
    } else mNChannels[det]=0;
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
  TDataSet* TheEmcData   = GetDataSet("StDAQReader");
  if(TheEmcData)
  {
    if(!getEmcFromDaq(TheEmcData)) return kFALSE;
    return kTRUE;
  }
      
  // check if there is event from StEvent
  cout <<"Trying to get event from StEvent\n";
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) return kFALSE;
  StEmcCollection* emctemp=event->emcCollection();
  if(!emctemp)  return kFALSE;
  if(getEmcFromStEvent(emctemp)) return kTRUE;
  
  return kFALSE;
  
}
//_____________________________________________________________________________
/*!
This method gets the pedestal table for a given detector from database and subtract 
pedestal values from data if required.
*/
Bool_t StEmcADCtoEMaker::subtractPedestal(Int_t det)
{
  cout <<"getting pedestal tables for "<<detname[det].Data()<<endl;
  TString TableName=detname[det]+"Pedestal";
  emcPedestal_st *emcpedst=NULL;
  smdPedestal_st *smdpedst=NULL;
   
  if(det<2) //bemc and bprs
  {
    St_emcPedestal* ped=(St_emcPedestal*)mCalibDb->Find(TableName.Data());
    if(!ped) return kFALSE;
    emcpedst=ped->GetTable();
    if(!emcpedst) return kFALSE;
  }
  else // SMD
  {
    St_smdPedestal* ped=(St_smdPedestal*)mCalibDb->Find(TableName.Data());
    if(!ped) return kFALSE;
    smdpedst=ped->GetTable();
    if(!smdpedst) return kFALSE;  
  }
    
  Int_t nMax=120*mGeo[det]->NEta()*mGeo[det]->NSub();
  for(Int_t id=1;id<=nMax;id++)
  {
    Float_t pedestal=0;
        
    if(det<2) //bemc and bprs
      pedestal=emcpedst[id-1].AdcPedestal;
    else // SMD
    {
      Int_t m,e,s;
      mGeo[det]->getBin(id,m,e,s);
      Int_t RDO,index;
      mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
      Int_t timeBin=mSmdTimeBin[RDO];
      if(timeBin>=0 && timeBin<128) pedestal=smdpedst[id-1].AdcPedestal[timeBin];
    }
      
    if(mADC[det][id-1]>0) 
    {
      //cout <<"id = "<<id<<"  ADC = "<<ADCTemp[id-1]<<"  ped = "<<pedestal;
      mADCPedSub[det][id-1]=mADC[det][id-1]-pedestal;
      if(mADCPedSub[det][id-1]<0) mADCPedSub[det][id-1]=0;
      //cout <<"  ADCSub = "<<ADCTemp[id-1]<<endl;
    }
    else mADCPedSub[det][id-1]=0;
  }  
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method applies the calibration constants to get the hit energy. The calibration
is applied only to the hits which tower/strip status is set to 1 (mStatus[det][index==1).
It also checks if the calibration is done for that bin
*/
Bool_t StEmcADCtoEMaker::calibrate(Int_t det,Int_t* NHITS,Float_t* ENERGY)
{
  cout <<"applying calibration for detector "<<detname[det].Data()<<endl;
  Int_t nhits=0;
  Float_t totalenergy=0;

  if(mControlADCtoE->DeductPedestal[det]==1)  subtractPedestal(det);
  else for(Int_t i=0;i<18000;i++)  mADCPedSub[det][i] = mADC[det][i];

    
  cout <<"getting calibration table \n";
  TString TableName=detname[det]+"Calibration";
  St_emcCalibration* cal=(St_emcCalibration*)mCalibDb->Find(TableName.Data());
  if(!cal) return kFALSE;

  emcCalibration_st* cal_st=cal->GetTable();
  if(!cal_st) return kFALSE;

  cout <<"applying calibration... loop over modules \n";
  Int_t nMax=120*mGeo[det]->NEta()*mGeo[det]->NSub();
  for(Int_t id=1;id<=nMax;id++)
  {
    Float_t adc=mADCPedSub[det][id-1];
    Float_t energy=0;
    if(adc>0 && 
       mStatus[det][id-1]==1 && 
       cal_st[id-1].Status==1 && 
       cal_st[id-1].CalibStatus==1)
    {
      Float_t adcpower=1;
      for(Int_t i=0;i<5;i++) 
      {
        energy+=cal_st[id-1].AdcToE[i]*adcpower; 
        adcpower*=adc;
      }            
      if (energy<0) energy=0;
      nhits++;
      //cout <<"id = "<<id<<" ADC = "<<mADC[det][id-1]<<"  ADC ped Sub = "<<adc<<"  energy = "<<energy<<endl;
      totalenergy+=energy;
    }
    mEnergy[det][id-1] = energy;
  }
       
  *NHITS=nhits;
  *ENERGY=totalenergy;
  return kTRUE;
}
//_____________________________________________________________________________
/*!
This method fills QA histograms
*/
Bool_t StEmcADCtoEMaker::fillHistograms(Int_t det,Int_t nhits,Float_t energy)
{
  cout <<"***** Filling histograms for detector "<<detname[det].Data()<<endl;
  if(nhits>0)  mNhit->Fill(log((Float_t)nhits),(Float_t)det+1);
  if(energy>0) mEtot->Fill(log(energy),(Float_t)det+1);
        
  if(det==2) // fill time bin for SMDE only becasue they are the same for SMDP
  {
    for(Int_t RDO=0;RDO<8;RDO++) if(mSmdTimeBin[RDO]<128) mSmdTimeBinHist->Fill(RDO,mSmdTimeBin[RDO]);
  }
  
  if(det==0) // gain monitor histogram for tower
  {
    for(Int_t i=0;i<4800;i++) if(mADCPedSub[0][i]>0 && mADCPedSub[0][i]<500) mTower->Fill(i+1,mADCPedSub[0][i]);
  }
  
  if(mControlADCtoE->Calibration[det]==1)  
  {
    Float_t totalAdc=0;
    Int_t Max = 120*mGeo[det]->NEta()*mGeo[det]->NSub();
    for(Int_t idh=1;idh<=Max;idh++)
    {
      Float_t eta,phi;
      mGeo[det]->getEtaPhi(idh,eta,phi);
      Int_t ADC = (Int_t)mADC[det][idh-1];
      Float_t Energy = mEnergy[det][idh-1];
      if(ADC>0)
      {
        mHits[det]->Fill(eta,phi);
        mAdc[det]->Fill(eta,phi,ADC);
        totalAdc+=(Float_t)ADC;
      }
      if(Energy>0) mEnergyHist[det]->Fill(eta,phi,Energy);
    }
    if(totalAdc>0) mAdc1d[det]->Fill(log(totalAdc));
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
  
  mEmc = NULL;
  
  for(Int_t det=0;det<MAXDET;det++) if(mControlADCtoE->Calibration[det]==1 && mNChannels[det]>0)
  {
    Int_t totalhits=0,totalhitsused=0;
    Int_t Max = 120*mGeo[det]->NEta()*mGeo[det]->NSub();
    
    // first check if there is at least one valid hit to save
    Bool_t saveDet=kFALSE;
    for(Int_t idh=1;idh<=Max;idh++) if(saveHit(det,idh)) { saveDet = kTRUE; break; }

    if(saveDet)
    {
      if(!mEmc) mEmc =new StEmcCollection();
      StDetectorId id = static_cast<StDetectorId>(det+kBarrelEmcTowerId);
      StEmcDetector* detector = new StEmcDetector(id,120);
    
      for(Int_t idh=1;idh<=Max;idh++)
      {      
        if(saveHit(det,idh))
        {
          Int_t ADC = (Int_t)mADC[det][idh-1];
          Float_t Energy = mEnergy[det][idh-1];
          Int_t m,e,s;
          mGeo[det]->getBin(idh,m,e,s);
          
          StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
          hit->setEnergy(Energy);
          if(det==2 || det==3) 
          {
            Int_t RDO,index;
            mDecoder->GetSmdRDO(det+1,m,e,s,RDO,index);
            Int_t timeBin=mSmdTimeBin[RDO];
            hit->setCalibrationType(timeBin);
          }
          detector->addHit(hit);
          totalhitsused++;
          if(ADC>0) totalhits++;
         }
      }
      mEmc->setDetector(detector);
    }
    cout <<"Total hits for detector "<<detname[det].Data()<<" = "<<totalhits<<"  after clean up = "<<totalhitsused<<endl;
  }  
  // finished clean up
  
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) return kFALSE;
  StEmcCollection* emctemp=event->emcCollection();
  if(mEmc) event->setEmcCollection(mEmc);
  if(emctemp) delete emctemp;
  return kTRUE;
}
//_____________________________________________________________________________
/*!
Check if this hit is ok to be saved on StEvent
*/
Bool_t StEmcADCtoEMaker::saveHit(Int_t det,Int_t idh)
{  
  Int_t ADC = (Int_t)mADC[det][idh-1];
  Int_t ADCPedSub = (Int_t)mADCPedSub[det][idh-1];
  Float_t Energy = mEnergy[det][idh-1];
  
  Bool_t save = kTRUE;
      
  if(ADC<=0) save = kFALSE;
  if(mControlADCtoE->AdcCutOff[det]!=-1) if(ADCPedSub<mControlADCtoE->AdcCutOff[det]) save = kFALSE;
  if(mControlADCtoE->OnlyCalibrated[det]==1) if(Energy<=0) save = kFALSE;
  
  return save;
}






