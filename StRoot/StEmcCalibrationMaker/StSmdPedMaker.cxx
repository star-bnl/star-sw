/**********************************************************************
* StSmdPedMaker
* Author: Alexandre A. P. Suaide 
*
* This maker does calibration on the EMC detector
***********************************************************************/

#include "StSmdPedMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "TStopwatch.h"
#include <fstream.h>
#include "TFile.h"
#include "time.h"
#include "TDatime.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"

#include "tables/St_smdPed_Table.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

ClassImp(StSmdPedMaker);

//_____________________________________________________________________________
StSmdPedMaker::StSmdPedMaker(const char *name):StMaker(name)
{
  mPedDate = 0;
  mPedTime = 0;
  mPedInterval = 6; //in hours
  mMinEvents = 10000;
  mSaveToDB = kFALSE;
  mFakeRun = kFALSE;
  mMaxMultiplicity = 10;
  mGeo[0] = StEmcGeom::instance("bsmde");
  mGeo[1] = StEmcGeom::instance("bsmdp");
  mCapacitor = new TH2F("capacitor","Capacitor distributionn", 8,0,8,128,0,128);
}
//_____________________________________________________________________________
StSmdPedMaker::~StSmdPedMaker()
{
  if(mCapacitor) delete mCapacitor;
}
//_____________________________________________________________________________
Int_t StSmdPedMaker::Init()
{
  ZeroAll();
	return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSmdPedMaker::Make()
{
  cout <<"*** SMD Pedestal Maker\n";
  cout <<"Event Date = "<<GetDate()<<"  Time = "<<GetTime()<<endl;
	TStopwatch clock;
  clock.Start();
		
	if(!GetEvent()) return kStWarn;
	Int_t date = GetDate();
  Int_t time = GetTime();
  
  if(mPedStatus==-1)
  {
    Float_t dt = GetDeltaTime(date,time,mPedDate,mPedTime);
    if(dt>mPedInterval) 
    { 
      mPedDate = date; mPedTime = time; mPedStatus = 0;
      cout <<"New pedestal round is starting for SMD...\n"; 
    }
    else 
    {
      cout <<"last pedestal time for SMD = "<<mPedDate<<"  "<<mPedTime<<endl;
      cout <<"time left for new pedestal round for SMD = "<<mPedInterval-dt<<" hours"<<endl;
    }
  }
  if(mPedStatus>=0)
  {
    FillPedestal();
    cout <<"Number of events processed = "<<mNEvents<<endl;
    if(mNEvents>mMinEvents) mPedStatus=1;
    if(mPedStatus==1)
    {
      CalculatePedestals();
      SavePedestals(mPedDate,mPedTime);
      ZeroAll();
    }
  }

  clock.Stop();
  cout <<"Time to run StSmdPedMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
	
  return kStOK;
}
//_____________________________________________________________________________
Int_t StSmdPedMaker::Finish()
{
  return kStOk;
}
//_____________________________________________________________________________
Bool_t StSmdPedMaker::GetEvent()
{  
  StEvent *Event=(StEvent*)GetInputDS("StEvent");
  if(!Event) return kFALSE;
  mEmc=Event->emcCollection();
  if(!mEmc) return kFALSE;    	
  if(!mFakeRun)
  {
	  StL0Trigger* trg = Event->l0Trigger();
	  Int_t trigger=8192;
	  if(trg) trigger = trg->triggerWord();
	  //if(trigger!=0x2001 && trigger!=0x2002) return kFALSE;
    
    if(Event->l3Trigger())
    {
      StSPtrVecTrackNode& tracks=Event->l3Trigger()->trackNodes();
      Int_t NTracks=tracks.size();
      if(NTracks>mMaxMultiplicity) return kFALSE;        
    }
  }
	return kTRUE;
}
//_____________________________________________________________________________
void StSmdPedMaker::FillPedestal()
{ 
  StEmcDecoder *decoder = new StEmcDecoder(GetDate(),GetTime());
  Int_t CAP[8];
  for(Int_t i=0;i<8;i++) CAP[i]=-1;
  Bool_t ok = kFALSE;
	Float_t SMDESUM=0;
	Float_t SMDPSUM=0;
  for(Int_t i=0; i<2; i++)
  {  
    StDetectorId id = static_cast<StDetectorId>(i+2+kBarrelEmcTowerId);
    StEmcDetector* detector=mEmc->detector(id);
    if(detector) for(UInt_t j=1;j<121;j++)
    {
      ok = kTRUE;
      StEmcModule* module = detector->module(j);
      StSPtrVecEmcRawHit& rawHit=module->hits();
      for(Int_t k=0;k<rawHit.size();k++)
      {
        Int_t m = rawHit[k]->module();
        Int_t e = rawHit[k]->eta();
        Int_t s = abs(rawHit[k]->sub());
        Float_t adc = (Float_t)rawHit[k]->adc();
        Int_t cap = rawHit[k]->calibrationType();
        Int_t id;
        mGeo[i]->getId(m,e,s,id);
        Int_t c = 0;
        if(cap==124) c=1;
        if(cap==125) c=2;
        Int_t RDO,INDEX;
        decoder->GetSmdRDO(i+3,m,e,s,RDO,INDEX);
        CAP[RDO]=cap;
        if(adc>0)
        {
          mSmdPedX[i][c][id-1]+=adc;
          mSmdPedX2[i][c][id-1]+=adc*adc;
          mSmdPedSum[i][c][id-1]++;
					if(i==0) SMDESUM+=(Float_t)adc;
					if(i==1) SMDPSUM+=(Float_t)adc;
        }
      }
    }
  }
	cout <<"Total SMDE sum = "<<SMDESUM<<"  SMDP sum = "<<SMDPSUM<<endl;
  delete decoder;
  if(ok)
  {
    for(Int_t i=0;i<8;i++) if(CAP[i]!=-1) mCapacitor->Fill((Float_t)i,(Float_t)CAP[i]);
    mNEvents++;
  }
} 
//_____________________________________________________________________________
void StSmdPedMaker::CalculatePedestals()
{  
  cout <<"Calculating SMD pedestals\n";
  for(Int_t i=0;i<2;i++)
    for(Int_t j=0;j<3;j++)
      for(Int_t k=0;k<18000;k++)
      if(mSmdPedSum[i][j][k]>0)
      {
        mSmdPed[i][j][k] = mSmdPedX[i][j][k]/mSmdPedSum[i][j][k];
        mSmdRMS[i][j][k] = sqrt(mSmdPedX2[i][j][k]/mSmdPedSum[i][j][k]-mSmdPed[i][j][k]*mSmdPed[i][j][k]);
      }
}
//_____________________________________________________________________________
void StSmdPedMaker::ZeroAll()
{  
  for(Int_t i=0;i<2;i++)
    for(Int_t j=0;j<3;j++)
      for(Int_t k=0;k<18000;k++)
      {
        mSmdPedX[i][j][k] = 0;
        mSmdPedX2[i][j][k] = 0;
        mSmdPedSum[i][j][k] = 0;
        mSmdPed[i][j][k] = 0;
        mSmdRMS[i][j][k] = 0;
      }
  mNEvents = 0;
 	mPedStatus=-1;
}
//_____________________________________________________________________________
Float_t StSmdPedMaker::GetDeltaTime(Int_t date, Int_t time, Int_t date0, Int_t time0)
{
  struct tm t0;
  struct tm t1;
    
  Int_t year  = (Int_t)(date/10000);
  t0.tm_year=year-1900;
  Int_t month = (Int_t)(date-year*10000)/100; 
  t0.tm_mon=month-1;
  Int_t day   = (Int_t)(date-year*10000-month*100);
  t0.tm_mday=day;
  Int_t hour  = (Int_t)(time/10000);
  t0.tm_hour=hour;
  Int_t minute= (Int_t)(time-hour*10000)/100;
  t0.tm_min=minute;
  Int_t second= (Int_t)(time-hour*10000-minute*100);
  t0.tm_sec=second;
  t0.tm_isdst = -1;
  
  year  = (Int_t)(date0/10000);
  t1.tm_year=year-1900;
  month = (Int_t)(date0-year*10000)/100; 
  t1.tm_mon=month-1;
  day   = (Int_t)(date0-year*10000-month*100);
  t1.tm_mday=day;
  hour  = (Int_t)(time0/10000);
  t1.tm_hour=hour;
  minute= (Int_t)(time0-hour*10000)/100;
  t1.tm_min=minute;
  second= (Int_t)(time0-hour*10000-minute*100);
  t1.tm_sec=second;
  t1.tm_isdst = -1;

  time_t ttime0=mktime(&t0);
  time_t ttime1=mktime(&t1);
  double diff=difftime(ttime0,ttime1);
  
  //cout <<"startdate = "<<startDate<<" now = "<<d<<"  dt = "<<diff/60<<endl;
  //cout <<diff<<"  "<<diff/60.<<endl;
  Float_t ddd=diff/3600.;
  return ddd;

}
//_____________________________________________________________________________
void StSmdPedMaker::SavePedestals(Int_t date, Int_t time)
{
	TDatime *tt = new TDatime(date,time);
  TString timestamp = tt->AsSQLString();
  delete tt;
  
  char file[100];
  sprintf(file ,"SmdPedestal.%08d.%06d.data.root",date,time);
  TFile *f=new TFile(file,"RECREATE");
  for(Int_t i=0;i<2;i++)
  {
    TString detname0 = "Bsmde";
    if(i==1) detname0 = "Bsmdp";
    
    for(Int_t k=0;k<3;k++)
    {
      char tmp[20];
      sprintf(tmp,"%s-%i-",detname0.Data(),k);
      TString detname = tmp;
      
      TString name = detname+"Pedestals";
      TString title = detname+" pedestals values";
      TH1F *h1=new TH1F(name.Data(),title.Data(),18000,1.0,18001.);
    
      name = detname+"RMS";
      title = detname+" RMS values";
      TH1F *h2=new TH1F(name.Data(),title.Data(),18001,1.0,18001.);
    
      name = detname+"Status";
      title = detname+" Status values";    
      TH1F *h4=new TH1F(name.Data(),title.Data(),18001,1.0,18001.);
    
	    for(Int_t j=0;j<18000;j++)
	    {
        Float_t ped  = mSmdPed[i][k][j];
        Float_t rms  = mSmdRMS[i][k][j];
        Float_t status=1;
        if(k==0 && (ped==0 || rms ==0)) status =0;
        h1->Fill((Float_t)j+1,ped);
        h2->Fill((Float_t)j+1,rms);
        h4->Fill((Float_t)j+1,status);
      }
      h1->Write();
      h2->Write();
      h4->Write();
      delete h1; delete h2;  delete h4;
    }
  }
  mCapacitor->Write();
  f->Close();
  delete f;
  mCapacitor->Reset();
  
  
  if(!mSaveToDB) return;         
  StDbManager* mgr=StDbManager::Instance();
	StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
		  
  TString tn[2];
  tn[0]="bsmdePed";
  tn[1]="bsmdpPed";
  	
  for(Int_t i=0;i<2;i++)
  {
    smdPed_st tnew;
	  for(Int_t j=0;j<18000;j++)
	  {
      Int_t status = 1;
      for(Int_t k = 0;k<3;k++)
      {
        short int ped = (short int)(100*mSmdPed[i][k][j]);
        short int rms = (short int)(100*mSmdRMS[i][k][j]);
        if(k==0 && (ped==0 || rms==0)) status =0;
        tnew.AdcPedestal[j][k] = ped;
        tnew.AdcPedestalRMS[j][k] = rms;
      }
      tnew.Status[j] = (char) status;
	  }		
		/////////////////////
	  StDbTable* tab=node->addDbTable(tn[i].Data());
	  tab->SetTable((char*)&tnew,1);
	  mgr->setStoreTime(timestamp.Data());
	  //mgr->setStoreTime("2003-01-01 00:00:00");
	  mgr->storeDbTable(tab);
  }
}





