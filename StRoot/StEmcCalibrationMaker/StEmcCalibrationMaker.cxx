/**********************************************************************
* StEmcCalibrationMaker
* Author: Alexandre A. P. Suaide 
*
* This maker does calibration on the EMC detector
***********************************************************************/

#include "StEmcCalibrationMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "TStopwatch.h"
#include <fstream.h>
#include "TFile.h"
#include "StEmcUtil/StEmcGeom.h"
#include "StEmcUtil/StEmcFilter.h"
#include "StEmcUtil/StEmcPosition.h"
#include "StMessMgr.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include "tables/St_MagFactor_Table.h"
#include "stdlib.h"
#include "StThreeVectorD.hh"
#include "time.h"
#include "TDatime.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"

#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include "tables/St_emcCalib_Table.h"
#include "tables/St_smdCalib_Table.h"
#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"

#ifndef ST_NO_NAMESPACES
using units::tesla;
#endif

ClassImp(StEmcCalibrationMaker);

//_____________________________________________________________________________
StEmcCalibrationMaker::StEmcCalibrationMaker(const char *name):StMaker(name)
{
  mFilter = new StEmcFilter();
	mPosition = new StEmcPosition();
	
	mDetName="bemc"; mDetNum=0;
	
	mDoUseL3=kFALSE;
	mDoEqual=kTRUE;
  mDoMip=kTRUE;  
  mDoGain = kTRUE;
  mGainMode = 1;  // 0 = daily basis, 1 = statistics basis
  
  mNEtaBins = 5;
  mEtaBinSize = 4;
    
	mSubPedestal=kTRUE;
	mZVertexMax=40;
	mEvNumber=0;
	mFirstEventDate=29990101;
  
  mEoverMIP = 0.261;
  mSaveCalibToDB = kFALSE;
  
  mWaitForPed = kFALSE;
  mUseLocalPed = kFALSE;
  mSavePedToDB = kFALSE;
  mDoPed = kTRUE;
  mPedDate = 0;
  mPedTime = 0;
  mPedInterval = 6; //in hours
}
//_____________________________________________________________________________
StEmcCalibrationMaker::~StEmcCalibrationMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Init()
{
  mNBins=4800;
	if(mDetNum>1) mNBins=18000;
	Float_t x = (Int_t)mNBins+1;
	mHitsAdc = new TH1F("mHitsAdc","mHitsAdc",mNBins,1,x);
	mHitsE   = new TH1F("mHitsE","mHitsE",mNBins,1,x);
	mIsOnOff = new TH1F("mIsOnOff","mIsOnOff",mNBins,1,x);
	mCalib   = new TH2F("mCalib","mCalib",mNBins,1,x,10,0,10);
	mEqual   = new TH2F("mEqual","mEqual",mNBins,1,x,10,0,10);
	
	mCalibGeo=StEmcGeom::instance(mDetName.Data());
	
	if(mDoPed)
  {
 		mPedStatus=-1;
		mPedSpec = new StEmcPedSpectra(mDetName.Data());
		mPedSpec->SetIsOnOff(mIsOnOff);
    mPedSpec->SetMinHits(300);
		mPedSpec->Init();    
  } 
  if(mDoEqual) 
	{
		mEqStatus=0;
		mEqualSpec = new StEmcEqualSpectra(mDetName.Data());
		mEqualSpec->SetEqual(mEqual);
		mEqualSpec->SetCalib(mCalib);
		mEqualSpec->SetIsOnOff(mIsOnOff);
		mEqualSpec->SetDoUseL3(mDoUseL3);
		mEqualSpec->SetFilter(mFilter);
		mEqualSpec->SetPosition(mPosition);
    mEqualSpec->SetNEtaBins(mNEtaBins);
    mEqualSpec->SetEtaBinWidth(mEtaBinSize);
		mEqualSpec->Init();
	}
  if(mDoGain)
  {
    mGainStatus = 0;
    mGainSpec = new StEmcEqualSpectra(mDetName.Data());
		mGainSpec->SetEqual(mEqual);
		mGainSpec->SetCalib(mCalib);
		mGainSpec->SetIsOnOff(mIsOnOff);
		mGainSpec->SetDoUseL3(mDoUseL3);
		mGainSpec->SetFilter(mFilter);
		mGainSpec->SetPosition(mPosition);
    mGainSpec->SetNEtaBins(mNEtaBins);
    mGainSpec->SetEtaBinWidth(mEtaBinSize);
    mGainSpec->SetMinHits(1000);
		mGainSpec->Init();
  }
	if(mDoMip)
  {
    mMipStatus = 0;
    mMipSpec = new StEmcMipSpectra(mDetName.Data());
		mMipSpec->SetEqual(mEqual);
		mMipSpec->SetCalib(mCalib);
		mMipSpec->SetIsOnOff(mIsOnOff);
		mMipSpec->SetDoUseL3(mDoUseL3);
		mMipSpec->SetFilter(mFilter);
		mMipSpec->SetPosition(mPosition);
    mMipSpec->SetNEtaBins(mNEtaBins);
    mMipSpec->SetEtaBinWidth(mEtaBinSize);
    mMipSpec->SetMaxMultiplicity(1000);
    mMipSpec->SetMinMomentum(1.2);
		mMipSpec->Init();    
  }
	SetStatus();
  mGain = kTRUE;
	return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Make()
{
  cout <<"*** BEMC Calibration Maker\n";
  cout <<"Event Date = "<<GetDate()<<"  Time = "<<GetTime()<<endl;
	TStopwatch clock;
  clock.Start();
		
	if(!GetEvent()) return kStWarn;
	Int_t date = GetDate();
  Int_t time = GetTime();
  
  if(mDoPed) 
  {
    if(mPedStatus==-1)
    {
      Float_t dt = GetDeltaTime(date,time,mPedDate,mPedTime);
      if(dt>mPedInterval) 
      { 
        mPedDate = date; mPedTime = time; mPedStatus = 0;
        cout <<"New pedestal round is starting ...\n"; 
      }
      else 
      {
        cout <<"last pedestal time = "<<mPedDate<<"  "<<mPedTime<<endl;
        cout <<"time left for new pedestal round = "<<dt<<" hours"<<endl;
      }
    }
    if(mPedStatus>=0)
    {
      if(mPedSpec->Fill(mHitsAdc,mEvent) && mPedStatus==0) mPedStatus=1;
      if(mPedStatus==1)
      {
        mPedSpec->CalculatePedestals();
        mPed = mPedSpec->GetPedestals();
        SavePedestals(mPedDate,mPedTime);
        mPedSpec->ZeroAll();
        mPedStatus=-1; //wait for next pedestal 
        mWaitForPed = kFALSE;
      }
    }
  }
  else mWaitForPed = kFALSE;
  
  if(!mWaitForPed)
  {  
    if(!CheckPedestal()) return kStWarn;
    if(mDoEqual) 
    {
      if(mEqualSpec->Fill(mHitsAdc,mEvent) && mEqStatus==0) mEqStatus=1; // ready to equalize
      if(mEqStatus==1) if(mEqualSpec->Equalize()) mEqStatus=2;  // equalized*/
    }
  
    if(mDoGain) 
    {
      if(mGain) { mGainDate = GetDate(); mGainTime = GetTime(); mGain = kFALSE; }
      cout <<"Filling Gain Monitor information\n";
      Bool_t gainst = gainst = mGainSpec->Fill(mHitsAdc,mEvent);
      if(mGainMode == 0)  { if(GetDate()!=mGainDate) mGain = kTRUE; } // daily basis
      else if(gainst) mGain = kTRUE; // statistics basis
      if(mGain)
      {
	      char spec[200];
		    sprintf(spec ,"%sGainMonitor.%08d.%06d.data.root",detname[mDetNum].Data(),mGainDate,mGainTime);
        mGainSpec->SaveAll(spec);
        mGainSpec->ZeroAll();
      }
    }
  
    if(mDoMip)
    {
      if(mMipSpec->Fill(mHitsAdc, mEvent))
      {
        if(mDoEqual) { if(mEqStatus == 2) mMipStatus = 1; }
        else mMipStatus = 1;  
      }
      if(mMipStatus==1) if(mMipSpec->MipCalib()) mMipStatus=2; 
    }
    
    Int_t final = 0;
    if(mDoEqual) final += 1;
    if(mDoMip)   final += 10;
  
    Int_t cal = 0;
    if(mDoEqual && mEqStatus  == 2) cal +=1;
    if(mDoEqual && mMipStatus == 2) cal +=10;
  
    if(cal==final)
    {
      MakeCalibration();
      SaveCalibration(mFirstEventDate,mFirstEventTime);
    }

    if(((Int_t)mEvNumber%200)==0) SaveSpectra("Recovery");
  }
  clock.Stop();
  cout <<"Time to run StEmcCalibrationMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
	
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Finish()
{
  gMessMgr->Info("StEmcCalibrationMaker::Finish()");
	SaveTables();
	SaveSpectra("Spec");
  return kStOk;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::GetEvent()
{
	Bool_t kReadOk=ReadStEvent();
  if(!kReadOk) return kFALSE;	
	
	StL0Trigger* trg = mEvent->l0Trigger();
	Int_t trigger=0;
	if(trg) trigger = trg->triggerWord();
	if(trigger!=8192 && trigger!=4096) return kFALSE;
	
  // reading B field from Database//////////
  TDataSet *RunLog=GetInputDB("RunLog");
  mBField=0.5;
  if(RunLog)
  {
    St_MagFactor *mag=(St_MagFactor*)RunLog->Find("MagFactor");
    if(mag)
    {
      MagFactor_st *magst=mag->GetTable();
      mBField=0.5*magst[0].ScaleFactor;
    }
  }
  mFilter->setBField(mBField);
  //////////////////////////////////////////
	
	if(!CalcZVertex()) return kFALSE;
  if(fabs(mZVertex)>mZVertexMax) return kFALSE;

  if(!FillEmcVector()) return kFALSE;  
  
  Int_t mode=0;
  if(mDoUseL3) mode =1;
  mFilter->setBField(mBField);
  mFilter->initEmcTowers(mEvent,mode);

  mEvNumber++;
	
	Int_t time=GetTime();
  Int_t date=GetDate();

  if(date<mFirstEventDate || (date==mFirstEventDate && time<mFirstEventTime))
  {
    mFirstEventTime=time;
    mFirstEventDate=date;
    mFirstEventRun=GetRunNumber();
  }
	return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::ReadStEvent()
{
  mEvent=(StEvent*)GetInputDS("StEvent");
  if(!mEvent) return kFALSE;
  mEmc=mEvent->emcCollection();
  if(!mEmc) return kFALSE;    
  if(mDoUseL3) // use L3Tracks
  {
    if(mEvent->l3Trigger())
    {
      StSPtrVecTrackNode& tracks=mEvent->l3Trigger()->trackNodes();
      mNTracks=tracks.size();
      if(mNTracks==0) return kFALSE;        
    }
    else return kFALSE; 
  }
  else
  {
    StSPtrVecTrackNode& tracks=mEvent->trackNodes();
    mNTracks=tracks.size();
    if (mNTracks==0) return kFALSE;
  }    
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CalcZVertex()
{
  mZVertex=0;
  if(mDoUseL3) 
  {
    StPrimaryVertex* Vertex=mEvent->l3Trigger()->primaryVertex();
    Int_t nvertex=mEvent->l3Trigger()->numberOfPrimaryVertices();
    if(Vertex && nvertex==1) 
    {
      mZVertex = Vertex->position().z();
      return kTRUE;
    }
  }
  else
  {
    StPrimaryVertex* Vertex=mEvent->primaryVertex();
    Int_t nvertex=mEvent->numberOfPrimaryVertices();
    if(Vertex && nvertex==1) 
    {
      mZVertex = Vertex->position().z();
      return kTRUE;
    }    
  }  
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CheckPedestal()
{
  if(!mSubPedestal) return kTRUE;
  if(mWaitForPed) return kTRUE;
  cout <<"Checking pedestals for detector "<<mDetName.Data()<<endl;  
  if(mUseLocalPed)
  {
    if(!mPed) return kFALSE;
    Int_t maxid = 4800;
    if(mDetNum>1) maxid = 18000;
    for(Int_t id=1;id<=maxid;id++)
    {
      Int_t ibin = mPed->FindBin(id,0); //pedestal
      Float_t p  = mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,1); //rms
      Float_t rms= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,2); //chi2
      Float_t chi= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,3); //status
      Int_t stat = (Int_t)mPed->GetBinContent(ibin);
			ibin       = mHitsAdc->FindBin(id);
			Float_t y  = mHitsAdc->GetBinContent(ibin);
      y-=p;
      if(y<2.*rms) y=0;
			mHitsAdc->SetBinContent(ibin,y);
    }
  }
  else // get from database
  {
    St_emcPed* ped=NULL;
    TString calibDb="Calibrations/emc/y3"+mDetName;
    TDataSet *emcDb=GetInputDB(calibDb.Data());  
    if(!emcDb && mSubPedestal) return kFALSE;  
    if(mDetNum==0) // bemc
    {
      if(emcDb) ped=(St_emcPed*)emcDb->Find("bemcPed");
      if(!ped && mSubPedestal) return kFALSE;
      emcPed_st *pedst=NULL;
      if(ped) pedst=ped->GetTable();

      for(Int_t i=0;i<mNBins;i++)
      {
			  if (pedst && mSubPedestal) 
			  {
				  Float_t id = (Float_t)i+1;
				  Int_t ibin = mHitsAdc->FindBin(id);
				  Float_t y  = mHitsAdc->GetBinContent(ibin);
          Float_t p  = (Float_t)pedst[0].AdcPedestal[i]/100.;
          Float_t rms= (Float_t)pedst[0].AdcPedestalRMS[i]/100.;
				  y-=p;
          if(y<2.*rms) y=0;
				  mHitsAdc->SetBinContent(ibin,y);
        }
      }
    }  
  }
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::MakeCalibration()
{
  Int_t nb = mCalibGeo->NModule()*mCalibGeo->NEta()*mCalibGeo->NSub();
  for(Int_t id=1;id<=nb;id++)
  {
    Int_t np = 0;
    Float_t x[10],ex[10],y[10],ey[10];
    Float_t eta,phi;
    mCalibGeo->getEtaPhi(id,eta,phi);
    Float_t theta=2.*atan(exp(-eta));
    if(mMipSpec)
    {
      Float_t MipE = mEoverMIP*(1.+0.056*eta*eta)/sin(theta);
      Float_t ADC,ERR;
      if(mMipSpec->GetMipPosition(id,&ADC,&ERR))
      {
        x[np]  = ADC;
        ex[np] = ERR;
        y[np]  = MipE;
        ey[np] = 0.07*MipE;
        np++;
      }
    }
    
    Float_t A[5];
    Float_t status = 0;
    for(Int_t i=0;i<5;i++) A[i]=0;
    if(np==1 && x[1]!=0) // one point only
    {
      A[0] = 0;
      A[1] = y[1]/x[1];
      status = 1;
    }
    
    for(Int_t i=0;i<5;i++)
    {
      Int_t ibin = mCalib->FindBin((Float_t)id,(Float_t)i);
      mCalib->SetBinContent(ibin,A[i]);
    }
    Int_t ibin = mCalib->FindBin((Float_t)id,9);
    mCalib->SetBinContent(ibin,status);
  }

  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::SaveSpectra(char* flag)
{
	char spec[200];
	if(mEqualSpec)
	{
		sprintf(spec ,"%sEqual%s.%08d.%06d.data.root",detname[mDetNum].Data(),flag,mFirstEventDate,mFirstEventTime);
    mEqualSpec->SaveAll(spec);
  }
	if(mMipSpec)
	{
		sprintf(spec ,"%sMip%s.%08d.%06d.data.root",detname[mDetNum].Data(),flag,mFirstEventDate,mFirstEventTime);
    mMipSpec->SaveAll(spec);
  }
	return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::SaveTables()
{
  return kTRUE;
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::SetStatus()
{  
	cout <<"Setting calibration status ...";
  Int_t nc=0;

  St_emcStatus*  Bemc = new St_emcStatus("emcStatus",1);
  St_smdStatus*  Bsmd = new St_smdStatus("smdStatus",1);

	mFilter->setBemcStatus(Bemc);
	mFilter->setBprsStatus(Bemc);
	mFilter->setBsmdeStatus(Bsmd);
	mFilter->setBsmdpStatus(Bsmd);

	Int_t status[18000];
  for(Int_t i=1;i<=mNBins;i++)
  {
    status[i-1]=0;
		if (i>=1861 && i<=2340) status[i-1]=1; // initial AuAu 2001 configuration
    if (i>=2021 && i<=2100) status[i-1]=0; // initial AuAu 2001 configuration

		//if (i>=1941 && i<=2400) status[i-1]=1; // pp conf
		//if (i>=1    && i<=180 ) status[i-1]=1; // pp conf
  }
  status[2309-1]=0;
  status[2254-1]=0;
  status[2288-1]=0;
  status[2325-1]=0;
  status[2150-1]=0;
  status[1986-1]=0;
  status[1979-1]=0;
	
	for(Int_t i=1;i<=mNBins;i++)
	{
		Int_t ibin = mIsOnOff->FindBin((Float_t)i);
		mIsOnOff->SetBinContent(ibin,(Float_t)status[i-1]);
		if(mDetNum<2)
		{
			emcStatus_st *st=Bemc->GetTable();
			st[0].Status[i-1]=status[i-1];
		}
		else
		{
			smdStatus_st *st=Bsmd->GetTable();
			st[0].Status[i-1]=status[i-1];
		}
		if(status[i-1]==1) nc++;
  }
	cout << " valid channels = "<<nc<<endl;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillEmcVector()
{
	Bool_t status=kFALSE;
	mHitsAdc->Reset();
	mHitsE->Reset();
  StDetectorId id = static_cast<StDetectorId>(mDetNum+kBarrelEmcTowerId);
  StEmcDetector* detector=mEmc->detector(id);
  if(detector)
    for(Int_t m=1;m<=120;m++)
    {
      StEmcModule* module = detector->module(m);
      if(module)
      {
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k=0;k<rawHit.size();k++) if(rawHit[k])
        {
          Int_t did;
          Int_t mod=rawHit[k]->module();
          Int_t e=rawHit[k]->eta(); 
          Int_t s=abs(rawHit[k]->sub());
					//cout <<"m = "<<mod<<"  eta = "<<e<<"  sub = "<<s<<"  adc = "<<rawHit[k]->adc()<<endl;
          mCalibGeo->getId(mod,e,s,did);
          if(abs(e)<=mCalibGeo->NEta()) 
					{
						mHitsAdc->Fill((Float_t)did,(Float_t)rawHit[k]->adc());
						mHitsE->Fill((Float_t)did,rawHit[k]->energy());
						status=kTRUE;
					}
        }
      }
    }
  return status;
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::LoadSpectra(char *time)
{
	char spec[200];
	if(mEqualSpec)
	{
		sprintf(spec ,"%sEqualSpectra.%s.data.root",detname[mDetNum].Data(),time);
    mEqualSpec->LoadAll(spec);
  }
	if(mMipSpec)
	{
		sprintf(spec ,"%sMipSpectra.%s.data.root",detname[mDetNum].Data(),time);
    mMipSpec->LoadAll(spec);
  }
	return;
}
//_____________________________________________________________________________
Float_t StEmcCalibrationMaker::GetDeltaTime(Int_t date, Int_t time, Int_t date0, Int_t time0)
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
void StEmcCalibrationMaker::SavePedestals(Int_t date, Int_t time)
{
	TDatime *tt = new TDatime(date,time);
  TString timestamp = tt->AsSQLString();
  delete tt;
        
  char file[100];
	sprintf(file ,"%sPedestal.raw.%08d.%06d.data.root",detname[mDetNum].Data(),date,time);
  mPedSpec->SaveAll(file);
  sprintf(file ,"%sPedestal.%08d.%06d.data.root",detname[mDetNum].Data(),date,time);
  TFile *f=new TFile(file,"RECREATE");
  mPed->Write();
  delete f;
  
  if(!mSavePedToDB) return;
  
  StDbManager* mgr=StDbManager::Instance();
	if(mDetNum==0 || mDetNum==1) // bemcPedestal
	{
		TString tn = "bemcPed";
    if(mDetNum==1) tn = "bprsPed";
    emcPed_st tnew;
		// fill new table here		
		for(Int_t id=1;id<=4800;id++)
		{
      Int_t ibin = mPed->FindBin(id,0); //pedestal
      Float_t p  = mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,1); //rms
      Float_t rms= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,2); //chi2
      Float_t chi= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,3); //status
      char stat = (char)mPed->GetBinContent(ibin);
      tnew.Status[id-1] = stat;
			tnew.AdcPedestal[id-1] = (short int)(100*p);	
			tnew.AdcPedestalRMS[id-1] = (short int)(100*rms);	
			tnew.ChiSquare[id-1] = chi;	
		}		
		/////////////////////
		StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
		StDbTable* tab=node->addDbTable(tn.Data());
		tab->SetTable((char*)&tnew,1);
		mgr->setStoreTime(timestamp.Data());
		mgr->storeDbTable(tab);
	}
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::SaveCalibration(Int_t date, Int_t time)
{
	TDatime *tt = new TDatime(date,time);
  TString timestamp = tt->AsSQLString();
  delete tt;
  
  char file[100];
  sprintf(file ,"%sCalibration.%08d.%06d.data.root",detname[mDetNum].Data(),date,time);
  TFile *f=new TFile(file,"RECREATE");
  mCalib->Write();
  delete f;
  
  if(!mSaveCalibToDB) return;
  
  StDbManager* mgr=StDbManager::Instance();
	if(mDetNum==0 || mDetNum==1) // bemcCalibration
	{
		TString tn = "bemcCalib";
    if(mDetNum==1) tn = "bprsCalib";
    emcCalib_st tnew;
		// fill new table here		
		for(Int_t id=1;id<=4800;id++)
		{
      for(Int_t i=0;i<5;i++)
      {
        Int_t ibin = mCalib->FindBin((Float_t)id,(Float_t)i);
        tnew.AdcToE[id-1][i] = mCalib->GetBinContent(ibin);
      }
      Int_t ibin = mCalib->FindBin((Float_t)id,9);
      tnew.Status[id-1] = (char)mCalib->GetBinContent(ibin);
		}		
		/////////////////////
		StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
		StDbTable* tab=node->addDbTable(tn.Data());
		tab->SetTable((char*)&tnew,1);
		mgr->setStoreTime(timestamp.Data());
		mgr->storeDbTable(tab);
  }
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::CorrectGain(char* file,Int_t outGain,Int_t mode)
{
  ofstream out(file);
  
  for(Int_t id = 1;id<=mNBins;id++)
  {
    Int_t ibin = mCalib->FindBin((Float_t)id,1.0);
    Float_t gain =  mCalib->GetBinContent(ibin);
    Float_t a = 1;
    Float_t eta,phi;
    mCalibGeo->getEtaPhi(id,eta,phi);
    Float_t theta=2.*atan(exp(-eta));
    if(mode==1) a = 1./sin(theta); // gain equalized in Et
    Float_t gainShift = (outGain/gain)*a;
    out <<id<<"  "<<eta<<"  "<<phi<<"  "<<gain<<"  "<<outGain<<"  "<<gainShift<<endl;
  }
  out.close();
  return;
}




