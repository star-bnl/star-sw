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
#include "Stiostream.h"
#include "TFile.h"
#include "StMessMgr.h"
#include "StarClassLibrary/SystemOfUnits.h"
#include "tables/St_MagFactor_Table.h"
#include "stdlib.h"
#include "StThreeVectorD.hh"
#include "time.h"
#include "TDatime.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/filters/StEmcFilter.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/voltageCalib/VoltCalibrator.h"

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
  
  mFakeRun = kFALSE;
	
	mDir = "./";
	mHVDatabase = "oldHVDatabase.dat";
	mHVCalibration = "pmtGainVsHV.dat";
}
//_____________________________________________________________________________
StEmcCalibrationMaker::~StEmcCalibrationMaker()
{
  if(mPedSpec) delete mPedSpec;
  if(mEqualSpec) delete mEqualSpec;
  if(mGainSpec) delete mGainSpec;
  if(mMipSpec) delete mMipSpec;
  if(mFilter) delete mFilter;
  if(mPosition) delete mPosition;
  if(mHitsAdc) delete mHitsAdc;
  if(mHitsE) delete mHitsE;
  if(mIsOnOff) delete mIsOnOff;
  if(mCalib) delete mCalib;
  if(mEqual) delete mEqual;
  if(mBemc) delete mBemc;
  if(mBsmd) delete mBsmd;
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
    mPedSpec->SetMaxMultiplicity(20);
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
        cout <<"time left for new pedestal round = "<<mPedInterval-dt<<" hours"<<endl;
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
	
	Bool_t vertexOk=CalcZVertex();
  
  if(!mWaitForPed) if(fabs(mZVertex)<mZVertexMax)
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
  
    if(mDoMip && vertexOk)
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

  }
  if(((Int_t)mEvNumber%200)==0) SaveSpectra((char*)mDir.Data());
  cout <<"Number of events processed = "<<mEvNumber<<endl;
  clock.Stop();
  cout <<"Time to run StEmcCalibrationMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
	
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Finish()
{
  gMessMgr->Info("StEmcCalibrationMaker::Finish()");
	//SaveTables();
	//SaveSpectra((char*)mDir.Data());
  return kStOk;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::GetEvent()
{
	Bool_t kReadOk=ReadStEvent();
  if(!kReadOk) return kFALSE;	
  if(!mFakeRun)
  {
	  StL0Trigger* trg = mEvent->l0Trigger();
	  Int_t trigger=0;
	  if(trg) trigger = trg->triggerWord();
		cout <<"Trigger word = 0x"<<hex<<trigger<<dec<<endl;
	  //if(trigger!=0x2001 && trigger!=0x2002) return kFALSE;
  }
	
  // reading B field from Database//////////
  mBField=0.5;
	StEventSummary *sum = mEvent->summary();
	
  if(sum)
  {
    mBField=sum->magneticField()/10.;
  }
  cout <<"Magnetic Field = "<<mBField<<" Tesla\n";
  mFilter->setBField(mBField);
  //////////////////////////////////////////
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
  if(!mFakeRun)
  {
    if(mDoUseL3) // use L3Tracks
    {
      if(mEvent->l3Trigger())
      {
        StSPtrVecTrackNode& tracks=mEvent->l3Trigger()->trackNodes();
        mNTracks=tracks.size();
      }
      else mNTracks=0; 
    }
    else
    {
      StSPtrVecTrackNode& tracks=mEvent->trackNodes();
      mNTracks=tracks.size();
    }
  }
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CalcZVertex()
{
  mZVertex=0;
	if(!mEvent->l3Trigger()) return kFALSE;
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
//VPunused      Float_t chi= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,3); //status
//VPunused      Int_t stat = (Int_t)mPed->GetBinContent(ibin);
			ibin       = mHitsAdc->FindBin(id);
			Float_t y  = mHitsAdc->GetBinContent(ibin);
      y-=p;
      if(y<2.*rms) y=0;
		  if(p==0 || rms==0) y=0;
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
					if(p==0 || rms==0) y=0;
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
		Float_t MipE;
    if(mMipSpec)
    {
      MipE = mEoverMIP*(1.+0.056*eta*eta)/sin(theta);
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
    if(np==1 && x[0]!=0) // one point only
    {
      A[0] = 0;
      if(x[0]>0) A[1] = y[0]/x[0]; else A[1] = 0;
      status = 1;
    }
    char line[200];
		sprintf(line,"ID = %4d  MIP E = %7.5f MipADC = %7.5f E = %7.5f  + %7.5f * ADC ",id,MipE,x[0],A[0],A[1]);
		if(id<=2400) cout <<line<<endl;
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
Bool_t StEmcCalibrationMaker::SaveSpectra(char* dir)
{
	char spec[300];
	if(mEqualSpec)
	{
		sprintf(spec ,"%s%sEqualSpec.%08d.%06d.data.root",dir,detname[mDetNum].Data(),mFirstEventDate,mFirstEventTime);
    mEqualSpec->SaveAll(spec);
  }
	if(mMipSpec)
	{
		sprintf(spec ,"%s%sMipSpec.%08d.%06d.data.root",dir,detname[mDetNum].Data(),mFirstEventDate,mFirstEventTime);
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

  mBemc = new St_emcStatus("emcStatus",1);
  mBsmd = new St_smdStatus("smdStatus",1);

	mFilter->setBemcStatus(mBemc);
	mFilter->setBprsStatus(mBemc);
	mFilter->setBsmdeStatus(mBsmd);
	mFilter->setBsmdpStatus(mBsmd);

	Int_t status[18000];
  for(Int_t i=1;i<=mNBins;i++)
  {
    status[i-1]=0;
		//if (i>=1861 && i<=2340) status[i-1]=1; // initial AuAu 2001 configuration
    //if (i>=2021 && i<=2100) status[i-1]=0; // initial AuAu 2001 configuration

		//if (i>=1941 && i<=2400) status[i-1]=1; // pp conf
		//if (i>=1    && i<=180 ) status[i-1]=1; // pp conf
		
    if (i>=1 && i<=2400) status[i-1]=1; // initial Y2003 configuration for towers
  }
  
  //year 2001 dead channels
  //status[2309-1]=0;
  //status[2254-1]=0;
  //status[2288-1]=0;
  //status[2325-1]=0;
  //status[2150-1]=0;
  //status[1986-1]=0;
  //status[1979-1]=0;
	
	//y2003 run	
	Int_t bad[] = {36,39,49,176,184,246,263,264,282,288,415,460,508,541,542,553,561,691,775,789,844,846,1008,1014,1052,
	               1062,1115,1137,1151,1176,1259,1286,1390,1407,1421,1422,1423,1444,1456,1457,1459,1460,1501,1502,
								 1518,1538,1540,1565,1764,1776,1795,1830,1868,1943,1978,2001,2002,2003,2004,2071,2089,2107,2108,2148,2149,
								 2150,2151,2183,2226,2249,2278,2369,1289,931,2011,733,1355,1359,2139,818,
								 1393,1394,1395,1396,1413,1414,1415,1416,1433,1434,1435,1436,1453,1454,1455,1456,1473,1474,1475,1476,
								 1493,1494,1495,1496,1513,1514,1515,1516,1533,1534,1535,1536,
								 -1};
								 
  Int_t index=0;
	do
	{
		if(bad[index]!=-1)  status[bad[index]-1] = 0;
		index++;
	} while (bad[index]!=-1);
	
	for(Int_t i=1;i<=mNBins;i++)
	{
		Int_t ibin = mIsOnOff->FindBin((Float_t)i);
		mIsOnOff->SetBinContent(ibin,(Float_t)status[i-1]);
		if(mDetNum<2)
		{
			emcStatus_st *st=mBemc->GetTable();
			st[0].Status[i-1]=status[i-1];
		}
		else
		{
			smdStatus_st *st=mBsmd->GetTable();
			st[0].Status[i-1]=status[i-1];
		}
		if(status[i-1]==1) nc++;
  }
	cout << " valid channels = "<<nc<<endl;
	
	// this is just for creating starting status tables
	/*
	char timestamp[]="2003-01-01 00:00:00";
	emcStatus_st *tnew=mBemc->GetTable();
  StDbManager* mgr=StDbManager::Instance();
	StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
	StDbTable* tab=node->addDbTable("bemcStatus");
	tab->SetTable((char*)tnew,1);
	mgr->setStoreTime(timestamp);
	mgr->storeDbTable(tab);
	
	smdStatus_st tnew1;
	for(int i=0;i<18000;i++) tnew1.Status[i] = 1;
	StDbTable* tab1=node->addDbTable("bsmdeStatus");
	tab1->SetTable((char*)&tnew1,1);
	mgr->setStoreTime(timestamp);
	mgr->storeDbTable(tab1);
	
	smdStatus_st tnew2;
	for(int i=0;i<18000;i++) tnew2.Status[i] = 1;
	StDbTable* tab2=node->addDbTable("bsmdpStatus");
	tab2->SetTable((char*)&tnew1,1);
	mgr->setStoreTime(timestamp);
	mgr->storeDbTable(tab2);
	
	emcStatus_st tnew3;
	for(int i=0;i<4800;i++) tnew3.Status[i] = 0;
	StDbTable* tab3=node->addDbTable("bprsStatus");
	tab3->SetTable((char*)&tnew3,1);
	mgr->setStoreTime(timestamp);
	mgr->storeDbTable(tab3);*/
	
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
		sprintf(spec ,"%s%sEqualSpec.%s.data.root",mDir.Data(),detname[mDetNum].Data(),time);
    cout <<"Loading ... "<<spec<<endl;
		mEqualSpec->LoadAll(spec);
  }
	if(mMipSpec)
	{
		sprintf(spec ,"%s%sMipSpec.%s.data.root",mDir.Data(),detname[mDetNum].Data(),time);
    cout <<"Loading ... "<<spec<<endl;
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
  TH1F *h1=new TH1F("pedestals","Pedestal values",mNBins,1.0,(Float_t)mNBins+1.);
  TH1F *h2=new TH1F("rms","RMS values",mNBins,1.0,(Float_t)mNBins+1.);
  TH1F *h3=new TH1F("chi","Chi values",mNBins,1.0,(Float_t)mNBins+1.);
  TH1F *h4=new TH1F("status","Status values",mNBins,1.0,(Float_t)mNBins+1.);
	for(Int_t id=1;id<=4800;id++)
	{
      Int_t ibin = mPed->FindBin(id,0); //pedestal
      Float_t p  = mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,1); //rms
      Float_t rms= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,2); //chi2
      Float_t chi= mPed->GetBinContent(ibin);
      ibin       = mPed->FindBin(id,3); //status
      Float_t stat = (Float_t)mPed->GetBinContent(ibin);
      h1->Fill((Float_t)id,p);
      h2->Fill((Float_t)id,rms);
      h3->Fill((Float_t)id,chi);
      h4->Fill((Float_t)id,stat);
  }
  h1->Write();
  h2->Write();
  h3->Write();
  h4->Write();
  delete h1; delete h2; delete h3; delete h4;
  f->Close();
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
void StEmcCalibrationMaker::GenerateHV(Float_t outGain,char* outfile,Int_t mode)
{
  char GainShift[]="HVGainShift.dat";
	
	ofstream out1("EmcGain.dat");
	ofstream out(GainShift);
	ifstream inp(mHVDatabase.Data());
  TH1F *h = new TH1F("Gain","Current Gain Distribution",100,0,0.02);
	do
	{
	  Int_t a,b,c,d,e,f,id;
		Float_t v;
		inp>>a>>id>>b>>c>>d>>e>>f>>v;
		
    Int_t ibin = mCalib->FindBin((Float_t)id,1.0);
    Float_t gain =  mCalib->GetBinContent(ibin);
    Float_t a0 = 1;
    Float_t eta,phi;
    mCalibGeo->getEtaPhi(id,eta,phi);
    Float_t theta=2.*atan(exp(-eta));
    if(mode==1) a0 = sin(theta); // gain equalized in Et
		Float_t outGain1 = outGain/a0;
    Float_t gainShift = gain/outGain1;
		char line[200];
		sprintf(line,"%4d  %6.3f  %6.3f  %7.5f  %7.5f  %7.5f",id,eta,phi,gain,outGain1,gainShift);
		if(gain>0) h->Fill(gain);
    out1 <<line<<endl;
		//if(gainShift==0) gainShift=1;
		sprintf(line,"%2d %5d  %5d  %5d  %5d  %5d  %5d  %7.5f",a,id,b,c,d,e,f,gainShift);
		out <<line<<endl;
		
	} while(!inp.eof());	
  out.close();
	out1.close();
	
  // Instantiate the voltage calibrator class
  VoltCalibrator vc;
  // Select the file that contains the Gain vs HV data
  vc.setRefFile(mHVCalibration.Data());
  // Select the file that contains the relative gains to be
  // be achieved
  vc.setGainFile(GainShift);
  // Select the file that containe the current voltages
  vc.setVoltInputFile(mHVDatabase.Data());
  // Select the file that will contain the new request voltages
  vc.setVoltOutputFile(outfile);
  // Perform the calculation
  vc.process();
	
	ifstream inp1(mHVDatabase.Data());
	ifstream inp2(outfile);
  TH1F *h1 = new TH1F("HVShift","High Voltages shift (NEW-OLD)",200,-100,100);
	do
	{
	  Int_t a,b,c,d,e,f,id;
		Float_t v,v1;
		inp1>>a>>id>>b>>c>>d>>e>>f>>v;
		inp2>>a>>id>>b>>c>>d>>e>>f>>v1;
		if(v1>0)h1->Fill(v1-v);
		
	} while(!inp1.eof());	
	
	TFile f("EmcGain.root","RECREATE");
	h->Write();
	h1->Write();
	f.Close();
	delete h;
		
  return;
}




