#include "StEmcCalibrationMaker.h"

#include <math.h>

#include <TFile.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "Stiostream.h"
#include "Stsstream.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/filters/StEmcFilter.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/StElectron.hh"
#include "tables/St_emcStatus_Table.h" 
#include "PhysicalConstants.h"
#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"
#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"
#include "StDaqLib/EMC/StEmcDecoder.h"

#define CAP1 124
#define CAP2 125

ClassImp(StEmcCalibrationMaker)

//_____________________________________________________________________________
StEmcCalibrationMaker::StEmcCalibrationMaker(const char *name):StMaker(name)
{
  this->Clear();
  TString n[MAXBEMC]={"bemc","bprs","bsmde","bsmdp"};
  for(int i=0;i<MAXBEMC;i++) mGeom[i] = StEmcGeom::instance(n[i].Data());
  mPosition = new StEmcPosition();
  mNChannel[0] = 4800;
  mNChannel[1] = 4800;
  mNChannel[2] = 18000;
  mNChannel[3] = 18000;
}
//_____________________________________________________________________________
StEmcCalibrationMaker::~StEmcCalibrationMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Init()
{
  return StMaker::Init();
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::zeroAll()              
{
  mStEvent = NULL;
  mEmcCol = NULL;
  mField = 0;
  mL3Tracks = false;
  mVx = mVy = mVz = 0;
  mNTracks = 0;
  for(int i=0;i<MAXTRACK;i++)
  {
    mTrack[i] = NULL;
    mTrackP[i] = mTrackPt[i] = mTrackTower[i] = mTrackNPoints[i] = mTrackTowerExit[i] =0;
  }
  for(int i=0;i<MAXBEMC;i++) for(int j=0;j<mNChannel[i];j++)
  {
    mADC[i][j] = mADCPedSub[i][j] = 0;
    mCap[i][j] = mNTracksTower[j] = 0;
    mPedRms[i][j] = mPed[i][j] =  0;
    if(i==0) mIsIsolatedTower[j] = true;
		mHasDetector[i] = false;
  }
	mZDCSum = 0;
	mCTBSum = 0;
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Make()
{  
  zeroAll();
	mStEvent = (StEvent*)GetInputDS("StEvent");
	if(!mStEvent) return kStOk;
  mEmcCol=(StEmcCollection*)mStEvent->emcCollection();  
  if(!mEmcCol) return kStOk;
  
  StPrimaryVertex *v = mStEvent->primaryVertex();  
  if(v)
  {
    StThreeVectorF position = v->position();
    mVx-=position.x(); mVy=position.y(); mVz=position.z();
  }
    
	mIsTriggerOk = true;
		
	//need to set BField ////////////////
  StEventSummary *evtSummary = mStEvent->summary();
  if (evtSummary) mField = evtSummary->magneticField()/10;
  
  fillEmcArrays();
  fillTrackArrays();
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Finish()
{
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::fillEmcArrays()              
{    
  TString n[MAXBEMC]={"bemc","bprs","bsmde","bsmdp"};
  
  for(int i = 0;i<MAXBEMC;i++)
  {
    mHasDetector[i] = false;
		TString calibDb="Calibrations/emc/y3";
    calibDb+=n[i];
    TDataSet *emcDb=GetInputDB(calibDb.Data());
    TString table=n[i];
    table+="Ped";
    if(i<2 && emcDb) // tower and preShower
    {
      St_emcPed *ped = (St_emcPed*)emcDb->Find(table.Data());
      emcPed_st *pedst=NULL;
      if(ped) pedst=ped->GetTable();
      if(pedst) for(int j=0;j<mNChannel[i];j++) 
      { 
        mPed[i][j] = (float)pedst[0].AdcPedestal[j]/100.;
        mPedRms[i][j] = (float)pedst[0].AdcPedestalRMS[j]/100.;
        //cout <<"Pedestal "<<i<<"  id = "<<j+1<<" Ped = "<<mPed[i][j]<<"  rms = "<<mPedRms[i][j]<<endl;
      }
    }
    else if(i>=2 && emcDb)
    {
      St_smdPed *ped = (St_smdPed*)emcDb->Find(table.Data());
      smdPed_st *pedst=NULL;
      if(ped) pedst=ped->GetTable();
      if(pedst) for(int j=0;j<mNChannel[i];j++) // uses only capacitor index 0
      { 
        mPed[i][j] = (float)pedst[0].AdcPedestal[j][0]/100.;
        mPedRms[i][j] = (float)pedst[0].AdcPedestalRMS[j][0]/100.;
        //cout <<"Pedestal "<<i<<"  id = "<<j+1<<" Ped = "<<mPed[i][j]<<"  rms = "<<mPedRms[i][j]<<endl;
      }		  
    }
    
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=mEmcCol->detector(id);
    if(detector) for(int m=1;m<=120;m++)
    {
			StEmcModule* module = detector->module(m);
      if(module)
      {
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(unsigned int k=0;k<rawHit.size();k++) if(rawHit[k])
        {
          mHasDetector[i] = true;
					int did;
          int mod=rawHit[k]->module();
          int e=rawHit[k]->eta(); 
          int s=abs(rawHit[k]->sub());
          int ADC = rawHit[k]->adc();
          int stat = mGeom[i]->getId(mod,e,s,did);
          if(stat ==0) 
          {
            mADC[i][did-1] = ADC;
            mADCPedSub[i][did-1] = (short)(ADC - mPed[i][did-1]);
            mCap[i][did-1] = (char) rawHit[k]->calibrationType();
						if(mCap[i][did-1]>127) mCap[i][did-1]-=128;
            if(mPed[i][did-1]==0 || mPedRms[i][did-1]==0) mADCPedSub[i][did-1] = 0;
						if(i>=2)
						{
							if(mCap[i][did-1]==CAP1 || mCap[i][did-1]==CAP1) //uses only capacitor 0 data
							{
							  mADCPedSub[i][did-1] = 0;
								mADC[i][did-1] = 0;
							}
						}
          }
        }
      }
    }
  }
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::fillTrackArrays()              
{
	StSPtrVecTrackNode& tracks=mStEvent->trackNodes();
  mNTracks =  tracks.size();
  mL3Tracks = false;
	if(mNTracks<=0) 
	{
		if(mStEvent->l3Trigger())
		{
      tracks =mStEvent->l3Trigger()->trackNodes();
		  mNTracks = tracks.size();	
			mL3Tracks = true;
		}
	}
  if(mNTracks > 0)
  {
	  for(int t=0;t<mNTracks;t++)
	  { 
		  StTrack *track = tracks[t]->track(0);
      if(track)
      {
        if(track->geometry())
        {
          mTrackPt[t] = track->geometry()->momentum().perp();
          mTrackP[t]  = track->geometry()->momentum().mag();
          mTrackNPoints[t] = track->fitTraits().numberOfFitPoints();
          StThreeVectorD momentum,position;
          bool tok = mPosition->trackOnEmc(&position,&momentum,(StTrack*)track,(Double_t)mField,(Double_t)mGeom[0]->Radius());
          if(tok)
          {
            int m,e,s,id=0;
            float eta=position.pseudoRapidity();
            float phi=position.phi();
            int stat = mGeom[0]->getBin(phi,eta,m,e,s);
            stat = mGeom[0]->getId(m,e,s,id); 
            if(stat==0) 
            {
              mTrackTower[t] = id;
              mNTracksTower[id-1]++;
              StThreeVectorD momentum1,position1;
              bool tok1 = mPosition->trackOnEmc(&position1,&momentum1,(StTrack*)track,(Double_t)mField,(Double_t)mGeom[0]->Radius()+30.0);
              if(tok1)
              {
                int id2=0;
                eta=position1.pseudoRapidity();
                phi=position1.phi();
                stat = mGeom[0]->getBin(phi,eta,m,e,s);
                stat = mGeom[0]->getId(m,e,s,id2);
                if(stat==0)
                {
                  mTrackTowerExit[t] = id2;
                  if(id2!=id) mNTracksTower[id2-1]++;
                }
              }
            }
          }
        }
      }
    }
  }
  
  for(int i=0;i<mNChannel[0];i++)
  {
    int id = i+1;
    mIsIsolatedTower[id-1] = true;
    for(int j=-1;j<=1;j++) for(int k=-1;k<=1;k++) 
    {
      int id2 = mPosition->getNextTowerId(id,j,k);
      if(id2!=id) if(id2>=1 && id2<=4800) if(mNTracksTower[id2-1]>0) { mIsIsolatedTower[id-1] = false; break; }
    }
  }
}
void StEmcCalibrationMaker::makeStatus(bool t, bool p, bool se, bool sp)
{
  // for the towers
	//////////////////////////////////////////////////////////////////////////	
	int towers[4800];
	for(int i =0;i<4800;i++) towers[i] = 1; // include all towers
	
	int crates[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	                1,1,1,1,0,1,1,1,1,1,0,1,1,1,1}; // exclude some crates
	
	//////////////////////////////////////////////////////////////////////////	
	// for the PSD
	int psd[4800];
	for(int i =0;i<4800;i++) psd[i] = 1; // include all PSD towers
	for(int i =2400;i<4800;i++) psd[i] = 0; // include all PSD towers
	
	//////////////////////////////////////////////////////////////////////////	
	// for the SMD-eta
	int smde[18000];
	for(int i =0;i<18000;i++) smde[i] = 1; // include all SMD-eta towers
	
	int smdeMod[120];
	for(int i =0;i<120;i++)  smdeMod[i] = 1; // include all SMD-eta modules
	for(int i =60;i<120;i++) smdeMod[i] = 0; // include all SMD-eta modules
	
	//////////////////////////////////////////////////////////////////////////	
	// for the SMD-phi
	int smdp[18000];
	for(int i =0;i<18000;i++) smdp[i] = 1; // include all SMD-phi towers
	
	int smdpMod[120];
	for(int i =0;i<120;i++)  smdpMod[i] = 1; // include all SMD-phi modules
	for(int i =60;i<120;i++) smdpMod[i] = 0; // include all SMD-eta modules
	
	//////////////////////////////////////////////////////////////////////////	
	StEmcDecoder *dec = new StEmcDecoder(20300101,0);
	
	const char *timestamp = "2004-01-01 00:00:00";
	
  StDbManager* mgr=StDbManager::Instance();
	StDbConfigNode* node=mgr->initConfig(dbCalibrations,dbEmc);
	
	// for the towers
	
	if(t)
	{
	  St_emcStatus *mBemc = new St_emcStatus("emcStatus",1);
		emcStatus_st *bemc=mBemc->GetTable();
	
		for(int i=0;i<4800;i++) 
		{
	  	bemc[0].Status[i] = towers[i];
			int daq;
			dec->GetDaqIdFromTowerId(i+1,daq);
			int crate, pos;
			dec->GetTowerCrateFromDaqId(daq,crate,pos);
			bemc[0].Status[i] = crates[crate-1];
		}
	
		StDbTable* tab1=node->addDbTable("bemcStatus");
		tab1->SetTable((char*)bemc,1);
		mgr->setStoreTime(timestamp);
		mgr->storeDbTable(tab1);
	}
	
	// for the PSD
	
  if(p)
	{
		St_emcStatus *mPSD = new St_emcStatus("emcStatus",1);
		emcStatus_st *psdt = mPSD->GetTable();
	
		for(int i=0;i<4800;i++) 
		{
	  	psdt[0].Status[i] = psd[i];
		}
	
		StDbTable* tab2=node->addDbTable("bprsStatus");
		tab2->SetTable((char*)psdt,1);
		mgr->setStoreTime(timestamp);
		mgr->storeDbTable(tab2);
	}
	
	// for the SMD-eta
	
	if(se)
	{
    St_smdStatus *mSMDE = new St_smdStatus("smdStatus",1);
		smdStatus_st *smdet = mSMDE->GetTable();
		StEmcGeom *geo3 = StEmcGeom::instance("bsmde");
	
		for(int i=0;i<18000;i++) 
		{
	  	smdet[0].Status[i] = smde[i];
			int m,e,s;
			geo3->getBin(i+1,m,e,s);
	  	smdet[0].Status[i] = smdeMod[m-1];
		}
	
		StDbTable* tab3=node->addDbTable("bsmdeStatus");
		tab3->SetTable((char*)smdet,1);
		mgr->setStoreTime(timestamp);
		mgr->storeDbTable(tab3);
	}
	
	// for the SMD-phi
	if(sp)
	{
  	St_smdStatus *mSMDP = new St_smdStatus("smdStatus",1);
		smdStatus_st *smdpt = mSMDP->GetTable();
		StEmcGeom *geo4 = StEmcGeom::instance("bsmdp");
	
		for(int i=0;i<18000;i++) 
		{
	  	smdpt[0].Status[i] = smdp[i];
			int m,e,s;
			geo4->getBin(i+1,m,e,s);
	  	smdpt[0].Status[i] = smdpMod[m-1];
		}
	
		StDbTable* tab4=node->addDbTable("bsmdpStatus");
		tab4->SetTable((char*)smdpt,1);
		mgr->setStoreTime(timestamp);
		mgr->storeDbTable(tab4);
	}
	
	delete dec;
	dec = 0;
  
}
