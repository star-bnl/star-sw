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
#include "StMessMgr.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StarClassLibrary/SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using units::tesla;
#endif

ClassImp(StEmcCalibrationMaker);

#ifdef StEmcCalibrationMaker_DEBUG
ofstream emclog("StEmcCalibrationMaker.emclog");
#endif

Int_t                nbins;

TRandom*             ran=new TRandom();
StEmcGeom*           Geo;
StEmcCollection*     emc;
StEvent*             event;
/*Float_t              miptemp,equaltemp;
Float_t              evnumber;
Float_t              avg,sigma;
Int_t                firstEventTime,lastEventTime,firstEventRun,lastEventRun;
Int_t                firstEventDate,lastEventDate;*/
//emcCalSummary_st*    Summary_st;
//emcCalSettings_st*   Settings_st;
//emcCalibration_st*   Calib_st;
//emcEqualization_st*  Equal_st;
//emcMipCalib_st*      Mip_st;

//_____________________________________________________________________________
StEmcCalibrationMaker::StEmcCalibrationMaker(const char *name):StMaker(name)
{

  gMessMgr->SetLimit("StEmcCalibrationMaker",1000);

  detnum=-1;

// creating summary table
  SummaryTable = new St_emcCalSummary("Summary",1);
  SummaryTable->SetNRows(1);

  emcCalSummary_st* Summary_st=SummaryTable->GetTable();
  
  Summary_st[0].DetNumber=1;
  Summary_st[0].FirstRun=0;
  Summary_st[0].LastRun=0;
  Summary_st[0].NEvents=0;
    
// finished summary table
  
// creating settings table
  SettingsTable = new St_emcCalSettings("Settings",1);
  SettingsTable->SetNRows(1);
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  
  Settings_st[0].DataType=0;
  Settings_st[0].UseL3Tracks=0;
  Settings_st[0].ZVertexCut=40;
  
  Settings_st[0].NEtaBins=10;
  Settings_st[0].EtaBinWidth=0.2;
  
  Settings_st[0].DoEqualization=1;
  Settings_st[0].EqualizationMethod=1;
  Settings_st[0].EqEventsPerBin=750; //670
  Settings_st[0].EqMinNumberOfTracks=50;
  Settings_st[0].EqMinOccupancy=0.99;
  
  Settings_st[0].UseMipCalib=1;
  Settings_st[0].UseMipEtaBin=1;
  Settings_st[0].EOverMipCte=0.290;  // e/MIP for geant
  Settings_st[0].MipPeakFitFuntion=0;
  Settings_st[0].MipEventsPerBin=1200; //600
  Settings_st[0].MipMaxNumberOfTracks=1000;
  Settings_st[0].MipMinOccupancy=0.99;
  Settings_st[0].MipMinimumMomentum=1.5;
  
  Settings_st[0].UsePi0Calib=0;
  
  Settings_st[0].UseElectronCalib=0;
// finished settings table

  SetMode(1);

}
//_____________________________________________________________________________
StEmcCalibrationMaker::~StEmcCalibrationMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Init()
{
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"===========================================================================\n";
  emclog <<"StEmcCalibrationMaker::Init()\n";
  #endif
  
  emcCalSummary_st* Summary_st=SummaryTable->GetTable();
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  
  if(Settings_st[0].DataType==1) Settings_st[0].EOverMipCte=0.252; // e/MIP for real data
  
  detnum=Summary_st[0].DetNumber-1;  // this is only the vector index
  
  if(detnum<0 || detnum >7) 
  {
    gMessMgr->Warning("StEmcCalibrationMaker::Init() - Invalid detector number");
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"StEmcCalibrationMaker::Init() - Invalid detector number\n";
    #endif
    return kStWarn;
  }

  evnumber=0;
  if(Geo) delete Geo;
  Geo=new StEmcGeom(detname[detnum].Data());
  nbins=Geo->NModule()*Geo->NEta()*Geo->NSub();
  
  firstEventTime=0;
  firstEventRun=0;
  firstEventDate=0;
  lastEventTime=0;
  lastEventRun=0;
  lastEventDate=0;
  
  Int_t nb=nbins;
  if(CalibTable) delete CalibTable;
  CalibTable=new St_emcCalibration("CalibTable",nb);
  CalibTable->SetNRows(nb);
  ClearCalibTable();  
  
  SetCalibStatus();
  
  if(Settings_st[0].DoEqualization==1) // do equalization
  {
    Int_t nb=nbins;

    if(EqualTable) delete EqualTable;
    EqualTable=new St_emcEqualization("EqualTable",nb);
    EqualTable->SetNRows(nb);
    ClearEqualTable();

    if(EqualSpec) delete EqualSpec;
    EqualSpec=new StEmcEqualSpectra(detname[detnum].Data());
    EqualSpec->EqualTable=EqualTable;  
    EqualSpec->CalibTable=CalibTable;
    EqualSpec->nEtaBins=Settings_st[0].NEtaBins;
    EqualSpec->etaBinWidth=Settings_st[0].EtaBinWidth;
    EqualSpec->Init();
    
    EqStatus=0;
    m_equalCounter=0;
    equaltemp=Settings_st[0].EqEventsPerBin;
    
    //QA histogram for equalization
    if (!m_EqualOccupancy) 
    {
      TString name1="Equal_"+detname[detnum];
      TString title1="Equalization Occupancy for detetctor "+detname[detnum];
      m_EqualOccupancy=new TH1F(name1.Data(),title1.Data(),nb,1,(Float_t)nb);
    }
    //
    gMessMgr->Info("StEmcCalibrationMaker::Init() - Equalization initialized");
  }

  if(Settings_st[0].UseMipCalib==1) // do MIP calibration
  {
    Int_t nb;
    if(Settings_st[0].UseMipEtaBin==0) nb=nbins;
    else nb=Settings_st[0].NEtaBins;

    if(MipTable) delete MipTable;
    MipTable=new St_emcMipCalib("MipTable",nb);
    MipTable->SetNRows(nb);
    ClearMipTable();

    if(MipSpec) delete MipSpec;
    MipSpec=new StEmcMipSpectra(detname[detnum].Data());
    if(Settings_st[0].DoEqualization==0) Settings_st[0].UseMipEtaBin=0;
    if(Settings_st[0].UseMipEtaBin==1)
    {
      MipSpec->EqualTable=EqualTable;
      MipSpec->nEtaBins=Settings_st[0].NEtaBins;
      MipSpec->etaBinWidth=Settings_st[0].EtaBinWidth;      
    }
    else
    {
      MipSpec->nEtaBins=-1;
      MipSpec->etaBinWidth=0;          
    }
    MipSpec->MipTable=MipTable;
    MipSpec->CalibTable=CalibTable;
    MipSpec->Init();
    MipStatus=0;
    m_mipCounter=0;
    miptemp=Settings_st[0].MipEventsPerBin;
    
    //QA histograms for MIP
    if(!m_MipOccupancy)
    {
      TString name2="Mip_"+detname[detnum];
      TString title2="MIP occupancy for detetctor "+detname[detnum];
      m_MipOccupancy=new TH1F(name2.Data(),title2.Data(),nbins,1,(Float_t)nbins);
    }
    //
    gMessMgr->Info("StEmcCalibrationMaker::Init() - MIP Calibration initialized");
  }
  
  if(Settings_st[0].UsePi0Calib==1) // do Pi0 calibration
  {
  }
  
  if(Settings_st[0].UseElectronCalib==1) // do Electron calibration
  {
  }
  
  CalibStatus=0;
  CalcPedestal();
  
  // setting Calibration Mode
  Summary_st[0].CalibMode=0;  
  if(Settings_st[0].DoEqualization==1)   Summary_st[0].CalibMode+=1;
  if(Settings_st[0].UseMipCalib==1)      Summary_st[0].CalibMode+=10;
  if(Settings_st[0].UsePi0Calib==1)      Summary_st[0].CalibMode+=100;
  if(Settings_st[0].UseElectronCalib==1) Summary_st[0].CalibMode+=1000;
    
  #ifdef StEmcCalibrationMaker_DEBUG  
  emclog <<"finished StEmcCalibrationMaker::Init()\n";
  #endif
    
  gMessMgr->Info("StEmcCalibrationMaker::Init() - Finished Ok");

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Make()
{
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"\n*****************************************************************************\n";
  emclog <<"***** Entering StEmcCalibrationMaker::Make()\n";
  emclog <<"***** Event number "<<evnumber<<"\n";
  #endif

  if(detnum<0 || detnum >7) 
  {
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - Invalid detector number");
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"StEmcCalibrationMaker::Make() - Invalid detector number\n";
    #endif
    return kStWarn;
  }
  
  TStopwatch clock;
  clock.Start();
  
  emcCalSummary_st* Summary_st=SummaryTable->GetTable();
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
   
  zVertex=0;
  Bool_t kReadOk=kFALSE; 
  switch (runMode)
  {
    case(0): kReadOk=ReadHitsOnline();   break;
    case(1): kReadOk=ReadHitsOffline();  break;
  }

  if(!kReadOk)
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** Can not get Emc Hits or Tracks... Exiting StEmcCalibrationMaker::Make()\n";
    #endif
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - Can not get Emc Hits or Tracks");
    return kStWarn;
  }
 
  if(!CalcZVertex()) 
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** zVertex calc fails... Exiting StEmcCalibrationMaker::Make()\n";
    #endif
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - zVertex calc fails");
    return kStWarn;
  }

  evnumber++;
  if(evnumber==1) 
  {
    firstEventTime=GetTime();
    firstEventDate=GetDate();
    firstEventRun=GetRunNumber();
    gMessMgr->Info()<<"StEmcCalibrationMaker::Make() - First Event time = "<<firstEventTime<<endm;
    gMessMgr->Info()<<"StEmcCalibrationMaker::Make() - First Event run = "<<firstEventRun<<endm;
    Summary_st[0].FirstRun=firstEventRun;
  }
   
  if(Settings_st[0].DoEqualization==1) FillEqual();
  if(Settings_st[0].DoEqualization==1 && EqStatus==1) 
  {
    gMessMgr->Info("StEmcCalibrationMaker::Make() - doing equalization");
    Equalize();
  }

  if(Settings_st[0].UseMipCalib==1) FillMipCalib();
  if(Settings_st[0].UseMipCalib==1 && MipStatus==1) 
  {
    gMessMgr->Info("StEmcCalibrationMaker::Make() - doing MIP calibration");
    MipCalib();
   }
    
  Int_t done=0;
  if(Settings_st[0].DoEqualization==1 && EqStatus==2)   done+=1;
  if(Settings_st[0].UseMipCalib==1    && MipStatus==2)  done+=10;
//  if(Settings_st[0].UsePi0Calib==1)      done+=100;
//  if(Settings_st[0].UseElectronCalib==1) done+=1000;

  if(done==Summary_st[0].CalibMode) //ready to find calibration contants
  {
    lastEventTime=GetTime();
    lastEventDate=GetDate();
    lastEventRun=GetRunNumber();
    Summary_st[0].LastRun=lastEventRun;
    Summary_st[0].NEvents=(long)evnumber;
    gMessMgr->Info()<<"StEmcCalibrationMaker::Make() - last Event time = "<<lastEventTime<<endm;
    gMessMgr->Info()<<"StEmcCalibrationMaker::Make() - last Event run = "<<lastEventRun<<endm;
    gMessMgr->Info("StEmcCalibrationMaker::Make() - Finding calibration constants");
    MakeCalibration();
    gMessMgr->Info("StEmcCalibrationMaker::Make() - saving tables");
    SaveTables();
    gMessMgr->Info("StEmcCalibrationMaker::Make() - starting again");
    Init();
//    return kStERR;
  }
    
  clock.Stop();

  #ifdef StEmcCalibrationMaker_DEBUG  
  emclog <<"Time to run StEmcCalibrationMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  #endif

  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Finish()
{

  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"StEmcCalibrationMaker::Finish()\n";
  emclog.close();
  #endif
  gMessMgr->Info("StEmcCalibrationMaker::Finish()");

  return kStOk;
}

//_____________________________________________________________________________
Int_t StEmcCalibrationMaker::Clear()
{
  if(emc) delete emc;
  if(event) delete event;
  return kStOk;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::ReadHitsOnline()
{
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::ReadHitsOffline()
{

  event=(StEvent*)GetInputDS("StEvent");
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();

  if(!event)  
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog << "***** Can not get StEvent pointer\n";
    #endif
    return kFALSE;
  }

  emc=event->emcCollection();
  if(!emc)
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** Can not get StEvent->emcCollection pointer.\n";
    #endif
    return kFALSE;
  }
    
  if(Settings_st[0].UseL3Tracks==1) // use L3Tracks
  {
    if(event->l3Trigger())
    {
      StSPtrVecTrackNode& tracks=event->l3Trigger()->trackNodes();
      nTracks=tracks.size();
      if(nTracks==0)
      {
        #ifdef StEmcCalibrationMaker_DEBUG
        emclog << "***** Can not get StEvent->l3Trigger pointer.\n";
        #endif
        return kFALSE;        
      }
    }
    else
    {
      #ifdef StEmcCalibrationMaker_DEBUG
      emclog << "***** Can not get StEvent->l3Trigger pointer.\n";
      #endif
      return kFALSE; 
    }
  }
    
  if(Settings_st[0].UseL3Tracks!=1)
  {
    StSPtrVecTrackNode& tracks=event->trackNodes();
    nTracks=tracks.size();
    if (nTracks==0) 
    {
      #ifdef StEmcCalibrationMaker_DEBUG
      emclog << "***** Can not get StEvent tracks.\n";
      #endif
      return kFALSE;
    }
  }
    
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CalcZVertex()
{
  // first try to get ZVertex from StEvent...
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();

  if(Settings_st[0].UseL3Tracks==1) 
  {
    StPrimaryVertex* Vertex=event->l3Trigger()->primaryVertex();
    Int_t nvertex=event->l3Trigger()->numberOfPrimaryVertices();
    if(Vertex && nvertex==1) 
    {
      zVertex = Vertex->position().z();
      return kTRUE;
    }
  }
  else
  {
    StPrimaryVertex* Vertex=event->primaryVertex();
    Int_t nvertex=event->numberOfPrimaryVertices();
    if(Vertex && nvertex==1) 
    {
      zVertex = Vertex->position().z();
      return kTRUE;
    }    
  }
  
  // no vertex in the StEvent.  try to calculate one...  
  if(nTracks<3) return kFALSE;
    
  zVertex=0;
  Float_t c=0;
  for(Int_t i=0;i<nTracks;i++)
  {
    StTrack* track;
    if(Settings_st[0].UseL3Tracks==1) 
    {
      StSPtrVecTrackNode& tracks =event->l3Trigger()->trackNodes();
      track=tracks[i]->track(0);
    }
    else 
    {
      StSPtrVecTrackNode& tracks=event->trackNodes();
      track=tracks[i]->track(0);
    }    
    StTrackGeometry* trgeo=track->geometry();
    const StThreeVectorF origin=trgeo->origin();
    const StThreeVectorF momentum=trgeo->momentum();
    Float_t r0=sqrt(origin.x()*origin.x()+origin.y()*origin.y());
    Float_t z0=origin.z();
    Float_t psi=acos(momentum.x()/momentum.mag());
    Float_t phi=acos(origin.x()/r0);
    Float_t tanl=trgeo->dipAngle();
    if(momentum.mag()>0.7)
    {
      zVertex+=z0-r0*cos(psi-phi)*tanl;
      c++;
    }
  }
  if(c<3) return kFALSE;
  zVertex/=c;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::IsThisTrackGood(Int_t tr,Float_t *ETA,Float_t *PHI)
{  
  if(tr>nTracks) return kFALSE;
  
  StTrack* track;
  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();

  if(Settings_st[0].UseL3Tracks==1) 
  {
    StSPtrVecTrackNode& tracks =event->l3Trigger()->trackNodes();
    track=tracks[tr]->track(0);
  }
  else 
  {
    StSPtrVecTrackNode& tracks=event->trackNodes();
    track=tracks[tr]->track(0);
  }    

  Float_t eta,phi;

// project track in the begining of the detector
  if(!ProjectTrack(track,(double)Geo->Radius(),&eta,&phi)) return kFALSE;
  Int_t mtr,etr,str;
  if(Geo->getBin(phi,eta,mtr,etr,str)==1) return kFALSE; //out of detector

// project track in the exit of the detector (tower radius + tower width)
  if(!ProjectTrack(track,(double)(Geo->Radius()+Geo->YWidth()),&eta,&phi)) return kFALSE;
  Int_t mtr1,etr1,str1;
  if(Geo->getBin(phi,eta,mtr1,etr1,str1)==1) return kFALSE; //out of detector

// check if the track does not change the tower
  if(mtr1!=mtr || etr1!=etr || str1!=str) return kFALSE; 

// find eta and phi of the center of the tower  
  Int_t id;
  Geo->getId(mtr,etr,str,id);
  Geo->getEtaPhi(id,eta,phi);

// check if there are tracks in the neighbor towers

  for(Int_t i=0;i<nTracks;i++)
  {
    if(i!=tr)
    {
      Float_t eta1,phi1;
      StTrack* track1;
      if(Settings_st[0].UseL3Tracks==1) 
      {
        StSPtrVecTrackNode& tracks =event->l3Trigger()->trackNodes();
        track1=tracks[i]->track(0);
      }
      else 
      {
        StSPtrVecTrackNode& tracks=event->trackNodes();
        track1=tracks[i]->track(0);
      }    

      if(ProjectTrack(track1,(double)Geo->Radius(),&eta1,&phi1) && fabs(eta1)<1) // enter of detector (pre shower)
      {
        Int_t m,e,s,id1;

        if(Geo->getBin(phi1,eta1,m,e,s)==0) // track hit detector
        {
          Geo->getId(m,e,s,id1);
          Geo->getEtaPhi(id1,eta1,phi1); // get eta phi of the center of tower

          Float_t x=fabs(eta1-eta);
          Float_t y=fabs(phi1-phi);
        
          if(y>=2*3.14159) y-=2*3.14159;
        
          if(y<=0.05 && x<=0.05) return kFALSE;
        }
      }
    }
  }
  
// project track in the proper detector
  if(!ProjectTrack(track,(double)Geo->Radius(),&eta,&phi)) return kFALSE;

  *ETA=eta;
  *PHI=phi;

  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::ProjectTrack(StTrack* track,double radius, Float_t *ETA, Float_t *PHI)
{  
  StTrackGeometry* trgeo = track->geometry();
  Short_t charge=trgeo->charge();
  const StThreeVectorF origin=trgeo->origin();
  const StThreeVectorF momentum=trgeo->momentum();
  StPhysicalHelixD helix(momentum,origin,0.5*tesla,(double)charge);
  
  pairD s=helix.pathLength(radius);
  
  if(s.first<=0 && s.second<=0) return kFALSE;
  
  Float_t eta,phi;
  double r =  (s.first < 0 || s.second < 0) 
              ? max(s.first, s.second) : min(s.first, s.second); 
  
  StThreeVectorD p=helix.at(r);
  eta=(Float_t) p.pseudoRapidity();
  phi=(Float_t) p.phi();
  
  *ETA=eta;
  *PHI=phi;
  return kTRUE;
  
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CalcPedestal()
{
  Int_t nb=nbins;
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  emcCalibration_st* Calib_st=CalibTable->GetTable();  
  
  if(Settings_st[0].DataType==0)  // simulation data
    for(Int_t i=1;i<=nb;i++)
    {
      Calib_st[i-1].AdcPedestal=0;
      Calib_st[i-1].AdcPedestalRms=0;
    }
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillEqual()
{      
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  emcCalibration_st* Calib_st=CalibTable->GetTable();
  
  if(nTracks<Settings_st[0].EqMinNumberOfTracks) return kFALSE;
  
  if(fabs(zVertex)>fabs(Settings_st[0].ZVertexCut)) return kFALSE;  

  StDetectorId id = static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
  StEmcDetector* detector=emc->detector(id);
  for(UInt_t j=1;j<121;j++)
  {
    StEmcModule* module = detector->module(j);
    StSPtrVecEmcRawHit& rawHit=module->hits();
    for(UInt_t k=0;k<rawHit.size();k++)
    {
      Int_t did=EqualSpec->GetID(rawHit[k]->module(),rawHit[k]->eta(),abs(rawHit[k]->sub()));
      if(EqualSpec->GetStatus(did)==1) 
      {
        EqualSpec->FillSpectra(did,rawHit[k]->adc()-Calib_st[did-1].AdcPedestal);
        //FILL QA HISTOGRAM*********************
        m_EqualOccupancy->Fill(did);
        //**************************************
      }
    }
  }
  m_equalCounter++;
  
  if(EqStatus!= 0) return kTRUE; // it reached the minimum number of events. I don't need to check it again
  if(m_equalCounter<1.2*Settings_st[0].EqEventsPerBin) return kTRUE; // gives 20% more 
  
// checking the average event number per bin event numbers ...
  Float_t x,y,z;
  EqualSpec->GetOccupancy(Settings_st[0].EqEventsPerBin,&x,&y,&z);
  
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"EQUALIZATION: Avg Nevents/bin = "<<x<<" +- "<<y<<"\n";
  emclog <<"EQUALIZATION: fraction of bins nevents > minimum = "<<z<<"\n";
  #endif

  if(z<Settings_st[0].EqMinOccupancy) return kTRUE;  // minimum occupancy not reached yed...
  
  EqStatus=1; // ready to equalize
  
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::Equalize()
{
  #ifdef StEmcCalibrationMaker_DEBUG  
  emclog <<"****************** Equalizing spectra for detector "<< detname[detnum].Data()<<"\n";
  #endif
  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();

  Int_t ndiv=Settings_st[0].NEtaBins;
  Float_t etabin=Settings_st[0].EtaBinWidth;
  
  for(Int_t i=1;i<=ndiv;i++)
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** Equalizing EtaBin "<< i<<"\n";
    #endif
    gMessMgr->Info()<<"StEmcCalibrationMaker::Equalize() - Equalizing EtaBin "<< i<<endm;
    
    Int_t mi,mf,ei,ef;
    CalcEtaBin(i,etabin,&mi,&mf,&ei,&ef);
    Int_t numberReady=0;
    for(Int_t m=mi;m<mf+1;m++)
      for(Int_t e=ei;e<ef+1;e++)
        for(Int_t s=1;s<EqualSpec->GetNSub()+1;s++)
        {
          Int_t id1=EqualSpec->GetID(m,e,s); 
          if(EqualSpec->GetStatus(id1)>0 && EqualSpec->GetSum(id1)>Settings_st[0].EqEventsPerBin) 
            numberReady++;
        }      

    if(numberReady>0)
    {
      Int_t l=(Int_t)(ran->Uniform()*(Double_t)numberReady);
      Int_t l1=0;
      Int_t ref=0;
      for(Int_t m=mi;m<mf+1;m++)
        for(Int_t e=ei;e<ef+1;e++)
          for(Int_t s=1;s<EqualSpec->GetNSub()+1;s++)
          {
            Int_t id1=EqualSpec->GetID(m,e,s); 
            if(EqualSpec->GetStatus(id1)>0 && EqualSpec->GetSum(id1)>Settings_st[0].EqEventsPerBin)
              if (l1==l) ref=id1;
              else l1++;
          }
      #ifdef StEmcCalibrationMaker_DEBUG
      emclog <<"***** Reference spectrum choice for etabin = "<<div<<"   ref = "<<ref<<"\n";
      #endif
      for(Int_t m=mi;m<mf+1;m++)
        for(Int_t e=ei;e<ef+1;e++)
          for(Int_t s=1;s<EqualSpec->GetNSub()+1;s++)
          {
            Int_t id1=EqualSpec->GetID(m,e,s);
            if(EqualSpec->GetStatus(id1)>0 && EqualSpec->GetSum(id1)>Settings_st[0].EqEventsPerBin) 
              EqualSpec->Equalize(ref,id1,Settings_st[0].EqualizationMethod);
          }
    }
  }

  EqStatus=2;  // equalized*/
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillMipCalib()
{  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();  
  emcCalibration_st* Calib_st=CalibTable->GetTable();

  if(nTracks>Settings_st[0].MipMaxNumberOfTracks) return kFALSE;

  if(fabs(zVertex)>fabs(Settings_st[0].ZVertexCut)) return kFALSE;  
  
  Bool_t ok=kFALSE;
  StDetectorId id = static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
  StEmcDetector* detector=emc->detector(id);
    
  for(Int_t tr=0;tr<nTracks;tr++)
  {
    Float_t eta,phi;
    StTrack* track;
    if(Settings_st[0].UseL3Tracks==1) 
    {
      StSPtrVecTrackNode& tracks =event->l3Trigger()->trackNodes();
      track=tracks[tr]->track(0);
    }
    else 
    {
      StSPtrVecTrackNode& tracks=event->trackNodes();
      track=tracks[tr]->track(0);
    }    

    StTrackGeometry* trgeo = track->geometry();
    const StThreeVectorF momentum=trgeo->momentum();
        
    if(momentum.mag()>=Settings_st[0].MipMinimumMomentum) 
      if(IsThisTrackGood(tr,&eta,&phi))
      {
        Int_t mtr,etr,str;
        Geo->getBin(phi,eta,mtr,etr,str);
        StEmcModule* module = detector->module(mtr);
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k=0;k<rawHit.size();k++)
        {
          if(rawHit[k]->module()==(UInt_t)mtr 
             && rawHit[k]->eta()==(UInt_t)etr 
             && abs(rawHit[k]->sub())==abs(str))
          {
            Int_t did=MipSpec->GetID(mtr,etr,str);
            if(MipSpec->GetStatus(did)==1) 
            {
              MipSpec->FillSpectra(did,rawHit[k]->adc()-Calib_st[did-1].AdcPedestal);
              //FILL QA HISTOGRAM*********************
              m_MipOccupancy->Fill(did);
              //**************************************
              ok=kTRUE;
              #ifdef StEmcCalibrationMaker_DEBUG
              emclog <<"CALIBRATION: MIP track = "<<tr<<"  momentum = "
                     <<momentum.mag()<<"  eta = "<<eta
                     <<"  phi = "<<phi<<"  id = "<<did
                     <<"  adc = "<<rawHit[k]->adc()<<"\n";
              #endif
            }
          }
        }
      }
  }
  if(ok) m_mipCounter++;

  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"CALIBRATION: MIP Nevents = "<<m_mipCounter<<"\n";
  #endif
  
  if(MipStatus!=0) return kTRUE;
  
  if(m_mipCounter<1.2*Settings_st[0].MipEventsPerBin) return ok; // gives 20% more 

  Float_t occ=0,x=0,y=0;
  
  if (Settings_st[0].UseMipEtaBin==1)
    MipSpec->GetOccupancyEtaBin(Settings_st[0].MipEventsPerBin,&x,&y,&occ);
  else
    MipSpec->GetOccupancy(Settings_st[0].MipEventsPerBin,&x,&y,&occ);

  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"CALIBRATION: Avg Nevents/(Bin or EtaBin) = "<<x<<" +- "<<y<<"\n";  
  emclog <<"CALIBRATION: fraction of bins nevents > minimum = "<<occ<<"\n";
  #endif
  
  if(occ<Settings_st[0].MipMinOccupancy) return kTRUE;
  
  MipStatus=1;  // ready to calibrate MIP
  return kTRUE;  
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::MipCalib()
{
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"****************** Calibrating spectra for detector "<< detname[detnum].Data()<<"\n";
  #endif
  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();

  Int_t ndiv;
  if(Settings_st[0].UseMipEtaBin==1)
    ndiv=MipSpec->GetNEtaBin();
  else
    ndiv=MipSpec->GetNBin();
  
  for(Int_t i=1;i<=ndiv;i++)
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** Calibrating EtaBin "<< i<<"\n";
    #endif    
    gMessMgr->Info()<<"StEmcCalibrationMaker::MipCalib() - MIP Calibration for EtaBin or tower "<< i<<endm;

    if(Settings_st[0].UseMipEtaBin==1)
      if(MipSpec->GetSumEtaBin(i)>Settings_st[0].MipEventsPerBin) 
        MipSpec->CalibrateEtaBin(i,Settings_st[0].MipPeakFitFuntion);
    else
      if(MipSpec->GetStatus(i)==1)
        if(MipSpec->GetSum(i)>Settings_st[0].MipEventsPerBin)
          MipSpec->CalibrateBin(i,Settings_st[0].MipPeakFitFuntion);
  }

  MipStatus=2;  // Mip fit done
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::MakeCalibration()
{
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  emcCalibration_st* Calib_st=CalibTable->GetTable();
  emcEqualization_st* Equal_st=EqualTable->GetTable();
  emcMipCalib_st* Mip_st=MipTable->GetTable();
  
  Int_t nb=nbins,np;
  Float_t x[10],ex[10],y[10],ey[10];
  Float_t p[5],ep[5],cov[5][5],chisqr;
  for(Int_t bin=1;bin<=nb;bin++) if(Calib_st[bin-1].Status==1)
  {
    // Zero all
    np=0;
    for(Int_t i=0;i<10;i++) {x[i]=0;ex[i]=0;y[i]=0;ey[i]=0;}
    for(Int_t i=0;i<5;i++)  
    {
      p[i]=0;ep[i]=0;chisqr=0;
      for(Int_t j=0;j<5;j++) cov[i][j]=0;
    }
    
    // MIP
    if(Settings_st[0].UseMipCalib==1)
    {
      if(Settings_st[0].UseMipEtaBin==0) // no Eta Bin
      {
        if(Mip_st[bin-1].Status==1)
        {
          x[0]=0; ex[0]=1; y[0]=0; ey[0]=0;
          x[1]=Mip_st[bin-1].MipPeakPosition;
          ex[1]=Mip_st[bin-1].MipPeakPositionError;
          np=2;
        }
      }
      else // use eta bin
      {
        Int_t m,e,s;
        Geo->getBin(bin,m,e,s);
        Int_t etabin=MipSpec->GetEtaBinId(m,e);
        if(Mip_st[etabin-1].Status==1 && Equal_st[bin-1].EqStatus==1 && Equal_st[bin-1].EqSlope!=0)
        {
          Float_t MP=0,eMP=0,SL=0,eSL=0,SH=0,eSH=0;
          MP=Mip_st[etabin-1].MipPeakPosition;
          eMP=Mip_st[etabin-1].MipPeakPositionError;
          SL=Equal_st[bin-1].EqSlope;
          if(SL!=0) eSL=Equal_st[bin-1].EqSlopeError/SL;
          else eSL=0;
          
          SH=Equal_st[bin-1].EqShift;
          if(SH!=0) eSH=Equal_st[bin-1].EqShiftError/SH;
          else eSH=0;
          
          Float_t term1=MP/SL;
          Float_t eterm1=fabs(term1*sqrt((eMP/MP)*(eMP/MP)+eSL*eSL));
          Float_t term2=SH/SL;
          Float_t eterm2=fabs(term2*sqrt(eSH*eSH+eSL*eSL));
          
          x[0]=-term2;
          ex[0]=eterm2;
          if(ex[0]==0) ex[0]=1;
          x[1]=term1-term2;
          ex[1]=sqrt(eterm1*eterm1+eterm2*eterm2);
          if(ex[1]==0) ex[1]=1;
          np=2;
        }
      }
      
      // calculate E/MIP for this tower...
      y[0]=0; ey[0]=0;
      y[1]=Settings_st[0].EOverMipCte; ey[1]=0;
      if(Settings_st[0].DataType==1)
      {         
        Float_t eta=0,phi=0;
        Geo->getEtaPhi(bin,eta,phi);
        Float_t theta=2.*atan(exp(-eta));
        y[1]*=(1.+0.056)/sin(theta); // from V.Rykov
      }
    }
    
    // Pi0
    if(Settings_st[0].UsePi0Calib==1)
    {
    }
    
    // Electron
    if(Settings_st[0].UseElectronCalib==1)
    {
    }
    
    //calculate coefficients. Only MIP now.
    
    if(np>1) // only linear fit for now...
    {
      Float_t aproxslope=(y[np-1]-y[0])/(x[np-1]-x[0]);
      Float_t ss=0,sx=0,sx2=0,sy=0,sxy=0;
      for(Int_t i=0;i<np;i++)
      {
        Float_t err=sqrt((aproxslope*ex[i])*(aproxslope*ex[i])+ey[i]*ey[i]);
        ss+=1./(err*err);
        sx+=x[i]/(err*err);
        sx2+=x[i]*x[i]/(err*err);
        sy+=y[i]/(err*err);
        sxy+=x[i]*y[i]/(err*err);
      }
      Float_t delta=ss*sx2-sx*sx;
      if(delta!=0)
      {
        p[1]=(ss*sxy-sx*sy)/delta;
        p[0]=(sx2*sy-sx*sxy)/delta;
        ep[1]=sqrt(ss/delta);
        ep[0]=sqrt(sx2/delta);
        Float_t covr=-sx/delta;
        cov[0][0]=ep[0]*ep[0];
        cov[1][1]=ep[1]*ep[1];
        cov[1][0]=covr;
        cov[0][1]=covr;
        chisqr=0;
        if(np>2)
        {
          for(Int_t i=0;i<np;i++)
          {
            Float_t err=sqrt((p[1]*ex[i])*(p[1]*ex[i])+ey[i]*ey[i]);
            chisqr+=(y[i]-(p[0]+p[1]*x[i]))*(y[i]-(p[0]+p[1]*x[i]))/(err*err);
          }
          chisqr=sqrt(chisqr/(np-2));
        }
      }
      else 
      {
        #ifdef StEmcCalibrationMaker_DEBUG
        emclog <<"----------------------------------------\n";
        emclog <<"Failed to find calibration constants to tower "<<bin<<"\n";
        for(Int_t k=0;k<np;k++) emclog<<" x = "<<x[k]<<" +- "<<ex[k]<<"    y = "<<y[k]<<" +- "<<ey[k]<<"\n";
        emclog <<"ss = "<<ss<<"  sx = "<<sx<<"  sx2 = "<<sx2<<"  sy = "<<sy<<"  sxy = "<<sxy<<"  delta = "<<delta<<"\n";
        #endif

        np=0;
      }
    }
    
    //Fill Calib Table
    if(np>0)
    {
      Calib_st[bin-1].CalibStatus=1;
      for(Int_t j=0;j<5;j++)
      {
        Calib_st[bin-1].AdcToE[j]=p[j];
        Calib_st[bin-1].AdcToEErr[j]=ep[j];
        for(Int_t k=0;k<5;k++) Calib_st[bin-1].AdcToECov[j][k]=cov[j][k];
      }
      Calib_st[bin-1].AdcToEChiSqr=chisqr;
    }
  }
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::SaveTables()
{
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  emcCalibration_st* Calib_st=CalibTable->GetTable();  
  emcMipCalib_st* Mip_st=MipTable->GetTable();
  emcEqualization_st* Equal_st=EqualTable->GetTable(); 

  char file1[80],file2[80],file3[80],file4[80],file5[80],histfile[80]; 

  // saving tables ...
  
  sprintf(file1,"%sCalSummary.%08d.%06d.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime,
          lastEventDate,lastEventTime);
  sprintf(file2,"%sCalSettings.%08d.%06d.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime,
          lastEventDate,lastEventTime);
  sprintf(file3,"%sCalibration.%08d.%06d.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime,
          lastEventDate,lastEventTime);
  sprintf(file4,"%sEqualization.%08d.%06d.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime,
          lastEventDate,lastEventTime);
  sprintf(file5,"%sMipCalib.%08d.%06d.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime,
          lastEventDate,lastEventTime);
 
  ofstream *out1 = new ofstream(file1);
  SummaryTable->SavePrimitive(*out1,"");
  out1->close();
  delete out1;

  ofstream *out2 = new ofstream(file2);
  SettingsTable->SavePrimitive(*out2,"");
  out2->close();
  delete out2;
    
  ofstream *out3 = new ofstream(file3);
  CalibTable->SavePrimitive(*out3,"");
  out3->close();
  delete out3;
  
  if(EqualTable)
  {
    ofstream *out4 = new ofstream(file4);
    EqualTable->SavePrimitive(*out4,"");
    out4->close();
    delete out4;  
  }

  if(MipTable)
  {
    ofstream *out5 = new ofstream(file5);
    MipTable->SavePrimitive(*out5,"");
    out5->close();
    delete out5;  
  }
  
  // creating histogram file ...
  
  sprintf(histfile,"%sCalib..%08d.%06d.%08d.%06d.hist.root",
          detname[detnum].Data(),firstEventDate,firstEventTime,
          lastEventDate,lastEventTime);
          
  TFile f(histfile,"RECREATE");
  Int_t nb;
  
  TH1F h0("Calib_P0","Calibration constants p0",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h1("Calib_P1","Calibration constants p1",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h2("Calib_P2","Calibration constants p2",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h3("Calib_P3","Calibration constants p3",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h4("Calib_P4","Calibration constants p4",nbins,0.5,(Float_t)nbins+0.5);
  TH1F equa0("EqualSlope","Equalization Slope",nbins,0.5,(Float_t)nbins+0.5);
  TH1F equa1("EqualShift","Equalization Shift",nbins,0.5,(Float_t)nbins+0.5);
  TH1F equalocc("EqualOcc","Equalization Occupancy",nbins,0.5,(Float_t)nbins+0.5);
  TH1F mipocc("MipOcc","Mip occupancy",nbins,0.5,(Float_t)nbins+0.5);
  
  if(MipTable && Settings_st[0].UseMipEtaBin==0) nb=nbins;
  else nb=Settings_st[0].NEtaBins;
  
  TH1F mip0("MipPos","MIP peak position",nb,0.5,(Float_t)nb+0.5);

  for(Int_t i=1;i<=nbins;i++)
  {
    Float_t x=(Float_t)i;
    
    if(Calib_st[i-1].CalibStatus==1)
    {
      h0.Fill(x,Calib_st[i-1].AdcToE[0]);
      h0.SetBinError(h0.GetBin(x),Calib_st[i-1].AdcToEErr[0]);
      h1.Fill(x,Calib_st[i-1].AdcToE[1]);
      h1.SetBinError(h1.GetBin(x),Calib_st[i-1].AdcToEErr[1]);
      h2.Fill(x,Calib_st[i-1].AdcToE[2]);
      h2.SetBinError(h2.GetBin(x),Calib_st[i-1].AdcToEErr[2]);
      h3.Fill(x,Calib_st[i-1].AdcToE[3]);
      h3.SetBinError(h3.GetBin(x),Calib_st[i-1].AdcToEErr[3]);
      h4.Fill(x,Calib_st[i-1].AdcToE[4]);
      h4.SetBinError(h4.GetBin(x),Calib_st[i-1].AdcToEErr[4]);
    }
    
    if(EqualSpec)
    {
      equalocc.Fill(x,EqualSpec->GetSum(i));
      if(Equal_st[i-1].EqStatus==1)
      {
        equa0.Fill(x,Equal_st[i-1].EqSlope);
        equa0.SetBinError(equa0.GetBin(x),Equal_st[i-1].EqSlopeError);
        equa1.Fill(x,Equal_st[i-1].EqShift);
        equa1.SetBinError(equa1.GetBin(x),Equal_st[i-1].EqShiftError);
      }
    }
    
    if(MipSpec) mipocc.Fill(x,MipSpec->GetSum(i));
  }
  
  if(MipSpec)
    for(Int_t i=1;i<=nb;i++)
      if(Mip_st[i-1].Status==1)
      {
        Float_t x=(Float_t)i;
        mip0.Fill(x,Mip_st[i-1].MipPeakPosition);
        mip0.SetBinError(mip0.GetBin(x),Mip_st[i-1].MipPeakPositionError);
      }
        
  h0.Write(); h1.Write(); h2.Write(); h3.Write(); h4.Write();
  if(EqualSpec) { equa0.Write(); equa1.Write(); equalocc.Write();}
  if(MipSpec) {mip0.Write(); mipocc.Write();}
  f.Close();
  
  // finished creating hist file....
  
  return kTRUE;
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::ClearCalibTable()
{ 
  emcCalibration_st* Calib_st=CalibTable->GetTable(); 
  
  for(Int_t i=1;i<=nbins;i++) 
  {
    Calib_st[i-1].CalibStatus=0;
    for(Int_t j=0;j<5;j++)
    {
      Calib_st[i-1].AdcToE[j]=0;
      Calib_st[i-1].AdcToEErr[j]=0;
      for(Int_t k=0;k<5;k++) Calib_st[i-1].AdcToECov[j][k]=0;
    }
    Calib_st[i-1].AdcToEChiSqr=0;
    Calib_st[i-1].AdcPedestal=0;
    Calib_st[i-1].AdcPedestalRms=0;
  }
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::SetCalibStatus()
{
  emcCalibration_st* Calib_st=CalibTable->GetTable();  

  for(Int_t i=1;i<=nbins;i++) 
    if (i>=1861 && i<=2340) Calib_st[i-1].Status=1; // initial 2001 configuration
    else Calib_st[i-1].Status=0;
    
  //for(Int_t i=1;i<=nbins;i++) Calib_st[i-1].Status=1; // FULL EMC
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::ClearEqualTable()
{
  Int_t nb=nbins;
  emcEqualization_st* Equal_st=EqualTable->GetTable(); 
  for(Int_t i=1;i<=nb;i++) 
  {
    Equal_st[i-1].EqStatus=0;
    Equal_st[i-1].EqRef=i;
    Equal_st[i-1].EqSlope=1;
    Equal_st[i-1].EqSlopeError=0;
    Equal_st[i-1].EqShift=0;
    Equal_st[i-1].EqShiftError=0;
    Equal_st[i-1].EqCovariance=0;
    Equal_st[i-1].EqChiSqr=0;
  }
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::ClearMipTable()
{
  Int_t nb=nbins;
  
  emcMipCalib_st* Mip_st=MipTable->GetTable();
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();  
  
  if(Settings_st[0].UseMipEtaBin==0)
  {
    for(Int_t i=1;i<=nb;i++) 
    {
      Int_t m,e,s;
      Geo->getBin(i,m,e,s);
      Mip_st[i-1].FirstModule=m;
      Mip_st[i-1].LastModule=m;
      Mip_st[i-1].FirstEta=e;
      Mip_st[i-1].LastEta=e;
      Mip_st[i-1].FirstSub=s;
      Mip_st[i-1].LastSub=s;
    } 
  }
  else
  {
    nb=Settings_st[0].NEtaBins;
    Float_t ebin=Settings_st[0].EtaBinWidth;
    for(Int_t i=1;i<=nb;i++) 
    {
      Int_t mi,mf,ei,ef,si=1,sf=Geo->NSub();
      CalcEtaBin(i,ebin,&mi,&mf,&ei,&ef);
      Mip_st[i-1].FirstModule=mi;
      Mip_st[i-1].LastModule=mf;
      Mip_st[i-1].FirstEta=ei;
      Mip_st[i-1].LastEta=ef;
      Mip_st[i-1].FirstSub=si;
      Mip_st[i-1].LastSub=sf;
    }
  }
  
  // general information for each eta bin
  for(Int_t i=1;i<=nb;i++) 
  {
    Mip_st[i-1].Status=0;
    Mip_st[i-1].MipFitAdcMin=0;
    Mip_st[i-1].MipFitAdcMax=0;
    for(Int_t j=0;j<10;j++)
    {
      Mip_st[i-1].MipFitParam[j]=0;
      Mip_st[i-1].MipFitParamError[j]=0;
      for(Int_t k=0;k<10;k++) Mip_st[i-1].MipFitCovMatrix[j][k]=0;
    }
    Mip_st[i-1].MipFitChiSqr=0;
    Mip_st[i-1].MipPeakPosition=0;
    Mip_st[i-1].MipPeakPositionError=0;
    Mip_st[i-1].MipPeakWidth=0;
    Mip_st[i-1].MipPeakWidthError=0;
  }
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::CalcEtaBin(Int_t i,Float_t ebin,
                                      Int_t* mi,Int_t* mf,Int_t* ei,Int_t* ef)
{
  
  Int_t e,e1;
  Float_t etai=-1+(Float_t)(i-1)*ebin;
  Float_t etaf=-1+(Float_t)(i)*ebin;
  if (fabs(etai)<0.0001) etai=0;
  if (fabs(etaf)<0.0001) etaf=0;
  if(fabs(etai)>fabs(etaf)) {Float_t t=etai;etai=etaf;etaf=t;}

  e = (Int_t)(fabs(etai)/0.05 + 1.5 );
  e1= (Int_t)(fabs(etaf)/0.05 + 0.5 );
  
  if(etai>=0) {*mi=1;  *mf=60;}
  else        {*mi=61; *mf=120;}
  
  *ei=e;  *ef=e1;

  gMessMgr->Info()<<"StEmcCalibrationMaker: EtaBin "<<i<<"  etai="<<etai<<"  etaf="<<etaf<<"  mi="<<*mi<<"  mf="<<*mf<<"  ei="<<*ei<<"  ef="<<*ef<<endm;
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"EtaBin "<<i<<"  etai = "<<etai<<"  etaf = "<<etaf<<"  mi = "<<*mi<<"  mf = "<<*mf<<"  ei = "<<*ei<<"  ef = "<<*ef<<"\n";
  #endif

}
