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
#include "tables/St_MagFactor_Table.h"
#include "stdlib.h"

#ifndef ST_NO_NAMESPACES
using units::tesla;
#endif

ClassImp(StEmcCalibrationMaker);

#ifdef StEmcCalibrationMaker_DEBUG
ofstream emclog("StEmcCalibrationMaker.emclog");
#endif

//_____________________________________________________________________________
StEmcCalibrationMaker::StEmcCalibrationMaker(const char *name):StMaker(name)
{

  gMessMgr->SetLimit("StEmcCalibrationMaker",1000);

  detnum=-1;

// creating summary table
  SummaryTable = new St_emcCalSummary("Summary",1);
  SummaryTable->SetNRows(1);

  emcCalSummary_st* Summary_st=SummaryTable->GetTable();
  
  Summary_st[0].DetNumber=1;  // 1 = bemc, 2 = bprs, 3 = bsmde, 4 = bsmdp
  Summary_st[0].FirstRun=0;
  Summary_st[0].LastRun=0;
  Summary_st[0].NEvents=0;
    
// finished summary table
  
// creating settings table
  SettingsTable = new St_emcCalSettings("Settings",1);
  SettingsTable->SetNRows(1);
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  
  Settings_st[0].DataType=0;              // 0 - simulation, 1 - Real data
                                          
  Settings_st[0].UseL3Tracks=0;           // 0 - tpt tracks, 1 = l3 tracks
  Settings_st[0].ZVertexCut=40;           // z vertex cut in cm
  
  Settings_st[0].NEtaBins=10;             // number of eta bins
  Settings_st[0].EtaBinWidth=4;           // etabin width in detetor division
    
  Settings_st[0].DoEqualization=1;        // 0 - do not equalize, 1 - equalize
  Settings_st[0].EqualizationMethod=1;    // 0 - mean and rms, 1 - fit
  Settings_st[0].EqEventsPerBin=750;      //670
  Settings_st[0].EqMinNumberOfTracks=50;
  Settings_st[0].EqMinOccupancy=0.99;
  
  Settings_st[0].UseMipCalib=1;
  Settings_st[0].UseMipEtaBin=1;
  Settings_st[0].EOverMipCte=0.290;       // e/MIP for geant
  Settings_st[0].MipPeakFitFuntion=0;
  Settings_st[0].MipEventsPerBin=950;     //600
  Settings_st[0].MipMaxNumberOfTracks=1000;
  Settings_st[0].MipMinOccupancy=0.99;
  Settings_st[0].MipMinimumMomentum=1.5;
  
  Settings_st[0].UsePi0Calib=0;
  
  Settings_st[0].UseElectronCalib=0;
  
  Settings_st[0].DoPedSubtraction=0;      // electronic pedestal subtraction 0=none
  
  Settings_st[0].DoEffPedCalculation=0;
  Settings_st[0].DoEffPedSubtraction=0;
  Settings_st[0].EffPedMethod=0;  // mean and rms
  Settings_st[0].EffPedEventsPerBin=1000;
  Settings_st[0].EffPedMinOccupancy=1.0;
  Settings_st[0].EffPedMaxNumberOfTracks=500;

// finished settings table

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
  
  if(Settings_st[0].DataType!=0) Settings_st[0].EOverMipCte=0.261; // e/MIP for real data
  
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
  //if(calibGeo) delete calibGeo;
  //Geo=new StEmcGeom(detname[detnum].Data());
  calibGeo=StEmcGeom::getEmcGeom(detname[detnum].Data());
  nbins=calibGeo->NModule()*calibGeo->NEta()*calibGeo->NSub();
  
  emcHits.Set(nbins);
  emcHits.Reset();

  trackTower.Set(nbins);  // 0 - not good
  trackTower.Reset();     // 1 - good effective pedestal candidate
                          // 2 - good MIP candidate
  
  firstEventTime=0;
  firstEventRun=0;
  firstEventTime=000000;
  firstEventDate=20300101;    
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
    EqualSpec->SettingsTable=SettingsTable;
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
    MipSpec->SettingsTable=SettingsTable;
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
  
  if(Settings_st[0].DoEffPedCalculation==1) // do eff pedestal calculation
  {    
    Int_t nb=nbins;

    if(EffPedSpec) delete EffPedSpec;
    EffPedSpec=new StEmcEffPedSpectra(detname[detnum].Data());
    EffPedSpec->CalibTable=CalibTable;
    EffPedSpec->SettingsTable=SettingsTable;
    EffPedSpec->nEtaBins=Settings_st[0].NEtaBins;
    EffPedSpec->etaBinWidth=Settings_st[0].EtaBinWidth;
    EffPedSpec->Init();
    
    PedStatus=0;
    m_pedCounter=0;
    
    //QA histogram for equalization
    if (!m_EffPedOccupancy) 
    {
      TString name1="EffPed_"+detname[detnum];
      TString title1="EffPed Occupancy for detetctor "+detname[detnum];
      m_EffPedOccupancy=new TH1F(name1.Data(),title1.Data(),nb,1,(Float_t)nb);
    }
    //
    gMessMgr->Info("StEmcCalibrationMaker::Init() - EffPed initialized");

  }
  
  CalibStatus=0;
  
  // setting Calibration Mode
  Summary_st[0].CalibMode=0;  
  if(Settings_st[0].DoEqualization==1)      Summary_st[0].CalibMode+=1;
  if(Settings_st[0].UseMipCalib==1)         Summary_st[0].CalibMode+=10;
  if(Settings_st[0].UsePi0Calib==1)         Summary_st[0].CalibMode+=100;
  if(Settings_st[0].UseElectronCalib==1)    Summary_st[0].CalibMode+=1000;
  if(Settings_st[0].DoEffPedCalculation==1) Summary_st[0].CalibMode+=10000;
    
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

  cout <<"***** Reading HITS\n";
  switch (Settings_st[0].DataType)
  {
    case(0): kReadOk=ReadHitsStEvent();   break;
    case(1): kReadOk=ReadHitsStEvent();   break;
  }
  
  if(!kReadOk)
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** Can not get Emc Hits or Tracks... Exiting StEmcCalibrationMaker::Make()\n";
    #endif
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - Can not get Emc Hits or Tracks");
    return kStWarn;
  }
  
  // reading B field from Database
  cout <<"***** Reading Magnetic Field\n";
  TDataSet *RunLog=GetInputDB("RunLog");
  BField=0.5;
  if(RunLog)
  {
    St_MagFactor *mag=(St_MagFactor*)RunLog->Find("MagFactor");
    if(mag)
    {
      MagFactor_st *magst=mag->GetTable();
      BField=0.5*magst[0].ScaleFactor;
      cout <<"Magnetic Field = "<<BField<<endl;
    }
  }
  //
 
  cout <<"***** Getting z Vertex\n";
  if(!CalcZVertex()) 
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** zVertex calc fails... Exiting StEmcCalibrationMaker::Make()\n";
    #endif
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - zVertex calc fails");
    return kStWarn;
  }
  
  if(fabs(zVertex)>fabs(Settings_st[0].ZVertexCut)) 
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** zVertex out of limit... Exiting StEmcCalibrationMaker::Make()\n";
    cout   <<"***** zVertex out of limit... Exiting StEmcCalibrationMaker::Make()\n";
    #endif    
    return kStWarn;  
  }
  
  
  cout <<"***** Filling EMC vector\n";
  emcHits.Reset();
  if(!FillEmcVector())
  {
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - No EMC Hits in this event");
    return kStWarn; 
  }
  
  cout <<"***** Checking tracks\n";
  trackTower.Reset();
  CheckTracks();
  
  cout <<"***** Checking Pedestals\n";
  if(!CheckPedestal())
  {
    gMessMgr->Warning("StEmcCalibrationMaker::Make() - Could not check pedestals");
    return kStWarn;
  }

  evnumber++;
  Int_t firstEventTimeNew=GetTime();
  Int_t firstEventDateNew=GetDate();
  
  cout <<"StEmcCalibrationMaker::Make() - First   event date = "<<firstEventDate<<"  time = "<<firstEventTime<<endl;  
  cout <<"StEmcCalibrationMaker::Make() - Current event date = "<<GetDate()<<"  time = "<<GetTime()<<endl;
  
  if(firstEventDateNew<firstEventDate || (firstEventDateNew==firstEventDate && firstEventTimeNew<firstEventTime))
  {
    firstEventTime=GetTime();
    firstEventDate=GetDate();
    firstEventRun=GetRunNumber();
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
   
  if(Settings_st[0].DoEffPedCalculation==1) FillEffPed();
  if(Settings_st[0].DoEffPedCalculation==1 && PedStatus==1)
  {
    gMessMgr->Info("StEmcCalibrationMaker::Make() - doing EffPed calculation");
    MakeEffPed();
  } 
  
    
  Int_t done=0;
  if(Settings_st[0].DoEqualization==1 && EqStatus==2)   done+=1;
  if(Settings_st[0].UseMipCalib==1    && MipStatus==2)  done+=10;
//  if(Settings_st[0].UsePi0Calib==1)      done+=100;
//  if(Settings_st[0].UseElectronCalib==1) done+=1000;
  if(Settings_st[0].DoEffPedCalculation==1 && PedStatus==2) done+=10000;
  
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
    //Init();
    return kStFATAL;
  }
    
  clock.Stop();

  #ifdef StEmcCalibrationMaker_DEBUG  
  emclog <<"Time to run StEmcCalibrationMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  #endif
  cout   <<"Time to run StEmcCalibrationMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";

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
void StEmcCalibrationMaker::Clear(const Option_t *option)
{
  //if(emc) delete emc;
  //if(event) delete event;
  return;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::ReadHitsStEvent()
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
      //cout <<"Number of tracks = "<<nTracks<<endl;
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
  
  cout<<"***** No vertex in the StEvent.  try to calculate one...\n";  
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
  return kFALSE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CheckTracks()
{    
      
  trackTower.Reset();
  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  emcCalibration_st* Calib_st=CalibTable->GetTable();  

  if(nTracks==0) // no tracks... all towers are good for pedestal
  {
    for(Int_t n=0;n<nbins;n++)  if(Calib_st[n].Status==1) trackTower[n]=1;
    return kTRUE;
  }

  Float_t eta,phi,eta1,phi1;
  
  //TArrayI tmp1,tmp2;
  tmp1.Set(nbins);
  tmp1.Reset(); // number of tracks projected in the bin
  tmp2.Set(nTracks);
  tmp2.Reset(); // bin of projected track
  
  StTrack* track;
  
  for(Int_t i=0;i<nTracks;i++)
  {
    Int_t mtr,etr,str,idtr=0,mtr1,etr1,str1,idtr1=0;

    eta=10;eta1=10;phi=0;phi1=0;
    
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
    
    if(ProjectTrack(track,(double)calibGeo->Radius(),&eta,&phi))
    {
      if(calibGeo->getBin(phi,eta,mtr,etr,str)==0) if(str!=-1) calibGeo->getId(mtr,etr,str,idtr);

      if(ProjectTrack(track,(double)(calibGeo->Radius()+2*calibGeo->YWidth()),&eta1,&phi1))
        if(calibGeo->getBin(phi1,eta1,mtr1,etr1,str1)==0) if(str1!=-1) calibGeo->getId(mtr1,etr1,str1,idtr1);
      
      if(idtr!=0 && idtr==idtr1) // good track candidate
      {
        /*Float_t etac,phic;
        calibGeo->getEtaPhi(idtr,etac,phic);
        Float_t radius=sqrt((eta-etac)*(eta-etac)+(phi-phic)*(phi-phic));
        Float_t radius1=sqrt((eta1-etac)*(eta1-etac)+(phi1-phic)*(phi1-phic));
        
        if(radius<0.015) */tmp2[i]=idtr; 
      }
      if(idtr!=0) tmp1[idtr-1]++;
      if(idtr1!=0 && idtr!=idtr1) tmp1[idtr1-1]++;
    }
  }

  for(Int_t i=0;i<nbins;i++)
  {
    if(tmp1[i]<=1) // max one track in this bin. 0 is for effective pedestal
    {
      trackTower[i]=1;
      Int_t id=i+1;
      Float_t eta,phi;
      calibGeo->getEtaPhi(id,eta,phi);
      for(Int_t j=-1;j<=1;j++)
        for(Int_t k=-1;k<=1;k++)
          if(!(j==0 && k==0))
          {
            Int_t m,e,s,m1,e1,s1,id1;
            calibGeo->getBin(id,m,e,s);
            e+=j; s+=k;
            if(e==0) {e=1; calibGeo->getBin(phi,-eta,m1,e1,s1); m=m1;}
            if(s==0) 
            {
              s=calibGeo->NSub(); 
              m--;
              if(m==60) m=120;
              if(m==0) m=60;
            }
            if(s>calibGeo->NSub()) 
            {
              s=1;
              m++;
              if(m==121) m=61;
              if(m==61) m=1;
            }
            if(e<=calibGeo->NEta())
            {
              calibGeo->getId(m,e,s,id1);
              if(tmp1[id1-1]>0) trackTower[i]=0; // there is one track in neigh. tower
            }
          }
    } 
    else trackTower[i]=0; // not a good tower 
  }
  
  // at this point if trackTower[i]=0 this tower is not good anyway
  // and trackTower[i]=1 if the tower is good. 
  // now check if tower is good for each case (pedestal,mip,etc)
  
  Int_t nmips=0,cand=0;
  
  for(Int_t i=0;i<nTracks;i++)
  {
    if(tmp2[i]!=0)
    {
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
      
      StTrackGeometry* trgeo = track->geometry();
      const StThreeVectorF momentum=trgeo->momentum();
      StTrackFitTraits&  fit=track->fitTraits();
      Int_t npoints=fit.numberOfFitPoints();
        
      if(momentum.mag()>=Settings_st[0].MipMinimumMomentum && npoints>0)
      {
        if(trackTower[tmp2[i]-1]==1) 
        {
          trackTower[tmp2[i]-1]=2; 
          cand++;
          cout   <<"TRACKCHECK: MIP TowerCandidate = "<<tmp2[i]<<"  TowerStatus = "<<Calib_st[tmp2[i]].Status<<"  momentum = "<<momentum.mag()<<"  npoints = "<<npoints<<"  adc = "<<emcHits[tmp2[i]-1]<<"\n";
          #ifdef StEmcCalibrationMaker_DEBUG
          emclog <<"TRACKCHECK: MIP TowerCandidate = "<<tmp2[i]<<"  TowerStatus = "<<Calib_st[tmp2[i]].Status<<"  momentum = "<<momentum.mag()<<"  npoints = "<<npoints<<"  adc = "<<emcHits[tmp2[i]-1]<<"\n";
          #endif
        }// good MIP candidate
        nmips++;
      }
      else trackTower[tmp2[i]-1]=0;
    }
  }
  cout   <<"TRACKCHECK: NMips = "<<nmips<<"  TowersCandidates = "<<cand<<"\n";
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"TRACKCHECK: NMips = "<<nmips<<"  TowersCandidates = "<<cand<<"\n";
  #endif
  
  // check for valid pedestals bins
  for(Int_t i=0;i<nbins;i++) if(trackTower[i]==1 && tmp1[i]!=0) trackTower[i]=0;

  return kTRUE;  
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::ProjectTrack(StTrack* track,double radius, Float_t *ETA, Float_t *PHI)
{  
  StTrackGeometry* trgeo = track->geometry();
  Short_t charge=trgeo->charge();
  const StThreeVectorF origin=trgeo->origin();
  const StThreeVectorF momentum=trgeo->momentum();
   
  StPhysicalHelixD helix(momentum,origin,BField*tesla,(double)charge);
  
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
  //if(fabs(eta)>calibGeo->EtaMax()) return kFALSE;
  
  return kTRUE;
  
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::CheckPedestal()
{
  cout <<"Checking pedestals for detector "<<detname[detnum]<<endl;
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  
  St_emcPedestal* ped=NULL;
  TString calibDb="Calibrations/emc/"+detname[detnum];
  TDataSet *emcDb=GetInputDB(calibDb.Data());
  
  if(!emcDb && Settings_st[0].DoPedSubtraction == 1) return kFALSE;
  
  if(detnum==0) // bemc
  {
    if(emcDb) ped=(St_emcPedestal*)emcDb->Find("bemcPedestal");
    if(!ped && Settings_st[0].DoPedSubtraction == 1) return kFALSE;
    emcPedestal_st *pedst=NULL;
    if(ped) pedst=ped->GetTable();

    for(Int_t i=0;i<nbins;i++)
    {
      if (pedst && Settings_st[0].DoPedSubtraction == 1) emcHits[i]-=(Int_t)pedst[i].AdcPedestal;
      if (pedst)
      {
        if(emcHits[i]<2.*pedst[i].AdcPedestalRMS) emcHits[i]=0;
      }
      else
      { 
        if(emcHits[i]<4.0) emcHits[i]=0; // 2* avg ped RMS
      }
    }
  }
  
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillEqual()
{      
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  
  if(nTracks<Settings_st[0].EqMinNumberOfTracks && Settings_st[0].EqMinNumberOfTracks!=0) return kFALSE;
  
  if(fabs(zVertex)>fabs(Settings_st[0].ZVertexCut)) return kFALSE;  

  for(Int_t j=0;j<nbins;j++)
  {
    Int_t did=j+1;
    if(EqualSpec->GetStatus(did)==1 && emcHits[j]>0) 
    {
      EqualSpec->FillSpectra(did,emcHits[did-1]);
      //FILL QA HISTOGRAM*********************
      m_EqualOccupancy->Fill(did);
      //**************************************
    }
  }
  m_equalCounter++;
  
  if(EqStatus!= 0) return kTRUE; // it reached the minimum number of events. I don't need to check it again
  //if(m_equalCounter<1.2*Settings_st[0].EqEventsPerBin) return kTRUE; // gives 20% more 
  
// checking the average event number per bin event numbers ...
  Float_t x,y,z;
  EqualSpec->GetOccupancy(Settings_st[0].EqEventsPerBin,&x,&y,&z);
  
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"EQUALIZATION: Avg Nevents/bin = "<<x<<" +- "<<y<<"\n";
  emclog <<"EQUALIZATION: fraction of bins nevents > minimum = "<<z<<"\n";
  #endif
  cout   <<"EQUALIZATION: Avg Nevents/bin = "<<x<<" +- "<<y<<"\n";
  cout   <<"EQUALIZATION: fraction of bins nevents > minimum = "<<z<<"\n";

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
  
  Float_t sum=0,nb=0;
  for(Int_t i=1;i<=EqualSpec->GetNBin();i++)
    if(EqualSpec->GetStatus(i)>0 && EqualSpec->GetSum(i)>=Settings_st[0].EqEventsPerBin)
    {
      Float_t mean,rms;
      EqualSpec->GetLogMeanAndRms(i,20,140,&mean,&rms);
      sum+=mean;
      nb++;
    }
  if(nb==0) return kFALSE;
  Float_t MEAN=sum/nb; // global mean value used to choose reference spectra
  
  for(Int_t i=1;i<=ndiv;i++)
  {
    #ifdef StEmcCalibrationMaker_DEBUG
    emclog <<"***** Equalizing EtaBin "<< i<<"\n";
    #endif
    gMessMgr->Info()<<"StEmcCalibrationMaker::Equalize() - Equalizing EtaBin "<< i<<endm;
    sum=0;
    nb=0;
    Int_t mi,mf,ei,ef;
    CalcEtaBin(i,etabin,&mi,&mf,&ei,&ef);
    Int_t numberReady=0;
    for(Int_t m=mi;m<mf+1;m++)
      for(Int_t e=ei;e<ef+1;e++)
        for(Int_t s=1;s<EqualSpec->GetNSub()+1;s++)
        {
          Int_t id1=EqualSpec->GetID(m,e,s); 
          if(EqualSpec->GetStatus(id1)>0 && EqualSpec->GetSum(id1)>=Settings_st[0].EqEventsPerBin) 
          {
            numberReady++;
            Float_t mean,rms;
            EqualSpec->GetLogMeanAndRms(id1,20,140,&mean,&rms);
            sum+=mean;
          }
        }      

    if(numberReady>0)
    {
      Float_t LOCALMEAN = sum/numberReady;
      Float_t dmean=5000,refmean=0;
      Int_t ref=0;
      for(Int_t m=mi;m<mf+1;m++)
        for(Int_t e=ei;e<ef+1;e++)
          for(Int_t s=1;s<EqualSpec->GetNSub()+1;s++)
          {
            Int_t id1=EqualSpec->GetID(m,e,s); 
            if(EqualSpec->GetStatus(id1)>0 && EqualSpec->GetSum(id1)>=Settings_st[0].EqEventsPerBin)
            {
              Float_t mean,rms;
              EqualSpec->GetLogMeanAndRms(id1,20,140,&mean,&rms);
              Float_t dmean1=fabs(mean-LOCALMEAN);
              if(dmean1<dmean) {dmean=dmean1; refmean=mean; ref=id1;}
            }
          }
      #ifdef StEmcCalibrationMaker_DEBUG
      emclog <<"***** Ref spectrum for etabin = "<<i
             <<"  ref = "<<ref<<"  mean = "<<refmean<<"  Global mean = "<<MEAN
             <<"  Local mean = "<<LOCALMEAN<<"\n";
      #endif
      cout   <<"***** Ref spectrum for etabin = "<<i
             <<"  ref = "<<ref<<"  mean = "<<refmean<<"  Global mean = "<<MEAN
             <<"  Local mean = "<<LOCALMEAN<<"\n";
      for(Int_t m=mi;m<mf+1;m++)
        for(Int_t e=ei;e<ef+1;e++)
          for(Int_t s=1;s<EqualSpec->GetNSub()+1;s++)
          {
            Int_t id1=EqualSpec->GetID(m,e,s);
            if(EqualSpec->GetStatus(id1)>0 && EqualSpec->GetSum(id1)>Settings_st[0].EqEventsPerBin) 
              EqualSpec->Equalize(ref,id1,Settings_st[0].EqualizationMethod);
          }
    }
    else
    {
      #ifdef StEmcCalibrationMaker_DEBUG
      emclog <<"***** No Equalization done for etabin  = "<<i<<endl;
      #endif   
      cout   <<"***** No Equalization done for etabin  = "<<i<<endl;
    }
  }

  EqStatus=2;  // equalized*/
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillMipCalib()
{  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();  

  if(nTracks>Settings_st[0].MipMaxNumberOfTracks) return kFALSE;

  if(fabs(zVertex)>fabs(Settings_st[0].ZVertexCut)) return kFALSE;  
  
  Bool_t ok=kFALSE;
  Int_t mipCandidate=0,mipAccepted=0;
  
  for(Int_t did=1;did<=nbins;did++)
  {
    if(trackTower[did-1]==2 && MipSpec->GetStatus(did)==1 && emcHits[did-1]>0) 
    {
      MipSpec->FillSpectra(did,emcHits[did-1]);
      m_MipOccupancy->Fill(did);
      ok=kTRUE;
      mipAccepted++;
      cout <<"CALIBRATION: MIP Hit. id = "<<did<<"  adc = "<<emcHits[did-1]<<endl;
      #ifdef StEmcCalibrationMaker_DEBUG
      emclog <<"CALIBRATION: MIP Hit. id = "<<did<<"  adc = "<<emcHits[did-1]<<endl;
      #endif
    }
    if(trackTower[did-1]==2 && MipSpec->GetStatus(did)==1) mipCandidate++;  
  }
  if(ok) m_mipCounter++;
  cout   <<"CALIBRATION: MIP Candidates = "<<mipCandidate<<"  accepted = "<<mipAccepted<<endl;
  cout   <<"CALIBRATION: MIP Nevents = "<<m_mipCounter<<"\n";
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"CALIBRATION: MIP Candidates = "<<mipCandidate<<"  accepted = "<<mipAccepted<<endl;
  emclog <<"CALIBRATION: MIP Nevents = "<<m_mipCounter<<"\n";
  #endif
  
  if(MipStatus!=0) return kTRUE;
  
  //if(m_mipCounter<1.2*Settings_st[0].MipEventsPerBin) return ok; // gives 20% more 

  Float_t occ=0,x=0,y=0;
  
  if (Settings_st[0].UseMipEtaBin==1)
    MipSpec->GetOccupancyEtaBin(Settings_st[0].MipEventsPerBin,&x,&y,&occ);
  else
    MipSpec->GetOccupancy(Settings_st[0].MipEventsPerBin,&x,&y,&occ);

  cout   <<"CALIBRATION: Avg Nevents/(Bin or EtaBin) = "<<x<<" +- "<<y<<"\n";  
  cout   <<"CALIBRATION: fraction of bins nevents > minimum = "<<occ<<"\n";

  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"CALIBRATION: Avg Nevents/(Bin or EtaBin) = "<<x<<" +- "<<y<<"\n";  
  emclog <<"CALIBRATION: fraction of bins nevents > minimum = "<<occ<<"\n";
  #endif
  
  if(occ<Settings_st[0].MipMinOccupancy) return kTRUE;
  
  // if equalization is needed, check if it is done
  if(Settings_st[0].UseMipEtaBin==1 && EqStatus!=2) return kTRUE;
  
  // if effective pedestal is needed, check if it is done
  if(Settings_st[0].DoEffPedCalculation==1 && 
     Settings_st[0].DoEffPedSubtraction==1 &&
     PedStatus!=2) return kTRUE;

  // all checked at this point
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
    cout <<"***** Calibrating EtaBin "<< i<<"\n";
    #endif    
    gMessMgr->Info()<<"StEmcCalibrationMaker::MipCalib() - MIP Calibration for EtaBin or tower "<< i<<endm;

    if(Settings_st[0].UseMipEtaBin==1)
      if(MipSpec->GetSumEtaBin(i)>=Settings_st[0].MipEventsPerBin) 
        MipSpec->CalibrateEtaBin(i,Settings_st[0].MipPeakFitFuntion);
    else
      if(MipSpec->GetStatus(i)==1)
        if(MipSpec->GetSum(i)>=Settings_st[0].MipEventsPerBin)
          MipSpec->CalibrateBin(i,Settings_st[0].MipPeakFitFuntion);
  }

  MipStatus=2;  // Mip fit done
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillEffPed()
{  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();  

  if(nTracks>Settings_st[0].MipMaxNumberOfTracks) return kFALSE;

  if(fabs(zVertex)>fabs(Settings_st[0].ZVertexCut)) return kFALSE;  
  
  Bool_t ok=kFALSE;
  Int_t nn=0;
  
  for(Int_t did=1;did<=nbins;did++)
    if(trackTower[did-1]==1 && EffPedSpec->GetStatus(did)==1) 
    {
      EffPedSpec->FillSpectra(did,emcHits[did-1]);
      m_EffPedOccupancy->Fill(did);
      ok=kTRUE;
      nn++;
    }
  
  #ifdef StEmcCalibrationMaker_DEBUG
  if(ok)
  {
    cout   <<"EFFPED: Number of hits for pedestal in this event =  "<<nn<<endl;
    emclog <<"EFFPED: Number of hits for pedestal in this event =  "<<nn<<endl;
  }
  #endif

  if(ok) m_pedCounter++;

  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"EFFPED: Ped Nevents = "<<m_pedCounter<<"\n";
  #endif
  
  if(PedStatus!=0) return kTRUE;
  
  if(m_pedCounter<1.2*Settings_st[0].EffPedEventsPerBin) return ok; // gives 20% more 

  Float_t occ=0,x=0,y=0;
  
  EffPedSpec->GetOccupancy(Settings_st[0].EffPedEventsPerBin,&x,&y,&occ);

  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"EFFPED: Avg Nevents/(Bin or EtaBin) = "<<x<<" +- "<<y<<"\n";  
  emclog <<"EFFPED: fraction of bins nevents > minimum = "<<occ<<"\n";
  cout   <<"EFFPED: Avg Nevents/(Bin or EtaBin) = "<<x<<" +- "<<y<<"\n";  
  cout   <<"EFFPED: fraction of bins nevents > minimum = "<<occ<<"\n";
  #endif
  
  if(occ<Settings_st[0].EffPedMinOccupancy) return kTRUE;
  
  PedStatus=1;  // ready to calculate effective pedestals
  return kTRUE;  
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::MakeEffPed()
{
  #ifdef StEmcCalibrationMaker_DEBUG  
  emclog <<"****************** Equalizing spectra for detector "<< detname[detnum].Data()<<"\n";
  #endif
  
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  
  for(Int_t did=1;did<=nbins;did++)
  {
    if(EffPedSpec->GetStatus(did)>0 && 
       EffPedSpec->GetSum(did)>=Settings_st[0].EffPedEventsPerBin) 
    {
      EffPedSpec->CalcPedestal(did);
    }
  }
  
  PedStatus=2;
  return kTRUE;
}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::MakeCalibration()
{
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();
  emcCalibration_st* Calib_st=CalibTable->GetTable();
  
  emcMipCalib_st* Mip_st;
  if(MipTable) Mip_st=MipTable->GetTable();
  
  emcEqualization_st* Equal_st;
  if(EqualTable) Equal_st=EqualTable->GetTable(); 

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
        calibGeo->getBin(bin,m,e,s);
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
      Float_t eta=0,phi=0;
      calibGeo->getEtaPhi(bin,eta,phi);
      Float_t theta=2.*atan(exp(-eta));
      y[1]*=(1.+0.056*eta*eta)/sin(theta); // from V.Rykov
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
   
  emcMipCalib_st* Mip_st;
  if(MipTable) Mip_st=MipTable->GetTable();
  
  emcEqualization_st* Equal_st;
  if(EqualTable) Equal_st=EqualTable->GetTable(); 

  char file1[80],file2[80],file3[80],file4[80],file5[80],histfile[80],spec[80]; 

  // saving tables ...
  
  sprintf(file1,"%sCalSummary.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime);
  sprintf(file2,"%sCalSettings.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime);
  sprintf(file3,"%sCalibration.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime);
  sprintf(file4,"%sEqualization.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime);
  sprintf(file5,"%sMipCalib.%08d.%06d.C",
          detname[detnum].Data(),firstEventDate,firstEventTime);
           
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
  
  sprintf(histfile,"%sCalib.%08d.%06d.hist.root",
          detname[detnum].Data(),firstEventDate,firstEventTime);
          
  TFile f(histfile,"RECREATE");
  Int_t nb;
  
  TH1F h0("Calib_P0","Calibration constants p0",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h1("Calib_P1","Calibration constants p1",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h2("Calib_P2","Calibration constants p2",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h3("Calib_P3","Calibration constants p3",nbins,0.5,(Float_t)nbins+0.5);
  TH1F h4("Calib_P4","Calibration constants p4",nbins,0.5,(Float_t)nbins+0.5);

  TH1F h5("gain*SinTheta","Calibration gain*sin(theta)",nbins,0.5,(Float_t)nbins+0.5);

  TH2F h6("gain_X_theta","Calibration gain versus Theta",100,0,3.1415/2,200,0,.02);
  TH2F h7("gain*SinTheta_X_theta","Calibration gain*sin(theta) versus theat",100,0,3.1415/2,200,0,.02);

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
      h0.SetBinError(h0.FindBin(x),Calib_st[i-1].AdcToEErr[0]);
      h1.Fill(x,Calib_st[i-1].AdcToE[1]);
      h1.SetBinError(h1.FindBin(x),Calib_st[i-1].AdcToEErr[1]);
      h2.Fill(x,Calib_st[i-1].AdcToE[2]);
      h2.SetBinError(h2.FindBin(x),Calib_st[i-1].AdcToEErr[2]);
      h3.Fill(x,Calib_st[i-1].AdcToE[3]);
      h3.SetBinError(h3.FindBin(x),Calib_st[i-1].AdcToEErr[3]);
      h4.Fill(x,Calib_st[i-1].AdcToE[4]);
      h4.SetBinError(h4.FindBin(x),Calib_st[i-1].AdcToEErr[4]);

      Float_t eta=0,phi=0;
      calibGeo->getEtaPhi(i,eta,phi);
      Float_t theta=2.*atan(exp(-eta));
      
      h5.Fill(x,Calib_st[i-1].AdcToE[1]*sin(theta));
      h5.SetBinError(h4.FindBin(x),Calib_st[i-1].AdcToEErr[1]*sin(theta));
      
      h6.Fill(theta,Calib_st[i-1].AdcToE[1]);
      h7.Fill(theta,Calib_st[i-1].AdcToE[1]*sin(theta));
      
    }
    
    if(EqualSpec)
    {
      if(EqualSpec->GetStatus(i)==1) equalocc.Fill(x,EqualSpec->GetSum(i));
      if(Equal_st[i-1].EqStatus==1)
      {
        equa0.Fill(x,Equal_st[i-1].EqSlope);
        equa0.SetBinError(equa0.FindBin(x),Equal_st[i-1].EqSlopeError);
        equa1.Fill(x,Equal_st[i-1].EqShift);
        equa1.SetBinError(equa1.FindBin(x),Equal_st[i-1].EqShiftError);
      }
    }
    
    if(MipSpec) 
      if(MipSpec->GetStatus(i)==1) mipocc.Fill(x,MipSpec->GetSum(i));
  }

  if(EqualSpec) 
  {     
    sprintf(spec ,"%sEqualSpec.%08d.%06d.dat",
    detname[detnum].Data(),firstEventDate,firstEventTime);
    EqualSpec->SaveAll(spec);
    equa0.Write(); equa1.Write(); equalocc.Write();
  }
  
  if(MipSpec)
  {
    for(Int_t i=1;i<=nb;i++)
    {
      char name[80],title[80];
      sprintf(name,"MipEtaBin%02d",i);
      sprintf(title,"MIP Spectra for EtaBin %02d",i);
      TH1F mipspecetabin(name,title,MipSpec->GetNAdcMax(),(Float_t)0,(Float_t)MipSpec->GetNAdcMax()-1);
      if(Mip_st[i-1].Status==1)
      {
        Float_t x=(Float_t)i;
        mip0.Fill(x,Mip_st[i-1].MipPeakPosition);
        mip0.SetBinError(mip0.FindBin(x),Mip_st[i-1].MipPeakPositionError);
      }
      TArrayF spectemp=MipSpec->GetEtaBinSpectra(i);
      for(Int_t j=0;j<MipSpec->GetNAdcMax();j++) mipspecetabin.Fill((Float_t)j,spectemp[j]);
      mipspecetabin.Write();
    }
    sprintf(spec ,"%sMipSpec.%08d.%06d.dat",
    detname[detnum].Data(),firstEventDate,firstEventTime);
    MipSpec->SaveAll(spec);
    mip0.Write(); mipocc.Write();
  }
        
  h0.Write(); h1.Write(); h2.Write(); h3.Write(); h4.Write(); h5.Write(); h6.Write(); h7.Write(); 
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
  }
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::SetCalibStatus()
{
  emcCalibration_st* Calib_st=CalibTable->GetTable();  

  for(Int_t i=1;i<=nbins;i++)
  { 
    Calib_st[i-1].Status=0;
    if (i>=1861 && i<=2340) Calib_st[i-1].Status=1; // initial 2001 configuration
    if (i>=2021 && i<=2100) Calib_st[i-1].Status=0; // initial 2001 configuration    
  } 
  
  Calib_st[2309-1].Status=0;
  Calib_st[2254-1].Status=0;
  Calib_st[2288-1].Status=0;
  Calib_st[2325-1].Status=0;
  Calib_st[2150-1].Status=0;
  Calib_st[1986-1].Status=0;
  Calib_st[1979-1].Status=0;
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
      calibGeo->getBin(i,m,e,s);
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
      Int_t mi,mf,ei,ef,si=1,sf=calibGeo->NSub();
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
  emcCalSettings_st* Settings_st=SettingsTable->GetTable();  
  Int_t nb=Settings_st[0].NEtaBins;
  Int_t neta=calibGeo->NEta();
  Int_t eei=1;
  Int_t eef=eei+(Int_t)ebin-1;
  Int_t mmi=1;
  Int_t mmf=60;
  
  for(Int_t j=1;j<=nb;j++)
  {
    if (i==j) goto etabinok;
    eei=eef+1;
    if(eei>neta) {eei=1;mmi=61; mmf=120;}
    eef=eei+(Int_t)ebin-1;
    if(eef>neta) eef=neta;
  }
  etabinok:
  *ei=eei; *ef=eef;
  *mi=mmi; *mf=mmf;
  Float_t etai,etaf;
  calibGeo->getEta(mmi,eei,etai);
  calibGeo->getEta(mmi,eef,etaf);
  
  gMessMgr->Info()<<"StEmcCalibrationMaker: EtaBin "<<i<<"  etai="<<etai<<"  etaf="<<etaf<<"  mi="<<*mi<<"  mf="<<*mf<<"  ei="<<*ei<<"  ef="<<*ef<<endm;
  #ifdef StEmcCalibrationMaker_DEBUG
  emclog <<"EtaBin "<<i<<"  etai = "<<etai<<"  etaf = "<<etaf<<"  mi = "<<*mi<<"  mf = "<<*mf<<"  ei = "<<*ei<<"  ef = "<<*ef<<"\n";
  #endif

}
//_____________________________________________________________________________
Bool_t StEmcCalibrationMaker::FillEmcVector()
{
  Int_t tmp=0;
  StDetectorId id = static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
  StEmcDetector* detector=emc->detector(id);
  if(detector)
    for(Int_t m=1;m<=120;m++)
    {
      StEmcModule* module = detector->module(m);
      if(module)
      {
        StSPtrVecEmcRawHit& rawHit=module->hits();
        for(UInt_t k=0;k<rawHit.size();k++)
        {
          tmp++;
          Int_t did;
          Int_t mod=rawHit[k]->module();
          Int_t e=rawHit[k]->eta(); 
          Int_t s=abs(rawHit[k]->sub());
          calibGeo->getId(mod,e,s,did);
          emcHits[did-1]=rawHit[k]->adc();
        }
      }
    }
  if(tmp>0) return kTRUE;
  return kFALSE;
}
//_____________________________________________________________________________
void StEmcCalibrationMaker::LoadSpectra(char *time)
{
  cout <<"Recovering spectra from file with timestamp: "<<time<<"\n";
  char spec[120];
  TString stamp=time;
  if(EqualSpec) 
  {     
    sprintf(spec ,"%sEqualSpec.%s.dat",
    detname[detnum].Data(),stamp.Data());
    EqualSpec->LoadAll(spec);
  }
  if(MipSpec) 
  {     
    sprintf(spec ,"%sMipSpec.%s.dat",
    detname[detnum].Data(),stamp.Data());
    MipSpec->LoadAll(spec);
  }
  evnumber=1;
  Int_t firstEventTimeNew=atoi(stamp(9,6).Data());
  Int_t firstEventDateNew=atoi(stamp(0,8).Data());
  if(firstEventDateNew<firstEventDate || (firstEventDateNew==firstEventDate && firstEventTimeNew<firstEventTime))
  {
    firstEventTime=firstEventTimeNew;
    firstEventDate=firstEventDateNew;    
  }
}
