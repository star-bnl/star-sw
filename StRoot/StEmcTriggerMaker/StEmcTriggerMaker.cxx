
//////////////////////////////////////////////////////////////////////////
//                                                             
// StEmcTriggerMaker A. A. P. Suaide (C) 2001  
//                                                             
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <math.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMaker.h"
#include "StEmcTriggerMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "TFile.h"
#include "tables/St_emcTrigger_Table.h"
#include "tables/St_emcPatchTrigger_Table.h"
#include "tables/St_emcJetTrigger_Table.h"

ClassImp(StEmcTriggerMaker)

//_____________________________________________________________________________
StEmcTriggerMaker::StEmcTriggerMaker(char *name):StMaker(name)
{
  TitleHist="BEMC";
  
  Float_t et[]={0,25,50,75,100,125,150,175,200,225,250};
  TArrayF t((Int_t)11,et);
  EtTh=t;

  Float_t ra[]={0,0.2,0.4,0.6,0.8,1.0};
  TArrayF t1((Int_t)6,ra);
  RatioTh=t1;
    
  Float_t ht[]={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25};
  TArrayF temp((Int_t)26,ht);
  HighTowerTh=temp;
  PatchTh=temp;
  JetTh=temp;
}
//____________________________________________________________________________
StEmcTriggerMaker::~StEmcTriggerMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Init()
{
  TString a="Et - "+TitleHist;
  EtHist=new TH1F("EtHist",a.Data(),50,0,500);
  EtHist->SetFillColor(13);
  EtHist->SetXTitle("Et Thr (GeV)");

  a="Ratio - "+TitleHist;
  RatioHist=new TH1F("RatioHist",a.Data(),10,0,1);
  RatioHist->SetFillColor(13);
  RatioHist->SetXTitle("Ratio Thr (GeV)");
  
  a="HighTower - "+TitleHist;
  HighTowerHist=new TH1F("HighTowerHist",a.Data(),64,0,32);
  HighTowerHist->SetFillColor(13);
  HighTowerHist->SetXTitle("HT Thr (GeV)");

  a="Patch - "+TitleHist;
  PatchHist=new TH1F("PatchHist",a.Data(),64,0,32);
  PatchHist->SetFillColor(13);
  PatchHist->SetXTitle("Patch Thr (GeV)");

  a="Jet - "+TitleHist;
  JetHist=new TH1F("JetHist",a.Data(),64,0,32);
  JetHist->SetFillColor(13);
  JetHist->SetXTitle("Jet Thr (GeV)");

  a="HT x Patch - "+TitleHist;
  HTxPatch=new TH2F("HTxPatch",a.Data(),64,0,32,64,0,32);
  HTxPatch->SetXTitle("HT Thr (GeV)");
  HTxPatch->SetYTitle("Patch Thr (GeV)");

  a="HT trigger - "+TitleHist; 
  HT2D = new TH2F("HTtrigger",a.Data(),300,-0.5,299.5,64,0,64); 
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Make()
{

// Check if there is any pi0 present in the event generator level

  StEvent* event=(StEvent*)GetInputDS("StEvent");
  if(!event) 
  {
    cout <<"No StEvent \n";
    return kStWarn;
  }
  StEmcCollection* emc=event->emcCollection();
  if(!emc) 
  {
    cout <<"No EMC\n";
    return kStWarn;
  }
  BemcTrigger=new StBemcTrigger(GetDate(),GetTime());
  BemcTrigger->SetEmcCollection(emc);
  BemcTrigger->SetThreshold(EtTh,RatioTh,HighTowerTh,PatchTh,JetTh);
  BemcTrigger->MakeTrigger();
  
  AddData(BemcTrigger->EmcTrigger);
  AddData(BemcTrigger->PatchTrigger);
  AddData(BemcTrigger->JetTrigger);
  FillHistograms();
  
  return kStOK;
}
//_____________________________________________________________________________
void StEmcTriggerMaker::FillHistograms()
{

  St_emcTrigger* trigger=BemcTrigger->EmcTrigger;
  emcTrigger_st* table=trigger->GetTable();
  
  St_emcPatchTrigger* patch=BemcTrigger->PatchTrigger;
  emcPatchTrigger_st* pa=patch->GetTable();
  
  for(Int_t i=0;i<300;i++)
  {
    HT2D->Fill((Float_t)i,(Float_t)pa[i].HighTowerAdc6bits);
    //cout <<" i = "<<i<<"  HT = "<<pa[i].HighTowerAdc6bits<<endl;
  }
  
  Int_t set=table[0].NEtThresholds;
  Int_t sra=table[0].NRatioThresholds;
  Int_t sht=table[0].NHighTowerThresholds;
  Int_t spa=table[0].NPatchThresholds;
  Int_t sje=table[0].NJetThresholds;

  for(Int_t i=0;i<set;i++)
    if(table[0].EtBits[i]==1) 
      EtHist->Fill(table[0].EtThresholds[i]);

  for(Int_t i=0;i<sra;i++)
    if(table[0].RatioBits[i]==1) 
      RatioHist->Fill(table[0].RatioThresholds[i]);

  for(Int_t i=0;i<sht;i++)
    if(table[0].HighTowerBits[i]==1) 
      HighTowerHist->Fill(table[0].HighTowerThresholds[i]);

  for(Int_t i=0;i<spa;i++)
    if(table[0].PatchBits[i]==1) 
      PatchHist->Fill(table[0].PatchThresholds[i]);

  for(Int_t i=0;i<sje;i++)
    if(table[0].JetBits[i]==1) 
      JetHist->Fill(table[0].JetThresholds[i]);

  for(Int_t i=0;i<sht;i++)
    for(Int_t j=0;j<spa;j++)
      if(table[0].HighTowerBits[i]==1 && table[0].PatchBits[j]==1) 
        HTxPatch->Fill(table[0].HighTowerThresholds[i],table[0].PatchThresholds[j]);
}

//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Finish()
{  
  TFile f("StEmcTriggerMaker.hist.root","RECREATE");
  EtHist->Write();
  RatioHist->Write();
  HighTowerHist->Write();
  PatchHist->Write();
  JetHist->Write();
  HTxPatch->Write();
  f.Close();
  return StMaker::Finish();
}

