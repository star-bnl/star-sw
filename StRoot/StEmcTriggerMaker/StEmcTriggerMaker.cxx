
//////////////////////////////////////////////////////////////////////////
//                                                             
// StEmcTriggerMaker A. A. P. Suaide (C) 2001  
//                                                             
//   Update: 22-Feb-2002 
//	     J.L. Klay (LBNL)
//
//   This class now creates histograms of the (10-bit to 6-bit compressed)
//   DAQ data and the TRG 6-bit ADC data so that comparisons
//   can be made.  
//
//   In order to run on *event.root files, just load the library and call:
//     StEmcTriggerMaker* trigger=new StEmcTriggerMaker("bemctrigger");
//     trigger->SetHistFileName(outfile);
//
//   In order to run on *.daq files, make sure to load the St_trg_Maker
//   and StEmcCalibrationMaker libraries and then to call them in this
//   order:
//   	St_trg_Maker* trg=new St_trg_Maker("trigger");
//      trg->SetMode(1);
//      StEmcPreCalibrationMaker* precalib=new StEmcPreCalibrationMaker("precalib",1);
//      StEmcTriggerMaker* trigger=new StEmcTriggerMaker("bemctrigger");
//      trigger->SetHistFileName(outfile);
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
StEmcTriggerMaker::StEmcTriggerMaker(const char *name):StMaker(name)
{
  TitleHist="BEMC";

  HistFileName = "StEmcTriggerMaker.hist.root";
  
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

  a="High Tower 6bit ADC from DAQ";
  DAQHTower = new TH2F("DAQHTower",a.Data(),300,-0.5,299.5,64,-0.5,63.5);
  DAQHTower->SetXTitle("Tower/Patch Number");
  DAQHTower->SetYTitle("6bit ADC");

  a="6bit ADC Patch Sum from DAQ";
  DAQPatch = new TH2F("DAQPatch",a.Data(),300,-0.5,299.5,64,-0.5,63.5);
  DAQPatch->SetXTitle("Tower/Patch Number");
  DAQPatch->SetYTitle("6bit ADC");

  a="High Tower 6bit ADC from TRG";
  TRGHTower = new TH2F("TRGHTower",a.Data(),300,-0.5,299.5,64,-0.5,63.5);
  TRGHTower->SetXTitle("Tower/Patch Number");
  TRGHTower->SetYTitle("6bit ADC");

  a="6bit ADC Patch Sum from TRG";
  TRGPatch = new TH2F("TRGPatch",a.Data(),300,-0.5,299.5,64,-0.5,63.5);
  TRGPatch->SetXTitle("Tower/Patch Number");
  TRGPatch->SetYTitle("6bit ADC");

  a="TRG High Tower 6bit ADC vs. DAQ High Tower 6bit ADC";  
  TRGHTxDAQHT = new TH2F("TRGHTxDAQHT",a.Data(),64,-0.5,63.5,64,-0.5,63.5);
  TRGHTxDAQHT->SetXTitle("6bit DAQ HT ADC");
  TRGHTxDAQHT->SetYTitle("6bit TRG HT ADC");

  a="TRG 6bit Patch ADC Sum vs. DAQ 6bit ADC Patch Sum";  
  TRGPAxDAQPA = new TH2F("TRGPAxDAQPA",a.Data(),64,-0.5,63.5,64,-0.5,63.5);
  TRGPAxDAQPA->SetXTitle("6bit DAQ ADC Patch Sum");
  TRGPAxDAQPA->SetYTitle("6bit TRG ADC Patch Sum");

  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Make()
{

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
  StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
  if (!theTriggers) {
    printf("StEvent::TriggerDetectorCollection is missing\n");
    return kStWarn;
  }

  BemcTrigger=new StBemcTrigger(GetDate(),GetTime());
  BemcTrigger->SetEmcCollection(emc);
  BemcTrigger->SetThreshold(EtTh,RatioTh,HighTowerTh,PatchTh,JetTh);
  BemcTrigger->MakeTrigger();
  
  AddData(BemcTrigger->EmcTrigger);
  AddData(BemcTrigger->PatchTrigger);
  AddData(BemcTrigger->JetTrigger);
  FillHistograms(theTriggers);
  
  return kStOK;
}
//_____________________________________________________________________________
void StEmcTriggerMaker::FillHistograms(StTriggerDetectorCollection* theTriggers)
{

  cout << "Filling Histograms..." << endl;
  St_emcTrigger* trigger=BemcTrigger->EmcTrigger;
  emcTrigger_st* table=trigger->GetTable();
  
  St_emcPatchTrigger* patch=BemcTrigger->PatchTrigger;
  emcPatchTrigger_st* pa=patch->GetTable();

  StEmcTriggerDetector &theEmcTrg = theTriggers->emc();

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


  cout << endl << endl << "**** trigger data follow ***" << endl;

  for (Int_t j=0;j<300;j++) {
    Float_t PADAQ=pa[j].PatchAdcSum6bits;
    Float_t HTDAQ=pa[j].HighTowerAdc6bits;

    Float_t PATRG=theEmcTrg.patch(j);
    Float_t HTTRG=theEmcTrg.highTower(j);

    cout << "j=" << j 
	 << ", PADAQ=" << PADAQ << ", HTDAQ=" << HTDAQ
	 << ", PATRG=" << PATRG << ", HTTRG=" << HTTRG
	 << endl;

    DAQHTower->Fill((Float_t)j,HTDAQ);
    DAQPatch->Fill((Float_t)j,PADAQ);
    TRGHTower->Fill((Float_t)j,HTTRG);
    TRGPatch->Fill((Float_t)j,PATRG);
    TRGHTxDAQHT->Fill(HTDAQ,HTTRG);
    TRGPAxDAQPA->Fill(PADAQ,PATRG);
  }

  cout << endl << "**** end of trigger data ***" << endl << endl;

}

//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Finish()
{  
  TFile f(HistFileName.Data(),"RECREATE");
  EtHist->Write();
  RatioHist->Write();
  HighTowerHist->Write();
  PatchHist->Write();
  JetHist->Write();
  HTxPatch->Write();
  DAQHTower->Write();
  DAQPatch->Write();
  TRGHTower->Write();
  TRGPatch->Write();
  TRGHTxDAQHT->Write();
  TRGPAxDAQPA->Write();
  f.Close();
  return StMaker::Finish();
}



