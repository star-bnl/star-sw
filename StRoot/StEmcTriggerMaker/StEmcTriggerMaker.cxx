
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

#include <Stiostream.h>
#include <math.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMaker.h"
#include "StEmcTriggerMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "TFile.h"

ClassImp(StEmcTriggerMaker)

//_____________________________________________________________________________
StEmcTriggerMaker::StEmcTriggerMaker(const char *name):StMaker(name)
{
  mBemcTrigger = new StBemcTrigger();
  mSaveStEvent = true;
  mPrint = false;
}
//____________________________________________________________________________
StEmcTriggerMaker::~StEmcTriggerMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Init()
{
  mHTBefore = new TH2F("HighTower_DSM","High Tower trigger in DSM",300,-0.5,299.5,64,-0.5,63.5);
  mPABefore = new TH2F("Patch_DSM","Patch trigger in DSM",300,-0.5,299.5,64,-0.5,63.5);
  mHT       = new TH2F("HighTower","High Tower trigger",300,-0.5,299.5,64,-0.5,63.5);
  mPA       = new TH2F("Patch","Patch trigger",300,-0.5,299.5,64,-0.5,63.5);
  mHTCorrel = new TH2F("HighTower_Correl","High Tower trigger correlation",64,-0.5,63.5,64,-0.5,63.5);
  mPACorrel = new TH2F("Patch_Correl","Patch trigger correlation",64,-0.5,63.5,64,-0.5,63.5);
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Make()
{

  StEvent* event=(StEvent*)GetInputDS("StEvent");
  if(!event) return kStOk;
  
  mBemcTrigger->setPrint(mPrint);
  mBemcTrigger->setEvent(event);
  mBemcTrigger->makeTrigger();
  
  fillHistograms(event);
  
  if(mSaveStEvent) fillStEvent(event);
    
  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcTriggerMaker::Finish()
{  
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StEmcTriggerMaker::fillHistograms(StEvent *event)
{  
  emcTrigger emcTrg = mBemcTrigger->getTrigger();
  for(int i=0;i<300;i++)
  {
    mHT->Fill(i,emcTrg.HT[i]);
    mPA->Fill(i,emcTrg.Patch[i]);
  }
  
  // comparison with existing data in StTriggerData
  if(!event) return;
  StTriggerData* trg=event->triggerData();
  if(trg) 
  {
    for(int i=0;i<300;i++)
    {
      mHTBefore->Fill(i,trg->bemcHighTower(i));
      mPABefore->Fill(i,trg->bemcJetPatch(i));
      mHTCorrel->Fill(emcTrg.HT[i],trg->bemcHighTower(i));
      mPACorrel->Fill(emcTrg.Patch[i],trg->bemcJetPatch(i));
    }
  }
  return;
}
//_____________________________________________________________________________
void StEmcTriggerMaker::saveHistograms(char* file)
{  
  TFile *f = new TFile(file,"RECREATE");
  mHT->Write();
  mPA->Write();
  mHTBefore->Write();
  mPABefore->Write();
  mHTCorrel->Write();
  mPACorrel->Write();
  f->Close();
  delete f;
  return;
}

//_____________________________________________________________________________
void StEmcTriggerMaker::fillStEvent(StEvent *event)
{  
  if(!event) return;
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  if(!trg) 
  {
    trg = new StTriggerDetectorCollection();
    event->setTriggerDetectorCollection(trg);
  }
  StEmcTriggerDetector emc=trg->emc();
  emcTrigger emcTrg = mBemcTrigger->getTrigger();
  for(int i=0;i<300;i++)
  {
    emc.setHighTower(i,emcTrg.HT[i]);
    emc.setPatch(i,emcTrg.Patch[i]);
  }
  return;
}



