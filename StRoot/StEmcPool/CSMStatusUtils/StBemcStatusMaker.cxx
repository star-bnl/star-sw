#include "StBemcStatusMaker.h"

#include "StEnumerations.h"
#include "StEvent.h"
#include "StRunInfo.h"
#include "StTriggerIdCollection.h"
#include "StTriggerId.h"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StEmcUtil/geometry/StEmcGeom.h"

StBemcStatusMaker::StBemcStatusMaker(StMuDstMaker* maker) 
  : StMaker("bemcStatus"), mMuDstMaker(maker) {
  mOutputDirectory = "/tmp";
  mOutputFilePrefix = "junk";
  mOutputFile = NULL;
}

Int_t StBemcStatusMaker::Init() {
  // create the output file
  string s = mOutputDirectory + "/" + mOutputFilePrefix + "calorimeters.hist.root";
  mOutputFile = new TFile(s.c_str(),"RECREATE");
  if (!mOutputFile) return kStErr;
  return kStOk;
}

Int_t StBemcStatusMaker::Make() {
  
  StEvent* event = static_cast<StEvent*>(GetInputDS("StEvent"));
  if (!event) {
    cout <<" Not StEvent!"<<endl;
    return kStWarn;
  }

//make histograms to be saved
  TH2F* bemcAdcHist = getBemcAdcHist(event->runId());
  TH2F* bemcEnergyHist = getBemcEnergyHist(event->runId());
  TH2F* eemcAdcHist = getEemcAdcHist(event->runId());
  //  TH2F* eemcEnergyHist = getEemcEnergyHist(event->runId());
  if (!bemcAdcHist || !bemcEnergyHist || !eemcAdcHist 
  //      || !eemcEnergyHist
      ) {
    cout <<"Problems with Histo"<<endl;
    return kStWarn;
  }

  //  cout<<"runID = "<<event->runId();
// loop over bemc and fill histograms
  StEmcCollection* emcColl = event->emcCollection();
  if (!emcColl) {
    cout <<"No emc Collection"<<endl;
    return kStOk;
  }
  StEmcDetector* bemc = emcColl->detector(kBarrelEmcTowerId);
  if (!bemc) {
    return kStOk;
  }
  for (UInt_t i = 1; i <= bemc->numberOfModules(); i++) {
    StEmcModule* module = bemc->module(i);
    if (module) {
      StSPtrVecEmcRawHit& hits = module->hits();
      for (StEmcRawHitIterator hit = hits.begin(); hit!=hits.end(); ++hit) {
	       Int_t id = 0;
	       StEmcGeom* geom = StEmcGeom::instance("bemc");
	       geom->getId((*hit)->module(),(*hit)->eta(),(*hit)->sub(),id);
	       bemcAdcHist->Fill(id,(*hit)->adc());
	       bemcEnergyHist->Fill(id,(*hit)->energy());
      }
    }
  } 
// loop over eemc and fill histograms
  int id, sec, sub, etabin, rawadc;
  StMuEmcCollection* muemcColl = mMuDstMaker->muDst()->muEmcCollection();
  if(muemcColl) {
    for (int i = 0; i < muemcColl->getNEndcapTowerADC(); i++) {
      muemcColl->getEndcapTowerADC(i,rawadc,sec,sub,etabin);
//now we need to make up an ID for it
//I model the code in StEEmcUtil/EEfeeRaw/EEname2Index.cxx
      id = etabin + 12*(sub-1) + 5*12*(sec-1);
	    eemcAdcHist->Fill(id,rawadc);
    } 
  }
  return kStOk;
}

Int_t StBemcStatusMaker::Finish() {
  if (mOutputFile) {
    mOutputFile->Write();
    delete mOutputFile;
    mOutputFile = NULL;
  }
  return kStOk;
}

TH2F* StBemcStatusMaker::getBemcAdcHist(Int_t runNumber) {
  Char_t name[255];
  snprintf(name,254,"bemcStatusAdc_%d",runNumber);
  mOutputFile->cd();
  TH2F* hist = dynamic_cast<TH2F*>(mOutputFile->Get(name));
  if (!hist) {
    Char_t title[255];
    snprintf(title,254,"BEMC tower adc run %d",runNumber);
    hist = new TH2F(name, title,4801,-0.5,4800.5,150,0,150);
    hist->GetXaxis()->SetTitle("tower id");
    hist->GetYaxis()->SetTitle("adc");
  }
  return hist; 
}

TH2F* StBemcStatusMaker::getBemcEnergyHist(Int_t runNumber) {
  Char_t name[255];
  snprintf(name,254,"bemcStatusEnergy_%d",runNumber);
  mOutputFile->cd();
  TH2F* hist = dynamic_cast<TH2F*>(mOutputFile->Get(name));
  if (!hist) {
    Char_t title[255];
    snprintf(title,254,"BEMC tower energy run %d",runNumber);
    hist = new TH2F(name, title,4801,-0.5,4800.5,150,0,3);
    hist->GetXaxis()->SetTitle("tower id");
    hist->GetYaxis()->SetTitle("adc");
  }
  return hist; 
}

TH2F* StBemcStatusMaker::getEemcAdcHist(Int_t runNumber) {
  Char_t name[255];
  snprintf(name,254,"eemcStatusAdc_%d",runNumber);
  mOutputFile->cd();
  TH2F* hist = dynamic_cast<TH2F*>(mOutputFile->Get(name));
  if (!hist) {
    Char_t title[255];
    snprintf(title,254,"EEMC tower adc run %d",runNumber);
    hist = new TH2F(name, title,721,-0.5,720.5,150,0,150);
    hist->GetXaxis()->SetTitle("tower id");
    hist->GetYaxis()->SetTitle("adc");
  }
  return hist; 
}

TH2F* StBemcStatusMaker::getEemcEnergyHist(Int_t runNumber) {
  Char_t name[255];
  snprintf(name,254,"eemcStatusEnergy_%d",runNumber);
  mOutputFile->cd();
  TH2F* hist = dynamic_cast<TH2F*>(mOutputFile->Get(name));
  if (!hist) {
    Char_t title[255];
    snprintf(title,254,"EEMC tower energy run %d",runNumber);
    hist = new TH2F(name, title,4801,-0.5,4800.5,150,0,3);
    hist->GetXaxis()->SetTitle("tower id");
    hist->GetYaxis()->SetTitle("adc");
  }
  return hist; 
}

ClassImp(StBemcStatusMaker)
