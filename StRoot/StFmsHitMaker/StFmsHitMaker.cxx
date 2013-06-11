//*-- Author : Jingguo Ma

#include "StMessMgr.h"
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StFmsCollection.h"
#include "StFmsHit.h"
#include "StFmsHitMaker.h"
#include "StTriggerData2009.h"

#include <iostream>
#include <fstream>
using namespace std;

ClassImp(StFmsHitMaker);

/// This is StFmsHitMaker constructor, it initializes some variables to NULL.
StFmsHitMaker::StFmsHitMaker(const char* name) : StMaker(name){
  mFmsDbMaker=NULL;
  mFmsCollection = NULL;
  mMuFmsColl = NULL;
  LOG_DEBUG << "StFmsHitMaker::constructor." << endm;
}

/// This is StFmsHitMaker destructor, it does nothing right now.
StFmsHitMaker::~StFmsHitMaker(){
  LOG_DEBUG << "StFmsHitMaker::destructor." << endm;
}

/// This is StFmsHitMaker Clear()
void StFmsHitMaker::Clear(Option_t* option){
  LOG_DEBUG << "StFmsHitMaker::Clear." << endm;
  StMaker::Clear(option);
}

/// This is StFmsHitMaker Init()
int StFmsHitMaker::Init() {
  LOG_DEBUG<<"StFmsHitMaker Init Start"<<endm;
  return StMaker::Init();
}

/// This is StFmsHitMaker InitRun, it gets the pointer to the FmsDbMaker.
int StFmsHitMaker::InitRun(Int_t runNumber) {
  LOG_DEBUG << "StFmsHitMaker::InitRun - run = " << runNumber << endm;
  mFmsDbMaker = gStFmsDbMaker;
  return kStOK;
}

//// This is StFmsHitMaker Make, it reads in data and make hit and fill StFmsCollection
int StFmsHitMaker::Make(){
  LOG_DEBUG<<"StFmsHitMaker::Make start"<<endm;
  int flag = 0;
  StTriggerData* triggerData = 0;
  StFmsTriggerDetector* triggerDetector =0;

  ///first try to get StTriggerData from StTriggerDataMaker and create StFmsCollection
  TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
  if (os) {
    triggerData = (StTriggerData*)os->GetObject();
    if(triggerData){
      flag=1;
      mFmsCollection = new StFmsCollection();
      LOG_DEBUG<<"StFmsHitMaker::Make Found StTriggerData from StTriggerDataMaker"<<endm;
    }
  }
  
  ///2nd try to get StTriggerData from StEvent
  ///but once FMS data is killed in StEvent, this will not work and all you see is empty data
  StEvent* stEvent = (StEvent*) GetInputDS("StEvent");
  if(flag==0){
    if(stEvent){
      triggerData = stEvent->triggerData();
      if(triggerData) {
	flag=2;
	mFmsCollection = new StFmsCollection();
	LOG_DEBUG<<"StFmsHitMaker::Make Found StTriggerData from StEvent"<<endm;
      }
      else{
	mFmsCollection = stEvent->fmsCollection();
	if(mFmsCollection){
	  flag=3;
	  LOG_DEBUG<<"StFmsHitMaker::Make Found StFmsCollection from StEvent"<<endm;
	}
      }
    }
  }

  ///try to read QT data from MuDST (run8)
  StMuDst* muDst = (StMuDst*)GetInputDS("MuDst");
  if(flag==0) {
    if(muDst){
      triggerDetector = &(StMuDst::event()->fmsTriggerDetector());
      if(triggerDetector){
	flag=0; ///temporary
	mFmsCollection = new StFmsCollection();
	LOG_DEBUG<<"StFmsHitMaker::Make Found StFmsTriggerDetector in MuDst"<<endm;
      }
    }
  }
  
  ///creating StFmsHit and adding it to StFmsCollection
  if(flag>0){
    for(unsigned short crt=1; crt<=5; crt++){
      unsigned short crt2=crt;
      if(crt==5) crt2=7;
      for(unsigned short slot=1; slot<=16; slot++){
	for(unsigned short ch=0; ch<32; ch++){
	  unsigned short adc=0;
	  unsigned short tdc=0;
	  if(flag<3){
	    adc=triggerData->fmsADC(crt,slot-1,ch);
	    tdc=triggerData->fmsTDC(crt,slot-1,ch);
	  }
	  if(flag==4 && crt<5){
	    adc=triggerDetector->adc(crt+10,slot-1+16,ch/8,ch%8);
	    tdc=triggerDetector->tdc(crt+10,slot-1+16,ch/8,ch%8);
	  }
	  if(adc>0 || tdc>0){
	    StFmsHit* hit = new StFmsHit();
	    if(!hit){
	      LOG_ERROR <<"Failed to create FMS hit, skip this hit."<<endm;
	      continue;
	    }
	    hit->setDetectorId(0);
	    hit->setChannel(0);
	    hit->setQtCrate(crt2);
	    hit->setQtSlot(slot);
	    hit->setQtChannel(ch);
	    hit->setAdc(adc);
	    hit->setTdc(tdc);
	    hit->setEnergy(0.0);
	    mFmsCollection->addHit(hit);
	    if(GetDebug()>0) hit->print();
	  }
	}
      }	
    }
  }
    

  /// Read DB and put DetectorId, channeel and apply Calibration to get Energy
  if(flag>0){
    for(unsigned int i=0; i<mFmsCollection->numberOfHits(); i++){
      int d,c;
      StFmsHit* fmsHit = (mFmsCollection->hits())[i];
      int crt   =fmsHit->qtCrate();
      int slot  =fmsHit->qtSlot();
      int ch    =fmsHit->qtChannel();
      float adc =fmsHit->adc();
      mFmsDbMaker->getReverseMap(crt,slot,ch,&d,&c);
      float e=0.0;
      if(d>0 || c>0){
	float g1=mFmsDbMaker->getGain(d,c);
	float g2=mFmsDbMaker->getGainCorrection(d,c);
	e       =adc*g1*g2;
      }
      fmsHit->setDetectorId(d);
      fmsHit->setChannel(c);
      fmsHit->setEnergy(e);
      if(GetDebug()>0) fmsHit->print();
    }
  }
  
  LOG_INFO<<"StFmsHitMaker::Make got "<<mFmsCollection->numberOfHits()<<" hits in StFmsCollection"<<endm;
    
  ///try to read StFmsHit in MuDST and overwrite energy with current calibration
  if(flag==0) {
    if(muDst){
      mMuFmsColl = StMuDst::muFmsCollection();
      if(mMuFmsColl){
	int nhits = mMuFmsColl->numberOfHits();
	LOG_DEBUG<<"StFmsHitMaker::Make Found "<<nhits<<" hits in MuDst"<<endm;
	for(int i=0; i<nhits; i++){
	  StMuFmsHit* mHit = mMuFmsColl->getHit(i);
	  if(mHit && GetDebug()>0) mHit->print();
	}
	flag=5;
      }
    }
  }

  if(flag>0 && flag<=2) {
    ///Adding StFmsCollection to StEvent
    LOG_DEBUG<<"StFmsHitMaker::Make Adding StFmsCollection as FMSCOLLECTION"<<endm;
    stEvent->setFmsCollection(mFmsCollection);
  }

  return kStOk;
}

/// This is StFmsHitMaker Finish()
int StFmsHitMaker::Finish(){
  return kStOk;
}
