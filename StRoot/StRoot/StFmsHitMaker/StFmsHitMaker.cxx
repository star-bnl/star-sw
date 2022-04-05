// ROOT
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"

#include "StMessMgr.h"
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuFmsUtil.h"

#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StFmsCollection.h"
#include "StFmsHit.h"
#include "StFmsHitMaker.h"
#include "StTriggerData2009.h"

// PSU-FMS package
//#include "StFmsPointMaker/StFmsClusterFitter.h"

#include <iostream>
#include <fstream>
#include <stdlib.h>
using namespace std;

ClassImp(StFmsHitMaker);

StFmsHitMaker::StFmsHitMaker(const char* name) : StMaker(name) {
	mFmsDbMaker = NULL;
	mFmsCollection = NULL;
	mMuFmsColl = NULL;
	LOG_DEBUG << "StFmsHitMaker::constructor." << endm;
}

StFmsHitMaker::~StFmsHitMaker(){  
  LOG_DEBUG << "StFmsHitMaker::destructor." << endm;
}

void StFmsHitMaker::Clear(Option_t* option){
  LOG_DEBUG << "StFmsHitMaker::Clear()" << endm;
  StMaker::Clear(option);
}

int StFmsHitMaker::Init() {  
	LOG_DEBUG<<"StFmsHitMaker::Init() "<<endm;
	return StMaker::Init();
}

int StFmsHitMaker::InitRun(Int_t runNumber) {  
	LOG_INFO << "StFmsHitMaker::InitRun --run# changed to " << runNumber << endm;
	mFmsDbMaker = static_cast<StFmsDbMaker*>(GetMaker("fmsDb")); 
	if(!mFmsDbMaker){
	  LOG_ERROR  << "StFmsHitMaker::InitRun Failed to get StFmsDbMaker" << endm;
	  return kStFatal;
	}
	mCurrentRunNumber = runNumber; //called by maker's method :InitRun(run); when the run# changes
	return kStOK;
}

//// This is StFmsHitMaker Make, it reads in data and make hit and fill StFmsCollection
int StFmsHitMaker::Make(){
	LOG_DEBUG<<"StFmsHitMaker::Make start"<<endm;
	int flag = 0;
	StTriggerData* triggerData = 0;
	Float_t mCurrentEventNumber=0;
	
	if(mReadMuDst>0) return readMuDst();

	//first try to get StTriggerData from StTriggerDataMaker (works for proudction) and create StFmsCollection
	TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
	if (os) {
	    triggerData = (StTriggerData*)os->GetObject();
	    if(triggerData){
		flag=1;
		mCurrentEventNumber=triggerData->eventNumber();
		// mFmsCollection = new StFmsCollection();
		LOG_DEBUG<<"StFmsHitMaker::Make Found StTriggerData from StTriggerDataMaker"<<endm;
	    }
	}
  
	//2nd try to get StTriggerData from StEvent
	//but once FMS data is killed in StEvent, this will not work and all you see is empty data
	StEvent* stEvent = (StEvent*) GetInputDS("StEvent");
	if(flag==0){
	    if(stEvent){
		mCurrentEventNumber=stEvent->id();
		triggerData = stEvent->triggerData();
		if(triggerData) {
		    flag=2;
		    LOG_DEBUG<<"StFmsHitMaker::Make Found StTriggerData from StEvent"<<endm;
		}
		else{
		    mFmsCollection = stEvent->fmsCollection();
		    if(mFmsCollection){
			flag=3;
			LOG_DEBUG<<"StFmsHitMaker::Make Found StFmsCollection from StEvent"<<endm;
		    }
		}
	    } //found StEvent
	}
  
	//3rd try to get StTriggerData from StMuEvent, works for produced data (.MuDst.root) --Yuxi
	StMuDst* muDst = (StMuDst*)GetInputDS("MuDst");
	if(flag==0){
	    if(muDst && muDst->event()){
		mCurrentRunNumber = muDst->event()->runNumber();
		mCurrentEventNumber = muDst->event()->eventNumber();
		triggerData = (StTriggerData*)StMuDst::event()->triggerData();
		if(triggerData){
		    flag = 4; //Yuxi
		    LOG_DEBUG<<"StFmsHitMaker::Make Found StFmsTriggerData in MuDst"<<endm;
		}
		else LOG_ERROR << "Finally, no StFmsTriggerData in MuDst " <<endm;
	    }
	} 
	LOG_DEBUG<<"Flag="<<flag<<" (0=Not found, 1=StTriggerDataMaker, 2=StEvent, 3=FmsCollection 4=Mudst)"<<endm;
	//after this step triggerData is pointing to StTriggerData block of StEvent

	if(flag>0){
	    mFmsCollection = new StFmsCollection();
	    //create StFmsHit and add it to StFmsCollection
	    for(unsigned short crt=1; crt<=4; crt++){
		for(unsigned short slot=1; slot<=16; slot++){
		    for(unsigned short ch=0; ch<32; ch++){
			unsigned short adc=0;
			unsigned short tdc=0;
			if(flag<=4 && triggerData){ //wont work when flag=3
			    adc=triggerData->fmsADC(crt,slot-1,ch);
			    tdc=triggerData->fmsTDC(crt,slot-1,ch);
			}
			
			if(adc>0 || tdc>0){
			    //	LOG_INFO<<"adc of crt "<<crt<<", slot "<<slot<<", channel "<<ch<<" is: "<<adc<<endm;
			    //	LOG_INFO<<"tdc=====================================================is: "<<tdc<<endm;
			    StFmsHit* hit = new StFmsHit();
			    if(!hit){
				LOG_ERROR <<"Failed to create FMS hit, skip this hit."<<endm;
				continue;
			    }
			    hit->setDetectorId(0);
			    hit->setChannel(0);
			    hit->setQtCrate(crt);
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
	    
  /// Read DB and put DetectorId, channeel and apply Calibration to get Energy
	    for(unsigned int i=0; i<mFmsCollection->numberOfHits(); i++){
		int d,c;
		StFmsHit* fmsHit = (mFmsCollection->hits())[i];
		int crt   =fmsHit->qtCrate();
		int slot  =fmsHit->qtSlot();
		int ch    =fmsHit->qtChannel();
		unsigned short adc =fmsHit->adc();
		mFmsDbMaker->getReverseMap(crt,slot,ch,&d,&c);
		float e=0.0;
		if(d>0 || c>0){
		    //unsigned short rawadc=adc;
		    short bitshift=0;
		    if(mCorrectAdcOffByOne){
			bitshift = mFmsDbMaker->getBitShiftGain(d,c);
			if(bitshift>0){		  
			    int check=adc % (1<<bitshift);
			    if(check!=0){
				LOG_ERROR << Form("Bitshift in DB is not consistent with data! det=%2d ch=%3d adc=%4d bitshift=%2d adc%(1<<bitshift)=%d", 
						  d,c,adc,bitshift,check) << endm;
			    }
			}else if(bitshift<0){
			    int check=adc / (1<< (12+bitshift));
			    if(check!=0){
				LOG_ERROR << Form("Bitshift in DB is not consistent with data! det=%2d ch=%3d adc=%4d bitshift=%2d adc/(1<<(12+bitshift))=%d",
						  d,c,adc,bitshift,check) << endm;
			    }
			}
			//Leaving ADC value in StFmsHit as it was recorded, so that when we read from MuDST, we don't double correct!!!!
			//   if(bitshift>=0) {
			//     adc += (0x1<<bitshift);
			//     fmsHit->setAdc(adc); 
			//   }
			//LOG_INFO << Form("RawADC=%4d NewADC=%4d Bitshift=%d",rawadc,adc,bitshift) << endm;
		    }
		    float g1=mFmsDbMaker->getGain(d,c);
		    float g2=mFmsDbMaker->getGainCorrection(d,c);	  
		    float gt=1.0;
		    if(mTimeDepCorr==1){  // time dep. Correction                                                    
			gt = mFmsDbMaker->getTimeDepCorr(mCurrentEventNumber,d-8,c);
			if(gt<0){
			    if(mTowerRej==1){
				gt = 0;     // making -ve(tower to be rej) to zero  
			    }else{
				gt = -gt;  // making +ve : doing time dep corr. for all towers 
			    } 
			}
			//      cout<<d<<"  "<<ch<<"  "<<gt<<endl;
		    }
		    
		    if(mCorrectAdcOffByOne){
			e=(adc+pow(2.0,bitshift))*g1*g2*gt;
		    }else{
			e=adc*g1*g2*gt;
		    }
		}
		fmsHit->setDetectorId(d);
		fmsHit->setChannel(c);
		fmsHit->setEnergy(e);
		if(GetDebug()>0) fmsHit->print();
	    }
	    LOG_INFO<<"StFmsHitMaker::Make(): flag = "<<flag<<", got "<<mFmsCollection->numberOfHits()<<" hits in StFmsCollection"<<endm;
	}else{
	    LOG_INFO<<"StFmsHitMaker::Make(): flag = "<<flag<<", no StTrigger data found"<<endm;
	}

	if(stEvent) {
	  //Adding StFmsCollection to StEvent
	  LOG_DEBUG<<"StFmsHitMaker::Make Adding StFmsCollection to StEvent"<<endm;
	  stEvent->setFmsCollection(mFmsCollection);
	}else{
	    LOG_INFO << "StEvent is empty" << endm;	
	}

	return kStOk;
}

int StFmsHitMaker::Finish(){  
  LOG_DEBUG << "StFmsHitMaker::Finish() " << endm;
  return kStOk;
}

Int_t StFmsHitMaker::readMuDst(){
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event){LOG_ERROR<<"StFmsHitMaker::readMuDst found no StEvent"<<endm; return kStErr;}
  StFmsCollection* fmsColl = event->fmsCollection();
  if(!fmsColl){
    fmsColl=new StFmsCollection;
    event->setFmsCollection(fmsColl);
  }
  StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
  if(!mudst){LOG_ERROR<<"StFmsHitMaker::readMuDst found no MuDst"<<endm; return kStErr;}
  StMuFmsCollection* mufmsColl= mudst->muFmsCollection();
  if(!mufmsColl){LOG_ERROR<<"StFmsHitMaker::readMuDst found no MuFmsCollection"<<endm; return kStErr;}
  StMuFmsUtil util;
  util.fillFms(fmsColl,mufmsColl); 
  return kStOk;
}
