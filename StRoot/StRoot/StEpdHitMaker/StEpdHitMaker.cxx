#include <stdlib.h>

/*!
  StEpdHitMaker:

  \author Mike Lisa
  \date 5 Jan 2018

  \description

  This takes the StEvent and asks for the StEpdHitCollection.
  If it is not there, it creates one and fills it from the
  StTriggerData object and info from the StEpdDbMaker (database)

*/

#include "StEpdHitMaker.h"
#include "StMaker.h"
#include "StEventTypes.h"
#include "StEvent/StTriggerData.h"
#include "StEpdHit.h"
#include <TSystem.h> 
#include "StEvent/StEpdCollection.h"
#include "StEpdDbMaker/StEpdDbMaker.h"


#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

ClassImp(StEpdHitMaker)


//----------------------------------------------
StEpdHitMaker::StEpdHitMaker() : StMaker("epdHit"),
  mEventCounter(0), mTriggerEventCounter(0), mEpdCollection(0), mTriggerData(0), mEpdDbMaker(0), mStEvent(0) {
  LOG_DEBUG << "StEpdHitMaker::ctor"  << endm;
}

//----------------------------------------------
int StEpdHitMaker::Init(){
  return kStOK;
}

//----------------------------------------------
int StEpdHitMaker::Make(){
  mEventCounter++ ;
  mTriggerEventCounter++;
  mTriggerData   = this->GetTriggerData();
  if (!mTriggerData){
    LOG_ERROR << "StEpdHitMaker::Make - no TriggerData object" << endm;
    return kStErr;
  }
  mEpdDbMaker    = this->GetEpdDbMaker();
  if (!mEpdDbMaker){
    LOG_ERROR << "StEpdHitMaker::Make - no EpdDbMaker object" << endm;
    return kStErr;
  }
  mEpdCollection = this->GetEpdCollection();
  if (!mEpdCollection){
    LOG_ERROR << "StEpdHitMaker::Make - no EpdCollection object" << endm;
    return kStErr;
  }
  FillStEpdData();
  return kStOK;
}


//------------------------------------------------
StEpdDbMaker* StEpdHitMaker::GetEpdDbMaker(){
  //  StEpdDbMaker* dbMaker = (StEpdDbMaker*)GetDataSet("epdDb");  // no, this doesn't work!!
  StEpdDbMaker* dbMaker = (StEpdDbMaker*)GetMaker("epdDb");
  if (!dbMaker) LOG_WARN << "No StEpdDbMaker found by StEpdHitMaker::GetEpdDbMaker" << endm;
  return dbMaker;
}

//----------------------------------------------
int StEpdHitMaker::Finish(){
  return kStOK;
}

//----------------------------------------------
StTriggerData* StEpdHitMaker::GetTriggerData(){
  StTriggerData* trg=0;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));  // this is how the StBtofHitMaker does it.
  if (mStEvent){
    trg = mStEvent->triggerData();
  }
  else {LOG_WARN << "No StEvent found by StEpdHitMaker::GetTriggerData" << endm;}  
  return trg;
}

//----------------------------------------------
// this is patterned after the StBTofHitMaker
StEpdCollection* StEpdHitMaker::GetEpdCollection(){
  StEpdCollection* epdCollection = 0;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));  // this is how the StBtofHitMaker does it.
  if (mStEvent){
    epdCollection = mStEvent->epdCollection();
    /// need to create the hit collection if it's not there, and give it to StEvent
    if (!epdCollection){
      LOG_INFO << "StEpdHitMaker::GetEpdCollection - making new StEpdCollection and giving it to StEvent" << endm;
      epdCollection = new StEpdCollection();
      mStEvent->setEpdCollection(epdCollection);
    }
    else { 
      LOG_INFO << "StEpdHitMaker::GetEpdCollection - StEvent already has a StEpdCollection - not making a new one" << endm;
    }
  }
  else {
    LOG_WARN << "No StEvent found by StEpdHitMaker::GetEpdCollection" << endm;
  }

  return epdCollection;
}

void StEpdHitMaker::FillStEpdData(){

  StTriggerData* trg=mTriggerData;

  //  This is for BBC.  We can do this if we ever have a StBbc class.
  // for (Int_t ew=0; ew<2; ew++){
  //   for (Int_t pmt=0; pmt<24; pmt++){
  //     mStEpdData.BBCadc[ew][pmt] = trg->bbcADC(eastwest(ew),pmt+1,0);
  //     mStEpdData.BBCtac[ew][pmt] = trg->bbcTDC(eastwest(ew),pmt+1,0);
  //     mStEpdData.BBCtdc[ew][pmt] = trg->bbcTDC5bit(eastwest(ew),pmt+1);
  //   }
  // }

  int PrePost=0;  // this probably will henceforth always be zero, but keep it

  // ofstream EpdOfs;
  // EpdOfs.open("FromStEpdHitMaker.txt");
  // ofstream EpdOfs2;
  // EpdOfs.open("FromStEpdHitMaker2.txt");


  int nHitsAdded=0;
  for (short ew=0; ew<2; ew++){        // EastWest (ew) = 0,1 for east,west
    for (short PP=1; PP<13; PP++){     // note position (PP) goes from 1..12
      for (short TT=1; TT<32; TT++){   // note tile number (TT) goes from 1..31

	short crateAdc = mEpdDbMaker->GetCrateAdc(ew,PP,TT);

	short EWforHit=(ew==0)?-1:+1;

	//---------------------------------------------------
	//  This below is for debugging only...
	// cout << "StEpdHitMaker::FillStEpdData()  -- this is what I get from StEpdDbMaker...\n";
	// cout << "EW\tPP\tTT\tcrateAdc\t\boardAdc\tchannelAdc\tcrateTac\tboardTac\tchannelTac\n";
	// cout << ew << "\t" << PP << "\t" << TT << "\t" 
	//      << mEpdDbMaker->GetCrateAdc(ew,PP,TT) << "\t"
	//      << mEpdDbMaker->GetBoardAdc(ew,PP,TT) << "\t"
	//      << mEpdDbMaker->GetChannelAdc(ew,PP,TT) << "\t"
	//      << mEpdDbMaker->GetCrateTac(ew,PP,TT) << "\t"
	//      << mEpdDbMaker->GetBoardTac(ew,PP,TT) << "\t"
	//      << mEpdDbMaker->GetChannelTac(ew,PP,TT) << "\n";
	//---------------------------------------------------


	// EpdOfs << ew << "\t" << PP << "\t" << TT << "\t" 
	//        << mEpdDbMaker->GetCrateAdc(ew,PP,TT) << "\t"
	//        << mEpdDbMaker->GetBoardAdc(ew,PP,TT) << "\t"
	//        << mEpdDbMaker->GetChannelAdc(ew,PP,TT) << "\t"
	//        << mEpdDbMaker->GetCrateTac(ew,PP,TT) << "\t"
	//        << mEpdDbMaker->GetBoardTac(ew,PP,TT) << "\t"
	//        << mEpdDbMaker->GetChannelTac(ew,PP,TT) << "\n";



	if (crateAdc>0){     // only create a StEpdHit object if database says it is in the data

	  short boardAdc   = mEpdDbMaker->GetBoardAdc(ew,PP,TT);
	  short channelAdc = mEpdDbMaker->GetChannelAdc(ew,PP,TT);
	  int ADC = trg->epdADC(crateAdc,boardAdc,channelAdc,PrePost);

	  if (ADC>0){        // we are zero-suppressing at QT level, so don't just write a bunch of zeros into the data
	    int TDC = trg->epdTDC(crateAdc,boardAdc,channelAdc,PrePost);

	    //	  bool isGood = kTRUE;  // until it is in the database
	    bool isGood = true;  // until it is in the database
	    
	    bool HasTac = (mEpdDbMaker->GetChannelTac(ew,PP,TT)>=0);
	    int TAC;
	    if (HasTac){
	      TAC = trg->epdADC(mEpdDbMaker->GetCrateTac(ew,PP,TT),    // relying on conversion from int to short
				mEpdDbMaker->GetBoardTac(ew,PP,TT),
				mEpdDbMaker->GetChannelTac(ew,PP,TT),
				PrePost);}
	    else{TAC = 0;}
	    
	    // finally.... CALIBRATED data....
	    double gain = mEpdDbMaker->GetMip(ew,PP,TT);
	    if (gain<=0.0) gain = 1.0;  // not yet calibrated.  Give it a gain of unity
	  
	    float nMIP = (ADC + mEpdDbMaker->GetOffset(ew,PP,TT)) / mEpdDbMaker->GetMip(ew,PP,TT);

	    int truthId=0;  // this is for simulation


	    // EpdOfs2 << ew << "\t" << PP << "\t" << TT << "\t" 
	    // 	  << mEpdDbMaker->GetCrateAdc(ew,PP,TT) << "\t"
	    // 	  << mEpdDbMaker->GetBoardAdc(ew,PP,TT) << "\t"
	    // 	  << mEpdDbMaker->GetChannelAdc(ew,PP,TT) << "\t"
	    // 	  << mEpdDbMaker->GetCrateTac(ew,PP,TT) << "\t"
	    // 	  << mEpdDbMaker->GetBoardTac(ew,PP,TT) << "\t"
	    // 	  << mEpdDbMaker->GetChannelTac(ew,PP,TT) << "\t"
	    // 	  << ADC << "\t" << nMIP << endl;

	    StEpdHit* hit = new StEpdHit(PP,TT,EWforHit,ADC,TAC,TDC,HasTac,nMIP,isGood,truthId);
	    mEpdCollection->addHit(hit);
	    nHitsAdded++;
	  }
	}
      }
    }
  }

  //  EpdOfs.close();
  //  EpdOfs2.close();

  LOG_INFO << "StEpdHitMaker::FillStEpdData - added " << nHitsAdded << " to StEpdHitCollection" << endm;
}

