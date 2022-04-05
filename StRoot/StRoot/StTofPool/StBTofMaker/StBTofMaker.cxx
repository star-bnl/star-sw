/***************************************************************************
 *
 * $Id: StBTofMaker.cxx,v 1.4 2018/02/26 23:26:50 smirnovd Exp $
 *
 * Author: Valeri Fine, BNL Feb 2008
 ***************************************************************************
 *
 * Description:  Convert StBTofRawHitCollection to StBTofHitCollection
 *
 * Input:  BTofRawHitCollection
 * Output: BTofHitCollection
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StBTofMaker.h"

#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StBTofRawHit.h"
#include "StBTofUtil/StBTofRawHitCollection.h"  
#include "StBTofUtil/StBTofHitCollection.h"
#include "StBTofUtil/StBTofDaqMap.h"
#include "StBTofUtil/StBTofINLCorr.h"
#include "StEvent/StEvent.h"


//_____________________________________________________________
StBTofMaker::StBTofMaker(const char *name):StMaker("tof",name)
, mStEvent(0)
{
  mDoINLCorr = kTRUE;     // default
  mDoTriggerCut = kTRUE;  // default

  mBTofDaqMap = 0;
  mBTofSortRawHit = 0;
  mBTofINLCorr = 0;

  LOG_INFO << "StBTofMaker::ctor"  << endm;
}

//_____________________________________________________________
StBTofMaker::~StBTofMaker() 
{ }

//_____________________________________________________________
void StBTofMaker::Clear(Option_t* option) 
{ 
  for(int i=0;i<4;i++) mTriggerTimeStamp[i] = 0;
}

//_____________________________________________________________
Int_t StBTofMaker::Init()
{
  Clear("");
  return kStOK;
}

//_____________________________________________________________
Int_t StBTofMaker::InitRun(Int_t runnumber)
{
  ///////////////////////////////////////////////////////////////
  // TOF Daq map and INL initialization -- load from StBTofUtil
  ///////////////////////////////////////////////////////////////
  LOG_INFO << " Initialize Daq map ... " << endm;
  mBTofDaqMap = new StBTofDaqMap();
  mBTofDaqMap->Init(this);

  mNValidTrays = mBTofDaqMap->numberOfValidTrays();

  LOG_INFO << " Initialize StBTofSortRawHit() ... " << endm;
  mBTofSortRawHit = new StBTofSortRawHit();
  mBTofSortRawHit->Init(this, mBTofDaqMap);

  if(mDoINLCorr) {
    LOG_INFO << " Initialize INL table ... " << endm;
    mBTofINLCorr = new StBTofINLCorr();
    mBTofINLCorr->initFromDbase(this);
  }


  return kStOK;
}

//_____________________________________________________________
Int_t StBTofMaker::FinishRun(Int_t runnumber)
{
  if(mBTofDaqMap) delete mBTofDaqMap;
  mBTofDaqMap = 0;

  if(mDoINLCorr) {
    if(mBTofINLCorr) delete mBTofINLCorr;
    mBTofINLCorr = 0;
  }

  if(mBTofSortRawHit) delete mBTofSortRawHit;
  mBTofSortRawHit = 0;

  return kStOK;
}

//-------------------------------------------------------------
Int_t StBTofMaker::Finish()
{
  Clear("");
  return kStOK;
}

//_____________________________________________________________
/*!
 * This method is to obtain the btofCollection from StEvent.
 * If StEvent is in the chain, retrieve it; if no StEvent in the chain,
 * a new StEvent is created.
 */
StBTofCollection *StBTofMaker::GetBTofCollection()
{
  /// Get StEvent if any at once
  StBTofCollection *btofCollection = 0;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
//  LOG_INFO << "StBTofMaker::Make : StEvent has been retrieved " 
//        <<mStEvent<< endm;

  if (mStEvent) {
     btofCollection = mStEvent->btofCollection();

     /// Need to create the hit collection
     if ( !btofCollection )  {
        ///  Save the hit collection to StEvent
        btofCollection = new StBTofCollection();
        mStEvent->setBTofCollection(btofCollection);
     }
  }
  return btofCollection;
}

//_____________________________________________________________
Int_t StBTofMaker::Make()
{
   mBTofCollection = GetBTofCollection();
//   LOG_INFO << " getting the tof collection " << mBTofCollection << endm;
   if (mBTofCollection) {
     fillBTofHitCollection();
   }
   return kStOk;
}
//____________________________________________
/*!
 * Fill the data from BTofRawHit into BTofHit Collection in StEvent
 */
void StBTofMaker::fillBTofHitCollection()
{
  mBTofSortRawHit->setBTofCollection(mBTofCollection);

  // multi-tray system
  IntVec validtray = mBTofDaqMap->ValidTrays();
  for(size_t i=0;i<validtray.size();i++) {
    int trayId = validtray[i];
    IntVec validchannel = mBTofSortRawHit->GetValidChannel(trayId);
    if(Debug()) {
      LOG_INFO << " Number of fired hits on tray " << trayId << " = " << validchannel.size() << endm;
    }

    for(size_t iv=0;iv<validchannel.size();iv++) {
      UIntVec leTdc = mBTofSortRawHit->GetLeadingTdc(trayId, validchannel[iv], mDoTriggerCut);
      UIntVec teTdc = mBTofSortRawHit->GetTrailingTdc(trayId, validchannel[iv], mDoTriggerCut);

      if(Debug()) {
        LOG_INFO << " Number of fired hits on channel " << validchannel[iv] << " = " <<  leTdc.size() << " / " << teTdc.size() << endm;
      }

      if(!leTdc.size() || !teTdc.size()) continue;

      int chan = validchannel[iv];
      IntVec map = mBTofDaqMap->TDIGChan2Cell(chan);
      int moduleId = map[0];
      int cellId = map[1];  

      // store raw hit from trays and vpds into StBTofHit
      //
      // correct for INL
      unsigned int tmptdc = leTdc[0];    // select the first hit
      int bin = tmptdc&0x3ff;
      double corr = mDoINLCorr ? mBTofINLCorr->getTrayINLCorr(trayId, chan, bin) : 0.0;
      double tmptdc_f = tmptdc + corr;
      double letime = tmptdc_f*VHRBIN2PS / 1000.;

      tmptdc = teTdc[0];
      bin = tmptdc&0x3ff;
      corr = mDoINLCorr ? mBTofINLCorr->getTrayINLCorr(trayId, chan, bin) : 0.0;
      tmptdc_f = tmptdc + corr;
      double tetime = tmptdc_f*VHRBIN2PS / 1000.;

      StBTofHit *aHit = new StBTofHit();
      aHit->setTray((UChar_t)trayId);   
      aHit->setModule((UChar_t)moduleId);
      aHit->setCell((UChar_t)cellId);
      aHit->setLeadingEdgeTime(letime);
      aHit->setTrailingEdgeTime(tetime);
      mBTofCollection->addHit(aHit);

    } // end channel
  }  // end tray

  // vpd -> StBTofHit
  for(int ivpd=0;ivpd<2;ivpd++) { // west and east sides
    StBeamDirection eastwest = (ivpd==0) ? west : east; 
    int trayId = (ivpd==0) ? mWestVpdTrayId : mEastVpdTrayId;
    IntVec validtube = mBTofSortRawHit->GetValidChannel(trayId);
    if(Debug()) {
      LOG_INFO << " Number of fired hits on tray(vpd) " << trayId << " = " << validtube.size() << endm;
    }

    if(!validtube.size()) continue;
    for(int i=0;i<mNVPD;i++) {
      int tubeId = i+1;
      int lechan = (ivpd==0) ? mBTofDaqMap->WestPMT2TDIGLeChan(tubeId) : mBTofDaqMap->EastPMT2TDIGLeChan(tubeId);
      int techan = (ivpd==0) ? mBTofDaqMap->WestPMT2TDIGTeChan(tubeId) : mBTofDaqMap->EastPMT2TDIGTeChan(tubeId);
      UIntVec leTdc = mBTofSortRawHit->GetLeadingTdc(trayId, lechan, mDoTriggerCut);
      UIntVec teTdc = mBTofSortRawHit->GetTrailingTdc(trayId, lechan, mDoTriggerCut);  // channel number should be le, sorted in StBTofSortRawHit

      if(Debug()) {
        LOG_INFO << " Number of fired hits on tube " << tubeId << " = " <<  leTdc.size() << " / " << teTdc.size() << endm;
      }

      if(leTdc.size() && teTdc.size()) {

        // correct for INL
        unsigned int tmptdc = leTdc[0];    // select the first hit
        int bin = tmptdc&0x3ff;
        double corr = mDoINLCorr ? mBTofINLCorr->getVpdINLCorr(eastwest, lechan, bin) : 0.0;
        double tmptdc_f = tmptdc + corr;
        double letime = tmptdc_f*VHRBIN2PS / 1000.;

        tmptdc = teTdc[0];
        bin = tmptdc&0x3ff;
        corr = mDoINLCorr ? mBTofINLCorr->getVpdINLCorr(eastwest, techan, bin) : 0.0;
        tmptdc_f = tmptdc + corr;
        double tetime = tmptdc_f*VHRBIN2PS / 1000.;

        StBTofHit *aHit = new StBTofHit();
        aHit->setTray((UChar_t)trayId);   
        aHit->setModule(0);
        aHit->setCell((UChar_t)tubeId);
        aHit->setLeadingEdgeTime(letime);
        aHit->setTrailingEdgeTime(tetime);
        mBTofCollection->addHit(aHit);
      }
    }  
  }    
  //   

}
