/***************************************************************************
 *
 * $Id: StBTofMaker.cxx,v 1.1 2009/02/26 18:27:23 dongx Exp $
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

ClassImp(StBTofMaker);

//_____________________________________________________________
StBTofMaker::StBTofMaker(const char *name):StMaker("tof",name)
, mStEvent(0)
{
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
  mBTofDaqMap = new StBTofDaqMap();
  mBTofDaqMap->Init(this);
  LOG_INFO << " Initialize Daq map ... " << endm;

  mNValidTrays = mBTofDaqMap->numberOfValidTrays();

  mBTofINLCorr = new StBTofINLCorr();
  mBTofINLCorr->setNValidTrays(mNValidTrays);
  mBTofINLCorr->initFromDbase(this);
  LOG_INFO << " Initialize INL table ... " << endm;

  mBTofSortRawHit = new StBTofSortRawHit();
  mBTofSortRawHit->Init(this, mBTofDaqMap);
  LOG_INFO << " Initialize StBTofSortRawHit() ... " << endm;

  return kStOK;
}

//_____________________________________________________________
Int_t StBTofMaker::FinishRun(Int_t runnumber)
{
  if(mBTofDaqMap) delete mBTofDaqMap;
  mBTofDaqMap = 0;

  if(mBTofINLCorr) delete mBTofINLCorr;
  mBTofINLCorr = 0;

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
      UIntVec leTdc = mBTofSortRawHit->GetLeadingTdc(trayId, validchannel[iv], kTRUE);
      UIntVec teTdc = mBTofSortRawHit->GetTrailingTdc(trayId, validchannel[iv], kTRUE);

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
      double tmptdc_f = tmptdc + mBTofINLCorr->getTrayINLCorr(trayId, chan, bin);
      double letime = tmptdc_f*VHRBIN2PS / 1000.;

      tmptdc = teTdc[0];
      bin = tmptdc&0x3ff;
      tmptdc_f = tmptdc + mBTofINLCorr->getTrayINLCorr(trayId, chan, bin);
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
      UIntVec leTdc = mBTofSortRawHit->GetLeadingTdc(trayId, lechan, kTRUE);
      UIntVec teTdc = mBTofSortRawHit->GetTrailingTdc(trayId, lechan, kTRUE);  // channel number should be le, sorted in StBTofSortRawHit

      if(leTdc.size() && teTdc.size()) {

        // correct for INL
        unsigned int tmptdc = leTdc[0];    // select the first hit
        int bin = tmptdc&0x3ff;
        double tmptdc_f = tmptdc + mBTofINLCorr->getVpdINLCorr(eastwest, lechan, bin);
        double letime = tmptdc_f*VHRBIN2PS / 1000.;

        tmptdc = teTdc[0];
        bin = tmptdc&0x3ff;
        tmptdc_f = tmptdc + mBTofINLCorr->getVpdINLCorr(eastwest, techan, bin);
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
