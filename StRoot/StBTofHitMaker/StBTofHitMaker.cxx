/***************************************************************************
 *
 * $Id: StBTofHitMaker.cxx,v 1.14 2009/12/14 19:39:44 dongx Exp $
 *
 * Author: Valeri Fine, BNL Feb 2008
 ***************************************************************************
 *
 * Description:  Create the TOF data coming from the RTS_READER
 *
 * Input:  RTS_Reader
 * Output: TOF data
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StBTofHitMaker.h"

#include "StEventTypes.h"
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StBTofRawHit.h"
#include "StBTofUtil/StBTofRawHitCollection.h"  
#include "StBTofUtil/StBTofHitCollection.h"
#include "StBTofUtil/StBTofDaqMap.h"
#include "StBTofUtil/StBTofINLCorr.h"
#include "StEvent/StEvent.h"
#include "StDAQMaker/StDAQReader.h"

#include "StRtsTable.h"
#include "DAQ_TOF/daq_tof.h"

ClassImp(StBTofHitMaker);

//_____________________________________________________________
StBTofHitMaker::StBTofHitMaker(const char *name):StRTSBaseMaker("tof",name)
   , mStEvent(0),fTof(0)
   , mNValidTrays(-1)          //! number of valid TOF trays
   , mBTofCollection(0)        //! pointer to StBTofCollection
   , mRawHitCollection(0)      //! pointer to StBTofRawHitCollection
   , mHitCollection(0)         //! pointer to StBTofHitCollection
   , mBTofDaqMap(0)            //! pointer to the TOF daq map
   , mBTofINLCorr(0)           //! INL corretion;
   , mBTofSortRawHit(0)        //! to sort the TOF hits
{
  LOG_INFO << "StBTofHitMaker::ctor"  << endm;
}

//_____________________________________________________________
StBTofHitMaker::~StBTofHitMaker() 
{ }

//_____________________________________________________________
void StBTofHitMaker::Clear(Option_t* option) 
{ 
  TofLeadingHits.clear();
  TofTrailingHits.clear();

  for(int i=0;i<4;i++) mTriggerTimeStamp[i] = 0;
}

//_____________________________________________________________
Int_t StBTofHitMaker::Init()
{
  Clear("");
  return kStOK;
}

//_____________________________________________________________
Int_t StBTofHitMaker::InitRun(Int_t runnumber)
{
  ///////////////////////////////////////////////////////////////
  // TOF Daq map and INL initialization -- load from StBTofUtil
  ///////////////////////////////////////////////////////////////
  mBTofDaqMap = new StBTofDaqMap();
  mBTofDaqMap->Init(this);
  LOG_INFO << " Initialize Daq map ... " << endm;

  mNValidTrays = mBTofDaqMap->numberOfValidTrays();

  mBTofINLCorr = new StBTofINLCorr();
  mBTofINLCorr->initFromDbase(this);
  LOG_INFO << " Initialize INL table ... " << endm;

  mBTofSortRawHit = new StBTofSortRawHit();
  mBTofSortRawHit->Init(this, mBTofDaqMap);
  mBTofSortRawHit->setVpdDelay(runnumber);
  LOG_INFO << " Initialize StBTofSortRawHit() ... " << endm;

  return kStOK;
}

//_____________________________________________________________
Int_t StBTofHitMaker::FinishRun(Int_t runnumber)
{
  if(mBTofDaqMap) delete mBTofDaqMap;
  mBTofDaqMap = 0;

  if(mBTofINLCorr) delete mBTofINLCorr;
  mBTofINLCorr = 0;

  return kStOK;
}

//-------------------------------------------------------------
Int_t StBTofHitMaker::Finish()
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
StBTofCollection *StBTofHitMaker::GetBTofCollection()
{
  /// Get StEvent if any at once
  StBTofCollection *btofCollection = 0;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  LOG_INFO << "StBTofHitMaker::Make : StEvent has been retrieved " 
        <<mStEvent<< endm;

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
StRtsTable *StBTofHitMaker::GetNextRaw() 
{
  /// Query  RTS/tof/raw cluster data from DAQ system
  LOG_INFO  << " StBTofHitMaker::GetNextRaw()" << endm;

  StRtsTable *daqTofTable = GetNextLegacy();
  if (daqTofTable) {
     fTof = (tof_t*)*DaqDta()->begin();
  }
  return daqTofTable;
}
//_____________________________________________________________
Int_t StBTofHitMaker::Make()
{
   mBTofCollection = GetBTofCollection();
   LOG_INFO << " getting the tof collection " << mBTofCollection << endm;
   if (mBTofCollection) {
      if ( GetNextRaw() ) {
        /// Unpack TOF raw data from daq structure
        int errorType=UnpackTofRawData();                
        if(errorType>0) { 
          LOG_WARN<<"TOF_READER::UnPack TOF Data ERROR!"<<endm;
        }
        fillBTofRawHitCollection();
        fillBTofHeader();           /// BTofHeader should be filled before making BTofHits (trigger selection)
        fillBTofHitCollection();
        fillStEvent();
      }
   }
   return kStOk;
}
//____________________________________________________
/*!
 * The unpack function for TOF daq data. 
 * Please refer to the TOF fiber data format document.
 */
//____________________________________________________
Int_t StBTofHitMaker::UnpackTofRawData()
{
  if(!fTof) return 1;    /// fail to unpack TOF data

  /// Initialize raw hits vector.
  TofLeadingHits.clear();
  TofTrailingHits.clear();

  for(int ifib=0;ifib<4;ifib++){     // 4 fibers
    int nword=fTof->ddl_words[ifib];
    if(nword <= 0) continue;
    int halftrayid    = -99;
    int trayid        = -99;
    mTriggerTimeStamp[ifib] = 0;

//    printf("RDO %d: words %d:\n",ifib+1,fTof->ddl_words[ifib]) ;

    for (int iword=0;iword<nword;iword++) {
      unsigned int dataword=fTof->ddl[ifib][iword];


      ///  now process data word seperately, get TDC information from data words.
      if( (dataword&0xF0000000)>>28 == 0xD) continue;  /// header tag word
      if( (dataword&0xF0000000)>>28 == 0xE) continue;  /// TDIG separator word
      if( (dataword&0xF0000000)>>28 == 0xA) {  /// header trigger data flag
        ///  do nothing at this moment.
        continue;
      }            
      if( (dataword&0xF0000000)>>28 == 0x2) {   /// trigger time here.
        if(mTriggerTimeStamp[ifib]==0) mTriggerTimeStamp[ifib] = dataword;  // Save the first trigger time in each fiber
        continue; 
      } 
      if( (dataword&0xF0000000)>>28 == 0xC) {   /// geographical data
        halftrayid = dataword&0x01;    
        trayid     = (dataword&0x0FE)>>1;
        continue;
      }
      if(halftrayid<0) continue;
      if(trayid<1 || trayid >122) {
        LOG_ERROR<<"StBTofHitMaker::DATA ERROR!! unexpected trayID ! "<<endm;
        continue;
      }
      int edgeid =int( (dataword & 0xf0000000)>>28 );
      if((edgeid !=4) && (edgeid!=5)) continue;   /// not leading or trailing

      int tdcid=(dataword & 0x0F000000)>>24;  /// 0-15
      int tdigid=tdcid/4;   /// 0-3 for half tray.
      int tdcchan=(dataword&0x00E00000)>>21;         /// tdcchan is 0-7 here.
      ///
      TofRawHit temphit={0};
      memset(&temphit,0,sizeof(temphit));
      temphit.fiberid = (UChar_t)ifib;
      temphit.trayID  = (UChar_t)trayid;
      unsigned int timeinbin = ((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  /// time in tdc bin
      temphit.tdc     = timeinbin;
      /// global channel number here
      temphit.globaltdcchan = (UChar_t)(tdcchan + (tdcid%4)*8+tdigid*24+halftrayid*96); /// 0-191 for tray
      temphit.dataword      = dataword;

//      printf("\t%d: 0x%08X [%u dec]\n",iword,fTof->ddl[ifib][iword],fTof->ddl[ifib][iword]) ;

      if(edgeid == 4) {     /// leading edge data
        TofLeadingHits.push_back(temphit);
      } else if (edgeid==5){     /// trailing edge data
        TofTrailingHits.push_back(temphit);
      }  else {
        LOG_WARN<<"StBTofHitMaker::Unknown TDC data type ! "<<endm;
        continue;
      }
      /// end of unpack all data words.        
    }  /// end loop data words    
    ///
  } /// end loop fibers
  ///
  return -1;
}

//____________________________________________
/*!
 * Fill the data from DAQ into BTofRawHit Collection in StEvent
 */
void StBTofHitMaker::fillBTofRawHitCollection()
{
  /// TofRawdata collection.
  for (unsigned int i=0;i<TofLeadingHits.size();i++){  ///  Leading Edge
    char           flag   = (+1)*(TofLeadingHits[i].fiberid+1);
    unsigned char  trayid = TofLeadingHits[i].trayID;
    unsigned char  chn    = TofLeadingHits[i].globaltdcchan;
    unsigned int   tdc    = TofLeadingHits[i].tdc;
    mBTofCollection->addRawHit(new StBTofRawHit(flag,trayid,chn,tdc));
  }

  for (unsigned int i=0;i<TofTrailingHits.size();i++){  ///  Trailing Edge
    char           flag   = (-1)*(TofTrailingHits[i].fiberid+1);
    unsigned char  trayid = TofTrailingHits[i].trayID;
    unsigned char  chn    = TofTrailingHits[i].globaltdcchan;
    unsigned int   tdc    = TofTrailingHits[i].tdc;
    mBTofCollection->addRawHit(new StBTofRawHit(flag,trayid,chn,tdc));
  }
  
}
//____________________________________________
/*!
 * Fill the data into BTofHeader in StEvent
 */
void StBTofHitMaker::fillBTofHeader()
{
  /// fill the Tof header
  StBTofHeader *tofHeader = new StBTofHeader();
  for(int i=0;i<4;i++)
    tofHeader->setTriggerTime(mTriggerTimeStamp[i], i);
  // others
  mBTofCollection->setHeader(tofHeader);
}
//____________________________________________
/*!
 * Fill the data from BTofRawHit into BTofHit Collection in StEvent
 */
void StBTofHitMaker::fillBTofHitCollection()
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
  
//_________________________________________________________________________
/*!
 * Fill and store TOF Collections in StEvent. Create BTofCollection if necessary
 */
void StBTofHitMaker::fillStEvent() {

  LOG_INFO << "StBTofHitMaker::fillStEvent() Starting..." << endm;

  /// make sure we have a tofcollection
  if(!mBTofCollection){
    mBTofCollection = new StBTofCollection();
    mStEvent->setBTofCollection(mBTofCollection);
  }

  ///
  StBTofCollection* btofCollection = mStEvent->btofCollection();
  if(btofCollection){
    ///LOG_INFO << " + StEvent BTofCollection Exists" << endm;
    if(btofCollection->rawHitsPresent()) {
      ///LOG_INFO << " + StEvent BTofRawHitCollection Exists" << endm;
      StSPtrVecBTofRawHit& rawTofVec = btofCollection->tofRawHits();
      LOG_INFO << "   StEvent BTofRawHitCollection has " << rawTofVec.size() << " entries..." << endm;
      if(Debug()) {
        for(size_t i=0;i<rawTofVec.size();i++) {
          LOG_DEBUG << (*rawTofVec[i]) << endm;
        }
      }
    }
    else {
      LOG_INFO << " - StEvent BTofRawHitCollection does not Exist" << endm;
    }

    if(btofCollection->hitsPresent()) {
      ///LOG_INFO << " + StEvent BTofHitCollection Exists" << endm;
      StSPtrVecBTofHit& tofVec = btofCollection->tofHits();  
      LOG_INFO << "   StEvent BTofHitCollection has " << tofVec.size() << " entries..." << endm;
      if(Debug()) {
        for(size_t i=0;i<tofVec.size();i++) {
          LOG_DEBUG << (*tofVec[i]) << endm; 
        }
      }  
    }    
    else {
      LOG_INFO << " - StEvent BTofHitCollection does not Exist" << endm;
    }

  }
  else {
    LOG_INFO << " - StEvent BTofCollection does not Exist" << endm;
    LOG_INFO << " - StEvent BTofRawHitCollection does not Exist" << endm;
    LOG_INFO << " - StEvent BTofHitCollection does not Exist" << endm;
  }
}
