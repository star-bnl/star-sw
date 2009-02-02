/***************************************************************************
 *
 * $Id: StBTofHitMaker.cxx,v 1.1 2009/02/02 21:52:36 dongx Exp $
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
#include "StEvent/StEvent.h"
#include "StDAQMaker/StDAQReader.h"

#include "StRtsTable.h"
#include "DAQ_TOF/daq_tof.h"

ClassImp(StBTofHitMaker);

//_____________________________________________________________
StBTofHitMaker::StBTofHitMaker(const char *name):StRTSBaseMaker("tof",name)
, mStEvent(0),fTof(0)
{
  LOG_INFO << "StBTofHitMaker::ctor"  << endm;
}

//_____________________________________________________________
StBTofHitMaker::~StBTofHitMaker() 
{ }

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
    for (int iword=0;iword<nword;iword++) {
      unsigned int dataword=fTof->ddl[ifib][iword];
      ///  now process data word seperately, get TDC information from data words.
      if( (dataword&0xF0000000)>>28 == 0xD) continue;  /// header tag word
      if( (dataword&0xF0000000)>>28 == 0xE) continue;  /// TDIG separator word
      if( (dataword&0xF0000000)>>28 == 0xA) {  /// header trigger data flag
        ///  do nothing at this moment.
      }            
      if( (dataword&0xF0000000)>>28 == 0x2) {   /// trigger time here.
        mTriggerTimeStamp[ifib] = dataword;
        continue; 
      } 
      if( (dataword&0xF0000000)>>28 == 0xC) {   /// geographical data
        halftrayid = dataword&0x01;    
        trayid     = (dataword&0x0FE)>>1;
        if(trayid==121 && ifib==0) trayid=121;  /// west
        if(trayid==121 && ifib==2) trayid=122;  /// east
        continue;
      }
      if(halftrayid<0 || trayid<0) continue;

      int edgeid =int( (dataword & 0xf0000000)>>28 );
      if((edgeid !=4) && (edgeid!=5)) continue;   /// not leading or trailing

      int tdcid=(dataword & 0x0F000000)>>24;  /// 0-15
      int tdigid=tdcid/4;   /// 0-3 for half tray.
      int tdcchan=(dataword&0x00E00000)>>21;         /// tdcchan is 0-7 here.
      ///
      TofRawHit temphit;
      temphit.fiberid = ifib;
      temphit.trayID  = trayid;
      unsigned int timeinbin = ((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  /// time in tdc bin
      temphit.tdc     = timeinbin;
      /// global channel number here
      temphit.globaltdcchan = tdcchan + (tdcid%4)*8+tdigid*24+halftrayid*96; /// 0-191 for tray
      temphit.dataword      = dataword;
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

  /// fill the Tof header
  StBTofHeader *tofHeader = new StBTofHeader();
  for(int i=0;i<4;i++)
    tofHeader->setTriggerTime(mTriggerTimeStamp[i], i);
  // others
  mBTofCollection->setHeader(tofHeader);

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
  }
  else {
    LOG_INFO << " - StEvent BTofCollection does not Exist" << endm;
    LOG_INFO << " - StEvent BTofRawHitCollection does not Exist" << endm;
  }
}
