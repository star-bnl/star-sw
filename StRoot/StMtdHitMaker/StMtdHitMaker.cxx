/***************************************************************************
 *
 * $Id: StMtdHitMaker.cxx,v 1.4 2012/02/03 17:52:41 geurts Exp $ 
 *
 * Author: Frank Geurts (Rice)
 ***************************************************************************
 *
 * Description:  Create the MTD data coming from the RTS_READER
 *
 * Input:  RTS_Reader
 * Output: MTD StEvent objects
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StMtdHitMaker.h"
#include "StEventTypes.h"
#include "StEvent/StMtdCollection.h"
#include "StEvent/StMtdHeader.h"
#include "StEvent/StMtdRawHit.h"
#include "StEvent/StEvent.h"
#include "StDAQMaker/StDAQReader.h"
#include "StRtsTable.h"
#include "DAQ_MTD/daq_mtd.h"


ClassImp(StMtdHitMaker);

//_____________________________________________________________
StMtdHitMaker::StMtdHitMaker(const char *name):StRTSBaseMaker("mtd",name)
   , mStEvent(0),fMtd(0)
   , mNValidTrays(-1)          //! number of valid MTD trays
   , mMtdCollection(0)        //! pointer to StMtdCollection
{
  LOG_DEBUG << "StMtdHitMaker::ctor"  << endm;
}


//_____________________________________________________________
StMtdHitMaker::~StMtdHitMaker() 
{ }


//_____________________________________________________________
void StMtdHitMaker::Clear(Option_t* option)  { 
  MtdLeadingHits.clear();
  MtdTrailingHits.clear();
  memset(mTriggerTimeStamp,0,2);
}


//_____________________________________________________________
Int_t StMtdHitMaker::Init() {
  Clear("");
  /// Initialize Tray-to-Tdig map (-1 means no TDIG board)
  memset(mTray2TdigMap,-1,sizeof(mTray2TdigMap));

  return kStOK;
}


//_____________________________________________________________
Int_t StMtdHitMaker::InitRun(Int_t runnumber) {
  /// Find out what year we're in
  mYear= (Int_t)runnumber/1e6 -1 ;

  /// Run-12 (this will move to database)
  /// note: index runs from 0-29 for backlegs 1-30, same for tray#
  mTray2TdigMap[25][1] = 0; /// backleg #26 (3 trays)
  mTray2TdigMap[25][2] = 1;
  mTray2TdigMap[25][3] = 4;

  mTray2TdigMap[26][0] = 0; /// backleg #27 (5 trays)
  mTray2TdigMap[26][1] = 1;
  mTray2TdigMap[26][2] = 2;
  mTray2TdigMap[26][3] = 5;
  mTray2TdigMap[26][4] = 4;

  mTray2TdigMap[27][0] = 0; /// backleg #28 (5 trays)
  mTray2TdigMap[27][1] = 1;
  mTray2TdigMap[27][2] = 2;
  mTray2TdigMap[27][3] = 5;
  mTray2TdigMap[27][4] = 4;

  return kStOK;
}


//_____________________________________________________________
Int_t StMtdHitMaker::FinishRun(Int_t runnumber) {
  // Placeholder for deleting database objects
  return kStOK;
}


//-------------------------------------------------------------
Int_t StMtdHitMaker::Finish() {
  Clear("");
  return kStOK;
}


//_____________________________________________________________
/*!
 * This method is to obtain the mtdCollection from StEvent.
 * If StEvent is in the chain, retrieve it; if no StEvent in the chain,
 * a new StEvent is created.
 */
StMtdCollection *StMtdHitMaker::GetMtdCollection() {
  /// Get StEvent if any at once
  StMtdCollection *mtdCollection = 0;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));

  if (mStEvent) {
    mtdCollection = mStEvent->mtdCollection();

    /// Need to create the hit collection
    if ( !mtdCollection )  {
      ///  Save the hit collection to StEvent
      mtdCollection = new StMtdCollection();
      mStEvent->setMtdCollection(mtdCollection);
    }
  }
  else {LOG_WARN << "No StEvent found" << endm; }

  return mtdCollection;
}


//_____________________________________________________________
StRtsTable *StMtdHitMaker::GetNextRaw(int sec) {
  assert(0 && "RTS doesn't provide sector by sector legacy mtd banks yet");
  StRtsTable *daqMtdTable = GetNextLegacy(sec);
  if (daqMtdTable) {
    fMtd = (mtd_t*)*DaqDta()->begin();
  }
  return daqMtdTable;
}


//_____________________________________________________________
StRtsTable *StMtdHitMaker::GetNextRaw() {
  /// Query  RTS/mtd/raw cluster data from DAQ system
  LOG_DEBUG  << "GetNextRaw()" << endm;

  StRtsTable *daqMtdTable = GetNextLegacy();
  if (daqMtdTable) {
    fMtd = (mtd_t*)*DaqDta()->begin();
  }
  return daqMtdTable;
}


//_____________________________________________________________
Int_t StMtdHitMaker::Make() {
  mMtdCollection = GetMtdCollection();
  LOG_DEBUG << " getting the mtd collection " << mMtdCollection << endm;
  if (mMtdCollection) {
    if ( GetNextRaw() ) {
      /// Unpack MTD raw data from daq structure
      int errorType=UnpackMtdRawData();                
      if(errorType>0) { 
	LOG_WARN<<"MTD_READER::Unpack MTD Data ERROR!"<<endm;
      }
      fillMtdRawHitCollection();
      fillMtdHeader();           /// MtdHeader should be filled before making MtdHits (trigger selection)
      fillMtdHitCollection();
      fillStEvent();
    }
  }
  return kStOk;
}


//____________________________________________________
/*!
 * The unpack function for MTD daq data. 
 * Please refer to the MTD fiber data format document.
 */
//____________________________________________________
Int_t StMtdHitMaker::UnpackMtdRawData() {
  if(!fMtd) return 1;    /// fail to unpack MTD data

  /// Initialize raw hits vector.
  MtdLeadingHits.clear();
  MtdTrailingHits.clear();

  const int nTHUB=1;
  /// Loop over MTD THUBs (each connect by fiber to an RDO
  for(int ifib=0;ifib<nTHUB;ifib++){
    int nword=fMtd->ddl_words[ifib];
    if(nword <= 0) continue;
    int halfbacklegid    = -99;
    int backlegid        = -99;
    mTriggerTimeStamp[ifib] = 0;

    LOG_DEBUG << "RDO " << ifib+1 << ": words " << fMtd->ddl_words[ifib] << endm;

    /// process data word seperately, get TDC information from data words.
    for (int iword=0;iword<nword;iword++) {
      unsigned int dataword=fMtd->ddl[ifib][iword];
      LOG_DEBUG << "DATAWORD: 0x" << hex << dataword << dec << endm;

      /// Trigger time
      if( (dataword&0xF0000000)>>28 == 0x2) {
	// Save the first trigger time in each fiber
        if(mTriggerTimeStamp[ifib]==0) mTriggerTimeStamp[ifib] = dataword;
        continue; 
      }

      if( (dataword&0xF0000000)>>28 == 0xD) continue;  /// header tag word
      if( (dataword&0xF0000000)>>28 == 0xE) continue;  /// TDIG separator word
      if( (dataword&0xF0000000)>>28 == 0xA) continue;   /// header trigger data flag
              
      /// geographical data
      if( (dataword&0xF0000000)>>28 == 0xC) {
        halfbacklegid =  dataword&0x01;    
        backlegid     = (dataword&0x0FE)>>1;
        continue;
      }
      // range checks
      if(halfbacklegid<0) continue;
      if(backlegid<1 || backlegid >124) {
        LOG_ERROR<<"StMtdHitMaker::DATA ERROR!! unexpected backlegID ! "<<endm;
        continue;
      }


      if( (dataword&0xF0000000)>>28 == 0x6) continue; //error

      /// Look for edge type (4=leading, 5=trailing)
      int edgeid =int( (dataword & 0xF0000000)>>28 );
      if((edgeid !=4) && (edgeid!=5)) continue;   /// not leading or trailing 

      /// From here on assume TDC data, and decode accordingly

      /// decode TDIG-Id ...
      int tdcid=(dataword & 0x0F000000)>>24;  /// range: 0-15
      //int tdigid=tdcid/4;             /// for halftray0, 0-2 for tdig0; 4-6 for tdig1
      //if (halfbacklegid==1) tdigid=4; /// for halftray1, 0-2 for tdig 4

      /// MTD backlegs 27/28
      int tdigid=((tdcid & 0xC)>>2) + halfbacklegid*4;

      /// decode TDC channel ...
      int tdcchan=(dataword&0x00E00000)>>21; /// tdcchan range: 0-7


      /// decode TDC time bin ...
      unsigned int timeinbin = ((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  /// time in tdc bin


      /// Fill MTD raw hit structures
      MtdRawHit temphit={0};
      memset(&temphit,0,sizeof(temphit));
      temphit.fiberid = (UChar_t)ifib;
      temphit.backlegID  = (UChar_t)backlegid;
      temphit.tdc     = timeinbin;
      /// global channel number here, 
      if (mYear<12)
	temphit.globaltdcchan = (UChar_t)(tdcChan2globalStrip(tdigid,tdcid,tdcchan,backlegid)); 
      else
	temphit.globaltdcchan = (UChar_t)(tdcChan2globalStrip(tdigid,tdcid,tdcchan)); 
      temphit.dataword      = dataword;

      /// lookup corresponding tray# for TDIG-Id
      int itray;
      for (itray=1;itray<=5;itray++){
	if (mTray2TdigMap[backlegid-1][itray-1] == tdigid) break;
      }

      if(edgeid == 4) {     /// leading edge data
        MtdLeadingHits.push_back(temphit);
      } else if (edgeid==5){     /// trailing edge data
        MtdTrailingHits.push_back(temphit);
      }  else {
        LOG_WARN<<"StMtdHitMaker::Unknown TDC data type ! "<<endm;
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
 * Map TDC channel to a global strip coordinate (Run 12 and later)
 */
Int_t StMtdHitMaker::tdcChan2globalStrip(int tdigBoardId, int tdcId, int tdcChan) {
  if (mYear<12)
    LOG_WARN << "tdcchan2globalstrip(int,int,int) called for pre Run-12 run: wrong globalStrip" << endm;

  Int_t globalStripId=-1;
  int Hnum=tdcId%4+1;
  int globalTdcChan=Hnum*10+tdcChan;
  int mtdStrip[24]={21,12,32,20,14,35,25,13,30,24,11,31,
		    34,22,10,37,27,17,33,23,16,36,26,15};
  for(int i=0;i<24;i++){
    if(mtdStrip[i]==globalTdcChan) {globalStripId=i+1;break;}
  }
  if(tdigBoardId>3)
    globalStripId = (globalStripId>12)? globalStripId-12:globalStripId+12;
	
  return globalStripId;
}


//____________________________________________
/*!
 * Map TDC channel to a global strip coordinate (prior to Run 12)
 */
Int_t StMtdHitMaker::tdcChan2globalStrip(int tdigBoardId,int tdcId,int tdcChan,int backLegId) {

  /// This function is only useful before Run 12.
  if (mYear>11) {
    return tdcChan2globalStrip(tdigBoardId,tdcId,tdcChan);
  }

  /// 
  Int_t globalStripId=-1;
  if(backLegId==26){
    if (tdcId>3) tdcId=tdcId-4; //scale to H#
    int globalTdcChan=(tdcId+1)*10+tdcChan;
    int mtdStrip[24]={34,22,10,37,27,17,33,23,16,36,26,15,
		      21,12,32,20,14,35,25,13,30,24,11,31};
    for(int i=0;i<24;i++){
      if(mtdStrip[i]==globalTdcChan) {globalStripId=i+1; break;}
    }
  }
  
  if(backLegId==1){
    int globalTdcChan=(tdcId+1)*10+tdcChan;
    int mtdStrip[18]=  {34,22,10,37,27,17, 32,20,30,24,11,31, 33,23,16,36,26,15};
    for(int i=0;i<18;i++){
      if(mtdStrip[i]==globalTdcChan) {globalStripId=i+1;break;}
    }
  }
  
  return globalStripId;
}
//____________________________________________
/*!
 * Fill the data from DAQ into MtdRawHit Collection in StEvent
 */
void StMtdHitMaker::fillMtdRawHitCollection() {
  /// MtdRawdata collection.
  for (unsigned int i=0;i<MtdLeadingHits.size();i++){  ///  Leading Edge
    char           flag   = (+1)*(MtdLeadingHits[i].fiberid+1);
    unsigned char  backlegid = MtdLeadingHits[i].backlegID;
    unsigned char  chn    = MtdLeadingHits[i].globaltdcchan;
    unsigned int   tdc    = MtdLeadingHits[i].tdc;
    LOG_DEBUG << " mtd raw hit LE flag:" << (short)flag << " backlegid:"<< (short)backlegid << " chn:" << (short)chn << " tdc:"<< tdc << endm; 
    mMtdCollection->addRawHit(new StMtdRawHit(flag,backlegid,chn,tdc));
  }

  for (unsigned int i=0;i<MtdTrailingHits.size();i++){  ///  Trailing Edge
    char           flag   = (-1)*(MtdTrailingHits[i].fiberid+1);
    unsigned char  backlegid = MtdTrailingHits[i].backlegID;
    unsigned char  chn    = MtdTrailingHits[i].globaltdcchan;
    unsigned int   tdc    = MtdTrailingHits[i].tdc;
    LOG_DEBUG << " mtd raw hit TE flag:" << (short)flag << " backlegid:"<< (short)backlegid << " chn:" << (short)chn << " tdc:"<< tdc << endm; 
    mMtdCollection->addRawHit(new StMtdRawHit(flag,backlegid,chn,tdc));
  }  
}


//____________________________________________
/*!
 * Fill the data into MtdHeader in StEvent
 */
void StMtdHitMaker::fillMtdHeader() {
  /// fill the MTD header
  StMtdHeader *mtdHeader = new StMtdHeader();
  const int nTHUB=1;
  for(int i=0;i<nTHUB;i++){
    mtdHeader->setTriggerTime(mTriggerTimeStamp[i], i);
    LOG_DEBUG << "Trigger Time Stamp "<< i+1 <<": " << (unsigned int)mTriggerTimeStamp[i] << endm;
  }

  mMtdCollection->setHeader(mtdHeader);
}


//____________________________________________
/*!
 * Fill the data from MtdRawHit into MtdHit Collection in StEvent
 */
void StMtdHitMaker::fillMtdHitCollection() {
  // Place holder for MTD Hit object creation and storage
}

  
//_________________________________________________________________________
/*!
 * Fill and store MTD Collections in StEvent. Create MtdCollection if necessary
 */
void StMtdHitMaker::fillStEvent() {

  LOG_DEBUG << "fillStEvent() Starting..." << endm;

  /// make sure we have a mtdcollection
  if(!mMtdCollection){
    LOG_WARN << "No MtdCollection ... creating one in StEvent" << endm;
    mMtdCollection = new StMtdCollection();
    mStEvent->setMtdCollection(mMtdCollection);
  }

  //
  StMtdCollection* mtdCollection = mStEvent->mtdCollection();
  if(mtdCollection){
    if(mtdCollection->rawHitsPresent()) {
      StSPtrVecMtdRawHit& rawMtdVec = mtdCollection->mtdRawHits();
      LOG_INFO << "MtdRawHitCollection: " << rawMtdVec.size() << " entries" << endm;
      if(Debug()) {
        for(size_t i=0;i<rawMtdVec.size();i++) {
          LOG_DEBUG << (*rawMtdVec[i]) << endm;
        }
      }
    }
    else {
      LOG_INFO << "No MtdRawHitCollection" << endm;
    }

    if(mtdCollection->hitsPresent()) {
      StSPtrVecMtdHit& mtdVec = mtdCollection->mtdHits();  
      LOG_INFO << "MtdHitCollection: " << mtdVec.size() << " entries..." << endm;
      if(Debug()) {
        for(size_t i=0;i<mtdVec.size();i++) {
          LOG_DEBUG << (*mtdVec[i]) << endm; 
        }
      }  
    }    
    else {
      LOG_INFO << "No MtdHitCollection" << endm;
    }

  }
  else {
    LOG_WARN << "No MtdCollection" << endm;
    LOG_INFO << "No MtdRawHitCollection" << endm;
    LOG_INFO << "No MtdHitCollection" << endm;
  }
}
