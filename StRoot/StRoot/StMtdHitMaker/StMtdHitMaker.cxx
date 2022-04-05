/***************************************************************************
 *
 * $Id: StMtdHitMaker.cxx,v 1.25 2016/07/27 15:31:15 marr Exp $ 
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
#include "StBTofUtil/StBTofINLCorr.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMtdCollection.h"
#include "StMuDSTMaker/COMMON/StMuMtdRawHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "tables/St_mtdTrayToTdigMap_Table.h"
#include "tables/St_mtdTrayIdMap_Table.h"
#include "tables/St_mtdTdigIdMap_Table.h"
#include "tables/St_mtdTriggerTimeCut_Table.h"

ClassImp(StMtdHitMaker);

//_____________________________________________________________
StMtdHitMaker::StMtdHitMaker(const char *name):StRTSBaseMaker("mtd",name),
					       mStEvent(0),fMtd(0), mUseMuDst(0),
					       mCosmicFlag(kFALSE), mCosmicTrigTimeWinFile(""),
					       mTriggerWndSelection(kTRUE), mSwapBacklegInRun13(0), 
					       mReverseStripInRun14(kFALSE),
					       mYear(-1),
					       mNValidTrays(-1),         //! number of valid MTD trays
					       mMtdCollection(0),        //! pointer to StMtdCollection
					       mINLCorr(0)
{
  LOG_DEBUG << "StMtdHitMaker::ctor"  << endm;
 
  Int_t mStripMap[24]={21,12,32,20,14,35,25,13,30,24,11,31,34,22,10,37,27,17,33,23,16,36,26,15};
  for(int i=0;i<24;i++) mtdStrip[i] = mStripMap[i];
  hxhyhz = new TH3D("hxhyhz","hxhyhz",10,19.5,29.5,10,-0.5,9.5,30,-0.5,29.5);
}


//_____________________________________________________________
StMtdHitMaker::~StMtdHitMaker() 
{ 
}


//_____________________________________________________________
void StMtdHitMaker::Clear(Option_t* option)  
{ 
  MtdLeadingHits.clear();
  MtdTrailingHits.clear();
  memset(mTriggerTimeStamp,0,2);
}


//_____________________________________________________________
Int_t StMtdHitMaker::Init() 
{
  Clear("");

  memset(mTray2TdigMap,-1,sizeof(mTray2TdigMap));
  memset(mTrayId,0,sizeof(mTrayId));
  memset(mTdigId,0,sizeof(mTdigId));

  for(Int_t i=0;i<gMtdNModulesAll;i++)
    {
      mTriggerTimeWindow[i][0] = 0;
      mTriggerTimeWindow[i][1] = 9999;
    }
  return kStOK;
}


//_____________________________________________________________
Int_t StMtdHitMaker::InitRun(Int_t runnumber) 
{
  /// Find out what year we're in
  mYear= (Int_t)(runnumber/1000000 -1);
  
  LOG_INFO << "Process data from year " << mYear << endm;
  
  // Extract MTD maps from database
  LOG_INFO << "Retrieving mtdTrayToTdigMap table from database ..." << endm;
  TDataSet *dataset = GetDataBase("Geometry/mtd/mtdTrayToTdigMap");
  St_mtdTrayToTdigMap *mtdTrayToTdigMap = static_cast<St_mtdTrayToTdigMap*>(dataset->Find("mtdTrayToTdigMap"));
  if(!mtdTrayToTdigMap)
    {
      LOG_ERROR << "No mtdTrayToTdigMap table found in database" << endm;
      return kStErr;
    }
  mtdTrayToTdigMap_st *mTrayToTdigTable = static_cast<mtdTrayToTdigMap_st*>(mtdTrayToTdigMap->GetTable());
  for(Int_t i=0; i<gMtdNBacklegs; i++)
    {
      for(Int_t j=0; j<gMtdNModules; j++)
	mTray2TdigMap[i][j] = (Int_t)(mTrayToTdigTable->tdigId[i*5+j]);
    }
  
  LOG_INFO << "Retrieving mtdTrayIdMap table from database ..." << endm;
  dataset = GetDataBase("Geometry/mtd/mtdTrayIdMap");
  St_mtdTrayIdMap *mtdTrayIdMap = static_cast<St_mtdTrayIdMap*>(dataset->Find("mtdTrayIdMap"));
  if(!mtdTrayIdMap)
    {
      LOG_ERROR << "No mtdTrayIdMap table found in database" << endm;
      return kStErr;
    }
  mtdTrayIdMap_st *mtdTrayIdTable = static_cast<mtdTrayIdMap_st*>(mtdTrayIdMap->GetTable());
  for(Int_t i=0; i<gMtdNBacklegs; i++)
    {
      for(Int_t j=0; j<gMtdNModules; j++)
	mTrayId[i][j] = (Int_t)(mtdTrayIdTable->trayId[i*5+j]);
    }

  LOG_INFO << "Retrieving mtdTdigIdMap table from database ..." << endm;
  dataset = GetDataBase("Geometry/mtd/mtdTdigIdMap");
  St_mtdTdigIdMap *mtdTdigIdMap = static_cast<St_mtdTdigIdMap*>(dataset->Find("mtdTdigIdMap"));
  if(!mtdTdigIdMap)
    {
      LOG_ERROR << "No mtdTdigIdMap table found in database" << endm;
      return kStErr;
    }
  mtdTdigIdMap_st *mtdTdigIdTable = static_cast<mtdTdigIdMap_st*>(mtdTdigIdMap->GetTable());
  for(Int_t i=0; i<gMtdNModulesAll; i++)
    {
      mTdigId[i] = (Int_t)(mtdTdigIdTable->tdigBoardId[i]);
    }

  LOG_INFO << "Retrieving mtdTriggerTimeCut table from database ..." << endm;
  dataset = GetDataBase("Calibrations/mtd/mtdTriggerTimeCut");
  St_mtdTriggerTimeCut *mtdTriggerTimeCut = static_cast<St_mtdTriggerTimeCut*>(dataset->Find("mtdTriggerTimeCut"));
  if(!mtdTriggerTimeCut)
    {
      LOG_ERROR << "No mtdTriggerTimeCut table found in database" << endm;
      return kStErr;
    }
  mtdTriggerTimeCut_st *mtdTriggerTimeTable = static_cast<mtdTriggerTimeCut_st*>(mtdTriggerTimeCut->GetTable());
  for(Int_t i=0; i<gMtdNModulesAll; i++)
    {
      mTriggerTimeWindow[i][0] = mtdTriggerTimeTable->minTriggerTime[i];
      mTriggerTimeWindow[i][1] = mtdTriggerTimeTable->maxTriggerTime[i];
      LOG_DEBUG << "(" << i/5 << "," << i%5 << "): " << mTriggerTimeWindow[i][0] << " - " << mTriggerTimeWindow[i][1] << endm;
    }

  // pick up local trigger time window cuts for cosmic ray data
  if(mTriggerWndSelection && mCosmicFlag && mCosmicTrigTimeWinFile.Length()>0)
    {
      LOG_WARN << "Local trigger time window cuts are picked up for cosmic ray data." << endm;
      ifstream inData;
      inData.open(mCosmicTrigTimeWinFile.Data());
      Int_t backleg, module;

      for(Int_t i=0; i<gMtdNModulesAll; i++)
  	{
  	  inData >> backleg >> module >>  mTriggerTimeWindow[i][0] >>  mTriggerTimeWindow[i][1];
  	}
      inData.close();
    }
  
  /// INL Table provided by TOF
  LOG_DEBUG << "Initializing INL table:" << endm;
  mINLCorr = new StBTofINLCorr();
  mINLCorr->initFromDbase(this);
  
  return kStOK;
}


//_____________________________________________________________
Int_t StMtdHitMaker::FinishRun(Int_t runnumber) 
{
  /// clean up several maps
  memset(mTray2TdigMap,-1,sizeof(mTray2TdigMap));
  memset(mTrayId,0,sizeof(mTrayId));
  memset(mTdigId,0,sizeof(mTdigId));

  if(mINLCorr) delete mINLCorr;
  mINLCorr = 0;

  return kStOK;
}


//-------------------------------------------------------------
Int_t StMtdHitMaker::Finish() 
{
  Clear("");
  return kStOK;
}


//_____________________________________________________________
/*!
 * This method is to obtain the mtdCollection from StEvent.
 * If StEvent is in the chain, retrieve it; if no StEvent in the chain,
 * a new StEvent is created.
 */
StMtdCollection *StMtdHitMaker::GetMtdCollection() 
{
  /// Get StEvent if any at once
  LOG_DEBUG << " getting the mtd collection " << mMtdCollection << endm;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  if(mStEvent)
    {
      mUseMuDst = kFALSE;
      mMtdCollection = mStEvent->mtdCollection();
      if(!mMtdCollection)
	{
	  mMtdCollection = new StMtdCollection();
	  mStEvent->setMtdCollection(mMtdCollection);
	}
      if ( GetNextRaw() ) 
	{
	  /// Unpack MTD raw data from daq structure
	  int errorType=UnpackMtdRawData(); 
	  if(errorType>0) 
	    {
	      LOG_WARN<<"MTD_READER::Unpack MTD Data ERROR!"<<endm;
	      return 0;
	    }
	}
    }
  else
    {
      mUseMuDst = kTRUE;
      StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
      if(!mMuDstMaker)
	{
	  LOG_INFO << "No MuDstMaker ..bye-bye..."<<endm;
	  return 0;
	}
      StMuDst* mMuDst=mMuDstMaker->muDst();
      if(!mMuDst)
	{
	  LOG_INFO << "No Mudst ... bye-bye" <<endm;
	  return 0;
	}

      mMtdCollection = new StMtdCollection();

      StMuMtdCollection* muMtdCollection = mMuDst->MtdCollection();
      StMuMtdHeader* muMtdHeader = NULL;
      Int_t nMtdRawHits = 0;
      
      if(muMtdCollection)
	{
	  muMtdHeader = muMtdCollection->mtdHeader();
	  nMtdRawHits = muMtdCollection->rawHitsPresent();
	}
      else
	{
	  muMtdHeader = mMuDst->mtdHeader();
	  nMtdRawHits = mMuDst->numberOfBMTDRawHit();
	}

      for(int i=0;i<nTHUB;i++)
	{ 
	  mTriggerTimeStamp[i] = muMtdHeader->triggerTime(i+1);
	}

      StMuMtdRawHit *ahit = NULL;
      for(Int_t i=0; i<nMtdRawHits; i++)
	{
	  if(muMtdCollection) ahit = muMtdCollection->RawMtdHit(i);
	  else ahit = mMuDst->mtdRawHit(i);
	  Int_t backleg = (Int_t)ahit->backleg();
	  Int_t channel = (Int_t)ahit->channel();

	  if(mYear==13 && mSwapBacklegInRun13 !=0 )
	    {
	      // 0 - do not swap; 
	      // 1 - swapping scheme for first part of Run13, i.e. days 76-126
	      // 2 - swapping scheme for second part of Run13, i.e. days 129-161
	      if(mSwapBacklegInRun13==1)
		{
		  if(backleg==25) backleg = 26;
		  else if (backleg==26) backleg = 25;
		}
	      else if (mSwapBacklegInRun13==2)
		{
		  if(ahit->flag()<0)
		    {
		      if(backleg==25) backleg = 26;
		      else if (backleg==26) backleg = 25;
		    }
		}
	      else
		{
		  LOG_FATAL << "Please check your setup for mSwapBacklegInRun13" << endm;
		  return 0;
		}
	    }
	  
	  if(mYear>=14 && mReverseStripInRun14)
	    {
	      // The ribbon cable is reversed for backleg 7, module 5 since Run14
	      if(backleg==7)
		{
		  Int_t module = (channel-1)/24 + 1;
		  if(module==5)
		    {
		      Int_t cell = (channel-1)%24;
		      if(cell>=0 && cell<=5) cell = 5 - cell;
		      else if (cell>=6 && cell<=11) cell = 17 - cell;
		      else if (cell>=12 && cell<=17) cell = 29 - cell;
		      else if (cell>=18 && cell<=23) cell = 41 - cell;
		      channel = cell + 1 + (module-1)*24;
		    }
		}
	    }
	  mMtdCollection->addRawHit(new StMtdRawHit(ahit->flag(),(UChar_t)backleg,(UChar_t)channel,ahit->tdc()));
	}
    }
  return mMtdCollection;
}


//_____________________________________________________________
StRtsTable *StMtdHitMaker::GetNextRaw(int sec) 
{
  assert(0 && "RTS doesn't provide sector by sector legacy mtd banks yet");
  StRtsTable *daqMtdTable = GetNextLegacy(sec);
  if (daqMtdTable)
    {
      fMtd = (mtd_t*)*DaqDta()->begin();
    }
  return daqMtdTable;
}


//_____________________________________________________________
StRtsTable *StMtdHitMaker::GetNextRaw() 
{
  /// Query  RTS/mtd/raw cluster data from DAQ system
  LOG_DEBUG  << "GetNextRaw()" << endm;
  
  StRtsTable *daqMtdTable = GetNextLegacy();
  if (daqMtdTable) 
    {
      fMtd = (mtd_t*)*DaqDta()->begin();
    }
  return daqMtdTable;
}


//_____________________________________________________________
Int_t StMtdHitMaker::Make() {

  if(GetMtdCollection())
    {
      fillMtdRawHitCollection();
      fillMtdHeader();    
      fillMtdHitCollection();
      fillStEvent();

      return kStOk;
    }
  else
    return kStErr;

}


//____________________________________________________
/*!
 * The unpack function for MTD daq data. 
 * Please refer to the MTD fiber data format document.
 */
//____________________________________________________
Int_t StMtdHitMaker::UnpackMtdRawData() 
{
  if(!fMtd) return 1;    /// fail to unpack MTD data

  /// Initialize raw hits vector.
  MtdLeadingHits.clear();
  MtdTrailingHits.clear();

  /// Loop over MTD THUBs (each connect by fiber to an RDO
  for(int ifib=0;ifib<nTHUB;ifib++)
    {
      int nword=fMtd->ddl_words[ifib];
      if(nword <= 0) continue;
      int halfbacklegid    = -99;
      int backlegid        = -99;
      mTriggerTimeStamp[ifib] = 0;

      LOG_DEBUG << "RDO " << ifib+1 << ": words " << fMtd->ddl_words[ifib] << endm;
      
      /// process data word seperately, get TDC information from data words.
      for (int iword=0;iword<nword;iword++) 
	{
	  unsigned int dataword=fMtd->ddl[ifib][iword];
	  LOG_DEBUG << "DATAWORD: 0x" << hex << dataword << dec << endm;
	  
	  /// Trigger time
	  if( (dataword&0xF0000000)>>28 == 0x2) 
	    {
	      // Save the first trigger time in each fiber
	      if(mTriggerTimeStamp[ifib]==0) mTriggerTimeStamp[ifib] = dataword;
	      continue; 
	    }

	  if( (dataword&0xF0000000)>>28 == 0xD) continue;  /// header tag word
	  if( (dataword&0xF0000000)>>28 == 0xE) continue;  /// TDIG separator word
	  if( (dataword&0xF0000000)>>28 == 0xA) continue;   /// header trigger data flag
	  
	  /// geographical data
	  if( (dataword&0xF0000000)>>28 == 0xC) 
	    {
	      halfbacklegid =  dataword&0x01;    
	      backlegid     = (dataword&0x0FE)>>1;

	      // swap backleg 25 and 26 for run 13
	      if(mYear == 13)
		{
		  if(backlegid==25) backlegid = 26;
		  else if (backlegid==26) backlegid = 25;
		}

	      continue;
	    }
	  // range checks
	  if(halfbacklegid<0) continue;
	  if(backlegid<1 || backlegid>gMtdNBacklegs) 
	    {
	      LOG_ERROR<<"StMtdHitMaker::DATA ERROR!! unexpected backlegid "<< backlegid << " !" << endm;
	      continue;
	    }

	  if( (dataword&0xF0000000)>>28 == 0x6) continue; //error
	  
	  /// Look for edge type (4=leading, 5=trailing)
	  int edgeid =int( (dataword & 0xF0000000)>>28 );
	  if((edgeid !=4) && (edgeid!=5)) continue;   /// not leading or trailing 
	  
	  /// From here on assume TDC data, and decode accordingly

	  /// decode TDIG-Id ...
	  int tdcid=(dataword & 0x0F000000)>>24;  /// range: 0-15
	  
	  /// MTD backlegs 27/28
	  int tdigid=((tdcid & 0xC)>>2) + halfbacklegid*4;
	  
	  /// decode HPTDC id ...
	  //int ihptdcid = tdcid&0x3;
	  
	  /// decode TDC channel ...
	  int tdcchan=(dataword&0x00E00000)>>21; /// tdcchan range: 0-7
	  
	  /// decode TDC time bin ...
	  unsigned int timeinbin = ((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  /// time in tdc bin
	  
	  
	  /// lookup corresponding tray# for TDIG-Id
	  int itray=0;
	  for (int i=1;i<=5;i++)
	    {
	      if (mTray2TdigMap[backlegid-1][i-1] == tdigid)
		{ 
		  itray = i; break;
		}
	    }
	  if(itray<1||itray>5) LOG_FATAL<<" Wrong tray ID or mTray2TdigMap missing! backleg = "<<backlegid<<" trayid = "<<itray<<" tdigid ="<<tdigid<<endm;
	  
	  /// Fill MTD raw hit structures
	  MtdRawHit temphit={0};
	  memset(&temphit,0,sizeof(temphit));
	  temphit.fiberid = (UChar_t)ifib;
	  temphit.backlegid  = (UChar_t)backlegid;
	  temphit.tdc     = timeinbin;
	  /// global channel number here, 
	  if (mYear<12)
	    temphit.globaltdcchan = (UChar_t)(tdcChan2globalStrip11(tdigid,tdcid,tdcchan,backlegid)); 
	  else
	    temphit.globaltdcchan = (UChar_t)(tdcChan2globalStrip(itray, tdigid,tdcid,tdcchan)); 

	  if(mYear>=14 && backlegid==7)
	    {
	      Int_t channel = (Int_t) temphit.globaltdcchan;
	      Int_t module = (channel-1)/24 + 1;
	      if(module==5)
		{
		  Int_t cell = (channel-1)%24;
		  if(cell>=0 && cell<=5) cell = 5 - cell;
		  else if (cell>=6 && cell<=11) cell = 17 - cell;
		  else if (cell>=12 && cell<=17) cell = 29 - cell;
		  else if (cell>=18 && cell<=23) cell = 41 - cell;
		  channel = cell + 1 + (module-1)*24;
		  temphit.globaltdcchan = (UChar_t) channel;
		}
	      
	    }

	  temphit.dataword      = dataword;
	  
	  if(edgeid == 4) 
	    {     /// leading edge data
	      MtdLeadingHits.push_back(temphit);
	    } 
	  else if (edgeid==5)
	    {     /// trailing edge data
	      MtdTrailingHits.push_back(temphit);
	    }  
	  else 
	    {
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
Int_t StMtdHitMaker::tdcChan2globalStrip(int itray, int tdigBoardId, int tdcId, int tdcChan) 
{
  
  /// Make sure this mapping is not called for older Runs
  if (mYear<12)
    {
      LOG_WARN << "calling Run12++ tdc mapping for Run" << mYear << ". Not good." << endm;
    }
  
  Int_t globalStripId=-1;
  int Hnum=tdcId%4+1;
  int globalTdcChan=Hnum*10+tdcChan;
  for(int i=0;i<24;i++)
    {
      if(mtdStrip[i]==globalTdcChan) {globalStripId=i+1;break;}
    }
  if(tdigBoardId>3)
    globalStripId = (globalStripId>12)? globalStripId-12:globalStripId+12;

  /// offset stripId by trayId
  globalStripId += (itray-1)*24;
  
  return globalStripId;
}


//____________________________________________
/*!
 * Map TDC channel to a global strip coordinate (prior to Run 12)
 */
Int_t StMtdHitMaker::tdcChan2globalStrip11(int tdigBoardId,int tdcId,int tdcChan,int backlegId) 
{
  
  /// This function is only useful before Run 12.
  if (mYear>11) 
    {
      LOG_WARN << "calling pre-Run12 tdc mapping for Run" << mYear << ". Not good." << endm; 
    }
  
  /// 
  Int_t globalStripId=-1;
  if(backlegId==26)
    {
      if (tdcId>3) tdcId=tdcId-4; //scale to H#
      int globalTdcChan=(tdcId+1)*10+tdcChan;
      for(int i=0;i<24;i++)
	{
	  if(mtdStrip[i]==globalTdcChan) {globalStripId=i+1; break;}
	}
    }

  if(backlegId==1)
    {
      int globalTdcChan=(tdcId+1)*10+tdcChan;
      int mtdStripRun11[18]=  {34,22,10,37,27,17, 32,20,30,24,11,31, 33,23,16,36,26,15};
      for(int i=0;i<18;i++)
	{
	  if(mtdStripRun11[i]==globalTdcChan) {globalStripId=i+1;break;}
	}
    }
  
  return globalStripId;
}


//____________________________________________
/*!
 * Fill the data from DAQ into MtdRawHit Collection in StEvent
 */
void StMtdHitMaker::fillMtdRawHitCollection() 
{
  /// MtdRawdata collection.
  if(!mUseMuDst)
    {
      for (unsigned int i=0;i<MtdLeadingHits.size();i++)
	{  ///  Leading Edge
	  char           flag   = (+1)*(MtdLeadingHits[i].fiberid+1);
	  unsigned char  backlegid = MtdLeadingHits[i].backlegid;
	  unsigned char  chn    = MtdLeadingHits[i].globaltdcchan;
	  unsigned int   tdc    = MtdLeadingHits[i].tdc;
	  LOG_DEBUG << " mtd raw hit LE flag:" << (short)flag << " backlegid:"<< (short)backlegid << " chn:" << (short)chn << " tdc:"<< tdc << endm; 
	  mMtdCollection->addRawHit(new StMtdRawHit(flag,backlegid,chn,tdc));
	}
      
      for (unsigned int i=0;i<MtdTrailingHits.size();i++)
	{  ///  Trailing Edge
	  char           flag   = (-1)*(MtdTrailingHits[i].fiberid+1);
	  unsigned char  backlegid = MtdTrailingHits[i].backlegid;
	  unsigned char  chn    = MtdTrailingHits[i].globaltdcchan;
	  unsigned int   tdc    = MtdTrailingHits[i].tdc;
	  LOG_DEBUG << " mtd raw hit TE flag:" << (short)flag << " backlegid:"<< (short)backlegid << " chn:" << (short)chn << " tdc:"<< tdc << endm; 
	  mMtdCollection->addRawHit(new StMtdRawHit(flag,backlegid,chn,tdc));
	} 
    }
  else
    {
      MtdLeadingHits.clear();
      MtdTrailingHits.clear();
      StSPtrVecMtdRawHit& mtdRawHitVec = mMtdCollection->mtdRawHits();
      for(unsigned int i=0;i<mtdRawHitVec.size();i++)
	{
	  StMtdRawHit* aRawHit=mtdRawHitVec[i];
	  MtdRawHit rawHit;
	  rawHit.tdc=aRawHit->tdc();
	  rawHit.backlegid=aRawHit->backleg();
	  rawHit.fiberid=aRawHit->fiberId();
	  rawHit.globaltdcchan=aRawHit->channel();
	  if(aRawHit->flag()>0)
	    {
	      MtdLeadingHits.push_back(rawHit);
	    }
	  else
	    {
	      MtdTrailingHits.push_back(rawHit);
	    }
	}
    }
  
  fillMtdSingleHits();
}


//____________________________________________
/*!
 * Fill the data into MtdSingleHitVec in StEvent
 */
void StMtdHitMaker::fillMtdSingleHits() 
{
  for(int i=0;i<gMtdNModulesAll;i++) mSingleHitVec[i].clear();
  for(UInt_t i=0;i<MtdLeadingHits.size();i++)
    {
      unsigned char  ifiber    = MtdLeadingHits[i].fiberid;
      int  ibackleg = MtdLeadingHits[i].backlegid;
      unsigned char  chn    = MtdLeadingHits[i].globaltdcchan;
      unsigned int   tdc    = MtdLeadingHits[i].tdc;
      int itray = (chn-1)/24+1;
      int ichan = (chn-1)%24;
      if (ibackleg>gMtdNBacklegs || itray<=0 || itray>gMtdNModules || ichan<0 || ichan>=gMtdNChannels || ifiber>mNFIBER ) 
	{
	  LOG_FATAL << " StMtdHitMaker::fillMtdSingleHits() "
		    << ": ibackleg=" << ibackleg 
		    << ": itray=" << itray 
		    << ": ichan=" << ichan 
		    << ": ifiber=" << ifiber*1
		    << endm;
	  continue;
	}
      bool iexist = kFALSE;
      int igtray = (ibackleg-1)*gMtdNModules+itray;
      for(size_t ii=0; ii<mSingleHitVec[igtray-1].size(); ii++) 
	{
	  if(ibackleg==mSingleHitVec[igtray-1][ii].backleg&&itray==mSingleHitVec[igtray-1][ii].tray && ichan==mSingleHitVec[igtray-1][ii].channel) {
	    iexist = kTRUE;
	    break;
	  }
	}
      if(iexist) continue;
      MTDSingleHit aSingleHit;
      aSingleHit.fiberId = ifiber;
      aSingleHit.backleg = ibackleg;
      aSingleHit.tray 	 = itray;
      aSingleHit.channel = ichan;
      aSingleHit.leadingEdgeTime.push_back(tdc);
      for(size_t j=i+1;j<MtdLeadingHits.size();j++) 
	{
	  unsigned char  jchn    = MtdLeadingHits[j].globaltdcchan;
	  int jbackleg = MtdLeadingHits[j].backlegid;
	  int jtray = (jchn-1)/24+1; 
	  int jchan = (jchn-1)%24; 
	  int jtdc  = MtdLeadingHits[j].tdc; 
	  if(jbackleg == ibackleg 
	     && jtray == itray
	     && jchan == ichan
	     && jtdc)
	    {
	      aSingleHit.leadingEdgeTime.push_back(jtdc);
	    }
	}
      for(size_t j=0;j<MtdTrailingHits.size();j++) 
	{
	  unsigned char  jchn    = MtdTrailingHits[j].globaltdcchan;
	  int jbackleg = MtdTrailingHits[j].backlegid;
	  int jtray = (jchn-1)/24+1; 
	  int jchan = (jchn-1)%24; 
	  int jtdc  = MtdTrailingHits[j].tdc; 
	  if(jbackleg == ibackleg 
	     && jtray == itray
	     && jchan == ichan
	     && jtdc)
	    {
	      aSingleHit.trailingEdgeTime.push_back(jtdc);
	    }
	}
      if(aSingleHit.trailingEdgeTime.size()) mSingleHitVec[igtray-1].push_back(aSingleHit);
    }

  //debug
  if(Debug())
    {
      for(int i=0;i<gMtdNModulesAll;i++)
	{
	  for(size_t m=0;m<mSingleHitVec[i].size();m++)
	    {
	      LOG_INFO<<" idx backleg = "<<i/5+1<<" tray = "<< i%5+1 <<" channel = "<<mSingleHitVec[i][m].channel<<endm;
	      LOG_INFO<<" vec backleg = "<<mSingleHitVec[i][m].backleg<<" tray = "<< mSingleHitVec[i][m].tray <<" channel = "<<mSingleHitVec[i][m].channel<<endm;
	      LOG_INFO<<" leading tdcs = " <<endm;
	      for(size_t j=0;j<mSingleHitVec[i][m].leadingEdgeTime.size();j++) 
		{
		  LOG_INFO << " " << mSingleHitVec[i][m].leadingEdgeTime[j] << endm;
		}
	      LOG_INFO<<" trailing tdcs = " <<endm;
	      for(size_t j=0;j<mSingleHitVec[i][m].trailingEdgeTime.size();j++) 
		{
		  LOG_INFO << " " << mSingleHitVec[i][m].trailingEdgeTime[j] <<endm;
		}
	      
	    }
	}
    }
}

//____________________________________________
IntVec StMtdHitMaker::GetValidTrays() 
{
  IntVec gTrayVec;
  for(int i=0;i<gMtdNModulesAll;i++)
    {
      if(mSingleHitVec[i].size()>0)
	{
	  gTrayVec.push_back(i+1);
	}
    }
  return gTrayVec;
}


//____________________________________________
IntVec StMtdHitMaker::GetValidChannel(int backleg, int tray, int &fiber) 
{
  IntVec chanVec;
  int igtray = (backleg-1)*gMtdNModules+tray;
  if(igtray>gMtdNModulesAll) return chanVec;

  for(size_t i=0 ; i<mSingleHitVec[igtray-1].size() ; i++) 
    {
      if( mSingleHitVec[igtray-1][i].tray == tray
	  && mSingleHitVec[igtray-1][i].backleg == backleg )
	{
	  chanVec.push_back(mSingleHitVec[igtray-1][i].channel);
	  fiber = mSingleHitVec[igtray-1][i].fiberId;
	}
    }
  return chanVec;
}


//____________________________________________
UIntVec StMtdHitMaker::GetLeadingTdc(int backleg, int tray, int channel)
{
  UIntVec leTdc;
  int igtray = (backleg-1)*gMtdNModules+tray;
  for(size_t i=0 ; i<mSingleHitVec[igtray-1].size() ; i++) 
    {
      if(mSingleHitVec[igtray-1][i].backleg!=backleg) continue;
      if(mSingleHitVec[igtray-1][i].tray!=tray) continue;
      if(mSingleHitVec[igtray-1][i].channel!=channel) continue;
      for(size_t j=0; j<mSingleHitVec[igtray-1][i].leadingEdgeTime.size(); j++) 
	{
	  leTdc.push_back(mSingleHitVec[igtray-1][i].leadingEdgeTime[j]);
	}
    }
  return leTdc;
}


//____________________________________________
UIntVec StMtdHitMaker::GetTrailingTdc(int backleg, int tray, int channel)
{
  UIntVec teTdc;
  int igtray = (backleg-1)*gMtdNModules+tray;
  for(size_t i=0 ; i<mSingleHitVec[igtray-1].size() ; i++) 
    {
      if(mSingleHitVec[igtray-1][i].backleg!=backleg) continue;
      if(mSingleHitVec[igtray-1][i].tray!=tray) continue;
      if(mSingleHitVec[igtray-1][i].channel!=channel) continue;
      for(size_t j=0; j<mSingleHitVec[igtray-1][i].trailingEdgeTime.size(); j++) 
	{
	  teTdc.push_back(mSingleHitVec[igtray-1][i].trailingEdgeTime[j]);
	}
    }
  return teTdc;
}


//____________________________________________
/*!
 * Fill the data into MtdHeader in StEvent
 */
void StMtdHitMaker::fillMtdHeader() 
{
  /// fill the MTD header
  int shouldHaveRejectEvent = -1;
  unsigned int tpcSectorMask = 0;
  if(mUseMuDst)
    {
      StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
      StMuDst* mMuDst=mMuDstMaker->muDst();
      StMuMtdCollection* muMtdCollection = mMuDst->MtdCollection();
      StMuMtdHeader* muMtdHeader = NULL;     
      if(muMtdCollection)
	muMtdHeader = muMtdCollection->mtdHeader();
      else
	muMtdHeader = mMuDst->mtdHeader();
      
      shouldHaveRejectEvent = muMtdHeader->shouldHaveRejectEvent();
      tpcSectorMask = muMtdHeader->tpcSectorMask();
    }
  else
    {
      TDataSet* ds = FindDataSet("MtdShouldHaveRejectEvent");
      shouldHaveRejectEvent = (ds ? atoi(ds->GetTitle()) : -1);
      ds = FindDataSet("TpcSectorsByMtd");
      tpcSectorMask = (ds ? (unsigned int) atoi(ds->GetTitle()) : 0U);
    }

  StMtdHeader *mtdHeader = new StMtdHeader();
  for(int i=0;i<nTHUB;i++)
    {
      mtdHeader->setTriggerTime(mTriggerTimeStamp[i], i);
      LOG_DEBUG << "Trigger Time Stamp "<< i+1 <<": " << (unsigned int)mTriggerTimeStamp[i] << endm;
    }
  mtdHeader->setShouldHaveRejectEvent(shouldHaveRejectEvent);
  mtdHeader->setTpcSectorMask(tpcSectorMask);

  mMtdCollection->setHeader(mtdHeader);
}


//____________________________________________
/*!
 * Fill the data from MtdRawHit into MtdHit Collection in StEvent
 */
void StMtdHitMaker::fillMtdHitCollection()
{
  vector<MTDOneSideHit> mOneSideHits;
  IntVec validtray = GetValidTrays();
  LOG_DEBUG << " Number of fired trays " << validtray.size() <<endm;
  for(size_t i=0;i<validtray.size();i++)
    {
      int igtray = validtray[i];
      int backleg = (igtray-1)/gMtdNModules+1;
      int tray = (igtray-1)%gMtdNModules+1;
      int fiber = -1;
      IntVec validchan = GetValidChannel(backleg,tray,fiber);
      LOG_DEBUG << "fired backleg " << backleg << " tray " << tray << endm;
      LOG_DEBUG <<" total channels "<<validchan.size() <<endm;
      for(size_t iv=0;iv<validchan.size();iv++)
	{
	  UIntVec leTdc = GetLeadingTdc(backleg, tray, validchan[iv]);
	  UIntVec teTdc = GetTrailingTdc(backleg, tray, validchan[iv]);
	  
	  LOG_DEBUG <<" leTdc.size():"<<leTdc.size()<<" teTdc.size():"<<teTdc.size()<<endl;
	  if(!leTdc.size() || !teTdc.size()) continue;
	  int chan = validchan[iv]; // global tdc chan (0-23)
	  int channel = chan;
	  int tdigboardid = getTdigBoardId(backleg,tray,chan);
	  int ilocalchan = getLocalTdcChan(backleg,tray,chan);
	  unsigned int tmptdc = leTdc[0];
	  int bin = tmptdc&0x3ff;
	  double tmptdc_f = tmptdc+mINLCorr->getTdigINLCorr(tdigboardid,ilocalchan,bin);
	  double letime = tmptdc_f*VHRBIN2PS / 1000.;
	  
	  tmptdc = teTdc[0];
	  bin = tmptdc&0x3ff;
	  tmptdc_f = tmptdc+mINLCorr->getTdigINLCorr(tdigboardid,ilocalchan,bin);
	  double tetime = tmptdc_f*VHRBIN2PS / 1000.;
	  
	  MTDOneSideHit tmpHit;
	  tmpHit.fiberId  = fiber;
	  tmpHit.backleg  = backleg;
	  tmpHit.tray	  = tray;
	  tmpHit.channel  = channel;
	  tmpHit.leadingEdgeTime  = letime;
	  tmpHit.trailingEdgeTime = tetime;
	  mOneSideHits.push_back(tmpHit);
	} // end channel
    } // end tray
  
  int nMtdHits = 0;
  int nHits = mOneSideHits.size();
  LOG_DEBUG <<" one side hits # = "<<nHits<<endm;
  for(int i=0;i<nHits;i++)
    {
      int iBackLeg = mOneSideHits[i].backleg;
      int iTray	   = mOneSideHits[i].tray;
      int iCell	   = mOneSideHits[i].channel;
      int iFiber   = mOneSideHits[i].fiberId;
      float iLeadingEdgeTime  = mOneSideHits[i].leadingEdgeTime;
      float iTrailingEdgeTime = mOneSideHits[i].trailingEdgeTime;
      for(int j=i+1;j<nHits;j++)
	{
	  int jBackLeg = mOneSideHits[j].backleg;
	  int jTray    = mOneSideHits[j].tray;
	  int jCell    = mOneSideHits[j].channel;
	  int jFiber   = mOneSideHits[j].fiberId; 
	  float jLeadingEdgeTime  = mOneSideHits[j].leadingEdgeTime;
	  float jTrailingEdgeTime = mOneSideHits[j].trailingEdgeTime;
	  if(abs(iCell-jCell)!=12) continue;
	  if(iBackLeg != jBackLeg) continue;
	  if(iTray  != jTray)      continue;
	  if(iFiber != jFiber)     continue;
	  UChar_t mBackLeg = iBackLeg;
	  UChar_t mModule  = iTray;
	  UChar_t mCell    = 99;
	  pair<Double_t,Double_t>  mLeadingEdgeTime;
	  pair<Double_t,Double_t>  mTrailingEdgeTime;
	  
	  if(iCell<12)
	    { 
	      mCell 		= iCell;
	      mLeadingEdgeTime.first  = iLeadingEdgeTime;//west 
	      mLeadingEdgeTime.second = jLeadingEdgeTime;//east 
	      mTrailingEdgeTime.first  = iTrailingEdgeTime;//west 
	      mTrailingEdgeTime.second = jTrailingEdgeTime;//east 
	    }
	  else
	    {
	      mCell 		= jCell;
	      mLeadingEdgeTime.first  = jLeadingEdgeTime;//west 
	      mLeadingEdgeTime.second = iLeadingEdgeTime;//east 
	      mTrailingEdgeTime.first  = jTrailingEdgeTime;//west 
	      mTrailingEdgeTime.second = iTrailingEdgeTime;//east 
	    }

	  if(mTriggerWndSelection)
	    {
	      // trigger time window cuts
	      float timeDiff = (mLeadingEdgeTime.first+mLeadingEdgeTime.second)/2 - 25.*(mTriggerTimeStamp[iFiber] & 0xfff);
	      while(timeDiff<0) timeDiff += 51200;
	      int igtray = (iBackLeg-1)*gMtdNModules+iTray;
	      if(timeDiff<mTriggerTimeWindow[igtray-1][0] || timeDiff>mTriggerTimeWindow[igtray-1][1]) continue;
	    }

	  StMtdHit* aHit= new StMtdHit();
	  aHit->setBackleg(mBackLeg);
	  aHit->setModule(mModule);
	  aHit->setCell(mCell);
	  aHit->setLeadingEdgeTime(mLeadingEdgeTime);
	  aHit->setTrailingEdgeTime(mTrailingEdgeTime);
	  mMtdCollection->addHit(aHit);
	  nMtdHits++;
	  LOG_DEBUG << " backleg " << mBackLeg*1 
		    << " module "<< mModule*1
		    << " cell " <<mCell*1 
		    << " leading.first "<<mLeadingEdgeTime.first
		    << " leading.second "<<mLeadingEdgeTime.second
		    << " trailing.first "<<mTrailingEdgeTime.first
		    << " trailing.second "<<mTrailingEdgeTime.second<<endm;
	  
	}
    }
  LOG_DEBUG << " matched hits " << nMtdHits <<endm;
}

//_________________________________________________________________________
/*!
 * Fill and store MTD Collections in StEvent. Create MtdCollection if necessary
 */
void StMtdHitMaker::fillStEvent() 
{
  LOG_DEBUG << "fillStEvent() Starting..." << endm;
  
  /// make sure we have a mtdcollection
  if(!mMtdCollection)
    {
      LOG_WARN << "No MtdCollection ... creating an empty one in StEvent" << endm;
      mMtdCollection = new StMtdCollection();
      if(!mUseMuDst)
	mStEvent->setMtdCollection(mMtdCollection);
    }
  
  if(mUseMuDst)
    {
      StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
      StMuDst* mMuDst=mMuDstMaker->muDst();
      mMuDst->setMtdArray(mMtdCollection);
    }
	  
  //
  StMtdCollection* mtdCollection = mMtdCollection;
  if(mtdCollection)
    {
      if(mtdCollection->rawHitsPresent())
	{
	  StSPtrVecMtdRawHit& rawMtdVec = mtdCollection->mtdRawHits();

	  if(!mUseMuDst || Debug())
	    {
	      LOG_INFO << rawMtdVec.size() << " MTD raw hits in event ..." << endm;
	    }

	  if(Debug()) 
	    {
	      for(size_t i=0;i<rawMtdVec.size();i++) 
		{
		  LOG_DEBUG << (*rawMtdVec[i]) << endm;
		}
	    }
	}
      else 
	{
	  LOG_DEBUG << "No MtdRawHitCollection" << endm;
	}

      if(mtdCollection->hitsPresent())
	{
	  StSPtrVecMtdHit& mtdVec = mtdCollection->mtdHits();  

	  if(!mUseMuDst || Debug())
	    {
	      LOG_INFO << mtdVec.size() << " MTD hits in event ..." << endm;
	    }

	  if(Debug()) 
	    {
	      for(size_t i=0;i<mtdVec.size();i++)
		{
		  LOG_DEBUG << (*mtdVec[i]) << endm; 
		}
	    }  
	}    
      else {
	LOG_DEBUG << "No MtdHitCollection" << endm;
      }    
    }
  else 
    {
      LOG_WARN << "No MtdCollection" << endm;
      LOG_WARN << "No MtdRawHitCollection" << endm;
      LOG_WARN << "No MtdHitCollection" << endm;
    }
}


//____________________________________________
Int_t StMtdHitMaker::getTdigBoardId(Int_t backlegid, Int_t tray, Int_t chn) 
{
  Int_t tdigboardid = mTdigId[mTrayId[backlegid-1][tray-1]-200];
  return tdigboardid;
}


//____________________________________________
Int_t StMtdHitMaker::getLocalTdcChan(Int_t backlegid, Int_t tray, Int_t chn) 
{
  int   channel = chn;
  int   itdigid = mTray2TdigMap[backlegid-1][tray-1]; 
  if(itdigid>3) channel = (channel>11)?channel-12:channel+12;
  
  int    iglobalChan = mtdStrip[channel];
  int    ilocalChan = (iglobalChan/10-1)*8+iglobalChan%10;
  
  return ilocalChan;
}

//
// $Id: StMtdHitMaker.cxx,v 1.25 2016/07/27 15:31:15 marr Exp $
// $Log: StMtdHitMaker.cxx,v $
// Revision 1.25  2016/07/27 15:31:15  marr
// Fix coverity check: check the range of backlegid
//
// Revision 1.24  2015/05/01 20:04:33  marr
// Use AddData() to pass the information from event filtering stage
//
// Revision 1.23  2015/04/25 03:04:05  marr
// Fill the two new data members mShouldHaveRejectEvent and mTpcSectorMask in
// MTD header
//
// Revision 1.22  2015/01/22 22:10:58  marr
// Fix the reserved ribbon cable in backleg 7 module 5 since 2014 run. This is
// not fixed on the hardware level in 2015 run.
//
// Revision 1.21  2015/01/06 20:37:36  marr
// 1. Add an option to load trigger time window cuts from local files for cosmic ray data.
// This is motivated by the fact that the cuts are different for cosmic ray and collision data.
// 2. The time of a MTD hit is calcualted as: (mLeadingEdgeTime.first+mLeadingEdgeTime.second)/2
// 3. Add the scheme to swap backlegs 25 & 26 for the second part of Run13 data when running
// MuDst in afterburner mode. Note that the scheme is different for the first part of Run13 data.
// Different schemes can be select using function setSwapBacklegInRun13(Int_t swap).
// 4. Clean up in header file
//
// Revision 1.20  2014/09/25 14:36:25  marr
// Do not automatically print out hit information to log file when running on muDst
//
// Revision 1.19  2014/09/19 17:49:33  marr
// 1) Use the constants from StMtdUtil/StMtdConstants.h
// 2) Apply trigger time window cuts
//
// Revision 1.18  2014/08/25 17:06:20  marr
// Read in both MC hits and real hits during embedding
//
// Revision 1.17  2014/07/21 20:17:04  marr
// Add # of MTD hit information to the log file
//
// Revision 1.16  2014/06/24 20:01:24  marr
// Able to process Run12 UU muDst where only the muMtdCollection is stored
//
// Revision 1.15  2014/06/23 17:27:08  marr
// Reformatting
//
// Revision 1.14  2014/05/29 15:31:53  marr
//
// 1. Obtain the Tdig <-> Tray <-> Tdigboard map from the database
// 2. Able to run on muDst in afterburner mode
// 3. Obtain the MTD hits directly from the muDst instead of the hit array
// 4. Automatically swap backlegs 25 & 26 when running bfc chain. A flag with default value of "kFALSE" is added to control the swapping in the afterburner mode when running on muDst.
//
// Revision 1.13  2014/05/27 20:04:27  jeromel
// No change - removed trailing space (as a demo for cvs comments)
//
// Revision 1.12  2014/05/27 20:03:01  jeromel
// Adding cvs tags
//
//
