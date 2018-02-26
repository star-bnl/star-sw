/***************************************************************************
 *
 * $Id: StTofHitMaker.cxx,v 1.7 2018/02/26 23:26:50 smirnovd Exp $
 *
 * Author: Valeri Fine, BNL Feb 2008
 ***************************************************************************
 *
 * Description:  Create the TOF data coming from the RTS_READER
 *
 * Input:  RTS_Reader
 * Output: TOF data
 *
 **************************************************************************/
#include "StTofHitMaker.h"

#include "TError.h"
#include "StEventTypes.h"
#include "StEvent/StTofData.h"
#include "StEvent/StTofRawData.h"
#include "StTofUtil/StTofDataCollection.h"
#include "StTofUtil/StTofRawDataCollection.h"  
#include "StEvent/StTofRawData.h"
#include "StEvent/StEvent.h"
#include "StDAQMaker/StDAQReader.h"

#ifdef NEW_DAQ_READER
#  include "StRtsTable.h"
#  include "DAQ_TOF/daq_tof.h"
#else /* NEW_DAQ_READER */
#  include "RTS/src/EVP_READER/tofReader.h"
#endif /* NEW_DAQ_READER */

#ifdef NEW_DAQ_READER
#ifdef tof
# error TOF if defined elsewhere
#else
# define tof (*fTof)
#endif
#endif /* NEW_DAQ_READER */


//_____________________________________________________________
#ifndef NEW_DAQ_READER
StTofHitMaker::StTofHitMaker(const char *name):StRTSBaseMaker(name)
					       , mStEvent(0),fDaqReader(0), mInitialized(0)
#else /* NEW_DAQ_READER */
StTofHitMaker::StTofHitMaker(const char *name):StRTSBaseMaker("tof",name)
					       , mStEvent(0),fTof(0), mInitialized(0)
#endif /* NEW_DAQ_READER */
{
  LOG_INFO << "StTofHitMaker::ctor"  << endm;
}

//_____________________________________________________________
StTofHitMaker::~StTofHitMaker() 
{ }

//_____________________________________________________________
Int_t StTofHitMaker::InitRun(Int_t runnumber) {
  // prevent a chain from running this Maker for Run9+ data.
  if (runnumber>=10000000) {
    Error (":InitRun"," Wrong BFC configuration for run %d. Use StBTofHitMaker for Run9+ data.", runnumber);
    mInitialized=(1==0);
  } else {
    mInitialized=(1==1);
  }
  return 0;
}

Int_t StTofHitMaker::FinishRun(Int_t runnumber) {
  // re-initialize this to false
  mInitialized=(1==0);
  return 0;
}

//_____________________________________________________________
StTofCollection *StTofHitMaker::GetTofCollection()
{
  /// Get StEvent if any at once
  StTofCollection *tofCollection = 0;
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  LOG_INFO << "StTofHitMaker::Make : StEvent has been retrieved " 
        <<mStEvent<< endm;

  if (mStEvent) {
     tofCollection = mStEvent->tofCollection();

     /// Need to create the hit collection
     if ( !tofCollection )  {
        ///  Save the hit collection to StEvent
        tofCollection = new StTofCollection();
        mStEvent->setTofCollection(tofCollection);
     }
  }
  return tofCollection;
}
#ifndef NEW_DAQ_READER
//_____________________________________________________________
evpReader *StTofHitMaker::InitReader()
{
   /// Init EVP_READER
   if (!fDaqReader) { 
     StDAQReader *daqReader = 0;
     LOG_INFO << "StTofHitMaker::InitReader"  << endm;
     TDataSet *dr = GetDataSet("StDAQReader");
     if(dr) daqReader = (StDAQReader *)(dr->GetObject());

     if(daqReader == NULL) {
       LOG_INFO << "StTofHitMaker::InitRun No daqReader available..." << endm;
     } else {
       fDaqReader = daqReader->getFileReader();
       if(fDaqReader == NULL) {
          LOG_INFO << "StTofHitMaker::InitRun No evpReader available..." << endm;
       } else {
         LOG_INFO << "StTofHitMaker::InitReader: "  << fDaqReader << endm;
       }
     }
   }
   return fDaqReader;
}
#endif /* ! NEW_DAQ_READER */
//_____________________________________________________________
StRtsTable *StTofHitMaker::GetNextRaw() 
{
  if ( ! mInitialized) return NULL;
  /// Query  RTS/tof/raw cluster data from DAQ system
  LOG_INFO  << " StTofHitMaker::GetNextRaw()" << endm;
#ifndef NEW_DAQ_READER

  evpReader *evp = InitReader();
  return  (StRtsTable *)(evp ? tofReader((char *)evp) : 0);

#else /* NEW_DAQ_READER */
  StRtsTable *daqTofTable = GetNextLegacy();
  if (daqTofTable) {
     fTof = (tof_t*)*DaqDta()->begin();
  }
  return daqTofTable;
#endif /* NEW_DAQ_READER */
}
//_____________________________________________________________
Int_t StTofHitMaker::Make()
{
  if ( ! mInitialized) return 0;
   StTofCollection *tofCollection = GetTofCollection();
   LOG_INFO << " getting the tof collection " << tofCollection << endm;
   if (tofCollection) {
      if ( GetNextRaw() ) {
         // u_int ddl[4][10000] ;	/// content of up to 4 fibers
     	   // u_int ddl_words[4] ;	/// the count of words (32bit) for above
        for (int rb =0; rb < 3;rb++) {
           LOG_INFO<<" Rdo = "<<rb<<endm;
           for (unsigned int i = 0; i<tof.ddl_words[rb]; i++ ) {
	     //LOG_INFO<<"TOF:::::: "<<i<<" "<<tof.ddl[rb][i]<<endm;
           }
	   // 
        }

        /// Unpack TOF raw data from daq structure
        int errorType=UnpackTofRawData();                
        if(errorType>0) LOG_INFO<<"TOF_READER::UnPack Year8 Data ERROR!"<<endm;
        fillTofDataCollection();
        fillStEvent();

      }
   }
   return kStOk;
}
//____________________________________________________
/** The unpack function for TOF daq data. 
 * Please refer to the TOF fiber data format document.
 */
//____________________________________________________
Int_t StTofHitMaker::UnpackTofRawData()
{
  /// Initialize raw hits vector.
  TofLeadingHits.clear();
  TofTrailingHits.clear();

  for(int ifib=0;ifib<4;ifib++){     // 4 fibers
    int nword=tof.ddl_words[ifib];
    if(nword <= 0) continue;
    int halftrayid=-99;
    int trayid = -99;
    int triggerword = -99;
    for (int iword=0;iword<nword;iword++) {
      int dataword=tof.ddl[ifib][iword];
      ///  now process data word seperately, get TDC information from data words.
      if( (dataword&0xF0000000)>>28 == 0xD) continue;  /// header tag word
      if( (dataword&0xF0000000)>>28 == 0xE) continue;  /// TDIG separator word
      if( (dataword&0xF0000000)>>28 == 0xA) {  /// header trigger data flag
        ///  do nothing at this moment.
      }            
      if( (dataword&0xF0000000)>>28 == 0x2) {   /// trigger time here.
        triggerword= dataword;
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
      temphit.fiberid=ifib;
      temphit.trayID=trayid;
      int timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  /// time in tdc bin
      temphit.tdc=timeinbin;
      /// global channel number here
      temphit.globaltdcchan=tdcchan + (tdcid%4)*8+tdigid*24+halftrayid*96; /// 0-191 for tray
      temphit.timestamp=dataword;
      temphit.triggertimestamp=triggerword;
      if(edgeid == 4) {     /// leading edge data
        TofLeadingHits.push_back(temphit);
      } else if (edgeid==5){     /// trailing edge data
        TofTrailingHits.push_back(temphit);
      }  else {
        LOG_INFO<<"StTofHitMaker::Unknown TDC data type ! "<<endm;
        continue;
      }
      /// end of unpack all data words.        
    }  /// end loop data words    
    ///
  } /// end loop fibers
  ///
  return -1;
}

void StTofHitMaker::fillTofDataCollection()
{

  StTofCollection *tofCollection = GetTofCollection();

  /// TofRawdata collection.
  for (unsigned int i=0;i<TofLeadingHits.size();i++){  ///  Leading Edge
    unsigned short  flag=1; // 1 - leading; 2 - trailing
    unsigned short  trayid=TofLeadingHits[i].trayID;
    unsigned short  chn=TofLeadingHits[i].globaltdcchan;
    unsigned int    tdc=TofLeadingHits[i].tdc;
    unsigned int    triggertime=TofLeadingHits[i].triggertimestamp;
    tofCollection->addRawData (new StTofRawData(flag,trayid,chn,tdc,triggertime,1));
  }

  for (unsigned int i=0;i<TofTrailingHits.size();i++){  ///  Trailing Edge
    unsigned short  flag=2; // 1 - leading; 2 - trailing
    unsigned short  trayid=TofTrailingHits[i].trayID;
    unsigned short  chn=TofTrailingHits[i].globaltdcchan;
    unsigned int    tdc=TofTrailingHits[i].tdc;
    unsigned int    triggertime=TofTrailingHits[i].triggertimestamp;
    tofCollection->addRawData (new StTofRawData(flag,trayid,chn,tdc,triggertime,1));

  }
  
}
  
//_________________________________________________________________________
/// Fill and store TOF Collections in StEvent. Create TofCollection if necessary
void StTofHitMaker::fillStEvent() {

  LOG_INFO << "StTofHitMaker::fillStEvent() Starting..." << endm;

  /// make sure we have a tofcollection
  if(!tofCollection){
    tofCollection = new StTofCollection();
    mStEvent->setTofCollection(tofCollection);
  }

  ///
  StTofCollection* mtofCollection = mStEvent->tofCollection();
  if(mtofCollection){
    ///LOG_INFO << " + StEvent tofCollection Exists" << endm;
    if(mtofCollection->rawdataPresent()) {
      ///LOG_INFO << " + StEvent TofRawDataCollection Exists" << endm;
      StSPtrVecTofRawData& rawTofVec = mtofCollection->tofRawData();
      LOG_INFO << "   StEvent TofRawDataCollection has " << rawTofVec.size() << " entries..." << endm;
    }
    else LOG_INFO << " - StEvent TofRawDataCollection does not Exist" << endm;
  }
  else {
    LOG_INFO << " - StEvent tofCollection does not Exist" << endm;
    LOG_INFO << " - StEvent tofRawDataCollection does not Exist" << endm;
  }
}

/***************************************************************************
 * Add UnpackTofRawData() to process TOF raw data.
 * Add fillTofDataCollection to fill TOF data in StEvent Tofcollection.
 * Revision 1.7, 02/09/2008, Jing liu
 *
 * $Log: StTofHitMaker.cxx,v $
 * Revision 1.7  2018/02/26 23:26:50  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.6  2018/02/26 23:13:20  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.5  2009/06/09 19:45:36  jeromel
 * Changes for BT#1428
 *
 * Revision 1.4  2009/06/08 17:56:30  geurts
 * prevent a chain from running this maker on Run9+ data
 *
 * Revision 1.3  2008/12/15 21:27:31  fine
 * Prepare the code for the new DAQ_READER from Tonko
 *
 * Revision 1.2  2008/12/02 23:58:44  fine
 * Adjust the inteface to accnt the new base class
 *
 * Revision 1.1  2008/03/11 19:16:26  dongx
 * first release. TOF offline reader for Run8+
 *
 * Revision 1.6  2008/02/01 01:31:07  fine
 * Synch MAker with EVP_READER
 *
 * Revision 1.5  2008/01/30 20:04:35  fine
 * introdcue the test and tofCollection
 *
 * Revision 1.4  2008/01/30 14:01:36  fine
 * Replicate the tofReader.C code
 *
 * Revision 1.3  2008/01/30 04:38:49  fine
 * Add RTS_READER parameters follow the Jeff's EVP_READER impl
 *
 * Revision 1.2  2008/01/29 15:56:48  fine
 * remove the redundant dependency
 *
 * Revision 1.1  2008/01/25 22:30:24  fine
 * Add the base makr for all RTS-based makers and template for the TofHitMaker
 *
 * Revision 1.4  2008/01/12 00:22:01  fine
 * Update the test macro
  */
