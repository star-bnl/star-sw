/***************************************************************************
 *
 * $Id: StTofMaker.cxx,v 1.20 2018/02/26 23:26:50 smirnovd Exp $
 *
 * Author: W.J. Llope / Wei-Ming Zhang / Frank Geurts
 *
 ***************************************************************************
 *
 * Description: TOF offline software: maker to insert TOFp raw data onto DST
 *
 **************************************************************************/
//! Time-of-Flight Raw Data Maker
/*! \class StTofMaker
    \author  W.J. Llope, Wei-Ming Zhang, Frank Geurts

    <p>TOF offline software. This maker inserts the TOFp raw data onto the DST.
    It distinguishes between so-called beam and strobe event. If available
    the raw data is unpacked into a tofDataCollection() which is part of the
    StEvent::tofCollection(). Based on a rough estimate of the number of hits
    a tofTag is constructed and stored.</p>

    <p>History:
    <ul>
    <li> StTofDataCollection added </li>
    <li> ADC and TDC data for pVPD and Ramp now being saved </li>
    <li> Confirmed proper fill of DataCollection </li>
    <li> TOFP_DEBUG switch added to StTofMaker.h for print statements  </li>
    <li> Changed or Prettified all print statements (TOFP_DEBUG directive) </li>
    <li> Changed histograms filled & saved (TOFP_HISTOS directive) </li>
    <li> Overloaded << operator for dumping data to screen  </li>
    <li> Added direct check of StTofCollection entries in fillStEvent()  </li>
    <li> Added tofTag variable </li>
    </ul>
    </p>
*/
#include "StTofMaker.h"
#include <stdlib.h>
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "StEvent/StTofData.h"
#include "StEvent/StTofRawData.h"
#include "StTofUtil/StTofGeometry.h"
#include "StTofUtil/StTofDataCollection.h"
#include "StTofUtil/StTofRawDataCollection.h"  //RunV
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/TOF/TOF_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "tables/St_TofTag_Table.h"
#include "TFile.h"
#include "TH1.h"



/// default constructor
StTofMaker::StTofMaker(const char *name):StMaker(name) {
  drawinit=kFALSE;
  mTofGeom = 0;
 }

/// default empty destructor
StTofMaker::~StTofMaker(){/* nope */}



/// Init method, book histograms
Int_t StTofMaker::Init(){
  //  create histograms
  nAdcHitHisto = new TH1S("tof_nadchit","Crude No. ADC Hits/Event",181,-0.5,180.5);
  nTdcHitHisto = new TH1S("tof_ntdchit","Crude No. TDC Hits/Event",185,-0.5,184.5);
  return StMaker::Init();
}


/// InitRun method, (re)initialize TOFp data from STAR dBase
Int_t StTofMaker::InitRun(int runnumber){
  LOG_INFO << "StTofMaker::InitRun  -- initializing TofGeometry --" << endm;
  mTofGeom = new StTofGeometry();
  mTofGeom->init(this);
  return kStOK;
}



/// FinishRun method, clean up TOFp dBase entries
Int_t StTofMaker::FinishRun(int runnumber){
  LOG_INFO << "StTofMaker::FinishRun -- cleaning up TofGeometry --" << endm;
  if (mTofGeom) delete mTofGeom;
  mTofGeom=0;
  return 0;
}



//_________________________________________________________________________
/// Make method, check for collections; create and fill them according to m_Mode
Int_t StTofMaker::Make(){
  // 
  LOG_INFO << "StTofMaker::Make() -- All Your Base Are Belong To Us --"  << endm;
  StTofDataCollection myDataCollection;
  mDataCollection = &myDataCollection; 
  mTofCollectionPresent  = 0;
  mDataCollectionPresent = 0;
  
  //  RunV
  StTofRawDataCollection myRawDataCollection;
  mRawDataCollection = &myRawDataCollection;
  mRawDataCollectionPresent = 0;
  
  //--- check for existence of collections.................
  //
  tofTag = -99;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (mEvent) {
    LOG_INFO << "StTofMaker Checking for Tof and TofData collections ..." << endm;
    mTheTofCollection = mEvent->tofCollection();
    if(mTheTofCollection) {
      mTofCollectionPresent = 1;
      LOG_INFO << " + tofCollection Exists" << endm;
//--
      if(mTheTofCollection->dataPresent()) {
	LOG_INFO << " + tofDataCollection Exists" << endm;
	mDataCollectionPresent = 1;
      } else if(mTheTofCollection->rawdataPresent()) {
	mRawDataCollectionPresent = 1;  //RunV
      }
      else LOG_INFO << " - tofDataCollection & tofRawDataCollection does not exist" << endm;
      //--
    } else {
      LOG_INFO << " - tofCollection does not exist" << endm;
      LOG_INFO << " - tofDataCollection & tofRawDataCollection does not exist" << endm;	 //RunV
    }
    //-- create tofcollection...
    if (!mTheTofCollection){
      LOG_INFO << "StTofMaker Creating tofCollection in StEvent ... ";
      mTheTofCollection = new StTofCollection();
      mEvent->setTofCollection(mTheTofCollection);
      mTheTofCollection = mEvent->tofCollection();
      if (mTheTofCollection) LOG_INFO << "OK"<< endm;
      else                   LOG_INFO << "FAILED" << endm;
    } 	// end !mTheTofCollection check
  }   	// end mEvent check
  else LOG_INFO << "StTofMaker   No StEvent structures detected ... not good!" << endm;
  //
  //--- end check for collections....

  // check if slatCollection not already available, get data if not
  if(!mDataCollectionPresent) {
    if (m_Mode ==0) {             // default DAQ-reader setting 
      LOG_INFO << "StTofMaker Will get real data from TOFpDAQ Reader ... " << endm;
      mTheTofData = GetDataSet("StDAQReader");
      if (!mTheTofData) {
	LOG_INFO << "StTofMaker No StDAQReader dataset. Event skipped" << endm;
	return kStWarn;
      }
      mTheDataReader = (StDAQReader*)(mTheTofData->GetObject()); 
      if (!mTheDataReader) {
	LOG_INFO << "StTofMaker No StDAQReader object. Event skipped" << endm;
	return kStWarn;
      }
      if (!(mTheDataReader->TOFPresent())) {
	LOG_INFO << "StTofMaker TOF is not in datastream. Event skipped" << endm;
	return kStWarn;
      }
      //
      mTheTofReader = mTheDataReader->getTOFReader();
      if (!mTheTofReader) {
	LOG_INFO << "StTofMaker Failed to getTofReader()...." << endm;
	return kStWarn;
      }
      
      
      
#ifdef TOFP_DEBUG
      TOF_Reader* MyTest = dynamic_cast<TOF_Reader*>(mTheDataReader->getTOFReader());
      if (MyTest) MyTest->printRawData();
      if (MyTest->year2Data()) LOG_INFO << "StTofMaker: year2 data - TOFp+pVPD" << endm;
      if (MyTest->year3Data()) LOG_INFO << "StTofMaker: year3 data - TOFp+pVPD+TOFr" << endm;
      if (MyTest->year4Data()) LOG_INFO << "StTofMaker: year4 data - TOFp+pVPD+TOFrprime" << endm;
      if (MyTest->year5Data()) LOG_INFO << "StTofMaker: year5 data - TOFr5+pVPD  " << endm;
#endif
      
      //
      //--- copy daq data to collection. (TOFp and pVPD)
      //
      if (mTheTofReader->year2Data()||mTheTofReader->year3Data()||mTheTofReader->year4Data()){
	int nadchit=0;
	int ntdchit=0;
	int iStrobe = 0;
	
	// set limits on basic event statistics
	int tdcHitMax=1600;
	int pvpdStrobeMin=1500;
	if (mTheTofReader->year3Data()||mTheTofReader->year4Data()){
	  tdcHitMax=880;
	  pvpdStrobeMin=950;
	}
	
	for (int i=0;i<48;i++){
	  unsigned short slatid = mTofGeom->daqToSlatId(i);
	  unsigned short rawAdc = mTheTofReader->GetAdcFromSlat(i);
	  unsigned short rawTdc = mTheTofReader->GetTdcFromSlat(i);  
	  short rawTc  = 0;
	  if (i<32) rawTc = mTheTofReader->GetTc(i);
	  unsigned short rawSc = 0;
	  if (i<12) rawSc = mTheTofReader->GetSc(i);
	  // new StTofData
	  StTofData *rawTofData = new StTofData(slatid,rawAdc,rawTdc,rawTc,rawSc,0,0);
	  mDataCollection->push_back(rawTofData);      // local collection
	  if(i<41) {
	    if (rawAdc>  50) nadchit++; 
	    if (rawTdc<tdcHitMax) ntdchit++; 
	  }
	  if(i>41) {
	    if (rawTdc>pvpdStrobeMin) iStrobe++; 
	  }
	}    
	if (mTheTofReader->year3Data()){
	  for (int i=48;i<132;i++){
	    //arbitrary id, start from at 100.
	    unsigned short id = (100-48)+i;
	    unsigned short rawAdc = mTheTofReader->GetAdc(i);
	    unsigned short rawTdc = 0;
	    if (i<120) rawTdc = mTheTofReader->GetTdc(i);  
	    // new StTofData
	    StTofData *rawTofData = new StTofData(id,rawAdc,rawTdc,0,0,0,0);
	    mDataCollection->push_back(rawTofData);
	    if ((i>59) && (rawAdc>50))    nadchit++;
	    if ((i<120) && (rawTdc<tdcHitMax)) ntdchit++; 
	  }
	}
	
	if (mTheTofReader->year4Data()){
	  for (int i=48;i<184;i++){
	    //arbitrary id, start from at 100.
	    unsigned short id = (100-48)+i;
	    unsigned short rawAdc = 0;
	    if(i<180) {
	      rawAdc = mTheTofReader->GetAdc(i);
	    }
	    
	    unsigned short rawTdc = 0;
	    rawTdc = mTheTofReader->GetTdc(i);
	    // new StTofData
	    StTofData *rawTofData = new StTofData(id,rawAdc,rawTdc,0,0,0,0);
	    mDataCollection->push_back(rawTofData);
	    if ((i>59) && (rawAdc>50))    nadchit++;
	    if ((rawTdc<tdcHitMax)) ntdchit++; 
	  }
	}
		
	//--- end loop over data words...
	//
	//--- 
	if (iStrobe>4) {
	  LOG_INFO << "StTofMaker ... Strobe Event:" << iStrobe << endm;
	  tofTag = -1;     	// set tag to show this was a strobe event   
	  nAdcHitHisto->Fill(-1.);
	  nTdcHitHisto->Fill(-1.);
	} else {
	  LOG_INFO << "StTofMaker ... Beam Event: ~" << nadchit << " ADC and ~" 
	       << ntdchit << " TDC Raw Hits in TRAY" << endm;
	  tofTag = nadchit;	// set tag to show this was physics event (tofTag>=0)
	  nAdcHitHisto->Fill(nadchit);
	  nTdcHitHisto->Fill(ntdchit);
	}
	//---- 
	//
      } // end year2-year4

      //------  RunV Raw Data   Haidong Liu
      if (mTheTofReader->year5Data()){
	for (int i=0;i<(int)mTheTofReader->GetNLeadingHits();i++){  //  Leading Edge
	  unsigned short  flag; // 1 - leading; 2 - trailing
	  unsigned short  chn=mTheTofReader->GetLeadingGlobalTdcChan(i);
	  unsigned int    tdc=mTheTofReader->GetLeadingTdc(i);
	  flag=1;
	  unsigned short  quality = 1;
	  StTofRawData *rawTofData1 = new StTofRawData(flag,chn,tdc,quality);
	  mRawDataCollection->push_back(rawTofData1);    
	}
	for (int i=0;i<(int)mTheTofReader->GetNTrailingHits();i++){  // Trailing Edge
	  unsigned short  flag; // 1 - leading; 2 - trailing
	  unsigned short  chn=mTheTofReader->GetTrailingGlobalTdcChan(i);
	  unsigned int    tdc=mTheTofReader->GetTrailingTdc(i);
	  flag=2;
	  unsigned short  quality = 1;
	  StTofRawData *rawTofData1 = new StTofRawData(flag,chn,tdc,quality);
	  mRawDataCollection->push_back(rawTofData1);   
	}
	for (int i=0;i<TOF_MAX_TDC_CHANNELS;i++){  //TOF_MAX_TDC_CHANNELS=198;  
	  unsigned int rawLdTdc=mTheTofReader->GetLdTdc(i);
	  unsigned int rawTrTdc=mTheTofReader->GetTrTdc(i);
	  StTofData *rawTofData = new StTofData(i,0,0,0,0,rawLdTdc,rawTrTdc);
	  mDataCollection->push_back(rawTofData);
	}
	
      } // end year5

    } else if(m_Mode == 1)        // no action
      LOG_INFO << "StTofMaker No special action required for DataCollection"<<endm;
    else {
      LOG_INFO << "WRONG SetMode (" << m_Mode << ")! "
	   << "... should be either 0 (DAQ reader) or 1(no action, SIM)" << endm;
      return kStOK;
    }
    //--- end m_mode check....  
    
  } // end dataPresent
  
 //--- end section if !mDataCollectionPresent
  
  LOG_INFO << "StTofMaker tofTag = " << tofTag << endm;
  if (mEvent) storeTag();
  
  //--- fill StEvent, clean-up, and return...
  //
  if (mEvent) this->fillStEvent();
  mDataCollection=0;
  LOG_INFO << "StTofMaker::Make() -- see you next event --" << endm;
  
  return kStOK;
}
              

//_________________________________________________________________________
/// Fill and store TOF Collections in StEvent. Create TofCollection if necessary
void StTofMaker::fillStEvent() {

  LOG_INFO << "StTofMaker::fillStEvent() Starting..." << endm;

//--- make sure we have a tofcollection
  if(!mTheTofCollection){
    mTheTofCollection = new StTofCollection();
    mEvent->setTofCollection(mTheTofCollection);
  }

  //--- fill mTheTofCollection with ALL RAW DATA 
  if(mDataCollectionPresent != 1) {
    LOG_INFO << "StTofMaker::fillStEvent()  Size of mDataCollection = " << mDataCollection->size() << endm;
    for(size_t jj = 0; jj < mDataCollection->size(); jj++)
      mTheTofCollection->addData(mDataCollection->getData(jj));
  }
  if(mRawDataCollectionPresent != 1) {
    LOG_INFO << "StTofMaker::fillStEvent()  Size of mRawDataCollection = " << mRawDataCollection->size() << endm;
    for(size_t jj = 0; jj < mRawDataCollection->size(); jj++)
      mTheTofCollection->addRawData(mRawDataCollection->getRawData(jj));
  }
  
  //--- verify existence of tofCollection in StEvent (mEvent) 
  //
  // also check contents of StTofRaws filled in TofCollection. This kind of check
  // has never been done for StTofMCSlat too! (We only verified in local
  // collections) Here, we would just make sure that raw data is really in
  // TofCollection.  WMZ
  //
  LOG_INFO << "StTofMaker::fillStEvent() Verifying TOF StEvent data ..." << endm;
  StTofCollection* mmTheTofCollection = mEvent->tofCollection();
  if(mmTheTofCollection){
    LOG_INFO << " + StEvent tofCollection Exists" << endm;
    if(mmTheTofCollection->dataPresent()) {
      LOG_INFO << " + StEvent TofDataCollection Exists" << endm;
      StSPtrVecTofData& rawTofVec = mmTheTofCollection->tofData();
#ifdef TOFP_DEBUG
      for (size_t i = 0; i < rawTofVec.size(); i++) {
	StTofData* p = rawTofVec[i];
	LOG_INFO << *p;
      }
#endif                      
      LOG_INFO << "   StEvent TofDataCollection has " << rawTofVec.size() << " entries..." << endm;
    }
    else LOG_INFO << " - StEvent TofDataCollection does not Exist" << endm;
  }
  else {
    LOG_INFO << " - StEvent tofCollection does not Exist" << endm;
    LOG_INFO << " - StEvent tofDataCollection does not Exist" << endm;
  }
}

//_____________________________________________________________________________
/// store tofTag in StEvent structure.
void StTofMaker::storeTag(){
  // instantiate new TofTag class
  St_TofTag *tagTable = new St_TofTag("TofTag",1);
  if (!tagTable) return;
  tagTable->SetNRows(1);

  // add table to root .data directory
  AddData(tagTable,".data");
  TofTag_st *pTofTag = tagTable->GetTable();

  if (pTofTag) {
    pTofTag->tofEventType = tofTag;
    LOG_INFO << "StTofMaker::storeTag()  tofTag stored" << endm;
  }
  else
    LOG_INFO << "StTofMaker::storeTag()  unable to store tofTag" << endm;

}

//_____________________________________________________________________________
/// default Finish method (empty)
Int_t StTofMaker::Finish(){
  // nope 
  return kStOK;
}

/***************************************************************************
 *
 * $Log: StTofMaker.cxx,v $
 * Revision 1.20  2018/02/26 23:26:50  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.19  2018/02/26 23:13:20  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.18  2007/04/17 23:00:41  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.17  2005/04/12 17:33:18  dongx
 * update for year 5 new data format. Store into TofRawData from now on.
 *
 * Revision 1.16  2004/09/19 00:09:02  perev
 * Small Walgrind leak fixed
 *
 * Revision 1.15  2004/09/10 22:09:21  perev
 * more defence agains corrupted DAQ data
 *
 * Revision 1.14  2004/01/27 23:17:01  dongx
 *  change for year4 run (pVPD+TOFp+TOFr')
 *  - Additional TOFr' ADC and TDC channels put in
 *  - Add TOTs of TOFr' in
 *
 *
 * Revision 1.12  2003/09/17 19:54:28  geurts
 * zeroed geometry pointer
 *
 * Revision 1.11  2003/08/08 00:20:41  geurts
 * moved local collection code to StTofUtil, changed StTofMaker accordingly
 *
 * Revision 1.10  2003/07/18 18:31:49  perev
 * test for nonexistance of XXXReader added
 *
 * Revision 1.9  2003/02/06 05:02:05  geurts
 * Added TOFr and extra pVPD-ADC channels to the datastream:
 * StTofMaker is now aware of year2 (TOFp+pVPD) and year3 (TOFp+pVPD+TOFr) raw data.
 *
 * Revision 1.8  2002/01/22 06:50:34  geurts
 * modifications for STAR dBase access and doxygenized
 *
 * Revision 1.7  2001/10/09 03:06:38  geurts
 * TofTag introduced
 *
 * Revision 1.6  2001/10/07 19:01:46  geurts
 * change default operation mode to DAQ-reader
 *
 * Revision 1.5  2001/10/05 21:08:40  geurts
 * clean-up histograms and private root file
 *
 * Revision 1.4  2001/09/28 18:40:03  llope
 * first release
 *
 */
