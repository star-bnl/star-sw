/***************************************************************************
 *
 * $Id: StTofMaker.cxx,v 1.7 2001/10/09 03:06:38 geurts Exp $
 *
 * Author: W.J. Llope / Wei-Ming Zhang / Frank Geurts
 *
 *		+ StTofDataCollection added
 *		+ ADC and TDC data for pVPD and Ramp now being saved...
 *		+ Confirmed proper fill of DataCollection...
 *		+ TOFP_DEBUG switch added to StTofMaker.h for print statements...
 *		+ Changed or Prettified all print statements (TOFP_DEBUG directive)...
 *		+ Changed histograms filled & saved (TOFP_HISTOS directive)...
 *		- Track extrapolation removed...	(for future version)
 *		- Track matching removed...      	(for future version)
 *		- SlatCollection fill removed...	(for future version)
 *		- HitCollection fill removed... 	(for future version)
 *		- PIDTraits fill removed...     	(for future version)
 *      + Overloaded << operator for dumping data to screen... 
 *		+ Added direct check of StTofCollection entries in fillStEvent()...
 *		+ Added tofTag variable...
 *
 ***************************************************************************
 *
 * Description: TOF offline software: maker to insert TOFp raw data onto DST
 *
 ***************************************************************************
 *
 * $Log: StTofMaker.cxx,v $
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
 *
 **************************************************************************/

#include "StTofMaker.h"
#include <stdlib.h>
#include "StEventTypes.h"
#include "StTofUtil/StTofGeometry.h"
#include "StTofDataCollection.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/TOF/TOF_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "tables/St_TofTag_Table.h"
#include "TFile.h"
#include "TH1.h"

ClassImp(StTofMaker)

//_____________________________________________________________________________

StTofMaker::StTofMaker(const char *name):StMaker(name) { drawinit=kFALSE; }

//_____________________________________________________________________________

StTofMaker::~StTofMaker(){}

//_____________________________________________________________________________

Int_t StTofMaker::Init(){

//fg Will add additional checks on return values later.
  mGeomDb = new StTofGeometry();
  mGeomDb->init();
    
//  create histograms
  nAdcHitHisto = new TH1S("tof_nadchit","Crude No. ADC Hits/Event",51,-1.,50.);
  nTdcHitHisto = new TH1S("tof_ntdchit","Crude No. TDC Hits/Event",51,-1.,50.);

  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StTofMaker::Make(){

#ifdef TOFP_DEBUG
  cout << "===============================================================================" << endl;
#endif
  cout << "StTofMaker Make() starting..................................."  << endl;

  mDataCollection = new StTofDataCollection; 
  mTofCollectionPresent  = 0;
  mDataCollectionPresent = 0;

//--- check for existence of collections.................
//
  tofTag = -99;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (mEvent) {
    cout << "StTofMaker Checking for Tof and TofData collections ..." << endl;
    mTheTofCollection = mEvent->tofCollection();
    if(mTheTofCollection) {
      mTofCollectionPresent = 1;
      cout << " + tofCollection Exists" << endl;
//--
      if(mTheTofCollection->dataPresent()) {
	cout << " + tofDataCollection Exists" << endl;
	mDataCollectionPresent = 1;
      }
      else cout << " - tofDataCollection does not exist" << endl;
//--
    } else {
      cout << " - tofCollection does not exist" << endl;
      cout << " - tofDataCollection does not exist" << endl;
    }
//-- create tofcollection...
    if (!mTheTofCollection){
      cout << "StTofMaker Creating tofCollection in StEvent ... ";
      mTheTofCollection = new StTofCollection();
      mEvent->setTofCollection(mTheTofCollection);
      mTheTofCollection = mEvent->tofCollection();
      if (mTheTofCollection) cout << "OK"<< endl;
      else                   cout << "FAILED" << endl;
    } 	// end !mTheTofCollection check
  }   	// end mEvent check
  else cout << "StTofMaker   No StEvent structures detected ... not good!" << endl;
//
//--- end check for collections....

// check if slatCollection not already available, get data if not
  if(!mDataCollectionPresent) {
    if (m_Mode ==0) {             // default DAQ-reader setting 
      cout << "StTofMaker Will get real data from TOFpDAQ Reader ... " << endl;
      mTheTofData = GetDataSet("StDAQReader");
      if (!mTheTofData) {
	cout << "StTofMaker No StDAQReader dataset. Event skipped" << endl;
	return kStWarn;
      }
      mTheDataReader = (StDAQReader*)(mTheTofData->GetObject()); 
      if (!mTheDataReader) {
	cout << "StTofMaker No StDAQReader object. Event skipped" << endl;
	return kStWarn;
      }
      if (!(mTheDataReader->TOFPresent())) {
	cout << "StTofMaker TOF is not in datastream. Event skipped" << endl;
	return kStWarn;
      }
//
      mTheTofReader = mTheDataReader->getTOFReader();
      if (!mTheTofReader) {
	cout << "StTofMaker Failed to getTofReader()...." << endl;
      }
#ifdef TOFP_DEBUG
      TOF_Reader* MyTest = dynamic_cast<TOF_Reader*>(mTheDataReader->getTOFReader());
      if (MyTest) MyTest->printRawData();
#endif
//
//--- copy daq data to collection. (TOFp and pVPD)
//
      cout << "StTofMaker Unpacking data..." << endl;
      int nadchit=0;
      int ntdchit=0;
      int iStrobe = 0;
      for (int i=0;i<48;i++){
	unsigned short slatid = mGeomDb->daqToSlatId(i);
	unsigned short rawAdc = mTheTofReader->GetAdcFromSlat(i);
	unsigned short rawTdc = mTheTofReader->GetTdcFromSlat(i);  
	short rawTc  = 0;
	if (i<32) rawTc = mTheTofReader->GetTc(i);
	unsigned short rawSc = 0;
	if (i<12) rawSc = mTheTofReader->GetSc(i);
	StTofData *rawTofData = new StTofData(slatid,rawAdc,rawTdc,rawTc,rawSc);
	mDataCollection->push_back(rawTofData);      // local collection
	if(i<41) {
	  if (rawAdc>  50) nadchit++; 
	  if (rawTdc<1600) ntdchit++; 
	}
	if(i>41) {
	  if (rawTdc>1500) iStrobe++; 
	}
      }    
//--- end loop over data words...
//
//--- 
      if (iStrobe>4) {
	cout << "StTofMaker ...This is a Strobe Event... " << iStrobe << endl;
	tofTag = -1;     	// set tag to show this was a strobe event   
	nAdcHitHisto->Fill(-1.);
	nTdcHitHisto->Fill(-1.);
      } else {
	cout << "StTofMaker ...Saw Roughly " << nadchit << " ADC Hits and " 
	     << ntdchit << " TDC Hits in TRAY..." << endl;
	tofTag = nadchit;	// set tag to show this was physics event (tofTag>=0)
	nAdcHitHisto->Fill(nadchit);
	nTdcHitHisto->Fill(ntdchit);
      }
//---- 
//
    }
    else if(m_Mode == 1)        // no action
      cout << "StTofMaker No special action required for DataCollection"<<endl;
    else {
      cout << "WRONG SetMode (" << m_Mode << ")! "
	   << "... should be either 0 (DAQ reader) or 1(no action, SIM)" << endl;
      return kStOK;
    }
//--- end m_mode check....

  }  
//--- end section if !mDataCollectionPresent

  cout << "StTofMaker tofTag = " << tofTag << endl;
  if (mEvent) storeTag();

//--- fill StEvent, clean-up, and return...
//
  if (mEvent) this->fillStEvent();
  delete mDataCollection;
  cout << "StTofMaker Make() finished..................................." << endl;
#ifdef TOFP_DEBUG
  cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =" << endl;
#endif
  return kStOK;
}
              

//_________________________________________________________________________
void StTofMaker::fillStEvent() {

  cout << "StTofMaker fillStEvent() Starting..." << endl;

//--- make sure we have a tofcollection
  if(!mTheTofCollection){
    mTheTofCollection = new StTofCollection();
    mEvent->setTofCollection(mTheTofCollection);
  }

//--- fill mTheTofCollection with ALL RAW DATA 
  if(mDataCollectionPresent != 1) {
    cout << "StTofMaker Size of mDataCollection = " << mDataCollection->size() << endl;
    for(size_t jj = 0; jj < mDataCollection->size(); jj++)
      mTheTofCollection->addData(mDataCollection->getData(jj));
  }

//--- verify existence of tofCollection in StEvent (mEvent) 
//
// also check contents of StTofRaws filled in TofCollection. This kind of check
// has never been done for StTofMCSlat too! (We only verified in local
// collections) Here, we would just make sure that raw data is really in
// TofCollection.  WMZ
//
  cout << "StTofMaker Verifying TOF StEvent data ..." << endl;
  StTofCollection* mmTheTofCollection = mEvent->tofCollection();
  if(mmTheTofCollection){
    cout << " + StEvent tofCollection Exists" << endl;
    if(mmTheTofCollection->dataPresent()) {
      cout << " + StEvent TofDataCollection Exists" << endl;
      StSPtrVecTofData& rawTofVec = mmTheTofCollection->tofData();
#ifdef TOFP_DEBUG
      for (size_t i = 0; i < rawTofVec.size(); i++) {
	StTofData* p = rawTofVec[i];
	cout << *p;
      }
#endif                      
      cout << "   StEvent TofDataCollection has " << rawTofVec.size() << " entries..." << endl;
    }
    else cout << " - StEvent TofDataCollection does not Exist" << endl;
  }
  else {
    cout << " - StEvent tofCollection does not Exist" << endl;
    cout << " - StEvent tofDataCollection does not Exist" << endl;
  }
}

//_____________________________________________________________________________
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
    cout << "StTofMaker tofTag stored" << endl;
  }
  else
    cout << "StTofMaker unable to store tofTag" << endl;

}

//_____________________________________________________________________________
Int_t StTofMaker::Finish(){
  // TFile theFile("tof.root","RECREATE","tofstudy");
  // theFile.cd();
  // nAdcHitHisto->Write();
  // nTdcHitHisto->Write();
  return kStOK;
}
