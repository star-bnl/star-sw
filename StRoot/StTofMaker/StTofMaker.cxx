/***************************************************************************
 *
 * $Id: StTofMaker.cxx,v 1.4 2001/09/28 18:40:03 llope Exp $
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
#include "TFile.h"
#include "TH1.h"

int nadchit =   0;
int ntdchit =   0;
int iStrobe =   0;
int tofTag  = -99;	// this is the tof tag...

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
    
//  create histograms?
#ifdef TOFP_HISTOS
 nadchits = new TH1S("nadchit","Crude No. ADC Hits/Event",51,-1.,50.);
 ntdchits = new TH1S("ntdchit","Crude No. TDC Hits/Event",51,-1.,50.);
#endif

  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StTofMaker::Make(){

#ifdef TOFP_DEBUG
  cout << "===============================================================================" << endl;
#endif
  cout << "StTofMaker Make() starting..................................."  << endl;
  int iMake=kStOK;

    mDataCollection = new StTofDataCollection; 
    mTofCollectionPresent  = 0;
    mDataCollectionPresent = 0;

//--- check for existence of collections.................
//
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
      if (m_Mode ==0)              // default setting 
        cout << "StTofMaker No special action required for DataCollection"<<endl;
      else if(m_Mode == 1) {       // Daq-reader
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
        nadchit = ntdchit = iStrobe = 0;
        for (int i=0;i<48;i++){
          unsigned short slatid = mGeomDb->daqToSlatId(i);
          unsigned short rawAdc = mTheTofReader->GetAdcFromSlat(i);
          unsigned short rawTdc = mTheTofReader->GetTdcFromSlat(i);  
                   short rawTc  = 0; if (i<32) { rawTc  = mTheTofReader->GetTc(i); }
          unsigned short rawSc  = 0; if (i<12) { rawSc  = mTheTofReader->GetSc(i); }
//#ifdef TOFP_DEBUG
//       cout << i << ", " << slatid << ", " << rawAdc << ", " << rawTdc 
//                                   << ", " << rawTc  << ", " << rawSc  << endl;
//#endif
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
#ifdef TOFP_HISTOS
       nadchits->Fill(-1.);
       ntdchits->Fill(-1.);
#endif
       } else {
       cout << "StTofMaker ...Saw Roughly " << nadchit << " ADC Hits and " 
                                            << ntdchit << " TDC Hits in TRAY..." << endl;
       tofTag = nadchit;	// set tag to show this was physics event (tofTag>=0)
#ifdef TOFP_HISTOS
       nadchits->Fill(nadchit);
       ntdchits->Fill(ntdchit);
#endif
      }
//---- 
//
    } else {
       cout << "WRONG SetMode (" << m_Mode << ")! "
            << "... should be either 0 (no action) or 1(DAQ)" << endl;
       return iMake;
    }
//--- end m_mode check....

  }  
//--- end section if !mDataCollectionPresent

    cout << "StTofMaker tofTag = " << tofTag << endl;

//--- fill StEvent, clean-up, and return...
//
    if (mEvent) this->fillStEvent();
    delete mDataCollection;
    cout << "StTofMaker Make() finished..................................." << endl;
#ifdef TOFP_DEBUG
    cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =" << endl;
#endif
    return iMake;
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
Int_t StTofMaker::Finish(){

cout << "StTofMaker Finish() starting..." << endl;
#ifdef TOFP_HISTOS
  TFile theFile("my.root","RECREATE","mystudy");
  theFile.cd();
  nadchits->Write();
  ntdchits->Write();
#endif

  return kStOK;
}
