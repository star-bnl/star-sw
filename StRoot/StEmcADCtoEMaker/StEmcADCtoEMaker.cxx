
/* Author:  Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: EMC Calibration Maker:
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEmcADCtoEMaker.h"
#include "StEmcHandleInput.h"
#include "StEmcApplyCalib.h"
#include "StChain.h"
#include "TDataSetIter.h"

#include "StGlobals.hh"
#include "StEventTypes.h"

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"
#include <TBrowser.h>

ClassImp(StEmcADCtoEMaker) // macro
   
//-----------------------------------------------------------------

    StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name, int daq)
	: StMaker(name), mDaq(daq)
{}

//-----------------------------------------------------------------

StEmcADCtoEMaker::~StEmcADCtoEMaker() {}

//-----------------------------------------------------------------

Int_t StEmcADCtoEMaker::Init() {
    cout << "StEmcADCtoEMaker::init()" << endl;

    return StMaker::Init();
}
//-----------------------------------------------------------------

Int_t StEmcADCtoEMaker::Make() {
    cout << "EreaderMaker::Make()" << endl;

   //
    mevent = new StEvent();
    //    AddData(mevent->emcCollection());
//for the time being StEvent check is disabled,
// when it will be run in full chain, it will be enabled.
    // Get StEvent pointer
    //    mevent = (StEvent*)GetInputDS("StEvent");
//    if(!mevent)
//     {cout<<" stevent does not exist, must run after StEventMaker**"<<endl;return kStWarn;}


//if StEvent exist , then handle the inputs.

//	if(!mDaq) {
//	    cout << "Sim Mode" << endl;
//	}
//	else {
//	    cout << "DAQ" << endl;
// Look for EmcReader pointer
 
	    mTheEmcData   = GetDataSet("StDAQReader");

	    if(!mTheEmcData) {
		cout << "StEreaderMaker::Maker()\n";
		cout << "\t DataSet: StDAQReader not there\n";
		cout << "\tSkip this event\n" << endl;
		mTheEmcReader = NULL;
	    }
	    else{
            
              cout<<" GOT DaqReader dataset **"<<endl;
 	      mTheDataReader = (StDAQReader*)(mTheEmcData->GetObject());
	       if(!mTheDataReader) {
		cout << "StEreaderMaker::Maker()\n";
		cout << "\tStDAQReader*: not there\n";
		cout << "\tSkip this event\n" << endl;
		mTheEmcReader = NULL;
	       }
	    else{
             cout<<" GOT DaqReader object, look for emc**"<<endl;
	      if (!(mTheDataReader->EMCPresent())) {
		cout << "StEreaderMaker::Maker()\n";
		cout << "\tEMC not in datastream\n";
		cout << "\tSkip this event\n" << endl;
		mTheEmcReader = NULL;
	      }
	      else{
              cout<<"EMC prsent seen from datareader"<<endl;
	    mTheEmcReader = mTheDataReader->getEMCReader();
   	   }
         }
       }
      if(!mTheEmcReader){cout<<"No EMC READER***, QUIT"<<endl;return kStWarn;}
    //Get DB
  m_calibdb= GetInputDB("Calibrations/emc");
  if(!m_calibdb)
  {

    cout<<" Didn't get calib db** quiting"<<endl;
        return kStWarn;
  }

// you have DB, StEvent and EmcReader pointers, so handle the input now

 StEmcHandleInput * input = new StEmcHandleInput(mevent,mTheEmcReader,m_calibdb);
     if(input->ProcessInput()!=kStOK){
     cout<<" error in input handling, bail out**"<<endl;
     return kStWarn;
    }
    else
   {
   // apply calib now
    StEmcApplyCalib * calib = new StEmcApplyCalib(mevent,m_calibdb);
    if(calib->Calibrate()!= kStOK){   
     cout<<"error in calibrating **"<<endl;
    return kStWarn;
   }
  else
   {cout <<" Calibration done **"<<endl;}
   }

    // for what ?? after that program crash in StDAQReader::readEvent() 
    // when call fEMCReader ->Update() !!!
     /* 
    if(!mDaq) {

	if(mTheEmcReader) {
	    delete mTheEmcReader;
	}
	mTheEmcReader = 0;
    }
     */
    mEmcCollection = mevent->emcCollection();

    return kStOK;
}

Int_t StEmcADCtoEMaker::Finish() {

    return StMaker::Finish();
}

StEmcCollection* 
StEmcADCtoEMaker::getEmcCollection()
{
  mevent->setEmcCollection(0); // may be clean up StEvent ??
  return mEmcCollection;
}

void
StEmcADCtoEMaker::Browse(TBrowser* b)
{
  if(mEmcCollection) b->Add((TObject*)mEmcCollection);
  TDataSet::Browse(b);
}
