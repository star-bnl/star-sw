
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
#include <StMessMgr.h>
#include "TDataSetIter.h"

#include "StEmcUtil/emcDetectorName.h"
#include "StEmcUtil/StEmcGeom.h"

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

St_controlADCtoE *StEmcADCtoEMaker::mControlMaker=0;
controlADCtoE_st *StEmcADCtoEMaker::mControlTable=0;

StEmcADCtoEMaker::StEmcADCtoEMaker(const char *name, int daq)
: StMaker(name), mDaq(daq)
{
  mevent         = 0;
  mEmcCollection = 0;
  // default Control table
  mControlMaker = new St_controlADCtoE("controlADCtoE",1);
  mControlTable = mControlMaker->GetTable();

  mControlTable[0].bemcDeductPedestal = 0;
  mControlTable[0].bsmdDeductPedestal = 0;

  mControlTable[0].bemcCalibration    = 0;
  mControlTable[0].bsmdCalibration    = 0;

  mControlTable[0].messLimit = 100;

  mControlMaker->SetNRows(1);

  gMessMgr->SetLimit("StEmcADCtoEMaker", mControlTable[0].messLimit);

  AddRunco((TDataSet*)mControlMaker);
}

StEmcADCtoEMaker::~StEmcADCtoEMaker() {}

Int_t StEmcADCtoEMaker::Init() 
{
  gMessMgr->Info()<<"StEmcADCtoEMaker::Init() ended";
  return StMaker::Init();
}
//-----------------------------------------------------------------

Int_t StEmcADCtoEMaker::Make() {
  gMessMgr->Info()<<"StEmcADCtoEMaker::Make() started for Event "
                  << GetEventNumber() << endl;

  // This staf must be redesign
  mevent = new StEvent();
  mevent->SetName("StEventTmp"); // 2-oct-2001
  mEmcCollection = 0;
 
  mTheEmcData   = GetDataSet("StDAQReader");

  if(!mTheEmcData) {
    cout << "StEreaderMaker::Maker()\n";
    cout << "\t DataSet: StDAQReader not there\n";
    cout << "\tSkip this event\n" << endl;
    mTheEmcReader = NULL;
  } else{
    cout<<" GOT DaqReader dataset **"<<endl;
    mTheDataReader = (StDAQReader*)(mTheEmcData->GetObject());
    if(!mTheDataReader) {
      cout << "StEreaderMaker::Maker()\n";
      cout << "\tStDAQReader*: not there\n";
      cout << "\tSkip this event\n" << endl;
      mTheEmcReader = NULL;
    } else{
      cout<<" GOT DaqReader object, look for emc**"<<endl;
      if(!(mTheDataReader->EMCPresent())) {
	cout << "StEreaderMaker::Maker()\n";
	cout << "\tEMC not in datastream\n";
	cout << "\tSkip this event\n" << endl;
	mTheEmcReader = NULL;
      } else{
        cout<<"EMC prsent seen from datareader"<<endl;
	mTheEmcReader = mTheDataReader->getEMCReader();
      }
    }
  }
  if(!mTheEmcReader){
    cout<<"No EMC READER***, QUIT"<<endl;
    return kStWarn;
  }

  // Get DB if need 
  mCalibDb = GetInputDB("Calibrations/emc");
  if(!mCalibDb) {
    cout<<" Didn't get calib db** quiting"<<endl;
    return kStWarn;
  }

// you have DB, StEvent and EmcReader pointers, so handle the input now

  StEmcHandleInput * input = new StEmcHandleInput(mevent,mTheEmcReader,mCalibDb);

  if(input->ProcessInput()!=kStOK){
     cout<<" error in input handling, bail out**"<<endl;
     return kStWarn;
  } else {
   // apply calib now
    StEmcApplyCalib * calib = new StEmcApplyCalib(mevent,mCalibDb);
    if(calib->Calibrate()!= kStOK){   
      cout<<"error in calibrating **"<<endl;
      return kStWarn;
    } else cout <<" Calibration done **"<<endl;
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
  return mEmcCollection;
  //  return mevent->emcCollection();
}

void 
StEmcADCtoEMaker::clearStEventStaf() 
{
  mEmcCollection = 0;
}

void
StEmcADCtoEMaker::Browse(TBrowser* b)
{
  if(mevent) b->Add(mevent);
  TDataSet::Browse(b);
  print();
}

void 
StEmcADCtoEMaker::print() 
{ // print number of hits - 1-oct-2001
  if(mEmcCollection==0) {
    printf("EmcColection is empty !!!\n");
    return;
  }

  for(Int_t i=0; i<4; i++){
    //    StEmcGeom *geo = StEmcGeom::getEmcGeom(i+1);
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId); // 1-oct-2001
    if(StEmcDetector* detector=(StEmcDetector*)mEmcCollection->detector(id)){
      printf("Det %s #hits %5i \n", detname[i].Data(), detector->numberOfHits());
    }
  }
}
