
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
  mEvent         = 0;
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

Int_t 
StEmcADCtoEMaker::Init() 
{
  gMessMgr->Info()<<"StEmcADCtoEMaker::Init() ended";
  return StMaker::Init();
}

Int_t 
StEmcADCtoEMaker::Make()
{
  gMessMgr->Info()<<"StEmcADCtoEMaker::Make() started "
                  << GetEventNumber() << endm;

  mEmcCollection = 0;
  mEvent = (StEvent*)GetInputDS("StEvent");
  if(mEvent) mEmcCollection = mEvent->emcCollection();

  if(mEmcCollection == 0) {
    //
    // No StEvent or emc collection in StEvent
    // Will try to create emc hits from reader
    //
    mTheEmcData  = GetDataSet("StDAQReader");

    if(!mTheEmcData) {
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => DataSet: StDAQReader not there "
                      << "=>Skip this event\n" << endm;
      mTheEmcReader = NULL;
    } else {
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => got DaqReader dataset"
                      << endm;
      mTheDataReader = (StDAQReader*)(mTheEmcData->GetObject());
      if(!mTheDataReader) {
        gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => StDaqReader not there **"
                        << " : Skip this event\n" << endm;
        mTheEmcReader = NULL;
      } else {
        gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => got StDaqReader object"
                        << endm;
        if(!(mTheDataReader->EMCPresent())) {
          gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => Emc reader not there **"
                          << " : Skip this event\n" << endm;
	  mTheEmcReader = NULL;
        } else{
          gMessMgr->Info()<<"StEmcADCtoEMaker::Make() =>  Emc reader is present"
                          << endm;
	  mTheEmcReader = mTheDataReader->getEMCReader();
        }
      }
    }

    if(!mTheEmcReader){
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make() =>  No emc colection and "
		      <<"No EMC reader , QUIT **" << endm;
      return kStWarn;
    } else {
      mEmcCollection = new StEmcCollection;
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => create emc collection for StEvent"
                    <<endm;
    }

  } else {
    gMessMgr->Info()<<"StEmcADCtoEMaker::Make() => get emc collection from StEvent"
                    <<endm;
  }

  // Get DB if need -> very slowly 
  mCalibDb = GetInputDB("Calibrations/emc");
  if(!mCalibDb) {
    gMessMgr->Info()<<"StEmcADCtoEMaker::Make() =>  No calibration DB , quit **"
		    << endm;
    return kStWarn;
  }

  // We have DB, emc hits collection pointer and EmcReader (may be)
  StEmcHandleInput *input = 0;
  if(mTheEmcReader) {
    input = new StEmcHandleInput(mEmcCollection, mTheEmcReader, mCalibDb);
    if(input->processInput()!=kStOK){
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make()=>error in input , bail out**"
		      << endm;
      return kStWarn;
    }
  }

  
  if(mEmcCollection&&(mControlTable[0].bemcCalibration||mControlTable[0].bemcCalibration)){
   // apply calib now
    StEmcApplyCalib * calib = new StEmcApplyCalib(mEmcCollection, mCalibDb);
    if(calib->calibrate()!= kStOK){   
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make()=> error in calibration **"
		      << endm;
      return kStWarn;
    } else {
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make()=> calibration done" << endm;
    }
  } else {
    if(mEmcCollection) 
      gMessMgr->Info()<<"StEmcADCtoEMaker::Make()=> NO calibration "
		      << endm;
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
  return kStOK;
}

Int_t 
StEmcADCtoEMaker::Finish() {
    return StMaker::Finish();
}

StEmcCollection* 
StEmcADCtoEMaker::getEmcCollection()
{
  return mEmcCollection;
  //  return mEvent->emcCollection();
}

void 
StEmcADCtoEMaker::clearStEventStaf() 
{
  mEmcCollection = 0;
}

void
StEmcADCtoEMaker::Browse(TBrowser* b)
{
  if(mEmcCollection) b->Add((TObject*)mEmcCollection);
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

void 
StEmcADCtoEMaker::Clear(Option_t *option)
{// 13-oct
  if(option){};
  //  if(mEmcCollection) {
  //  delete mEmcCollection;
  //  mEmcCollection = 0;
  //}
}
