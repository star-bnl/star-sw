// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id
//  $Log
//
//
//
#ifndef STAR_StFgtRawMaker_HH
#define STAR_StFgtRawMaker_HH

#include <math.h>

#include <StEventTypes.h>
#include <StEvent.h>
#include <TStopwatch.h>
#include <TString.h>
#include <StDaqLib/GENERIC/EventReader.hh>
#include <StDAQMaker/StDAQReader.h>
#include <StMessMgr.h>
#include "DAQ_READER/daq_det.h"
#include "DAQ_FGT/daq_fgt.h"
#include "StFgtGeomDefs.h"
#include "StFgtGeom.h"

class StFgtRawMaker : public StRTSBaseMaker
{

 protected:
  StEvent*  mEvent;
  StFgtDB*  mDb;
  fgt_adc_t *mFgtRawData;
  StFgtEvent* mFgtEvent;
  //should be overridden by the test stand class to get the correct number
  virtual void constructDiscs();

 public: 
  StFgtRawMaker(const char* name="FgtRaw");
  virtual ~StFgtRawMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();



 private:
  Bool_t prepareEnvironment();

  ClassDef(StFgtRawMaker,1)
};

#endif
