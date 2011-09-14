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

#include "StEvent/StEventTypes.h"
#include "StEvent/StEvent.h"
#include <TStopwatch.h>
#include <TString.h>
#include <StDaqLib/GENERIC/EventReader.hh>
#include <StDAQMaker/StDAQReader.h>
#include <StMessMgr.h>
#include "DAQ_FGT/daq_fgt.h"
#include "DAQ_READER/daq_dta.h"
#include "StFgtUtil/geometry/StFgtGeomDefs.h"
#include "StFgtUtil/geometry/StFgtGeom.h"
#include "StChain/StRTSBaseMaker.h"
#include "StFgtUtil/database/StFgtDb.h"
#include "StFgtEvent/StFgtEvent.h"
#include "StChain/StRtsTable.h"

class StFgtRawMaker : public StRTSBaseMaker
{
 protected:
  StEvent*  mEvent;
  //  StFgtDb*  mDb;
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
  virtual Bool_t FillHits();

 private:
  Bool_t PrepareEnvironment();
  ClassDef(StFgtRawMaker,1);
};

#endif
