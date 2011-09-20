// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id: StFgtRawMaker.h,v 1.11 2011/09/20 15:53:09 sgliske Exp $
//  $Log: StFgtRawMaker.h,v $
//  Revision 1.11  2011/09/20 15:53:09  sgliske
//  Update so that everything compiles nicely
//  and so that one can execute the macro/simpleTestStandTest.C file
//
//
//
//
#ifndef STAR_StFgtRawMaker_HH
#define STAR_StFgtRawMaker_HH

#include <math.h>

#include <TStopwatch.h>
#include <TString.h>
#include <StMessMgr.h>
#include "StRoot/StChain/StRTSBaseMaker.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"

//#include "StRoot/StEvent/StEventTypes.h"
//#include <StDaqLib/GENERIC/EventReader.hh>
//#include <StDAQMaker/StDAQReader.h>
//#include "StRoot/StFgtUtil/database/StFgtDb.h"

class StFgtRawMaker : public StRTSBaseMaker
{
 protected:
   // StEvent*  mEvent;
  //  StFgtDb*  mDb;
  // fgt_adc_t *mFgtRawData;
  StFgtEvent* mFgtEvent;
  //should be overridden by the test stand class to get the correct number
  //virtual void constructDiscs();

 public: 
  StFgtRawMaker(const Char_t* name="FgtRaw");
  virtual ~StFgtRawMaker();
  //virtual Int_t Init();
  //virtual Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  //virtual Int_t Finish();
  virtual Int_t FillHits();

 private:
  Int_t PrepareEnvironment();
  ClassDef(StFgtRawMaker,1);
};

#endif
