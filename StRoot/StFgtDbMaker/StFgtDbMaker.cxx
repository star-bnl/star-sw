// $Id: StFgtDbMaker.cxx,v 1.2 2011/10/04 02:59:34 balewski Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske

*/

#include "StFgtDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
StFgtDbMaker* gStFgtDbMaker=NULL; 

ClassImp(StFgtDbMaker)
//_____________________________________________________________________________
StFgtDbMaker::StFgtDbMaker(const char *name) : 
  StMaker(name){gStFgtDbMaker = this;}

//_____________________________________________________________________________
StFgtDbMaker::~StFgtDbMaker() {}

//_____________________________________________________________________________
Int_t StFgtDbMaker::Init()
{
  LOG_DEBUG << "StFgtDbMaker::Init()"<<endm;

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFgtDbMaker::InitRun(Int_t runNumber) {
  LOG_INFO << Form("StFgtDbMaker::InitRun(), run=%d",runNumber)<<endm;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StFgtDbMaker::Make()
{
  LOG_DEBUG << "Make" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StFgtDbMaker::Clear(const char*)
{
  LOG_DEBUG << "Clear" << endm;
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StFgtDbMaker::Finish()
{
  LOG_DEBUG << "Finish" << endm;
  return kStOK;
}

// $Log: StFgtDbMaker.cxx,v $
// Revision 1.2  2011/10/04 02:59:34  balewski
// added guestimates of gains, grid absorption, charge sharing
//
