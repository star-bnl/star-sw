// $Id: StFgtDbMaker.cxx,v 1.3 2011/10/06 19:03:58 balewski Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske

*/

#include "StFgtDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
StFgtDbMaker* gStFgtDbMaker=NULL; 

// accessing Eloss cut off table for slow-simu

#include "tables/St_fgtElosCutoff_Table.h"
#include "St_db_Maker/St_db_Maker.h"

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

  // clear old pointers to DB tables, just in case. Do not delete the data which are owned by StDbMaker
  mLossTab =0;

  LOG_INFO <<  "::RequestDataBase() for Elos cut off ..."<< endl;  
  TDataSet *DB = GetDataBase("Calibrations/fgt/fgtElosCutoff");
  if (!DB) {
        LOG_FATAL<< "ERROR: no table found in db, or malformed local db config" <<endm;
	assert(10==1);
  }
  
  St_fgtElosCutoff *eLosDataset= (St_fgtElosCutoff*) DB->Find("fgtElosCutoff"); assert(eLosDataset);
  Int_t rows = eLosDataset->GetNRows();
  if (rows > 1) {
    LOG_FATAL<< " found INDEXED table with " << rows << " rows, this is fatal, fix DB content" << endm;
    assert(11==1);
  }
  
  if (eLosDataset) {
    mLossTab = eLosDataset->GetTable(); assert(mLossTab);
    LOG_INFO << Form("%s :: fgtElosCutoff table received, comment=%s",GetName(),mLossTab[0].comment)<<endm;
 
#if 0 // disable it later 
    for (int i = 0; i < 10000; i++) { 
            std::cout << i <<"tmp eLoss val: " << mLossTab[0].cutoff[i] << std::endl;
	    if(i>15) break;    }
#endif   
  } else {
    LOG_FATAL<< Form("%s :: fgtElosCutoff table  failed,",GetName())<<endm;
    assert(12==1);
  }
  
 
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


//_____________________________________________________________________________
Float_t StFgtDbMaker::eLossTab(int bin){
  assert(bin>=0);
  assert(bin<10000);
  return mLossTab[0].cutoff[bin];
}

// $Log: StFgtDbMaker.cxx,v $
// Revision 1.3  2011/10/06 19:03:58  balewski
// access Elos table from STAR DB
//
// Revision 1.2  2011/10/04 02:59:34  balewski
// added guestimates of gains, grid absorption, charge sharing
//
