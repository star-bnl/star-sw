//*-- Author : David Hardtke

//////////////////////////////////////////////////////////////////////////
// This make initializes and controls the TPC interface to the database
//
//////////////////////////////////////////////////////////////////////////

#include "StTpcDbMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTpcDb.h"
ClassImp(StTpcDbMaker)

//_____________________________________________________________________________
StTpcDbMaker::StTpcDbMaker(const char *name):StMaker(name){

}
//_____________________________________________________________________________
StTpcDbMaker::~StTpcDbMaker(){
delete m_TpcDb;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Init(){

// Create tables
   StDbDataSet* temp = (StDbDataSet*)GetInputDS("StarDb");
   StDbDataSet* DB = (StDbDataSet*)temp->Find("StarDb");
   assert(DB);
   m_TpcDb = new StTpcDb(DB);
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________

Int_t StTpcDbMaker::Make(){
 return kStOK;
}

StTpcDb* StTpcDbMaker::tpcDbInterface(){
 return m_TpcDb;
}
