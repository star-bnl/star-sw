//*-- Author : David Hardtke

//////////////////////////////////////////////////////////////////////////
// This make initializes and controls the TPC interface to the database
//
//////////////////////////////////////////////////////////////////////////

#include "StTpcDbMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTpcDb.h"
#include "tpc/St_tpg_main_Module.h"
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
// Create Needed Tables:    
   m_tpg_pad_plane = new St_tpg_pad_plane("tpg_pad_plane",1);
   m_tpg_pad_plane->SetNRows(1);
   AddConst(m_tpg_pad_plane);
   m_tpg_detector = new St_tpg_detector("tpg_detector",1);
   m_tpg_detector->SetNRows(1);
   AddConst(m_tpg_detector);
//
   return StMaker::Init();
}
//_____________________________________________________________________________

Int_t StTpcDbMaker::Make(){
  Update_tpg_pad_plane();
  Update_tpg_detector();
  return kStOK;
}

void StTpcDbMaker::Update_tpg_pad_plane(){
 tpg_pad_plane_st *pp = m_tpg_pad_plane->GetTable();
  m_tpg_pad_plane->SetNRows(1);
  pp->nrow_in = tpcDbInterface()->PadPlaneGeometry()->numberOfInnerRows();
  pp->nrow_out = tpcDbInterface()->PadPlaneGeometry()->numberOfOuterRows();
  pp->pad_len_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadLength();
  pp->pad_len_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadLength();
  pp->pad_sep_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadPitch();
  pp->pad_sep_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPitch();
  pp->pad_wid_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadWidth();
  pp->pad_wid_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadWidth();
  pp->nsect = tpcDbInterface()->Dimensions()->numberOfSectors();
  for (int i=1;i<=tpcDbInterface()->PadPlaneGeometry()->numberOfRows();i++){
   pp->npads[i-1] = tpcDbInterface()->PadPlaneGeometry()->numberOfPadsAtRow(i);
   pp->rad[i-1] = tpcDbInterface()->PadPlaneGeometry()->radialDistanceAtRow(i);
  }
}

void StTpcDbMaker::Update_tpg_detector(){
 tpg_detector_st *pp = m_tpg_detector->GetTable();
 m_tpg_detector->SetNRows(1);
 
    pp->nsectors = tpcDbInterface()->Dimensions()->numberOfSectors();
 //    m_tpg_detector->drift_length = 0.
   pp->clock_frequency = tpcDbInterface()->Electronics()->samplingFrequency();
 }

