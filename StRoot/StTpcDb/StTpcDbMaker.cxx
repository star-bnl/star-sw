/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.5 1999/12/16 22:00:53 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  
 *This make initializes and controls the TPC interface to the database
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.cxx,v $
 * Revision 1.5  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/

#include "StTpcDbMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StTpcDb.h"
#include "tpc/St_tpg_main_Module.h"
ClassImp(StTpcDbMaker)

//_____________________________________________________________________________
StTpcDbMaker::StTpcDbMaker(const char *name):StMaker(name){
 m_TpcDb = 0;
}
//_____________________________________________________________________________
StTpcDbMaker::~StTpcDbMaker(){
  //delete m_TpcDb;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Init(){

   m_TpcDb = 0;
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

  if (!m_TpcDb) m_TpcDb = new StTpcDb(this);
  Update_tpg_pad_plane();
  Update_tpg_detector();
  return kStOK;
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_pad_plane(){
  if (m_tpg_pad_plane) {
    St_tpg_pad_plane &pp = *m_tpg_pad_plane;
    pp[0].nrow_in = tpcDbInterface()->PadPlaneGeometry()->numberOfInnerRows();
    pp[0].nrow_out = tpcDbInterface()->PadPlaneGeometry()->numberOfOuterRows();
    pp[0].pad_len_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadLength();
    pp[0].pad_len_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadLength();
    pp[0].pad_sep_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadPitch();
    pp[0].pad_sep_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPitch();
    pp[0].pad_wid_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadWidth();
    pp[0].pad_wid_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadWidth();
    pp[0].nsect = tpcDbInterface()->Dimensions()->numberOfSectors();
    for (int i=1;i<=tpcDbInterface()->PadPlaneGeometry()->numberOfRows();i++){
      pp[0].npads[i-1] = tpcDbInterface()->PadPlaneGeometry()->numberOfPadsAtRow(i);
      pp[0].rad[i-1] = tpcDbInterface()->PadPlaneGeometry()->radialDistanceAtRow(i);
    }
  }
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_detector(){
 if (m_tpg_detector) {
     St_tpg_detector &pp = *m_tpg_detector;
     pp[0].nsectors = tpcDbInterface()->Dimensions()->numberOfSectors();
 //    m_tpg_detector->drift_length = 0.
     pp[0].clock_frequency = tpcDbInterface()->Electronics()->samplingFrequency();
 }
}

