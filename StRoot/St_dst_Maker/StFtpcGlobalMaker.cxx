//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcGlobalMaker class                                            //
//                                                                      //
//  
// 
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "TMath.h"
#include "StFtpcGlobalMaker.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_fill_ftpc_dst_Module.h"
ClassImp(StFtpcGlobalMaker)
  
//_____________________________________________________________________________
  StFtpcGlobalMaker::StFtpcGlobalMaker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
  StFtpcGlobalMaker::~StFtpcGlobalMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcGlobalMaker::Init(){
  // Create tables
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcGlobalMaker::Make(){
  PrintInfo();  
  
  int iMake = kStOK;
  St_DataSet *ftpc_tracks = GetDataSet("ftpc_tracks");
  if (!ftpc_tracks) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): ftpc_tracks is missing" << endm;
     return kStWarn;
  }
  St_fpt_fptrack *fpt_fptrack = 0;
  fpt_fptrack = (St_fpt_fptrack *) ftpc_tracks->Find("fpt_fptrack");
  if (!fpt_fptrack) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): fpt_fptrack is missing" << endm;
     return kStWarn;
  }

  St_DataSet *ftpc_hits   = GetDataSet("ftpc_hits");
  if (!ftpc_hits) {
    gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): ftpc_hits is missing" << endm;
     return kStWarn;
  }
  St_fcl_fppoint *fcl_fppoint = 0;
  fcl_fppoint = (St_fcl_fppoint *) ftpc_hits->Find("fcl_fppoint");
  if (!fcl_fppoint) {
     gMessMgr->Warning() << "StFtpcGlobalMaker::Make(): fcl_fppoint is missing" << endm;
     return kStWarn;
  }

   Int_t No_of_Tracks = 0;
   No_of_Tracks += fpt_fptrack->GetNRows();

   St_dst_track *globtrk=0;
   St_DataSet *match = GetDataSet("match");
   if (match) {
      globtrk = (St_dst_track *) match->Find("globtrk");
      if (globtrk) {
         globtrk->ReAllocate(globtrk->GetNRows() + No_of_Tracks);
      }
   }
   if (!globtrk) {
      globtrk = new St_dst_track("globtrk", No_of_Tracks); 
      AddData(globtrk);
   }

   Int_t No_of_Points = fcl_fppoint->GetNRows();
   St_dst_point *dst_point = new St_dst_point("point",No_of_Points);  
   AddData(dst_point);

   St_dst_dedx *dst_dedx = new St_dst_dedx("dst_dedx",No_of_Tracks); 
   AddData(dst_dedx);

   if(Debug()) gMessMgr->Debug()<<" run_dst: Calling fill_ftpc_dst"<<endm;
    St_DataSet *ftpcpars = GetInputDB("ftpc");
    assert(ftpcpars);
    St_DataSetIter gime(ftpcpars);
    m_fdepar = (St_fde_fdepar *) gime("fdepars/fdepar");
    Int_t iRes = fill_ftpc_dst(fpt_fptrack, fcl_fppoint, m_fdepar, globtrk,
                         dst_point,dst_dedx);
    //             ==========================================================
    if (iRes != kSTAFCV_OK) {
      iMake = kStWarn;
      gMessMgr->Warning() << "Problem on return from FILL_FTPC_DST" << endm;
      if(Debug()) gMessMgr->Debug() << " run_dst: finished calling fill_ftpc_dst" << endm;
    }

  return iMake;
}
//_____________________________________________________________________________


