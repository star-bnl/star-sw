//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0Maker class                                                    //
//                                                                      //
// $Id: StV0Maker.cxx,v 1.11 1999/09/12 23:03:04 fisyak Exp $
// $Log: StV0Maker.cxx,v $
// Revision 1.11  1999/09/12 23:03:04  fisyak
// Move parameters into makers
//
// Revision 1.10  1999/07/17 00:31:25  genevb
// Use StMessMgr
//
// Revision 1.9  1999/07/15 13:57:54  perev
// cleanup
//
// Revision 1.8  1999/07/14 15:48:19  caines
// Correct check on stk_track when eval turned on
//
// Revision 1.7  1999/07/12 23:04:16  fisyak
// Remove glob2
//
// Revision 1.6  1999/07/12 01:49:39  fine
// Clean up
//
// Revision 1.5  1999/07/11 01:55:45  fisyak
// Fix glob->impact
//
// Revision 1.4  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>

#include "TMath.h"
#include "StV0Maker.h"

#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_ev0_am2_Module.h"
#include "global/St_ev0_eval2_Module.h"

ClassImp(StV0Maker)
  
  //_____________________________________________________________________________
  StV0Maker::StV0Maker(const char *name):StMaker(name),
  m_ev0par(0)
{
  m_ev0EvalOn=kFALSE;
}
//_____________________________________________________________________________
StV0Maker::~StV0Maker(){
}
//_____________________________________________________________________________
Int_t StV0Maker::Init(){
  m_ev0par2 = new St_ev0_ev0par2("ev0par2",3);
  {
    ev0_ev0par2_st row;
    memset(&row,0,m_ev0par2->GetRowSize());
  // TPC only cuts
    row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
    row.dca	 =        0.8; // cut on dca between the two tracks ;
    row.dcav0	 =        0.3; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dlen	 =        0.6; // cut on dist. of decay from prim. vertex ;
    row.alpha_max=        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
    row.ptarm_max=        0.3; // Max. value of arm. pt allowed, only first entry used;
    row.dcapnmin=        0.7; // Min. value of tracks at interaction ;
    m_ev0par2->AddAt(&row,0);
    memset(&row,0,m_ev0par2->GetRowSize());
  //SVT only cuts
    row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
    row.dca	 =        0.8; // cut on dca between the two tracks ;
    row.dcav0	 =        0.7; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dlen	 =      10000; // cut on dist. of decay from prim. vertex ;
    row.alpha_max=        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
    row.ptarm_max=        0.3; // Max. value of arm. pt allowed, only first entry used;
    row.dcapnmin=         100; // Min. value of tracks at interaction ;
    m_ev0par2->AddAt(&row,1);
    memset(&row,0,m_ev0par2->GetRowSize());
  // SVT+TPC cuts
    row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
    row.dca	 =        0.8; // cut on dca between the two tracks ;
    row.dcav0	 =        0.3; // cut on dca(impact parameter) of V0 from event vertex ;
    row.dlen	 =        0.6; // cut on dist. of decay from prim. vertex ;
    row.alpha_max=        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
    row.ptarm_max=        0.3; // Max. value of arm. pt allowed, only first entry used;
    row.dcapnmin =        0.7; // Min. value of tracks at interaction ;
    m_ev0par2->AddAt(&row,2);
  }
  AddRunCont(m_ev0par2);
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0Maker::Make(){
  //if(Debug()) gMessMgr->Debug() << "Calling ev0..."<< endm; 
  PrintInfo();   
  
  int iMake = kStOK;
  int iRes = 0;
  
  St_DataSet     *match = GetDataSet("match"); 
  if (!match) {
    gMessMgr->Warning() << " StV0Maker: match is missing" << endm;
    return kStWarn;
  }
  St_DataSetIter matchI(match);         
  St_dst_track   *globtrk  = (St_dst_track *) matchI("globtrk");
  if (!globtrk) {
    gMessMgr->Warning() << " StV0Maker: globtrk is missing" << endm;
    return kStWarn;
  }
  
  St_DataSet     *primary = GetDataSet("primary"); 
  
  if (!primary) {
    gMessMgr->Warning() << " StV0Maker: primary is missing" << endm;
    return kStWarn;
  }
  St_DataSetIter primaryI(primary);
  St_dst_vertex  *vertex   = (St_dst_vertex *) primaryI("vertex");
  if (!vertex) {
    gMessMgr->Warning() << " StV0Maker: vertex is missing" << endm;
    return kStWarn;
  }
  
  St_dst_v0_vertex *dst_v0_vertex  = 0;
  St_ev0_eval      *ev0_eval = 0;
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
  }
  if (! evaltrk)    {evaltrk = new St_tte_eval("evaltrk",1); AddGarb(evaltrk);}
  
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_stk_track   *stk_track   = 0;
  
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
  }
  
  
  dst_vertex_st *vrtx = vertex->GetTable();
  if( vrtx->vtx_id != 1 || vrtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == 1 && vrtx->iflag == 1 ) break;
    }
  }
  if (vrtx->vtx_id == 1 && vrtx->iflag == 1) {
    // ev0
    if(Debug()) gMessMgr->Info() << "Calling ev0..." << endm;
    Int_t v0_limit = globtrk->GetNRows();
    v0_limit = (v0_limit*v0_limit)/6000;
    if (v0_limit < 1000) v0_limit=1000;
    if (! dst_v0_vertex) {
      dst_v0_vertex = new St_dst_v0_vertex("dst_v0_vertex",v0_limit); 
      AddData(dst_v0_vertex);
    }
    vertex->ReAllocate(3*v0_limit);
    St_ev0_track2 *ev0track2 = new St_ev0_track2("ev0_track2",globtrk->GetNRows());
    AddGarb(ev0track2);
    
    vertex->SetNRows(1); 
    
    iRes = ev0_am2(m_ev0par2,globtrk,vertex,dst_v0_vertex,ev0track2);
    //       =========================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if (iRes !=kSTAFCV_OK)
      gMessMgr->Warning() << " Problem on return from EV0 " << endm;
    if(m_ev0EvalOn){   
      //ev0_eval2
      if (tptrack && evaltrk) {
	if (! stk_track)    {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
	ev0_eval = new St_ev0_eval("ev0_eval",20000);
	AddData(ev0_eval);
	St_DataSetIter geant(GetInputDS("geant"));
	St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
	St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("g2t_vertex");
	if(Debug()) gMessMgr->Info() << " Calling ev0_eval2.." << endm;
	Int_t Res_ev0_eval = kSTAFCV_BAD;
	Res_ev0_eval = ev0_eval2(stk_track,tptrack,evaltrk,
				 vertex,dst_v0_vertex,ev0_eval,
				 g2t_track,g2t_vertex);
	
	if (Res_ev0_eval != kSTAFCV_OK) {
	  gMessMgr->Warning() << "Problem on return from ev0eval2" << endm;}
      }
    }     // If ev0 evaluation switched on 
  }  
  return iMake;
}

