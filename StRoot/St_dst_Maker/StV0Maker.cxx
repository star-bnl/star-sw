//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0Maker class                                                    //
//                                                                      //
// $Id: StV0Maker.cxx,v 1.5 1999/07/11 01:55:45 fisyak Exp $
// $Log: StV0Maker.cxx,v $
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
  St_DataSet *globalParams = GetInputDB("params/global");
  assert(globalParams);
  St_DataSetIter params(globalParams);
  
  m_ev0par2 = (St_ev0_ev0par2 *)  params("ev0pars/ev0par2");
  if (!m_ev0par2) {
    m_ev0par2 = new St_ev0_ev0par2("ev0par2",3);
    //AddConst(m_ev0par2);
  }
  AddConst(m_ev0par2);
  ev0_ev0par2_st *ev0par2 = m_ev0par2->GetTable();
  m_ev0par2->SetNRows(3);
  // TPC only cuts
  
  ev0par2->dca        =  0.8;
  ev0par2->dcav0      =  0.7;
  ev0par2->dlen       =  2.0;
  ev0par2->alpha_max  = 1.2;
  ev0par2->ptarm_max  = 0.3;
  ev0par2->dcapnmin   = 0.7;
  ev0par2++;
  
  //SVT only cuts
  
  ev0par2->dca        = 0.8;
  ev0par2->dcav0      = 0.3;
  ev0par2->dlen       = 10000.;
  ev0par2->alpha_max  = 1.2;
  ev0par2->ptarm_max  = 0.3;
  ev0par2->dcapnmin   = 100;
  ev0par2++;
  
  // SVT+TPC cuts
  ev0par2->dca        = 0.8;
  ev0par2->dcav0      = 0.3;
  ev0par2->dlen       = 0.6;
  ev0par2->alpha_max  = 1.2;
  ev0par2->ptarm_max  = 0.3;
  ev0par2->dcapnmin   = 0.7;
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0Maker::Make(){
  //if(Debug()) cout << "Calling ev0..."<< endl; 
  PrintInfo();   
  
  int iMake = kStOK;
  int iRes = 0;
  
  St_DataSet     *match = GetDataSet("match"); 
  if (!match) {
    cout << " StV0Maker: match is missing" << endl;
    return kStWarn;
  }
  St_DataSetIter matchI(match);         
  St_dst_track   *globtrk  = (St_dst_track *) matchI("globtrk");
  if (!globtrk) {
    cout << " StV0Maker: globtrk is missing" << endl;
    return kStWarn;
  }
  
  St_DataSet     *primary = GetDataSet("primary"); 
  
  if (!primary) {
    cout << " StV0Maker: primary is missing" << endl;
    return kStWarn;
  }
  
  St_DataSetIter primaryI(primary);         
  St_dst_track   *globtrk2 = (St_dst_track *) primaryI("globtrk2");
  if (!globtrk2) {
    cout << " StV0Maker: globtrk2 is missing" << endl;
    return kStWarn;
  }
  St_dst_vertex  *vertex   = (St_dst_vertex *) primaryI("vertex");
  if (!vertex) {
    cout << " StV0Maker: vertex is missing" << endl;
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
  
  
  dst_track_st *glob  = globtrk->GetTable();
  dst_track_st *glob2 = globtrk2->GetTable();
  dst_vertex_st *vrtx = vertex->GetTable();
  if( vrtx->vtx_id != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == 1) break;
    }
  }
  if (vrtx->vtx_id == 1) {
#if 0
    // This was done in StPrimary ?    
    Float_t *v0 = &vrtx->x;
    for( Int_t no_rows=0; no_rows<globtrk2->GetNRows() &&
	                  no_rows<globtrk->GetNRows(); no_rows++, glob++,glob2++)
      {
	double qwe = pow(glob2->x0-v0[0],2)+pow(glob2->y0-v0[1],2)+pow(glob2->z0-v0[2],2);
	glob->impact = TMath::Sqrt(qwe);
      }
#endif
    // ev0
    if(Debug()) cout << "Calling ev0..." << endl;
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
    if (iRes !=kSTAFCV_OK) cout << " Problem on return from EV0 " << endl;
    if(m_ev0EvalOn){   
      //ev0_eval2
      if (stk_track && tptrack && evaltrk) {
	
	ev0_eval = new St_ev0_eval("ev0_eval",20000);
	AddData(ev0_eval);
	St_DataSetIter geant(GetInputDS("geant"));
	St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("g2t_track");
	St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("g2t_vertex");
	if(Debug()) cout << " Calling ev0_eval2.." << endl;
	Int_t Res_ev0_eval = kSTAFCV_BAD;
	Res_ev0_eval = ev0_eval2(stk_track,tptrack,evaltrk,
				 vertex,dst_v0_vertex,ev0_eval,
				 g2t_track,g2t_vertex);
	
	if (Res_ev0_eval != kSTAFCV_OK) {cout << "Problem on return from ev0eval2" << endl;}
      }
    }     // If ev0 evaluation switched on 
  }  
  return iMake;
}

//_____________________________________________________________________________
void StV0Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StV0Maker.cxx,v 1.5 1999/07/11 01:55:45 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
  
}

