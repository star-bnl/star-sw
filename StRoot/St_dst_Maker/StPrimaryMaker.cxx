//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPrimaryMaker class ( est + evr + egr )                             //
//                                                                      //
// $Id: StPrimaryMaker.cxx,v 1.4 1999/07/11 01:55:45 fisyak Exp $
// $Log: StPrimaryMaker.cxx,v $
// Revision 1.4  1999/07/11 01:55:45  fisyak
// Fix glob->impact
//
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include "TMath.h"
#include "StPrimaryMaker.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "global/St_evr_am_Module.h"
#include "global/St_egr_fitter_Module.h"
#include "global/St_track_propagator_Module.h"

//class St_tcl_tpcluster;
//class St_scs_cluster;
//class St_ctu_cor;

ClassImp(StPrimaryMaker)
  
  //_____________________________________________________________________________
  StPrimaryMaker::StPrimaryMaker(const char *name):StMaker(name),
  m_evr_evrpar(0),
  m_egr_egrpar(0)
{
  m_flag      = 2;
}
//_____________________________________________________________________________
StPrimaryMaker::~StPrimaryMaker(){
}
//_____________________________________________________________________________
Int_t StPrimaryMaker::Init(){
  // Create tables
  St_DataSet *globalParams = GetInputDB("params/global");
  assert (globalParams);
  St_DataSetIter params(globalParams);
  
  //egr 
  m_egr_egrpar = (St_egr_egrpar *) params("egrpars/egr_egrpar");  
  if( !m_egr_egrpar ) {
    St_DataSet *match = GetDataSet("match"); 
    St_DataSetIter matchI(match);         
    m_egr_egrpar = (St_egr_egrpar *) matchI("m_egr_egrpar");  
  }
  
  //evr
  m_evr_evrpar  = (St_evr_evrpar *) params("evrpars/evr_evrpar");
  AddConst(m_evr_evrpar);
  
  // prop
  m_tp_param = new St_egr_propagate("tp_param",1); 
  AddConst(m_tp_param);
  m_tp_param->SetNRows(1);
  egr_propagate_st *tp_param = m_tp_param->GetTable();
  tp_param->iflag =   m_flag;
  if (m_flag == 1 || m_flag == 2) {
    memset(tp_param->x,0,3*sizeof(Float_t));  
  }
  if (m_flag == 3) {
    tp_param->r     =  4.;
  }
  if (m_flag == 4) {
    tp_param->z =  0.; 
  }
  // egr2
  m_egr2_egrpar = (St_egr_egrpar *) params("egrpars/egr2_egrpar");
  if (!m_egr2_egrpar) {
    m_egr2_egrpar = new St_egr_egrpar("egr2_egrpar",1);
    AddConst(m_egr2_egrpar);
    m_egr2_egrpar->SetNRows(1);
  }
  egr_egrpar_st *egr2_egrpar = m_egr2_egrpar->GetTable();
  egr2_egrpar->scenario =  5;
  egr2_egrpar->mxtry =    10;
  egr2_egrpar->minfit =    5;
  egr2_egrpar->prob[0] =   2;
  egr2_egrpar->prob[1] =   2;
  memset (egr2_egrpar->debug,0,9*sizeof(Int_t));
  egr2_egrpar->debug[0] =  1;
  egr2_egrpar->svtchicut = 0;
  egr2_egrpar->usetpc    = 1;
  egr2_egrpar->usesvt    = 0;
  egr2_egrpar->usevert   = 1;
  egr2_egrpar->useglobal = 0;
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StPrimaryMaker::Make(){
  PrintInfo();  
  
  int iMake = kStOK;
  int iRes = 0;
  
  St_DataSet *match = GetDataSet("match"); 
  St_DataSetIter matchI(match);         
  
  St_dst_track     *globtrk  = (St_dst_track *) matchI("globtrk");
  St_svm_evt_match *evt_match = (St_svm_evt_match *) matchI("evt_match");
  St_dst_track     *primtrk     = 0;   
  St_dst_track_aux *primtrk_aux = 0;   
  St_dst_vertex *vertex = new St_dst_vertex("vertex",1); 
  AddData(vertex);   
  
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
  }
  if (! evaltrk)    {evaltrk = new St_tte_eval("evaltrk",1); AddGarb(evaltrk);}
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
  }
  
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *stk_track   = 0;
  St_sgr_groups  *groups      = 0;
  St_scs_spt     *scs_spt     = 0;
  
  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
    groups    = (St_sgr_groups *) svtracks->Find("groups");
  }
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
  if (!groups)    {groups = new St_sgr_groups("groups",1); AddGarb(groups);}
  if (!scs_spt)   {scs_spt = new St_scs_spt("scs_spt",1); AddGarb(scs_spt);}
  // 			Case running est tpc -> Si space point tracking
  if ( !(svtracks && svthits) ){
    groups = new St_sgr_groups("groups",10000); AddGarb(groups);
    stk_track    = (St_stk_track *) m_GarbSet->Find("stk_tracks");
    if( !stk_track){ stk_track = new St_stk_track("stk_tracks",5000); AddGarb(stk_track);}
  } 
  
  // evr
  if(Debug()) cout << "run_evr: calling evr_am" << endl;
  
  iRes = evr_am(m_evr_evrpar,m_egr_egrpar,globtrk,vertex);
  //	 ================================================
  
  if (iRes !=kSTAFCV_OK) return kStWarn;
  
  
  // track_propagator
  St_dst_track *globtrk2     = new St_dst_track("globtrk2");
  AddData(globtrk2);
  
  *globtrk2  = *globtrk;		//Copy table
  
  if(Debug()) cout << " Calling track_propagator " << endl;
  
  if (m_tp_param && vertex) {
    egr_propagate_st *tp_param = m_tp_param->GetTable();
    tp_param->iflag =   m_flag;
    if (m_flag == 1 || m_flag == 2) {
      dst_vertex_st *vrtx = vertex->GetTable();
      memcpy(tp_param->x,&vrtx->x,3*sizeof(Float_t));  
    }
  }
  iRes = track_propagator(globtrk,m_tp_param,globtrk2);
  //	 ==============================================
  
  if (iRes !=kSTAFCV_OK) iMake = kStWarn;
  if (iRes !=  kSTAFCV_OK) cout << "Problem on return from Track_Propagator" << endl;
  
  dst_track_st *glob  = globtrk->GetTable();
  dst_track_st *glob2 = globtrk2->GetTable();
  dst_vertex_st *vrtx = vertex->GetTable();
  if( vrtx->vtx_id != 1){
    for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,vrtx++){
      if( vrtx->vtx_id == 1) break;
    }
  }
  if (vrtx->vtx_id == 1) {
    
    Float_t *v0 = &vrtx->x;
    for( Int_t no_rows=0; no_rows<globtrk2->GetNRows() &&
                          no_rows<globtrk->GetNRows(); no_rows++, glob++,glob2++)
      {
	double qwe = pow(glob2->x0-v0[0],2)+pow(glob2->y0-v0[1],2)+pow(glob2->z0-v0[2],2);
	glob->impact = TMath::Sqrt(qwe);
      }
    
    if(Debug()) cout << " finished calling track-propagator" << endl;
    
    // egr2
    if (tphit && stk_track) {
      int nglob = globtrk->GetNRows();
      primtrk = new St_dst_track("primtrk",nglob);
      AddData(primtrk);
      primtrk_aux = new St_dst_track_aux("primtrk_aux",nglob);
      AddData(primtrk_aux);
      
      if(Debug()) cout << "Calling EGR_fitter - Second time" << endl;
      
      iRes = egr_fitter (tphit,    vertex,       tptrack,  evaltrk,
			 scs_spt,m_egr2_egrpar,stk_track,groups,
			 evt_match,primtrk,primtrk_aux);
      //	   ======================================================
      
      if (iRes !=kSTAFCV_OK) iMake = kStWarn;
      if (iRes !=kSTAFCV_OK){cout << "Problem on return from EGR_FITTER" << endl;}
      
      if(Debug()) cout <<" finished calling egr_fitter - second time" << endl;
    }
  }
  return iMake;
}
//_____________________________________________________________________________
void StPrimaryMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StPrimaryMaker.cxx,v 1.4 1999/07/11 01:55:45 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
  
}

