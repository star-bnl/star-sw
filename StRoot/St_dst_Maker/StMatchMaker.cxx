//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMatchMaker class ( svm + est + egr )                               //
//                                                                      //
// $Id: StMatchMaker.cxx,v 1.5 1999/07/15 13:57:52 perev Exp $
// $Log: StMatchMaker.cxx,v $
// Revision 1.5  1999/07/15 13:57:52  perev
// cleanup
//
// Revision 1.4  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
//#include "PhysicalConstants.h"
#include "TMath.h"
#include "StMatchMaker.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_tcl_tpcluster_Table.h"
#include "tables/St_scs_cluster_Table.h"
#include "tables/St_ctu_cor_Table.h"

#include "global/St_svm_am_Module.h"
#include "global/St_svm_eval2_Module.h"
#include "global/St_svm_svt_eval_Module.h"
#include "global/St_svm_efficiency_Module.h"

#include "global/St_est_am_Module.h"
#include "global/St_est_toglob2_Module.h"
#include "global/St_est_eval_Module.h"
#include "global/St_egr_fitter_Module.h"


class St_tcl_tpcluster;
class St_scs_cluster;
class St_ctu_cor;

ClassImp(StMatchMaker)
  
  //_____________________________________________________________________________
  StMatchMaker::StMatchMaker(const char *name):StMaker(name),
  m_svm_ctrl(0),
  m_egr_egrpar(0),
  m_est_ctrl(0)
{
  drawinit=kFALSE;
  m_scenario  = 8;
  m_svtchicut = 0;
  m_useglobal = 2;
  m_usesvt    = 1;
  m_usetpc    = 1;
  m_usevert   = 0;
  m_flag      = 2;
}
//_____________________________________________________________________________
StMatchMaker::~StMatchMaker(){
}
//_____________________________________________________________________________
Int_t StMatchMaker::Init(){
  // Create tables
  St_DataSet *globalParams = GetInputDB("params/global");
  assert (globalParams);
  St_DataSetIter params(globalParams);
  
  //svm
  m_svm_ctrl   = (St_svm_ctrl *)   params("svmpars/svm_ctrl");
  
  //est
  
  m_est_ctrl   = (St_est_ctrl *) params("estpars/est_ctrl");
  est_ctrl_st *est_ctrl = m_est_ctrl->GetTable();
  est_ctrl->svt_er = 0.01;
  
  //egr 
  m_egr_egrpar = (St_egr_egrpar *) params("egrpars/egr_egrpar");  
  AddConst(m_egr_egrpar);
  egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
  egr_egrpar->scenario =   0;
  egr_egrpar->mxtry =     10;
  egr_egrpar->minfit =     2;
  egr_egrpar->prob[0] =    2;
  egr_egrpar->prob[1] =    2;
  memset(egr_egrpar->debug, 0, 9*sizeof(Int_t));  
  egr_egrpar->debug[0] =   1; 
  egr_egrpar->svtchicut =  0;
  egr_egrpar->usetpc =     1;
  egr_egrpar->usesvt =     0;
  egr_egrpar->usevert =    0;
  egr_egrpar->useglobal =  2;
  //  egr_egrpar->scenario  = m_scenario;
  //  egr_egrpar->svtchicut = m_svtchicut;
  //  egr_egrpar->useglobal = m_useglobal;
  //  egr_egrpar->usetpc    = m_usetpc;
  //  egr_egrpar->usesvt    = m_usesvt; 
  //  egr_egrpar->usevert   = m_usevert;
  
  St_DataSetIter  svtpars(GetInputDB("params/svt"));
  m_svt_shape      = (St_svg_shape   *) svtpars("svgpars/shape");
  m_svt_config     = (St_svg_config  *) svtpars("svgpars/config");
  m_svt_geom       = (St_svg_geom    *) svtpars("svgpars/geom");
  m_srs_activea    = (St_srs_activea *) svtpars("srspars/srs_activea");
  m_srspar         = (St_srs_srspar  *) svtpars("srspars/srs_srspar");
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMatchMaker::Make(){
  PrintInfo();  
  
  int iMake = kStOK;
  int iRes = 0;
  
  St_dst_track     *globtrk     = new St_dst_track("globtrk",20000);  
  AddData(globtrk);
  
  St_dst_track_aux *globtrk_aux = new St_dst_track_aux("globtrk_aux",20000);
  AddData(globtrk_aux);
  
  St_dst_vertex *vertex = new St_dst_vertex("vertex",1); 
  AddGarb(vertex);   
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  St_tte_mctrk  *mctrk     = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
    mctrk     = (St_tte_mctrk  *) tpc_tracks("mctrk");
  }
  if (! evaltrk)    {evaltrk = new St_tte_eval("evaltrk",1); AddGarb(evaltrk);}
  if (! mctrk)    {mctrk = new St_tte_mctrk("mctrk",1); AddGarb(mctrk);}
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  St_tcl_tpcluster *tpcluster = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
    tpcluster = (St_tcl_tpcluster *) tpchits->Find("tpcluster");
  }
  if (! tpcluster)    {tpcluster = new St_tcl_tpcluster("tpcluster",1); AddGarb(tpcluster);}
  
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *stk_track   = 0;
  St_sgr_groups  *groups      = 0;
  St_scs_spt     *scs_spt     = 0;
  St_scs_cluster *scs_cluster = 0;
  
  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
    groups    = (St_sgr_groups *) svtracks->Find("groups");
  }
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
    scs_cluster = (St_scs_cluster *) svthits->Find("scs_cluster");
  }
  
  St_est_match   *est_match    = 0;
  
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
  if (!groups)    {groups = new St_sgr_groups("groups",1); AddGarb(groups);}
  if (!scs_spt)   {scs_spt = new St_scs_spt("scs_spt",1); AddGarb(scs_spt);}
  // 			Case running est tpc -> Si space point tracking
  if ( !(svtracks && svthits) ){
    groups = new St_sgr_groups("groups",10000); AddGarb(groups);
    stk_track    = (St_stk_track *) m_GarbSet->Find("stk_tracks");
    if( !stk_track){ stk_track = new St_stk_track("stk_tracks",5000); AddGarb(stk_track);}
    est_match    = (St_est_match *) m_GarbSet->Find("est_match");
    if ( !est_match){ est_match = new St_est_match("est_match",10000); AddGarb(est_match); }
  } 
  if (! scs_cluster) {scs_cluster = new St_scs_cluster("scs_cluster",1); AddGarb(scs_cluster);}
  St_DataSet *ctf = GetInputDS("ctf");
  St_ctu_cor *ctb_cor = 0;
  if (!ctf) {
    if(Debug()) cout << "St_ctf_Maker has not been called " << endl;
  } else {
    ctb_cor = (St_ctu_cor *)ctf->Find("ctb_cor"); 
    if (! ctb_cor) {ctb_cor = new St_ctu_cor("ctb_cor",1); AddGarb(ctb_cor);}
  }
  
  St_svm_evt_match *evt_match  = new St_svm_evt_match("evt_match",3000);    AddData(evt_match);
  
  if (tptrack && svtracks) {
    
    //			svm
    iRes =  svm_am (stk_track, tptrack, m_svm_ctrl, evt_match);
    //          ==================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    
  } else if (tptrack && svthits){
    
    //est
    
    egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
    egr_egrpar->useglobal = 3;
    St_g2t_track   *g2t_track    = 0;
    St_DataSet *geant = GetInputDS("geant");
    if (geant) {
      St_DataSetIter geantI(geant);
      g2t_track    = (St_g2t_track  *) geantI("g2t_track");
    }
    if (!g2t_track) {g2t_track = new St_g2t_track("g2t_track",1); AddGarb(g2t_track);}
    iRes = est_am(m_svt_geom, m_svt_shape,  m_srs_activea,
		  m_srspar,m_svt_config,scs_spt,tphit,
		  tptrack,evaltrk,mctrk,m_est_ctrl,
		  est_match,g2t_track,m_egr_egrpar);
    //          ==============================================
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if(Debug()) cout << "Calling EST_TOGLOB2" << endl;
    iRes = est_toglob2(est_match, tphit,     tptrack, scs_spt,
		       groups,stk_track,evt_match);
    //         ==================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if(Debug()) cout << "finished est_toglob2 " << endl;   
#if 0     
    if(Debug()) cout << "Calling EST_EVAL" << endl;    
    iRes_est_eval = est_eval(g2t_track, tptrack, mctrk, 
			     m_est_ctrl,est_match,est_ev,scs_spt); 
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
#endif
  }
  
  
  // egr
  
  iRes = egr_fitter (tphit,    vertex,      tptrack,   evaltrk,
		     scs_spt,m_egr_egrpar,stk_track,groups,
		     evt_match,globtrk,globtrk_aux);
  //	 ======================================================
  
  if (iRes !=kSTAFCV_OK) iMake = kStWarn;
  if (iRes !=kSTAFCV_OK) {cout << "Problem on return from EGR_FITTER" << endl;}
  if(Debug()) cout << " finished calling egr_fitter" << endl;
  
  return iMake;
}

//_____________________________________________________________________________

