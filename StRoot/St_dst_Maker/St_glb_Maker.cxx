// $Id: St_glb_Maker.cxx,v 1.56 1999/06/24 16:42:27 fisyak Exp $
// $Log: St_glb_Maker.cxx,v $
// Revision 1.56  1999/06/24 16:42:27  fisyak
// Preserve all space point
//
// Revision 1.55  1999/05/01 00:57:04  fisyak
// Change Clear function to defualt
//
// Revision 1.54  1999/04/21 18:32:58  genevb
// Vertex table declaration was reverted to old version when new maker schema was introduced
//
// Revision 1.53  1999/04/14 23:09:42  fisyak
// Add primtrk to dataset
//
// Revision 1.52  1999/04/01 23:31:42  fisyak
// Add summary information and set return kStWarn after evr fails
//
// Revision 1.51  1999/03/20 22:34:07  perev
// maker new schema
//
// Revision 1.50  1999/03/11 03:12:18  perev
// new schema
//
// Revision 1.49  1999/03/06 03:01:12  genevb
// Change dst_vertex table size as needed
//
// Revision 1.48  1999/03/04 03:08:14  fisyak
// take out PhysicalConstants
//
// Revision 1.47  1999/03/04 03:01:27  fisyak
// fix typo
//
// Revision 1.46  1999/03/04 01:19:21  fisyak
// Put release tag to run_summary table
//
// Revision 1.45  1999/03/04 00:06:22  caines
// Fixed so that tte_eval is made if no tracks - for pp events
//
// Revision 1.44  1999/03/03 17:13:43  caines
// Reduced v0,xi table memory allocations
//
// Revision 1.43  1999/03/03 04:52:55  fisyak
// Add protection for no vertex fit was made
//
// Revision 1.42  1999/02/28 15:48:48  caines
// CHanged no of xi_vertex booked
//
// Revision 1.41  1999/02/27 23:10:32  caines
// Fixed impact calc
//
// Revision 1.40  1999/02/26 19:49:27  caines
// Lots of memory fixes
//
// Revision 1.39  1999/02/23 17:10:47  caines
//  xi_aux table size reduced
//
// Revision 1.38  1999/02/23 16:50:16  fine
// wrong calling seq for ev0_eval2 module
//
// Revision 1.37  1999/02/23 03:13:52  fisyak
// Take out fake tof
//
// Revision 1.35  1999/02/22 21:27:20  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.34  1999/02/20 18:49:16  fisyak
// Add event/run information
//
// Revision 1.33  1999/02/20 00:24:51  kathy
// fixed some of the histograms
//
// Revision 1.32  1999/02/19 15:29:25  caines
// Corrected so egr now runs for year1
//
// Revision 1.31  1999/02/18 18:40:57  caines
// Altered the creation of svm tables
//
// Revision 1.30  1999/02/18 16:43:10  caines
// Added in est the 4th layer tracking
//
// Revision 1.29  1999/02/17 23:58:11  caines
// changed ev0 cuts
//
// Revision 1.28  1999/02/17 20:50:57  fisyak
// reduce no. of reconstructed tracks/verteces from 100K to 20K
//
// Revision 1.27  1999/02/16 21:34:01  caines
//  Added exi back in
//
// Revision 1.26  1999/02/16 03:03:46  fisyak
// Split Make and Histograms
//
// Revision 1.25  1999/02/14 18:38:22  caines
//  Fixed ev0 bugs in hist
//
// Revision 1.24  1999/02/13 20:22:31  caines
// Added exi and temp dir for when svt not there
//
// Revision 1.23  1999/02/12 22:27:35  ogilvie
// added in spectra/pid QA histograms
//
// Revision 1.22  1999/02/12 19:23:46  didenko
// updated v0 finding code from Helen
//
// Revision 1.21  1999/02/11 02:53:37  fisyak
// Janet update to FTPC dst table
//
// Revision 1.20  1999/02/05 17:58:18  fisyak
// Spiros correction to evr
//
// Revision 1.19  1999/01/28 17:09:59  fisyak
// Add ftpc to software monitor
//
// Revision 1.18  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.17  1999/01/02 19:08:17  fisyak
// Add ctf
//
// Revision 1.16  1998/12/21 19:41:50  fisyak
// Move dst 2 glb
//
// Revision 1.15  1998/12/21 19:26:08  fisyak
// Make ROOT include non system
//
// Revision 1.14  1998/12/17 14:37:19  fisyak
// Fix tp_param
//
// Revision 1.13  1998/12/16 22:22:40  fisyak
// New global from Spiros
//
// Revision 1.12  1998/12/12 02:37:53  fisyak
// fix evr
//
// Revision 1.11  1998/12/01 02:02:38  fisyak
// fix run_summary_param
//
// Revision 1.10  1998/11/25 21:58:24  fisyak
// Cleanup
//
// Revision 1.9  1998/11/12 23:38:36  fisyak
// Account new g2t
//
// Revision 1.8  1998/11/01 16:42:27  fisyak
// dst analysis
//
// Revision 1.7  1998/10/31 00:26:12  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.5  1998/09/23 20:22:54  fisyak
// Prerelease SL98h
//
// Revision 1.4  1998/09/15 20:55:20  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.3  1998/09/08 22:43:10  fisyak
// Modify St_glb_Maker to account new calling sequence
//
// Revision 1.2  1998/08/26 12:15:08  fisyak
// Remove asu & dsl libraries
//
// Revision 1.1  1998/08/18 14:06:06  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_glb_Maker class for Makers ( est + evr + egr + ev0 + ev0_eval)           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
//#include "PhysicalConstants.h"
#include "TMath.h"
#include "St_glb_Maker.h"
#include "St_particle_Table.h"
#include "St_hepe_gent_Table.h"

#include "StChain.h"
#include "St_DataSetIter.h"

#include "global/St_svm_am_Module.h"
#include "global/St_svm_eval2_Module.h"
#include "global/St_svm_svt_eval_Module.h"
#include "global/St_svm_efficiency_Module.h"

#include "global/St_est_am_Module.h"
#include "global/St_est_toglob2_Module.h"
#include "global/St_est_eval_Module.h"

#include "global/St_evr_am_Module.h"
#include "global/St_egr_fitter_Module.h"
#include "global/St_track_propagator_Module.h"
#include "global/St_ev0_am2_Module.h"
#include "global/St_ev0_eval2_Module.h"
#include "global/St_exiam_Module.h"
#include "global/St_dst_dedx_filler_Module.h"
#include "global/St_fill_ftpc_dst_Module.h"
#include "global/St_dst_monitor_soft_filler_Module.h"

#include "global/St_particle_dst_filler_Module.h"
#include "global/St_dst_point_filler_Module.h"
#include "global/St_fill_dst_event_summary_Module.h"
#include "St_dst_summary_param_Table.h"
#include "St_dst_run_summary_Table.h"
ClassImp(St_glb_Maker)
  
  //_____________________________________________________________________________
  St_glb_Maker::St_glb_Maker(const char *name):StMaker(name),
  m_svm_ctrl(0),
  m_evr_evrpar(0),
  m_ev0par(0),
  m_magf(0),
  m_egr_egrpar(0),
  m_particle_dst_param(0),
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
St_glb_Maker::~St_glb_Maker(){
}
//_____________________________________________________________________________
Int_t St_glb_Maker::Init(){
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
  
  //evr
  m_evr_evrpar  = (St_evr_evrpar *) params("evrpars/evr_evrpar");
  
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
  
  //ev0   
  m_ev0par2 = (St_ev0_ev0par2 *)  params("ev0pars/ev0par2");
  if (!m_ev0par2) {
    m_ev0par2 = new St_ev0_ev0par2("ev0par2",3);
    AddConst(m_ev0par2);
  }
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
  
  //exi
  if (!m_exiaux) m_exiaux = new St_exi_aux("exi_aux",1);
  m_exipar = (St_exi_exipar *)  params("exipars/exipar");
  if (!m_exipar) {
    m_exipar = new St_exi_exipar("exipar",3);
    AddConst(m_exipar);
    m_exipar->SetNRows(3);
  }
  exi_exipar_st *exipar = m_exipar->GetTable();
  
  // TPC only cuts
  
  exipar->use_pid = 0;
  exipar->dca_max = 1.;
  exipar->bxi_max = 1.;
  exipar->rv_xi   = 2.;
  exipar->rv_v0   = 5.;
  exipar->dmass   = 0.01;
  exipar->bpn_v0  = 2.;
  exipar->pchisq  = 0.;
  exipar++;
  
  //SVT only cuts
  
  exipar->use_pid = 0;
  exipar->dca_max = 0.;
  exipar->bxi_max = 0.;
  exipar->rv_xi   = 999.;
  exipar->rv_v0   = 999.;
  exipar->dmass   = 0.;
  exipar->bpn_v0  = 999.;
  exipar->pchisq  = 0.;
  exipar++;
  
  // SVT+TPC cuts
  
  exipar->use_pid = 0;
  exipar->dca_max = 1.;
  exipar->bxi_max = 1.;
  exipar->rv_xi   = 2.;
  exipar->rv_v0   = 5.;
  exipar->dmass   = 0.01;
  exipar->bpn_v0  = 2.;
  exipar->pchisq  = 0.;
  exipar++;
  
  St_DataSetIter  svtpars(GetInputDB("params/svt"));
  m_svt_shape      = (St_svg_shape   *) svtpars("svgpars/shape");
  m_svt_config     = (St_svg_config  *) svtpars("svgpars/config");
  m_svt_geom       = (St_svg_geom    *) svtpars("svgpars/geom");
  m_srs_activea    = (St_srs_activea *) svtpars("srspars/srs_activea");
  m_srspar         = (St_srs_srspar  *) svtpars("srspars/srs_srspar");
  
  // Run summary
  m_dst_summary_param = new St_dst_summary_param("summary_param",1);
  AddConst(m_dst_summary_param);
  dst_summary_param_st dst_summary_param;
  dst_summary_param.eta_bins[0]=  -2.;
  dst_summary_param.eta_bins[1]=  -1.;
  dst_summary_param.eta_bins[2]=  -.5;
  dst_summary_param.eta_bins[3]=   .5;
  dst_summary_param.eta_bins[4]=   1.;
  dst_summary_param.eta_bins[5]=   2.;
  
  dst_summary_param.pt_bins[0]=    .1;
  dst_summary_param.pt_bins[1]=    .15;
  dst_summary_param.pt_bins[2]=    .2;
  dst_summary_param.pt_bins[3]=    .3;
  dst_summary_param.pt_bins[4]=    .5;
  dst_summary_param.pt_bins[5]=    1.;
  
  dst_summary_param.mt_bins[0]=    .03;
  dst_summary_param.mt_bins[1]=    .12;
  dst_summary_param.mt_bins[2]=    .2;
  dst_summary_param.mt_bins[3]=    .3;
  dst_summary_param.mt_bins[4]=    .5;
  dst_summary_param.mt_bins[5]=    1.;
  
  dst_summary_param.n_phi_bins=  8;
  
  m_dst_summary_param->AddAt(&dst_summary_param,0);
  St_dst_run_header *run_header = new St_dst_run_header("run_header",1);
  AddConst(run_header);

  St_dst_run_summary *dst_run_summary = new St_dst_run_summary("dst_run_summary",1);
  AddConst(dst_run_summary);
  dst_run_summary_st run_summary = 
{
  "Unknown", //char   version[20];     /* DST production software version               */
  0,         //long   prod_run;        /* Prod. run number, F.key to dst_run_header     */
  0,         //long   n_events_tot;    /* Total number of events in the run             */
  0,         //long   n_events_good;   /* Total number events successfully processed    */
  {0,0},     //long   date[2];         /* Start/stop date for processing                */
  {0,0},     //long   time[2];         /* Start/stop time of day (sec)                  */
  0.,        //float  cpu_total;       /* Total cpu sec for production run              */
  {0.,0.,0.,0.,0.,0.}, //float  eta_bins[6];     /* Pseudorapidity bins, lower->upper ranges      */
  {0.,0.,0.,0.,0.,0.}, //float  pt_bins[6];      /* Trans. momen. bins, lower->upper ranges       */
  {0.,0.,0.,0.,0.,0.}, //float  mt_bins[6];      /* Trans. mass bins, lower->upper ranges         */
  0,         //long   n_phi_bins;      /* # of phi bins, start at phi=0, max # is 8     */
  {0.,0.},   //float  mean_eta[2];     /* Mean and std.dev. of <eta> for all events     */
  {0.,0.},   //float  mean_pt[2];      /* Mean and std.dev. of <pt> for all events      */
  { 0, 0},   //long   multiplicity[2]; /* Mean and std.dev. of total chrg. mult.        */
  { 0, 0},   //long   num_vert[2];     /* Mean and std.dev. of total # vertices         */
  { 0, 0},   //long   energy_emc[2];   /* Mean and std.dev. of total energy in EMC      */
 };


  strcpy (&run_summary.version[0],VersionTag()); // DST production software version
  dst_run_summary->AddAt(&run_summary,0);
 
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_glb_Maker::Make(){
  
  int iMake = kStOK;
  int iRes = 0;
  
  // Make a temp dir. to hold SVT stuff when the SVT isnt there
  
  St_DataSetIter global(m_DataSet);         // data/global
  St_DataSet  *dst_loc = global.Mkdir("dst");
  
  St_DataSetIter dst(dst_loc);
  
  St_dst_track      *globtrk     = (St_dst_track     *) dst("globtrk");
  St_dst_track_aux  *globtrk_aux = (St_dst_track_aux *) dst("globtrk_aux");
  St_dst_track      *primtrk     = (St_dst_track     *) dst("primtrk");
  St_dst_track_aux  *primtrk_aux = (St_dst_track_aux *) dst("primtrk_aux");
  St_dst_vertex     *vertex      = (St_dst_vertex    *) dst("vertex");
  St_dst_v0_vertex  *dst_v0_vertex = (St_dst_v0_vertex *) dst("dst_v0_vertex"); 
  St_dst_xi_vertex  *dst_xi_vertex = (St_dst_xi_vertex *) dst("dst_xi_vertex");
  St_dst_dedx       *dst_dedx    = (St_dst_dedx      *) dst("dst_dedx");
  St_dst_point      *point       = (St_dst_point     *) dst("point");
  St_dst_event_header  *event_header  = (St_dst_event_header  *) dst("event_header");
  St_dst_event_summary *event_summary = (St_dst_event_summary *) dst("event_summary");
  St_dst_monitor_soft  *monitor_soft  = (St_dst_monitor_soft  *) dst("monitor_soft");

  St_dst_run_summary *dst_run_summary = (St_dst_run_summary   *) m_ConstSet->Find("dst_run_summary");
  
   if (! event_header) {
    event_header  = new St_dst_event_header("event_header",1);
    dst.Add(event_header);
  }
  event_header->SetNRows(1);
  dst_event_header_st  event =   {"Collision", //event_type
				  {0,0},       // n_event[2]
				  0, 0, 0, 0}; // n_run,time,trig_mask,bunch_cross
  if (GetEventType()) strcpy (&event.event_type[0],GetEventType());
  event.n_event[0] = GetEventNumber();
  event.n_event[1] = GetDate();
  event.n_run      = GetRunNumber();
  event.time       = GetTime();
  event_header->AddAt(&event,0);
  if (! event_summary) {
    event_summary = new St_dst_event_summary("event_summary",1);
    dst.Add(event_summary);
  }
  
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
  //			Create DST
  St_svm_evt_match *evt_match = 0;
  evt_match = (St_svm_evt_match *) global("tracks/evt_match");
  if (!evt_match)   {evt_match   = new St_svm_evt_match("evt_match",3000);    AddGarb(evt_match);}
  if (!globtrk)     {globtrk     = new St_dst_track("globtrk",20000);         dst.Add(globtrk);}
  if (!globtrk_aux) {globtrk_aux = new St_dst_track_aux("globtrk_aux",20000); dst.Add(globtrk_aux);}
  
  
  if (tptrack && svtracks) {
    
    //			svm
    iRes =  svm_am (stk_track, tptrack, m_svm_ctrl, evt_match);
    //          ==================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    
  } else if (tptrack && svthits){
    
    //est
    
    egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
    egr_egrpar->useglobal = 3;
    iRes = est_am(m_svt_geom, m_svt_shape,  m_srs_activea,
		  m_srspar,m_svt_config,scs_spt,tphit,
		  tptrack,evaltrk,mctrk,m_est_ctrl,
		  est_match,m_egr_egrpar);
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
  vertex = new St_dst_vertex("vertex",1); dst.Add(vertex);
  
  // egr
  
  iRes = egr_fitter (tphit,    vertex,      tptrack,   evaltrk,
		     scs_spt,m_egr_egrpar,stk_track,groups,
		     evt_match,globtrk,globtrk_aux);
  //	 ======================================================
  
  if (iRes !=kSTAFCV_OK) iMake = kStWarn;
  if (iRes !=kSTAFCV_OK) {cout << "Problem on return from EGR_FITTER" << endl;}
  if(Debug()) cout << " finished calling egr_fitter" << endl;
  // dst_mon_soft
  if (tphit &&  scs_spt) {
    if(Debug()) cout << " run_dst: Calling dst_point_filler" << endl;
    // dst_point_filler
    point = new St_dst_point("point",200000); dst.Add(point);
    iRes = dst_point_filler(tphit, scs_spt, point);
    //	   ========================================
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if (iRes !=kSTAFCV_OK) cout << "Problem on return from DST_POINT_FILLER" << endl;
    
    
    if(Debug()) cout << " run_dst: finished calling dst_point_filler" << endl;
  }
  
  // evr
  if(Debug()) cout << "run_evr: calling evr_am" << endl;
  
  iRes = evr_am(m_evr_evrpar,m_egr_egrpar,globtrk,vertex);
  //	 ================================================
  
  if (iRes !=kSTAFCV_OK) return kStWarn;
  
  
  // track_propagator
  St_dst_track *globtrk2     = new St_dst_track("globtrk2");
  dst.Add(globtrk2);
  
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
    for( Int_t no_rows=0; no_rows<globtrk2->GetNRows(); no_rows++, glob++,glob2++)
      {
	double qwe = pow(glob2->x0-v0[0],2)+pow(glob2->y0-v0[1],2)+pow(glob2->z0-v0[2],2);
	glob->impact = TMath::Sqrt(qwe);
      }
    
    if(Debug()) cout << " finished calling track-propagator" << endl;
    
    // egr2
    if (tphit && stk_track) {
      int nglob = globtrk->GetNRows();
      primtrk = new St_dst_track("primtrk",nglob);
      dst.Add(primtrk);
      primtrk_aux = new St_dst_track_aux("primtrk_aux",nglob);
      dst.Add(primtrk_aux);
      
      if(Debug()) cout << "Calling EGR_fitter - Second time" << endl;
      
      iRes = egr_fitter (tphit,    vertex,       tptrack,  evaltrk,
			 scs_spt,m_egr2_egrpar,stk_track,groups,
			 evt_match,primtrk,primtrk_aux);
      //	   ======================================================
      
      if (iRes !=kSTAFCV_OK) iMake = kStWarn;
      if (iRes !=kSTAFCV_OK){cout << "Problem on return from EGR_FITTER" << endl;}
      
      if(Debug()) cout <<" finished calling egr_fitter - second time" << endl;
    }
    // ev0
    if(Debug()) cout << "Calling ev0..." << endl;
    Int_t v0_limit = globtrk->GetNRows();
    v0_limit = (v0_limit*v0_limit)/6000;
    if (v0_limit < 1000) v0_limit=1000;
    if (! dst_v0_vertex) {
      dst_v0_vertex = new St_dst_v0_vertex("dst_v0_vertex",v0_limit); 
      dst.Add(dst_v0_vertex);
    }
    vertex->ReAllocate(3*v0_limit);
    St_ev0_track2 *ev0track2 = new St_ev0_track2("ev0_track2",globtrk->GetNRows());
    AddGarb(ev0track2);
    
    vertex->SetNRows(1); 
    
    iRes = ev0_am2(m_ev0par2,globtrk,vertex,dst_v0_vertex,ev0track2);
    //       =========================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if (iRes !=kSTAFCV_OK) cout << " Problem on return from EV0 " << endl;
#if 0      
    //ev0_eval2
    if (stk_track && tptrack && evaltrk) {
      St_ev0_eval *ev0_eval = new St_ev0_eval("ev0_eval",20000);
      dst.Add(ev0_eval);
      St_DataSetIter geant(GetInput("geant"));
      St_g2t_track   *g2t_track    = (St_g2t_track  *) geant("Event/g2t_track");
      St_g2t_vertex  *g2t_vertex   = (St_g2t_vertex *) geant("Event/g2t_vertex");
      if(Debug()) cout << " Calling ev0_eval2.." << endl;
      Int_t Res_ev0_eval = kSTAFCV_BAD;
      Res_ev0_eval = ev0_eval2(stk_track,tptrack,evaltrk,
			       vertex,ev0out,ev0_eval,
			       g2t_track,g2t_vertex);
      
      if (Res_ev0_eval != kSTAFCV_OK) {cout << "Problem on return from ev0eval2" << endl;}
    }
#endif      
    // exi
    if(Debug()) cout << "Calling exi..."<< endl;
    Int_t xi_limit = 2*dst_v0_vertex->GetNRows();
    if (xi_limit < 250) xi_limit=250;
    dst_xi_vertex = new St_dst_xi_vertex("dst_xi_vertex",xi_limit);
    dst.Add(dst_xi_vertex);
    
    iRes = exiam(m_exipar,globtrk,vertex,dst_v0_vertex,dst_xi_vertex,m_exiaux);
    //	 ===================================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) {cout << " Problem on return from EXI " << endl;}
  }
  // dst 
  // dst_dedx_filler
  if (tptrack && stk_track) {
    if(Debug()) cout << " run_dst: Calling dst_dedx_filler" << endl;
    dst_dedx = new St_dst_dedx("dst_dedx",20000); dst.Add(dst_dedx);
    
    iRes = dst_dedx_filler(tptrack,stk_track,dst_dedx);
    //	   ===========================================
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) cout << "Problem on return from DST_DEDX_FILLER" << endl; 
    if(Debug()) cout << " run_dst: finshed calling dst_dedx_filler" << endl;
  }
  
  St_DataSet *ftpc_hits   = GetInputDS("ftpc_hits");
  St_fcl_fppoint *fcl_fppoint = 0;
  if (ftpc_hits) 
    fcl_fppoint = (St_fcl_fppoint *) ftpc_hits->Find("fcl_fppoint");
  
  St_DataSet *ftpc_tracks = GetInputDS("ftpc_tracks");
  St_fpt_fptrack *fpt_fptrack = 0;
  if (ftpc_tracks) 
    fpt_fptrack = (St_fpt_fptrack *) ftpc_tracks->Find("fpt_fptrack");
  
  if (fcl_fppoint && fpt_fptrack) {
    if(Debug()) cout<<" run_dst: Calling fill_ftpc_dst"<<endl;
    
    iRes = fill_ftpc_dst(fpt_fptrack, fcl_fppoint, globtrk,
			 globtrk_aux,point,vertex,dst_dedx);
    //  	   ==========================================================
    if (iRes != kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) cout << "Problem on return from FILL_FTPC_DST" << endl;
    
    if(Debug()) cout << " run_dst: finished calling fill_ftpc_dst" << endl;
  }
  
  if (tphit && scs_spt && tptrack && stk_track && evt_match && ctf && fcl_fppoint && fpt_fptrack) {
    monitor_soft = new St_dst_monitor_soft("monitor_soft",1);
    dst.Add(monitor_soft);
    if(Debug()) cout << " run_dst: Calling dst_monitor_soft_filler" << endl;
    
    iRes = dst_monitor_soft_filler(tpcluster,
				   scs_cluster,
				   tphit,scs_spt,fcl_fppoint,
				   tptrack,stk_track,fpt_fptrack,
				   evt_match,
				   ctb_cor,vertex,event_summary,monitor_soft);
    //	   ===========================================================================
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if (iRes != kSTAFCV_OK) cout << "Problem on return from DST_MONITOR_SOFT_FILLER" << endl;
    if(Debug()) cout << " run_dst: finished calling dst_monitor_soft_filler" << endl;
  }
  //--------------- ????????? --------------
  St_dst_run_header    *run_header    = (St_dst_run_header *)    m_ConstSet->Find("run_header");
  St_dst_summary_param *summary_param = (St_dst_summary_param *) m_ConstSet->Find("summary_param");
  
  if (!run_header) {
    StEvtHddr * evthdr = (StEvtHddr*)GetDataSet("EvtHddr");
    assert(evthdr);
    run_header = new St_dst_run_header("run_header",1);
    dst_run_header_st run;
    run.run_id = evthdr->GetRunNumber();
    strcpy (run.event_type,(const char*)evthdr->GetEventType());
    run.sqrt_s = evthdr->GetCenterOfMassEnergy();
    run.east_a = evthdr->GetAEast();
    run.west_a = evthdr->GetAWest();
    run_header->AddAt(&run,0);
  }
  if (summary_param && run_header) {
    if(Debug()) cout << " run_dst: Calling fill_dst_event_summary" << endl;
    
    iRes = fill_dst_event_summary(summary_param, run_header, event_header,
				  globtrk,vertex,event_summary);
    //	   ================================================================
    
    if (iRes !=kSTAFCV_OK) {
      cout << "Problem on return from FILL_DST_EVENT_SUMMARY" << endl;
      iMake = kStWarn;
    }
    if(Debug()) cout << " run_dst: finished calling fill_dst_event_summary" << endl;
  }
  
  // Fill histograms
  
  // look for generator data
  
  // delete temp dir which holds fake svt when its not there
  
  return iMake;
}

//_____________________________________________________________________________
void St_glb_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_glb_Maker.cxx,v 1.56 1999/06/24 16:42:27 fisyak Exp $\n");
  //  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

