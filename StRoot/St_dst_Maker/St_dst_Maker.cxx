// $Id: St_dst_Maker.cxx,v 1.25 1999/09/13 15:05:29 caines Exp $
// $Log: St_dst_Maker.cxx,v $
// Revision 1.25  1999/09/13 15:05:29  caines
// Added creation of garb(tphit) and garb(tptrack) so its possible to
// run with TPC turned off
//
// Revision 1.24  1999/09/08 00:12:21  fisyak
// Add check that no_of_points > 0
//
// Revision 1.23  1999/08/12 16:54:02  ogilvie
// added tpc_dedx table to dst_dedx_filler
//
// Revision 1.22  1999/07/19 18:20:24  fisyak
// Add globtrk2 to dst
//
// Revision 1.21  1999/07/17 00:31:25  genevb
// Use StMessMgr
//
// Revision 1.20  1999/07/15 13:57:55  perev
// cleanup
//
// Revision 1.19  1999/07/14 15:48:42  caines
// Added ev0_eval to tables wrtten out
//
// Revision 1.18  1999/07/12 23:04:17  fisyak
// Remove glob2
//
// Revision 1.17  1999/07/12 01:49:39  fine
// Clean up
//
// Revision 1.16  1999/07/08 18:40:31  fisyak
// Wensheng Deng global chain
//
// Revision 1.15  1999/07/01 17:27:42  fisyak
// New global chain from  Wensheng Deng
//
// Revision 1.14  1999/06/15 18:39:12  fisyak
// shunt particle to dst
//
// Revision 1.13  1999/05/04 21:00:43  fisyak
// Step back to MDC2 version
//
// Revision 1.12  1999/05/03 01:39:22  fisyak
// Remove tables from DST, add access to different makers
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "TClass.h"
#include "TMath.h"

#include "St_dst_Maker.h"

#include "St_particle_Table.h"
#include "St_hepe_gent_Table.h"

#include "StChain.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_dst_dedx_filler_Module.h"
#include "global/St_fill_ftpc_dst_Module.h"
#include "global/St_dst_monitor_soft_filler_Module.h"

#include "global/St_particle_dst_filler_Module.h"
#include "global/St_dst_point_filler_Module.h"
#include "global/St_fill_dst_event_summary_Module.h"
#include "St_dst_summary_param_Table.h"
#include "St_dst_run_summary_Table.h"

static const char rcsid[] = "$Id: St_dst_Maker.cxx,v 1.25 1999/09/13 15:05:29 caines Exp $";
ClassImp(St_dst_Maker)
  
  //_____________________________________________________________________________
  St_dst_Maker::St_dst_Maker(const char *name):StMaker(name){
  fSelect = 0;
}
//_____________________________________________________________________________
St_dst_Maker::~St_dst_Maker(){
}

//_____________________________________________________________________________
Int_t St_dst_Maker::Init(){
  static const char *todst[] = {
    "match:",  "globtrk", "globtrk_aux",
    "primary:","globtrk2", "primtrk", "primtrk_aux", "vertex",
    "v0:",     "dst_v0_vertex","ev0_eval",
    "xi:",     "dst_xi_vertex",
    "kink:",   "kinkVertex",
    "geant:",  "particle", "g2t_rch_hit",
    "trg:",    "TrgDet",
    0};
  
  if (!fSelect) fSelect = todst;   
  
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
  {0.,0.,0.,0.,0.,0.}, //float  eta_bins[6];     /* Pseudorapidity bins, lower->upper ranges */
  {0.,0.,0.,0.,0.,0.}, //float  pt_bins[6];      /* Trans. momen. bins, lower->upper ranges  */
  {0.,0.,0.,0.,0.,0.}, //float  mt_bins[6];      /* Trans. mass bins, lower->upper ranges    */
  0,         //long   n_phi_bins;      /* # of phi bins, start at phi=0, max # is 8     */
  {0.,0.},   //float  mean_eta[2];     /* Mean and std.dev. of <eta> for all events     */
  {0.,0.},   //float  mean_pt[2];      /* Mean and std.dev. of <pt> for all events      */
  { 0, 0},   //long   multiplicity[2]; /* Mean and std.dev. of total chrg. mult.        */
  { 0, 0},   //long   num_vert[2];     /* Mean and std.dev. of total # vertices         */
  { 0, 0},   //long   energy_emc[2];   /* Mean and std.dev. of total energy in EMC      */
 };


  run_summary.version[0]=0;
  strncat (run_summary.version,GetCVS(),sizeof(run_summary.version)-1); // DST production software version
  dst_run_summary->AddAt(&run_summary,0);
 
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Make(){

  St_DataSetIter local(m_DataSet);
  St_DataSet *dst = local.Mkdir("dst");
  St_DataSetIter dstI(dst);
  St_DataSet *ds=0,*mk=0;
  const char *name,*mkname;
  
  
  for (int idst=0; (name=fSelect[idst]); idst++) {
    
    if (strchr(name,':')) {
      mkname = name; mk = GetInputDS(name); continue;}
    
    if (!mk) continue;
    ds = mk->Find(name);
    if (!ds) continue;
    dst = m_DataSet;
    if (strcmp(name,"dst")) dst = m_DataSet->Find("dst");
    if (dst) ds->Shunt(dst);
    if (Debug()) {
      gMessMgr->Debug() << "\n*** <" << ClassName() << "::Make> *** selected ";
      *gMessMgr << mkname << name << endm;}
  }
  return Filler();
}
//_____________________________________________________________________________
Int_t  St_dst_Maker::Filler(){
  St_DataSetIter dstI(m_DataSet);
  St_DataSet *dst = dstI.Cd("dst");
  if (!dst) return kStWarn;
  int iMake = kStOK;
  int iRes = 0;
  St_dst_track     *globtrk     = (St_dst_track *)     dstI("globtrk");
  St_dst_track_aux *globtrk_aux = (St_dst_track_aux *) dstI("globtrk_aux");
  St_dst_vertex    *vertex      = (St_dst_vertex *)    dstI("vertex");    
  St_svm_evt_match *evt_match   = (St_svm_evt_match *) dstI("evt_match");
  St_dst_run_summary *dst_run_summary = (St_dst_run_summary   *) m_ConstSet->Find("dst_run_summary");
  
  St_dst_event_header  *event_header  = new St_dst_event_header("event_header",1);
  dstI.Add(event_header);
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
  St_dst_event_summary *event_summary = new St_dst_event_summary("event_summary",1);
  dstI.Add(event_summary);
 
  // Get TPC and SVT points 
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  St_tcl_tpcluster *tpcluster = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
    tpcluster = (St_tcl_tpcluster *) tpchits->Find("tpcluster");
  }
  if (! tphit)        {tphit     = new St_tcl_tphit("tphit",1);         AddGarb(tphit);    }
  if (! tpcluster)    {tpcluster = new St_tcl_tpcluster("tpcluster",1); AddGarb(tpcluster);}

  St_scs_spt     *scs_spt     = 0;
  St_scs_cluster *scs_cluster = 0;
  
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
    scs_cluster = (St_scs_cluster *) svthits->Find("scs_cluster");
  }
  if (! scs_cluster) {scs_cluster = new St_scs_cluster("scs_cluster",1); AddGarb(scs_cluster);}
  if (!scs_spt)      {scs_spt     = new St_scs_spt("scs_spt",1);         AddGarb(scs_spt);}
  
  if(Debug()) gMessMgr->Debug() << " run_dst: Calling dst_point_filler" << endm;
  // dst_point_filler
  St_DataSet *ftpc_hits   = GetInputDS("ftpc_hits");
  St_fcl_fppoint *fcl_fppoint = 0;
  if (ftpc_hits) fcl_fppoint = (St_fcl_fppoint *) ftpc_hits->Find("fcl_fppoint");
  if (!fcl_fppoint) {fcl_fppoint = new St_fcl_fppoint("fcl_fppoint",1); AddGarb(fcl_fppoint);}
  Int_t no_of_points    = tphit->GetNRows() + scs_spt->GetNRows() + fcl_fppoint->GetNRows();
  St_dst_point   *point = 0;
  if (no_of_points > 0) { 
    point = new St_dst_point("point",no_of_points);  dstI.Add(point);
    iRes = dst_point_filler(tphit, scs_spt, point);
  //	   ========================================
    if (iRes !=kSTAFCV_OK) {
      iMake = kStWarn;
      gMessMgr->Warning() << "Problem on return from DST_POINT_FILLER" << endm;
      if(Debug()) gMessMgr->Debug() << " run_dst: finished calling dst_point_filler" << endm;
    }
  }
  // dst_dedx_filler
  St_dst_dedx       *dst_dedx    = 0;
  St_dst_dedx   *tpc_dedx = 0;
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    tpc_dedx   = (St_dst_dedx  *) tpc_tracks("tpc_dedx");
  }
  // Case tpc not there
  if (!tptrack) {tptrack = new St_tpt_track("tptrack",1); AddGarb(tptrack);}
  if (!tpc_dedx) {tpc_dedx = new St_dst_dedx("tpc_dedx",1); AddGarb(tpc_dedx);}


 // 			Case running est tpc -> Si space point tracking
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_stk_track   *stk_track = 0;
  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
  }
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}

  if(Debug()) gMessMgr->Debug() << " run_dst: Calling dst_dedx_filler" << endm;
  dst_dedx = new St_dst_dedx("dst_dedx",20000); dstI.Add(dst_dedx);
 
  iRes = dst_dedx_filler(tptrack,stk_track,tpc_dedx,dst_dedx);
    //     ===========================================
  if (iRes !=kSTAFCV_OK) {
    iMake = kStWarn;
    gMessMgr->Warning() << "Problem on return from DST_DEDX_FILLER" << endm;
    if(Debug()) gMessMgr->Debug() << " run_dst: finshed calling dst_dedx_filler" << endm;
  }
  
  St_DataSet *ftpc_tracks = GetInputDS("ftpc_tracks");
  St_fpt_fptrack *fpt_fptrack = 0;
  if (ftpc_tracks)  fpt_fptrack = (St_fpt_fptrack *) ftpc_tracks->Find("fpt_fptrack");
  if (point && fcl_fppoint &&  fpt_fptrack) {
    if(Debug()) gMessMgr->Debug()<<" run_dst: Calling fill_ftpc_dst"<<endm;
    Int_t No_of_Tracks = globtrk->GetNRows() + fpt_fptrack->GetNRows();
    globtrk->ReAllocate(No_of_Tracks);
    globtrk_aux->ReAllocate(No_of_Tracks);
    dst_dedx->ReAllocate(No_of_Tracks);
    iRes = fill_ftpc_dst(fpt_fptrack, fcl_fppoint, globtrk,
                         globtrk_aux,point,vertex,dst_dedx);
    //             ==========================================================
    if (iRes != kSTAFCV_OK) {
      iMake = kStWarn;
      gMessMgr->Warning() << "Problem on return from FILL_FTPC_DST" << endm;
      if(Debug()) gMessMgr->Debug() << " run_dst: finished calling fill_ftpc_dst" << endm;
    }
  }
  
 // dst_mon_soft
  if (!fpt_fptrack) {fpt_fptrack = new St_fpt_fptrack("fpt_fptrack",1); AddGarb(fpt_fptrack);}
  if (!evt_match)   {evt_match   = new St_svm_evt_match("evt_match",1); AddGarb(evt_match);}
  St_DataSet *ctf = GetInputDS("ctf");
  St_ctu_cor *ctb_cor = 0;
  if (!ctf) {
    if(Debug()) gMessMgr->Debug() << "St_ctf_Maker has not been called " << endm;
  } else {
    ctb_cor = (St_ctu_cor *)ctf->Find("ctb_cor"); 
  }
  if (! ctb_cor) {ctb_cor = new St_ctu_cor("ctb_cor",1); AddGarb(ctb_cor);}
 
  if(Debug()) gMessMgr->Debug() << " run_dst: Calling dst_monitor_soft_filler" << endm;
    
  St_dst_monitor_soft  *monitor_soft = new St_dst_monitor_soft("monitor_soft",1);
  dstI.Add(monitor_soft);
  iRes = dst_monitor_soft_filler(tpcluster,
				 scs_cluster,
				 tphit,scs_spt,fcl_fppoint,
				 tptrack,stk_track,fpt_fptrack,
				 evt_match,
				 ctb_cor,vertex,event_summary,monitor_soft);
    //     ===========================================================================
  if (iRes !=kSTAFCV_OK) {
    iMake = kStWarn;
    gMessMgr->Warning() << "Problem on return from DST_MONITOR_SOFT_FILLER" << endm;
    if(Debug()) gMessMgr->Debug() << " run_dst: finished calling dst_monitor_soft_filler" << endm;
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
    // count printable chars
    const Char_t *type = evthdr->GetEventType();
    Int_t n=0;
    for (Int_t i=0;i<80;i++) {if (isprint(type[i])) n++; else break;} 
    if (n>0) strncpy (run.event_type,type,n);
    run.sqrt_s = evthdr->GetCenterOfMassEnergy();
    run.east_a = evthdr->GetAEast();
    run.west_a = evthdr->GetAWest();
    run_header->AddAt(&run,0);
  }
  if (summary_param && run_header) {
    if(Debug()) gMessMgr->Debug() << " run_dst: Calling fill_dst_event_summary" << endm;
    
    iRes = fill_dst_event_summary(summary_param, run_header, event_header,
                                  globtrk,vertex,event_summary);
    //     ================================================================
    
    if (iRes !=kSTAFCV_OK) {
      gMessMgr->Warning() << "Problem on return from FILL_DST_EVENT_SUMMARY" << endm;
      iMake = kStWarn;
    }
    if(Debug()) gMessMgr->Debug() << " run_dst: finished calling fill_dst_event_summary" << endm;
  }
  return kStOK;
}
