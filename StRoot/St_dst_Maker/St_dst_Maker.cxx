// $Id: St_dst_Maker.cxx,v 1.90 2009/08/03 23:35:45 fine Exp $
// $Log: St_dst_Maker.cxx,v $
// Revision 1.90  2009/08/03 23:35:45  fine
// Silence the compilation warning
//
// Revision 1.89  2009/04/08 21:06:10  fine
// Fix the crash during debug print
//
// Revision 1.88  2007/04/28 17:55:49  perev
// Redundant StChain.h removed
//
// Revision 1.87  2007/01/25 19:04:45  perev
// GMT fix
//
// Revision 1.86  2007/01/24 21:35:54  perev
// GMT conversion fixed
//
// Revision 1.85  2005/10/06 20:21:28  fisyak
// Protection versus division by 0
//
// Revision 1.84  2005/07/23 02:50:35  perev
// array replaced by TArray
//
// Revision 1.83  2005/07/20 19:13:33  perev
// Cleanup
//
// Revision 1.82  2005/06/15 01:22:58  caines
//  Setup stuff for flagging hits used in fit of traack
//
// Revision 1.81  2004/05/24 13:49:04  jcs
// delete creation of St_dst_mon_soft_ftpc table - StEvent/StFtpcSoftwareMonitor filled directly
//
// Revision 1.80  2004/05/11 17:18:39  jcs
//  create an empty ftpc monitor soft table which will be filled in StFtpcTrackMaker.cxx
//
// Revision 1.79  2004/05/11 15:44:05  jcs
// remove filling of dst_mon_soft_ftpc table, information now stored directly into StEvent
// in StFtpcTrackMaker
//
// Revision 1.78  2004/05/03 23:32:11  perev
// Possible non init WarnOff
//
// Revision 1.77  2004/03/02 16:12:46  jcs
// Get dst_mon_soft_ftpc table from fglobal
// Create the table with all entries set to zero is it doesn't exist
//
// Revision 1.76  2003/09/02 17:59:26  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.75  2003/04/10 22:28:02  caines
// Fixing SVT for runs where the SVT oscilates in and out for different events
//
// Revision 1.74  2003/04/05 22:36:25  caines
// Fix filling on local coords so its time and anode not cm
//
// Revision 1.73  2002/05/16 01:59:19  caines
// Send in differnt group tables for the TPC and est refit so flagging of hits correct
//
// Revision 1.72  2002/05/09 22:49:19  caines
// Dont save svt hit info on events when SVT not there
//
// Revision 1.71  2002/04/17 23:56:43  jeromel
// Changes by Helen for the SVT in egr implementation.
//
// Revision 1.70  2002/04/06 03:09:23  jeromel
// Valeri found one biggy buggy causing crashes (pointer <-> array range)
//
// Revision 1.69  2002/02/22 01:43:08  caines
// Correct track-hit correlations for SVT
//
// Revision 1.68  2002/02/03 00:35:14  caines
// Remove annoying printout
//
// Revision 1.67  2001/10/26 16:49:28  caines
// Temporarily feed nanodes and npixels into the SVT hit errors
//
// Revision 1.66  2001/10/05 14:34:02  caines
// Fixed used in fit TPC hit
//
// Revision 1.65  2001/09/25 21:05:03  caines
// Improve dealing of SVT hits and still not overwrite FTPC
//
// Revision 1.64  2001/09/22 18:54:19  genevb
// Special handling of FTPC points
//
// Revision 1.63  2001/09/21 21:06:39  jcs
// Set HitIndex correctly
//
// Revision 1.62  2001/09/06 15:46:55  caines
// Indexing out by 1 for SVT
//
// Revision 1.61  2001/08/24 21:00:47  caines
// Write out correct index for Hyb. no matter what swapping went on
//
// Revision 1.60  2001/08/08 22:37:44  caines
// IMprove packing of peakvalue for svt
//
// Revision 1.59  2001/08/07 20:51:23  caines
// Implement better packing of svt hardware and charge values
//
// Revision 1.58  2000/11/29 14:15:14  fisyak
// Remove St_dst_dedx_filler_Module, Thanks Janet
//
// Revision 1.57  2000/11/25 23:21:55  fisyak
// Add dEdx maker to production chain
//
// Revision 1.56  2000/09/09 18:09:21  fisyak
// Janet Seyboth corrections
//
// Revision 1.55  2000/09/02 22:47:29  fisyak
// Accout the fact that event_header might be created in St_trg_Maker
//
// Revision 1.54  2000/09/01 13:27:50  fisyak
// Fix EventHeader
//
// Revision 1.53  2000/08/31 23:04:39  lbarnby
// get event_header table from trigger instead of creating it
//
// Revision 1.52  2000/08/31 03:44:45  lbarnby
// A more useful time stored in the event header now
//
// Revision 1.51  2000/08/07 14:39:41  caines
// Add to dst a copy of tpc and svt tracks called CpyTrk
//
// Revision 1.50  2000/07/29 00:16:19  margetis
// put information about whether a point was used in fit or not
//
// Revision 1.49  2000/06/26 22:13:18  fisyak
// remove params
//
// Revision 1.48  2000/05/12 20:01:12  lbarnby
// StRoot/St_dst_Maker
//
// Revision 1.47  2000/05/08 15:38:28  lbarnby
// Added l3Dedx table to dst
//
// Revision 1.46  2000/04/13 21:48:08  lbarnby
// No longer write particle and g2t_rch_hit tables to dst branch
//
// Revision 1.45  2000/03/01 14:48:09  caines
// Removed references to scs_cluster
//
// Revision 1.44  2000/02/05 19:06:33  lbarnby
// Added L2_Trigger and L2_Trigger to DST
//
// Revision 1.43  2000/01/31 20:12:14  fisyak
// Add rch:dst_rch_pixel for Lee
//
// Revision 1.42  2000/01/26 19:55:58  lbarnby
// Write out L0_Trigger table to DST
//
// Revision 1.41  2000/01/06 16:53:05  lbarnby
// fix warnings (Linux)
//
// Revision 1.40  1999/12/08 21:10:36  lbarnby
// bug fix: pointer to summary_param now obtained correctly as it is in different dir.
//
// Revision 1.39  1999/12/02 23:10:03  lbarnby
// put summary_param into runco branch
//
// Revision 1.38  1999/11/29 20:20:29  fisyak
// Janet changes due to ftpc
//
// Revision 1.36  1999/11/19 14:03:44  fisyak
// Missed dst lavel
//
// Revision 1.35  1999/11/18 23:42:30  lbarnby
// Allow l3Hit table to be written out when l3Clufi is run
//
// Revision 1.34  1999/11/18 16:49:30  fisyak
// Janet Seyboth fix when there is no globtrk table
//
// Revision 1.33  1999/11/17 15:22:53  fisyak
// Add soft monitor tables removed from dst_monitor_soft_filler
//
// Revision 1.32  1999/11/15 21:54:24  lbarnby
// changes in idl file to fix St_dst_Maker
//
// Revision 1.31  1999/11/15 17:44:03  lbarnby
// Add ctb,emc,l3,rich monitor soft tables (empty for now)
//
// Revision 1.30  1999/10/25 21:53:04  wdeng
// Use shorter names for monitoring tables
//
// Revision 1.29  1999/10/19 00:11:30  fisyak
// Remove aux tables
//
// Revision 1.28  1999/10/01 17:07:24  wdeng
// Accommodate new dst tables. Check it in though I am not sure what I did.
//
// Revision 1.27  1999/09/24 01:23:36  fisyak
// Reduced Include Path
//
// Revision 1.26  1999/09/17 21:28:11  fisyak
// Add l3Track to dst
//
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


#include <Stiostream.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "TClass.h"
#include "TMath.h"

#include "TUnixTime.h"
#include "St_dst_Maker.h"

#include "tables/St_particle_Table.h"
#include "tables/St_hepe_gent_Table.h"

#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "global/St_dst_monitor_soft_filler_Module.h"

#include "global/St_particle_dst_filler_Module.h"
#include "global/St_dst_point_filler_Module.h"
#include "global/St_fill_dst_event_summary_Module.h"
#include "tables/St_dst_summary_param_Table.h"
#include "tables/St_dst_run_summary_Table.h"
#include "tables/St_dst_mon_soft_ctb_Table.h"
#include "tables/St_dst_mon_soft_emc_Table.h"
#include "tables/St_dst_mon_soft_l3_Table.h"
#include "tables/St_dst_mon_soft_rich_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_sgr_groups_Table.h"

#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClusterMaker/StSvtAnalysedHybridClusters.hh"
enum EChainBits {  
  kIsCalibrated = BIT(24)   // if the TObject has been created after calibration 
};


static const char rcsid[] = "$Id: St_dst_Maker.cxx,v 1.90 2009/08/03 23:35:45 fine Exp $";
ClassImp(St_dst_Maker)
  
  //_____________________________________________________________________________
  St_dst_Maker::St_dst_Maker(const char *name):
StMaker(name)
{
  fSelect = 0;
}
//_____________________________________________________________________________
St_dst_Maker::~St_dst_Maker(){
}

//_____________________________________________________________________________
Int_t St_dst_Maker::Init(){
  static const char *todst[] = {
    "match:",  "globtrk","CpyTrk","EstGlobal",
    "fglobal:", "point","dst_dedx",
    "primary:","globtrk2", "primtrk", "vertex", "EstPrimary", 
    "v0:",     "dst_v0_vertex","ev0_eval",
    "xi:",     "dst_xi_vertex",
    "kink:",   "kinkVertex",
    "rch:",    "dst_rch_pixel",
    "trg:",    "TrgDet", "L0_Trigger","L1_Trigger","L2_Trigger","event_header",
    "l3Tracks:","l3Track","l3Dedx",
    "l3Clufi:","l3Hit",
    0};
  
  if (!fSelect) fSelect = todst;   
  
 // Run summary
  m_dst_summary_param = new St_dst_summary_param("summary_param",1);
  AddRunco(m_dst_summary_param);
  dst_summary_param_st dst_summary_param;
  //  dst_summary_param.bfc_run_id =  0;
  dst_summary_param.eta_bins[0] = -2.;
  dst_summary_param.eta_bins[1] = -1.;
  dst_summary_param.eta_bins[2] = -.5;
  dst_summary_param.eta_bins[3] =  .5;
  dst_summary_param.eta_bins[4] =  1.;
  dst_summary_param.eta_bins[5] =  2.;
  dst_summary_param.eta_bins[6] =  2.;
  dst_summary_param.eta_bins[7] =  2.;
  dst_summary_param.eta_bins[8] =  2.;
  
  dst_summary_param.pt_bins[0] = .1;
  dst_summary_param.pt_bins[1] = .15;
  dst_summary_param.pt_bins[2] = .2;
  dst_summary_param.pt_bins[3] = .3;
  dst_summary_param.pt_bins[4] = .5;
  dst_summary_param.pt_bins[5] = 1.;
  dst_summary_param.pt_bins[6] = 1.;
  dst_summary_param.pt_bins[7] = 1.;
  dst_summary_param.pt_bins[8] = 1.;

  dst_summary_param.phi_bins[0] = 0.;
  dst_summary_param.phi_bins[1] = 36.;
  dst_summary_param.phi_bins[2] = 72.;
  dst_summary_param.phi_bins[3] = 108.;
  dst_summary_param.phi_bins[4] = 144.;
  dst_summary_param.phi_bins[5] = 180.;
  dst_summary_param.phi_bins[6] = 216.;
  dst_summary_param.phi_bins[7] = 252.;
  dst_summary_param.phi_bins[8] = 288.;
  dst_summary_param.phi_bins[9] = 324.;
    
  m_dst_summary_param->AddAt(&dst_summary_param,0);
  St_run_header *run_header = new St_run_header("run_header",1);
  AddConst(run_header);

  St_dst_run_summary *dst_run_summary = new St_dst_run_summary("dst_run_summary",1);
  AddConst(dst_run_summary);
  //dst_run_summary_st run_summary;

#if 0
  // Spiros says he initialize the table in his filler. I am not sure what should be done here. 
  // So just comment it out. -Wensheng
  // comment-out begins
  if(0) {
  dst_run_summary_st run_summary = 
{
  0,     // long   bfc_run_id;      /* Unique BFC run ID, F.key to run_header          */
  0,     // long   n_events_tot;    /* Total number of events in the BFC prod. run     */
  0,     // long   n_events_good;   /* Total number events successfully processed      */
  {0,0}, // long   date[2];         /* Start/stop date for processing                  */
  {0,0}, // long   time[2];         /* Start/stop time of day (sec)                    */
  0,     // float  cpu_total;       /* Total cpu sec for production run                */
  0,     // float  east_pol_L;      /* Avg magnitude of east beam Longitudinal Pol     */
  0,     // float  east_pol_T;      /* Avg magnitude of east beam Transverse Pol       */
  0,     // float  west_pol_L;      /* Avg magnitude of west beam Longitudinal Pol     */
  0,     // float  west_pol_T;      /* Avg magnitude of west beam Transverse Pol       */
  0,     // float  luminosity;      /* Avg luminosity during experiment for events     */
                                    /* in production run, 1.0/[cm^2 sec]               */
  {0,0}, // float  eta[2];          /* Mean and std.dev. of &lt;eta&gt; for all events */
  {0,0}, // float  pt[2];           /* Mean and std.dev. of &lt;pt&gt; for all events  */
  {0,0}, // float  num_vert[2];     /* Mean and std.dev. of total # vertices           */
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, 
         // float  mean_mult[30];   /* Mean multiplicity (energy) per detector for run */ 
  {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} 
        // float  rms_mult[30];    /* RMS multiplicity (energy) per detector for run  */
 };

  run_summary.version[0]=0;
  strncat (run_summary.version,GetCVS(),sizeof(run_summary.version)-1); // DST production software version
  dst_run_summary->AddAt(&run_summary,0);
  }
  // comment-out ends
#endif
 
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Make(){

  St_DataSetIter local(m_DataSet);
  St_DataSet *dst = local.Mkdir("dst");
  St_DataSetIter dstI(dst);
  St_DataSet *ds=0,*mk=0;
  const char *name=0,*mkname=0;
  
  for (int idst=0; (name=fSelect[idst]); idst++) {
    
    if (strchr(name,':')) {
      mkname = name; mk = GetInputDS(name); continue;}
    
    if (!mk) continue;
    ds = mk->Find(name);
    if (!ds) continue;
    dst = m_DataSet;
    if (strcmp(name,"dst")) dst = m_DataSet->Find("dst");
    if (dst) ds->Shunt(dst);
    LOG_DEBUG << "*** <" << ClassName() << "::Make> *** selected "
                << mkname << name << endm;
  }
  return Filler();
}
//_____________________________________________________________________________
Int_t  St_dst_Maker::Filler(){
  St_DataSetIter dstI(m_DataSet);
  St_DataSet *dst = dstI.Cd("dst");
  
  St_DataSet *match = GetDataSet("match"); 
  St_DataSetIter matchI(match);         
  

  if (!dst) return kStWarn;
  int iMake = kStOK;
  int iRes = 0;

  St_dst_track     *globtrk     = (St_dst_track *)     dstI("globtrk");
  // If no TPC/SVT globtrk table, get FTPC globtrk table if it exists
  // otherwise create an empty globtrk table
  if (!globtrk) {
     St_DataSet *ds=0, *mk=0;
     mk = GetInputDS("fglobal");
     if (mk) { 
        ds = mk->Find("globtrk");
        if (ds) {
           ds->Shunt(dst);
           globtrk = (St_dst_track *) dstI("globtrk");
        }
     } 
     if(!globtrk) {globtrk = new St_dst_track("globtrk",1);AddGarb(globtrk);}
  }


  St_dst_vertex    *vertex      = (St_dst_vertex *)    dstI("vertex");    
  //Make empty vertex table if none exists
  if(!vertex) {vertex = new St_dst_vertex("vertex",1);AddGarb(vertex);}

  St_svm_evt_match *evt_match   = (St_svm_evt_match *) dstI("evt_match");
  //St_dst_run_summary *dst_run_summary = (St_dst_run_summary   *) m_ConstSet->Find("dst_run_summary");
  
  St_event_header  *event_header = (St_event_header *) dst->Find("event_header");
  if (!event_header){
    event_header  = new St_event_header("event_header",1);
    dstI.Add(event_header);
  }
  event_header->SetNRows(1);
  event_header_st  event =   {"Collision", //event_type
                              0,           // n_event
                              0, 0, 0, 0, // exp_run_id,time,trig_mask,bunch_cross
			      {0,0}}; // bunchXing
  if (GetEventType()) strcpy (&event.event_type[0],GetEventType());
  event.n_event    = GetEventNumber();
  event.exp_run_id = GetRunNumber();
  if(event_header){
    St_event_header &EventHeader = *event_header;
    event.bunchXing[0] = EventHeader[0].bunchXing[0];
    event.bunchXing[1] = EventHeader[0].bunchXing[1];
  }

  event.time = TUnixTime::Convert(GetDateTime(),1);
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
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  
  if (!scs_spt)      {scs_spt     = new St_scs_spt("scs_spt",1);         AddGarb(scs_spt);}
   
  St_sgr_groups     *tpc_groups = (St_sgr_groups *) matchI("tpc_groups");
  if (! tpc_groups)    {tpc_groups = new St_sgr_groups("tpc_groups",1); AddGarb(tpc_groups);} 

  St_sgr_groups    *svt_groups     = 0;
  St_DataSet     *estI  = GetInputDS("est");
  if( estI) {    svt_groups = (St_sgr_groups *) estI->Find("EstGroups");}
  if (! svt_groups)    {svt_groups = new St_sgr_groups("EstGroups",1); AddGarb(svt_groups);} 
 
  if(Debug()) gMessMgr->Debug() << " run_dst: Calling dst_point_filler" << endm;

  scs_spt_st* sgroups = scs_spt->GetTable();

  TArrayI svtIndex(10000);  int *svtindex= svtIndex.GetArray(); 

  for(int i=0; i<scs_spt->GetNRows(); i++,sgroups++){
    assert(sgroups->id >=0);
    if (sgroups->id >= svtIndex.GetSize()) {
        svtIndex.Set(Int_t(svtIndex.GetSize()*1.5));
        svtindex= svtIndex.GetArray();} 
    svtindex[sgroups->id] = sgroups->id_globtrk;
  }
 // Get pointer to svt cluster analysis 

    StSvtAnalysedHybridClusters *mSvtBigHit;
    StSvtHybridCollection *mSvtCluColl=0;
    St_DataSet *dataSetSvt =0;
    dataSetSvt = GetDataSet("StSvtAnalResults");
    
    // set this scs_spt nrows to zero and fill dst_point from svt collection
    
    int NSvtPoints = scs_spt->GetNRows();
    scs_spt->SetNRows(0);
    
  // dst_point_filler
  Int_t no_of_points    = tphit->GetNRows() + scs_spt->GetNRows();
  Int_t no_ftpc_points  = 0;
  // If FTPC in chain, a point table already exists
  St_dst_point   *point = 0;
  if (no_of_points > 0 || dataSetSvt ) { 
    point     = (St_dst_point *)     dstI("point");
    if (!point) {
       point = new St_dst_point("point",no_of_points);  dstI.Add(point);
    } else {
       no_ftpc_points = point->GetNRows();
       point->ReAllocate(no_ftpc_points + no_of_points);
    }
    if (GetMaker("tpc_raw")) point->SetBit(kIsCalibrated); // mark that tpc_raw made calibration
 
    iRes = dst_point_filler(tphit, scs_spt, point);

  //	   ========================================
    if (iRes !=kSTAFCV_OK) {
      iMake = kStWarn;
      gMessMgr->Warning() << "Problem on return from DST_POINT_FILLER" << endm;
      if(Debug()) gMessMgr->Debug() << " run_dst: finished calling dst_point_filler" << endm;
    }
          // Fill 'used in the fit' info


 
    //dst_point_st *mypoint  = point->GetTable();
    dst_point_st mypoint;                               // will use local dst_point
    memset(&mypoint,0,sizeof(mypoint));
    Int_t        nsize=point->GetNRows() + NSvtPoints;  // new expected size
    point->ReAllocate(nsize);                           // realloc to save time
   
    const float maxRange   = 22;
    const float mapFactor  = 23800;
    unsigned int svty11,svtz,svtx,svty10,svty;
    double cov;
    int index, index2=-1;

    //  pack SVT info into dst_point
 
    if( dataSetSvt)
      mSvtCluColl = (StSvtHybridCollection*)(dataSetSvt->GetObject());
    if(mSvtCluColl && NSvtPoints>0){
      
      for(int barrel = 1;barrel <= mSvtCluColl->getNumberOfBarrels();barrel++) {
	
	for (int ladder = 1;ladder <= mSvtCluColl->getNumberOfLadders(barrel);ladder++) {
	  
	  for (int wafer = 1;wafer <= mSvtCluColl->getNumberOfWafers(barrel);wafer++) {
	    
	    for (int hybrid = 1;hybrid <=mSvtCluColl->getNumberOfHybrids();hybrid++){
	      index2++;
	      index = mSvtCluColl->getHybridIndex(barrel,ladder,wafer,hybrid);
	      if(index < 0) continue;
	      
	      mSvtBigHit = (StSvtAnalysedHybridClusters*)mSvtCluColl->at(index);
	      if( !mSvtBigHit) continue;
	      
	      for( int clu=0; clu<mSvtBigHit->numOfHits(); clu++){
                StSvtHitData          *dat = mSvtBigHit->svtHitData()	+clu;
		StThreeVector<double> *waf = mSvtBigHit->WaferPosition()+clu;
		StSvtHit              *hit= mSvtBigHit->svtHit()	+clu;
		//		if( hit->flag() > 3 ||
		//   dat->peakAdc < 15 ) continue;
		
		mypoint.hw_position = 2;
		mypoint.hw_position += (1L<<4)*(index2);
		svtx = int(waf->x()*4);
		
		mypoint.hw_position += (1L<<13)*(svtx);
		
		svty = int(waf->y()*4);
		mypoint.hw_position += (1L<<22)*svty;
		
		if( hit->charge() < (1L<<10)){
		  mypoint.charge = (int)hit->charge();
		}
		else 
		  mypoint.charge = (1L<<10)-1;
		
		if( dat->peakAdc < (1L<<7)){
		  mypoint.charge +=  
		    dat->peakAdc*(1L<<10);
		}		  
		else  
		  mypoint.charge += (1L<<10)*((1L<<7)-1);
		
		mypoint.charge += (1L<<17)*hit->flag();
		
		if( hit->position().x() > (-1*maxRange) &&
		    hit->position().x() < maxRange)
		  svtx = int(mapFactor*(mSvtBigHit->svtHit()[clu]
					.position().x() + maxRange));
		else svtx = 0;
		
		if( hit->position().y() > (-1*maxRange) &&
		    hit->position().y() < maxRange)
		  svty = int(mapFactor*(mSvtBigHit->svtHit()[clu]
					.position().y() + maxRange));
		else svty = 0;
		
		if( hit->position().z() > (-1*maxRange) &&
		    hit->position().z() < maxRange)
		  svtz = int(mapFactor*(mSvtBigHit->svtHit()[clu]
					.position().z() + maxRange));
		else svtz = 0;
		svty10 = int(svty/(1L<<10));
		svty11 = svty - (1L<<10)*svty10;
		
		mypoint.id_simtrk = hit->idTruth();
		mypoint.id_quality= hit->qaTruth();
		mypoint.position[0] = svtx + (1L<<20)*svty11;
		mypoint.position[1] = svty10 + (1L<<10)*svtz; 
		
		
		//cov =  hit->positionError().x()
		  //  *hit->positionError().x();
		
		  // Temporarily fill with No. anodes and No. pixels
		  
		cov = 999.;
		if (dat->numOfAnodesInClu > 0) cov = 1./(100*(float)dat->numOfAnodesInClu);
		if( cov > 0.0 && cov < (1.0/float((1L<<6))))
		  svtx = int((1L<<26)*cov);
		else  svtx = 0;
		
		  //	cov =  hit->positionError().y()
		  // *hit->positionError().y();
		cov = 999.;
		if (dat->numOfPixelsInClu > 0) 	cov = 1./(100*(float)dat->numOfPixelsInClu);
		if( cov > 0.0 && cov <  (1.0/float((1L<<6))))
		  svty = int((1L<<26)*cov);
		else  svty = 0;
		
		cov =  hit->positionError().z()
		  *hit->positionError().z();
		if( cov > 0.0 && cov <  (1.0/float(1L<<6)))
		  svtz = int((1L<<26)*cov);
		else  svtz = 0;
		
		svty10 = int(svty/(1L<<10));
		svty11 = svty - (1L<<10)*svty10;
		
		mypoint.pos_err[0] = svtx + (1L<<20)*svty11;
		mypoint.pos_err[1] = svty10 + (1L<<10)*svtz;
		mypoint.id_track = 0;
                int idx = dat->id;
                assert(idx < svtIndex.GetSize());
		mypoint.id_track  = svtindex[idx];
		// Add the new point ; if allready allocated, will just add
		// if realloc required, this method will increase the table
		// size accordingly.
		point->AddAt(&mypoint);
	      }
	    }
	  }
	}
      }
    } // End of if mSvtCluColl
  
    //  Sanity check
    if(point->GetNRows() > nsize){
      gMessMgr->Warning() << "St_dst_Maker::Filler : New size " << point->GetNRows()
			  << " exceeds expected size " << nsize << " for table "
	                  << point->GetName() << endm;
    }


    int  spt_id = 0;

    sgr_groups_st *tgroup     =  tpc_groups->begin();
    sgr_groups_st *tgroup_end =  tpc_groups->end();
    
    for( ; tgroup != tgroup_end; tgroup++){
      if( (*tgroup).id1 != 0 && (*tgroup).ident >= 0){
	spt_id = (*tgroup).id2-1;
	if( spt_id <0) {
	  cout << spt_id << endl;
	  assert(0);
	} else{
	  point->GetTable(no_ftpc_points+spt_id)->charge |= 1UL<<24;
	}
      }
    }
  
      
      
  } //if #points>0

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
  // If FTPC in chain, dst_dedx table already exists
  dst_dedx     = (St_dst_dedx *)     dstI("dst_dedx");
  if (!dst_dedx) {
     dst_dedx = new St_dst_dedx("dst_dedx",20000); dstI.Add(dst_dedx);
  }
  else {
     dst_dedx->ReAllocate(20000);
  }
#if 0 
  iRes = dst_dedx_filler(tptrack,stk_track,tpc_dedx,dst_dedx);
    //     ===========================================
  if (iRes !=kSTAFCV_OK) {
    iMake = kStWarn;
    gMessMgr->Warning() << "Problem on return from DST_DEDX_FILLER" << endm;
    if(Debug()) gMessMgr->Debug() << " run_dst: finshed calling dst_dedx_filler" << endm;
  }
#endif
  
 // dst_mon_soft
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
    
  St_dst_mon_soft_glob *mon_soft_glob = new St_dst_mon_soft_glob("mon_soft_glob",1);
  St_dst_mon_soft_svt  *mon_soft_svt  = new St_dst_mon_soft_svt("mon_soft_svt",1);
  St_dst_mon_soft_tpc  *mon_soft_tpc  = new St_dst_mon_soft_tpc("mon_soft_tpc",1);
  St_dst_mon_soft_ctb  *mon_soft_ctb  = new St_dst_mon_soft_ctb("mon_soft_ctb",1);
  St_dst_mon_soft_emc  *mon_soft_emc  = new St_dst_mon_soft_emc("mon_soft_emc",1);
  St_dst_mon_soft_l3  *mon_soft_l3  = new St_dst_mon_soft_l3("mon_soft_l3",1);
  St_dst_mon_soft_rich  *mon_soft_rich  = new St_dst_mon_soft_rich("mon_soft_rich",1);
  dstI.Add(mon_soft_glob);
  dstI.Add(mon_soft_svt);
  dstI.Add(mon_soft_tpc);
  dstI.Add(mon_soft_ctb);
  dstI.Add(mon_soft_emc);
  dstI.Add(mon_soft_l3);
  dstI.Add(mon_soft_rich);

  //Make (empty) ctb,emc,l3,rich monitor soft tables

  mon_soft_ctb->SetNRows(1);
  //dst_mon_soft_ctb_st *mon_ctb = mon_soft_ctb->GetTable();

  mon_soft_emc->SetNRows(1);  
  //dst_mon_soft_emc_st *mon_emc = mon_soft_emc->GetTable();
  
  mon_soft_l3->SetNRows(1);
  //dst_mon_soft_l3_st *mon_l3 = mon_soft_l3->GetTable();

  mon_soft_rich->SetNRows(1);
  //dst_mon_soft_rich_st *mon_rich = mon_soft_rich->GetTable();

  iRes = dst_monitor_soft_filler(tpcluster, 
				 tphit, scs_spt,
				 tptrack,stk_track,
				 evt_match, ctb_cor, vertex, event_summary, 
                                  mon_soft_glob,
                                 mon_soft_svt, mon_soft_tpc);
  //===========================================================================
  if (iRes !=kSTAFCV_OK) {
    iMake = kStWarn;
    gMessMgr->Warning() << "Problem on return from DST_MONITOR_SOFT_FILLER" << endm;
    if(Debug()) gMessMgr->Debug() << " run_dst: finished calling dst_monitor_soft_filler" << endm;
  }
 //--------------- ????????? --------------
  St_run_header    *run_header    = (St_run_header *)    m_ConstSet->Find("run_header");
  St_dst_summary_param *summary_param = (St_dst_summary_param *) m_Runco->Find("summary_param");
  
  if (!run_header) {
    StEvtHddr * evthdr = (StEvtHddr*)GetDataSet("EvtHddr");
    assert(evthdr);
    run_header = new St_run_header("run_header",1);
    run_header_st run;
    run.bfc_run_id = evthdr->GetRunNumber();
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
    
    if(!globtrk) {globtrk = new St_dst_track("globtrk",1);AddGarb(globtrk);}

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

