
/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* ------- STAF/ROOT generated includes ------- */
#include "fill_dst_run_summary.h"
/* prototypes  */
#include "global_prototypes.h"

#define  PI_MASS     0.139569
#define  PHI_MIN     0.0
#define  PHI_MAX   360.0 
#define  NBINS      30
#define  NRANGE      6
#define  NETARANGE   3

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0


long  type_of_call fill_dst_run_summary_ (
  TABLE_HEAD_ST  *dstSummaryParam_h, DST_SUMMARY_PARAM_ST  *dstSummaryParam,
  TABLE_HEAD_ST  *dstRunHeader_h,        RUN_HEADER_ST         *dstRunHeader,
  TABLE_HEAD_ST  *dstEventHeader_h,      EVENT_HEADER_ST       *dstEventHeader,
  TABLE_HEAD_ST  *dstEventSummary_h, DST_EVENT_SUMMARY_ST  *dstEventSummary,
  TABLE_HEAD_ST  *dstTrack_h,         DST_TRACK_ST          *dstTrack,
  TABLE_HEAD_ST  *dstVertex_h,        DST_VERTEX_ST         *dstVertex,
  TABLE_HEAD_ST  *dstRunSummary_h,   DST_RUN_SUMMARY_ST    *dstRunSummary)
{
  
  /*
   *:>--------------------------------------------------------------------
   *: ROUTINE:     fill_dst_run_summary_
   *: DESCRIPTION: Fill run summary information based on DST global track, 
   *:              vertex and event_summary tables.
   *:              
   *:
   *: AUTHOR:      Dhammika Weerasundara -- University of Washington
   *:              dhammika@gibbs.npl.washington.edu
   *:
   *: ARGUMENTS:
   *:          IN:
   *:             run_header           - run header table
   *:             dstRunHeader_h         - Header Structure for run_header  
   *:             event_header         - event header table
   *:             dstEventHeader_h       - Header Structure for event_header
   *:             dstEventSummary     - DST event summary table 
   *:             dstEventSummary_h  - Header Structure for dstEventSummary
   *:             dstTrack            - DST tracks table       
   *:             dstTrack_h          - Header Structure for dstTrack
   *:             dstVertex           - DST vertex table
   *:             dstVertex_h         - Header Structure for dstVertex 
   *:             dstSummaryParam     - DST summary parameter table 
   *:             dstSummaryParam_h  - Header Structure for dstSummaryParam
   *:       INOUT:
   *:             dstRunSummary       - DST run summary table 
   *:             dstRunSummary_h    - Header Structure for dstRunSummary
   *:         OUT:
   *:             
   *:
   *: RETURNS:    STAF Condition Value
   *:
   *: HISTORY:    
   *:      Aug 10, 1998       Dhammika W.   Original
   *:                                       A place holder for the actual
   *:                                       routine.
   *:      Aug 27, 1998       Dhammika W.   Updated to fill run_summary table.
   *:      Nov 05, 1998       Dhammika W.   Removed filling pt, mt & eta
   *:                                       multiplicity bins. It was a 
   *:                                       misunderstanding. The values of
   *:                                       eta_bin, pt_bin & mt_bins are
   *:                                       filled in an initialization  kumac.
   *:      Nov 18, 1998       Dhammika W.   Add dst_summary_param table.
   *:                                       Load kinematic ranges from
   *:                                       dst_summary_param table into 
   *:                                       dst_run_summary table at the
   *:                                       very end of the event loop.
   *:
   *:      Sep 29, 1999       Margetis S.   Make it comply with new DSTs
   *:      Jan 06, 2000       Lee Barnby    Remove unused variables.
   *:>--------------------------------------------------------------------
   */

  /*  ==================  Local Variables  ======================== */
  
  int     i, glb_trk_good, itrk;
  double  pi, piov2;
  float   pt, mt, eta ,phi, theta;
  float   n_events_good, mean, stddev;
  static  int     first_event=1;
  static  double  pt_sum=0, pt2_sum=0, eta_sum=0, eta2_sum=0;
  static  double  nchgtrk_sum=0, nchgtrk2_sum=0, nvertx_sum=0, nvertx2_sum=0;
 
  /* ===========================  Begin Executable Code  =============== */
  
  /* Initialize dst_run_summary table at the begining of the event loop */
  if (first_event) {
    dstRunSummary_h->nok = 1;  
    dstRunSummary->bfc_run_id        = 0;
    dstRunSummary->n_events_tot      = 0;
    dstRunSummary->n_events_good     = 0;
    for (i=0; i<2; i++) {
      dstRunSummary->date[i]         = 0;
      dstRunSummary->time[i]         = 0;
    } 
    dstRunSummary->cpu_total         = 0;
    dstRunSummary->east_pol_L        = 0;
    dstRunSummary->east_pol_T        = 0;
    dstRunSummary->west_pol_L        = 0;
    dstRunSummary->west_pol_T        = 0;
    dstRunSummary->luminosity        = 0;
    
    /* Obsolete in newest DSTs   */
    /*    for (i=0; i<2; i++) {                         
          dstRunSummary->eta_bins[i]     = 0;        
          dstRunSummary->pt_bins[i]      = 0;        
          dstRunSummary->mt_bins[i]      = 0;        
          }                                            
          dstRunSummary->n_phi_bins        = 0; 
    */

    for (i=0; i<2; i++) {
      dstRunSummary->eta[i]     = 0;
      dstRunSummary->pt[i]      = 0;
      dstRunSummary->num_vert[i]     = 0;
    }

    for (i=0; i<30; i++) {
      dstRunSummary->mean_mult[i] = 0;
      dstRunSummary->rms_mult[i]   = 0;
    }

    first_event=0;
  }

  /*  We are done with looping over events if n_events_good is non-zero.
      Go fill the dst_run_summary table.
  */
  if (dstRunSummary->n_events_good)
    goto FillTable;

  /*  Return if dst_event_summary table has no entries.  */
  if (!dstEventSummary_h->nok){
    fprintf(stderr, "Null dst_event_summary...exiting.\n");
    return STAFCV_BAD;
  }

  /* Get double precision pi */
  pi = acos(-1.);
  piov2 = pi/2;

  if (DEBUG){
    fprintf (stderr, "\n");
    fprintf (stderr, "pt_sum=%f, pt2_sum=%f, eta_sum=%f, eta2_sum=%f \n",
	     pt_sum,pt2_sum,eta_sum,eta2_sum);
    fprintf (stderr, "ntrk_sum=%f, ntrk2_sum=%f,  vt_sum=%f, vt2_sum=%f \n",
	     nchgtrk_sum, nchgtrk2_sum, nvertx_sum,  nvertx2_sum);    
  }

  /* Fill pt, mt, eta & phi histograms  */
  glb_trk_good = 0;
  for (itrk=0; itrk < dstTrack_h->nok; itrk++) {/* begin global track loop */
    /*  Calculate kinematic varialbles  for good tracks only */
    if ( dstTrack[itrk].iflag < 0 )
      continue;
    theta = piov2 - atan(dstTrack[itrk].tanl);
    eta   = -log(tan(theta/2.));
    pt    = 1./dstTrack[itrk].invpt;
    mt    = sqrt(pt*pt + PI_MASS*PI_MASS)-PI_MASS;
    phi   = dstTrack[itrk].psi;
    if (phi<0) phi += 360.;
    /* Sum pt, pt^2, eta, eta^2  for all good global charged tracks*/ 
    pt_sum    += pt;
    pt2_sum   += pt*pt;
    eta_sum   += eta;
    eta2_sum  += eta*eta;
    glb_trk_good++;         /*  good track multipicity   */
  }/* end of global track loop  */

  /* charge track multiplicity and vertices */
  nchgtrk_sum  += glb_trk_good;
  nchgtrk2_sum += pow(glb_trk_good,2);
  nvertx_sum   += dstVertex_h->nok;
  nvertx2_sum  += pow(dstVertex_h->nok,2);
  
  /*  
      I assume here that dstRunSummary->n_events_good will only be filled at
      the kumac level before we make the final call to fill_dst_run_summary
      module to poperly fill the dstRunSummary table at the very end of the 
      event loop.
  */
  if (!dstRunSummary->n_events_good)
    goto NextEvent;

 FillTable:
  n_events_good = (float) dstRunSummary->n_events_good;
  
  /* Fill mean & stand. deviations  */
  mean   = pt_sum/nchgtrk_sum;
  stddev = pt2_sum/nchgtrk_sum - pow(mean,2);
  dstRunSummary->pt[0]       = mean;
  dstRunSummary->pt[1]       = sqrt(stddev);
  mean   = eta_sum/nchgtrk_sum;  
  stddev = eta2_sum/nchgtrk_sum - pow(mean,2);
  dstRunSummary->eta[0]      = mean;
  dstRunSummary->eta[1]      = sqrt(stddev);
  mean   = nchgtrk_sum/n_events_good;  
  stddev = nchgtrk2_sum/n_events_good - pow(mean,2);
  /* I used detector id for global =23 (temporarily), this
     has to be passed from bfc. SMargetis  */
  dstRunSummary->mean_mult[23]  = mean; 
  dstRunSummary->rms_mult[23]  = sqrt(stddev);
  mean   = nvertx_sum/n_events_good;  
  stddev = nvertx2_sum/n_events_good - pow(mean,2);
  dstRunSummary->num_vert[0]      = mean; 
  dstRunSummary->num_vert[1]      = sqrt(stddev);
  
  /* 
     Load kinematic ranges from dstSummaryParam into dstRunSummary.
     DSW  Nov. 18 , 1998
   */
  /* Obsolete in newest DSTs   */
  /*  for (i=0; i<6; i++) {                         
      dstRunSummary->eta_bins[i]     = dstSummaryParam->eta_bins[i];        
      dstRunSummary->pt_bins[i]      = dstSummaryParam->pt_bins[i];        
      dstRunSummary->mt_bins[i]      = dstSummaryParam->mt_bins[i];        
      }                                            
      dstRunSummary->n_phi_bins        = dstSummaryParam->n_phi_bins;
  */

 NextEvent:
  return STAFCV_OK;
}  /*  End of fill_dst_run_summary  */



