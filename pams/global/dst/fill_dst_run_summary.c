/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "asuAlloc.h"

/* ------- STAF/ROOT generated includes ------- */
#include "fill_dst_run_summary.h"

#define  PI_MASS     0.139569
#define  PT_MIN      0.0
#define  PT_MAX      1.5
#define  MT_MIN      0.
#define  MT_MAX      1.5
#define  ETA_MIN    -2.0
#define  ETA_MAX     2.0
#define  PHI_MIN  -180.0
#define  PHI_MAX   180.0 
#define  NBINS      30
#define  NRANGE      6
#define  NPHIRANGE   8
#define  NETARANGE   3

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0

float  mt_inverse_slope(double *mt_histo,int iBegin, int iStop);

long  type_of_call fill_dst_run_summary_ (
  TABLE_HEAD_ST  *dst_run_header_h,    DST_RUN_HEADER_ST     *dst_runheader,
  TABLE_HEAD_ST  *dst_event_header_h,  DST_EVENT_HEADER_ST   *dst_eventheader,
  TABLE_HEAD_ST  *dst_event_summary_h, DST_EVENT_SUMMARY_ST  *dst_eventsummary,
  TABLE_HEAD_ST  *dst_track_h,         DST_TRACK_ST          *dst_track,
  TABLE_HEAD_ST  *dst_vertex_h,        DST_VERTEX_ST         *dst_vertex,
  TABLE_HEAD_ST  *dst_run_summary_h,   DST_RUN_SUMMARY_ST    *dst_runsummary)
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
   *:             dst_runheader        - DST run header table
   *:             dst_run_header_h     - Header Structure for dst_runheader  
   *:             dst_eventheader      - DST event header table
   *:             dst_event_header_h   - Header Structure for dst_eventheader
   *:             dst_eventsummary     - DST event summary table 
   *:             dst_event_summary_h  - Header Structure for dst_eventsummary
   *:             dst_track            - DST tracks table       
   *:             dst_track_h          - Header Structure for dst_track
   *:             dst_vertex           - DST vertex table
   *:             dst_vertex_h         - Header Structure for dst_vertex 
   *:       INOUT:
   *:             dst_runsummary       - DST run summary table 
   *:             dst_run_summary_h    - Header Structure for dst_runsummary
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
   *:
   *:>--------------------------------------------------------------------
   */

  /*  ==================  Local Variables  ======================== */
  
  int     irange, i, glb_trk_good;
  int     itrk, ibin, iptbin, imtbin, ietabin, iphibin;
  int     minbin, maxbin, binrange;
  int     ivtx, vtx_id;
  double  pi, piov2;
  double  mt_histo[NBINS], pt_histo[NBINS], eta_histo[NBINS];
  float   pt_binsize, mt_binsize, eta_binsize, phi_binsize; 
  float   dmt, deta1, deta2, mtweight1, mtweight2;
  float   pt, mt, eta ,phi, theta;
  float   n_events_good, mean, stddev;
  static  int    first_event=1;
  static  int    nevents=0;
  static  double  pt_sum=0, pt2_sum=0, eta_sum=0, eta2_sum=0;
  static  double  nchgtrk_sum=0, nchgtrk2_sum=0, nvertx_sum=0, nvertx2_sum=0;
 
  /* ===========================  Begin Executable Code  =============== */
  
  /* Initialize dst_run_summary table at the begining of the event loop */
  if (first_event) {
    dst_run_summary_h->nok = 1;  
    dst_runsummary->prod_run          = 0;
    /* dst_runsummary->version[0]        = "u"; */
    dst_runsummary->n_events_tot      = 0;
    dst_runsummary->n_events_good     = 0;
    for (i=0; i<2; i++) {
      dst_runsummary->date[i]         = 0;
      dst_runsummary->time[i]         = 0;
    }
    dst_runsummary->cpu_total         = 0;
    for (i=0; i<6; i++) {
      dst_runsummary->eta_bins[i]     = 0;
      dst_runsummary->pt_bins[i]      = 0;
      dst_runsummary->mt_bins[i]      = 0;
    }
    dst_runsummary->n_phi_bins        = 0;
    for (i=0; i<2; i++) {
      dst_runsummary->mean_eta[i]     = 0;
      dst_runsummary->mean_pt[i]      = 0;
      dst_runsummary->multiplicity[i] = 0;
      dst_runsummary->num_vert[i]     = 0;
      dst_runsummary->energy_emc[i]   = 0;
    }
    first_event=0;
  }

  /*  We are done with looping over events if n_events_good is non-zero.
      Go fill the dst_run_summary table.
  */
  if (dst_runsummary->n_events_good)
    goto FillTable;

  /*  Return if dst_event_summary table has no entries.  */
  if (!dst_event_summary_h->nok){
    fprintf(stderr, "Null dst_event_summary...exiting.\n");
    return STAFCV_BAD;
  }

  /* Reset pt, mt, eta & phi  histograms  */
  memset (&pt_histo,      0, sizeof(double)*NBINS);
  memset (&mt_histo,      0, sizeof(double)*NBINS);
  memset (&eta_histo,     0, sizeof(double)*NBINS);

  /* Claculate  histogram bin size */
  pt_binsize  = (PT_MAX  - PT_MIN )/NBINS;
  mt_binsize  = (MT_MAX  - MT_MIN )/NBINS;
  eta_binsize = (ETA_MAX - ETA_MIN)/NBINS;

  /* Get double precision pi */
  pi = acos(-1.);
  piov2 = pi/2;

  if (DEBUG){
    fprintf (stderr, "\n");
    fprintf (stderr, "pt_sum=%f, pt2_sum=%f, eta_sum=%f, eta2_sum=%f \n",
	     pt_sum,pt2_sum,eta_sum,eta2_sum);
    fprintf (stderr, "ntrk_sum=%f, ntrk2_sum=%f,  vt_sum=%f, vt2_sum=%f \n",
	     nchgtrk_sum, nchgtrk2_sum, nvertx_sum,  nvertx2_sum);    
    fprintf (stderr, "\n");
    fprintf (stderr, "pt_bins"); 
    for (irange=0; irange<NRANGE; irange++)
      fprintf (stderr, "%f",dst_runsummary->pt_bins[irange]);
    fprintf (stderr, "\n");
    fprintf (stderr, "mt_bins"); 
    for (irange=0; irange<NRANGE; irange++)
      fprintf (stderr, "%f",dst_runsummary->mt_bins[irange]);
    fprintf (stderr, "\n");
    fprintf (stderr, "eta_bins"); 
    for (irange=0; irange<NRANGE; irange++)
      fprintf (stderr, "%f",dst_runsummary->eta_bins[irange]);
  }

  /* Fill pt, mt, eta & phi histograms  */
  glb_trk_good = 0;
  for (itrk=0; itrk < dst_track_h->nok; itrk++) {/* begin global track loop */
    /*  Calculate kinematic varialbles  for good tracks only */
    if ( dst_track[itrk].iflag < 0 )
      continue;
    theta = piov2 - atan(dst_track[itrk].tanl);
    eta   = -log(tan(theta/2.));
    pt    = 1./dst_track[itrk].invpt;
    mt    = sqrt(pt*pt + PI_MASS*PI_MASS)-PI_MASS;
    phi   = dst_track[itrk].psi;
    /*  Determine appropriate bin number */ 
    iptbin  =  ((pt - PT_MIN)/pt_binsize);
    imtbin  =  ((mt - MT_MIN)/mt_binsize);
    ietabin =  ((eta - ETA_MIN)/eta_binsize);
    iphibin =  ((phi - PHI_MIN)/phi_binsize);
    /*  Fill histograms.  Protect against going out of range. */
    if (iptbin<NBINS)
      pt_histo[iptbin]++;    /* pt histogram    */
    if (0 <= ietabin && ietabin<NBINS)
      eta_histo[ietabin]++;  /* eta histogram   */
    if (imtbin<NBINS) 
      mt_histo[imtbin]++;    /* mt histogram    */
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
  nvertx_sum   += dst_vertex_h->nok;
  nvertx2_sum  += pow(dst_vertex_h->nok,2);

  binrange = NBINS/NRANGE;  /* NBINS have to be a multiple of NRANGE */

  /* Fill pt, my, eta & phi  bin multiplicities  */
  for (irange=0; irange<NRANGE; irange++) { /* begin  looping over ranges  */
    minbin = binrange*irange;
    maxbin = minbin + binrange;
    for (ibin=minbin; ibin < maxbin; ibin++) {/* begin  looping over bins  */
      /* Fill pt  bin  multiplicities  */
      dst_runsummary->pt_bins[irange]  +=  pt_histo[ibin];                  
      /* Fill mt bin  multiplicities   */
      dst_runsummary->mt_bins[irange]  +=  mt_histo[ibin];      
      /* Fill eta bin  multiplicities  */
      dst_runsummary->eta_bins[irange] +=  eta_histo[ibin];     
    }  /* end of looping over bins */
  }  /* end of looping over ranges */
  
  /*  
      I assume here that dst_runsummary->n_events_good will only be filled at
      the kumac level before we make the final call to fill_dst_run_summary
      module to poperly fill the dst_runsummary table at the very end of the 
      event loop.
  */
  if (!dst_runsummary->n_events_good)
    goto NextEvent;

 FillTable:
  n_events_good = (float) dst_runsummary->n_events_good;
  
  /* Fill mean & stand. deviations  */
  mean   = pt_sum/nchgtrk_sum;
  stddev = pt2_sum/nchgtrk_sum - pow(mean,2);
  dst_runsummary->mean_pt[0]       = mean;
  dst_runsummary->mean_pt[1]       = sqrt(stddev);
  mean   = eta_sum/nchgtrk_sum;  
  stddev = eta2_sum/nchgtrk_sum - pow(mean,2);
  dst_runsummary->mean_eta[0]      = mean;
  dst_runsummary->mean_eta[1]      = sqrt(stddev);
  mean   = nchgtrk_sum/n_events_good;  
  stddev = nchgtrk2_sum/n_events_good - pow(mean,2);
  dst_runsummary->multiplicity[0]  = mean; 
  dst_runsummary->multiplicity[1]  = sqrt(stddev);
  mean   = nvertx_sum/n_events_good;  
  stddev = nvertx2_sum/n_events_good - pow(mean,2);
  dst_runsummary->num_vert[0]      = mean; 
  dst_runsummary->num_vert[1]      = sqrt(stddev);
  
 NextEvent:
  return STAFCV_OK;
}  /*  End of fill_dst_run_summary  */
