/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* ------- STAF/ROOT generated includes ------- */
#include "fill_dst_run_summary.h"

#define  PI_MASS     0.139569
#define  PT_MIN      0.
#define  PT_MAX      2.0
#define  ETA_MIN    -2.0
#define  ETA_MAX     2.0
#define  PHI_MIN     0.0
#define  PHI_MAX   360.0 
#define  NBINS     100
#define  NRANGE      5

/*  float  mt_inverse_slope(double *mt_histo,int iBegin, int iStop); */

long  type_of_call fill_dst_run_summary_ (
  TABLE_HEAD_ST  *dst_run_header_h,    DST_RUN_HEADER_ST     *dstRunHeader,
  TABLE_HEAD_ST  *dst_event_header_h,  DST_EVENT_HEADER_ST   *dstEventHeader,
  TABLE_HEAD_ST  *dst_event_summary_h, DST_EVENT_SUMMARY_ST  *dstEventSummary,
  TABLE_HEAD_ST  *dst_tof_h,           DST_TOF_ST            *dstTof,
  TABLE_HEAD_ST  *dst_track_h,         DST_TRACK_ST          *dstTrack,
  TABLE_HEAD_ST  *dst_track_aux_h,     DST_TRACK_AUX_ST      *dstTrackAux,
  TABLE_HEAD_ST  *dst_vertex_h,        DST_VERTEX_ST         *dstVertex,
  TABLE_HEAD_ST  *dst_run_summary_h,   DST_RUN_SUMMARY_ST    *dstRunSummary)
{
  
  /*
   *
   *:>--------------------------------------------------------------------
   *: ROUTINE:     fill_dst_run_summary_
   *: DESCRIPTION: Fill run summary information based on DST global track, 
   *:              auxiliary track, Tof, vertex and event_summary tables.
   *:              
   *:
   *: AUTHOR:      Dhammika Weerasundara -- University of Washington
   *:              dhammika@gibbs.npl.washington.edu
   *:
   *: ARGUMENTS:
   *:          IN:
   *:             dstRunHeader         - DST run header table
   *:             dst_run_header_h     - Header Structure for dstRunHeader  
   *:             dstEventHeader       - DST event header table
   *:             dst_event_header_h   - Header Structure for dstEventHeader
   *:             dstEventSummary      - DST event summary table 
   *:             dst_event_summary_h  - Header Structure for dstEventSummary
   *:             dstTof               - DST TOF table
   *:             dst_tof_h            - Header Structure for dstTof       
   *:             dstTrack             - DST tracks table       
   *:             dst_track_h          - Header Structure for dstTrack
   *:             dstTrackAux          - DST track auxiliary table
   *:             dst_track_aux_h      - Header Structure for dstTrackAux   
   *:             dstVertex            - DST vertex table
   *:             dst_vertex_h         - Header Structure for dstVertex 
   *:       INOUT:
   *:         OUT:
   *:             dstRunSummary      - DST run summary table 
   *:             dst_run_summary_h  - Header Structure for dstRunSummary
   *:             
   *:
   *: RETURNS:    STAF Condition Value
   *:
   *: HISTORY:    
   *:      Aug 10, 1998       Dhammika W.        Original. 
   *:                                            A place holder for the actual
   *:                                            routine.
   *:
   *:>--------------------------------------------------------------------
   */

  /*  ==================  Local Variables  ======================== */
  
  int i;

  /* ===========================  Begin Executable Code  =============== */

  
  dstRunSummary->prod_run          = 0;
  /* dstRunSummary->version[0]        = "u"; */
  dstRunSummary->n_events_tot      = 0;
  dstRunSummary->n_events_good     = 0;
  for (i=0; i<2; i++) {
    dstRunSummary->date[i]         = 0;
    dstRunSummary->time[i]         = 0;
  }
  dstRunSummary->cpu_total         = 0;
  for (i=0; i<6; i++) {
    dstRunSummary->eta_bins[i]     = 0;
    dstRunSummary->pt_bins[i]      = 0;
    dstRunSummary->mt_bins[i]      = 0;
  }
  dstRunSummary->n_phi_bins        = 0;
  for (i=0; i<2; i++) {
    dstRunSummary->mean_eta[i]     = 0;
    dstRunSummary->mean_pt[i]      = 0;
    dstRunSummary->multiplicity[i] = 0;
    dstRunSummary->num_vert[i]     = 0;
    dstRunSummary->energy_emc[i]   = 0;
  }

  return STAFCV_OK;
}  /*  End of fill_dst_run_summary  */


