/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* ------- STAF/ROOT generated includes ------- */
#include "fill_dst_event_summary.h"

#define  NRANGE     10


#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0

/* Define global variables for defining ranges in pt, mt & eta  */
float  pt_min, pt_max,  eta_min, eta_max;

float  mt_inverse_slope(double *mt_histo,int iBegin, int iStop);

long  type_of_call fill_dst_event_summary_ (
  TABLE_HEAD_ST  *dst_summary_param_h, DST_SUMMARY_PARAM_ST  *dst_summaryparam,
  TABLE_HEAD_ST  *run_header_h,            RUN_HEADER_ST     *dst_runheader,
  TABLE_HEAD_ST  *event_header_h,          EVENT_HEADER_ST   *dst_eventheader,
  TABLE_HEAD_ST  *dst_track_h,         DST_TRACK_ST          *dst_track,
  TABLE_HEAD_ST  *dst_vertex_h,        DST_VERTEX_ST         *dst_vertex,
  TABLE_HEAD_ST  *dst_event_summary_h, DST_EVENT_SUMMARY_ST  *dst_eventsummary)
{
  /*
   *
   *:>-------------------------------------------------------------------- 
   *: ROUTINE:     fill_dst_event_summary_
   *: DESCRIPTION: Fill event summary information based on DST global track, 
   *:              and vertex tables.
   *:              
   *:
   *: AUTHOR:      Dhammika Weerasundara -- University of Washington
   *:              dhammika@gibbs.npl.washington.edu
   *:
   *: ARGUMENTS:
   *:          IN:
   *:             dst_runheader         - run header table
   *:             dst_run_header_h      - Header Structure for dst_runheader  
   *:             dst_eventheader       - event header table
   *:             dst_event_header_h    - Header Structure for dst_eventheader
   *:             dst_track             - DST tracks table       
   *:             dst_track_h           - Header Structure for dst_track
   *:             dst_vertex            - DST vertex table
   *:             dst_vertex_h          - Header Structure for dst_vertex 
   *:             dst_summaryparam      - DST summary parameter table 
   *:             dst_summary_param_h   - Header Structure for dst_summaryparam
   *:       INOUT:
   *:         OUT:
   *:             dst_eventsummary      - DST event summary table 
   *:             dst_event_summary_h   - Header Structure for dst_eventsummary
   *:             
   *:
   *: RETURNS:    STAF Condition Value
   *:
   *: HISTORY:    
   *:      Aug 08, 1998       Dhammika W.   Original
   *:      Aug 27, 1998       Dhammika W.   Fixed mt inverse slope calculation.
   *:                                       Added mean_pt2, rms_eta and 
   *:                                       T_eta_bins[3] to dst_event_summary 
   *:                                       table.
   *:                                       Tested successfully on 500 Lanny's
   *:                                       DST events.
   *:      Sep 08, 1998       Dhammika W.   Fixed the check on global track
   *:                                       iflag to make it work properly
   *:                                       with fastdst chain.
   *:      Nov 18, 1998       Dhammika W.   Add dst_summary_param table.
   *:                                       Modified to allow user to define 
   *:                                       the ranges in pt, mt & eta via
   *:                                       dst_summary_param table. Number of 
   *:                                       ranges(NRANGE) is fixed and 
   *:                                       hardwired. User cannot change this.
   *:                                       #of ranges in phi (azimuth of the
   *:                                       p_T vector) is set via
   *:                                       dst_summaryparam->n_phi_bins.
   *:      Sep 30, 1999       Margetis S.   Converted to new DST format
   *:>-------------------------------------------------------------------- 
   */
  
  /*  ==================  Local Variables  ======================== */
  int     irange, i;
  int     glb_trk_good, glb_trk_prim, glb_trk_plus, glb_trk_minus;
  int     itrk, ibin, iptbin,  ietabin, iphibin;
  int     minbin, maxbin, binrange,nphirange;
  int     ivtx, vtx_id, iflag;
  double  pi, piov2;
  float   minval, maxval;
  float   pt_binsize,  eta_binsize, phi_binsize; 
  float   pt, eta, rms_eta=0 ,phi, theta;
  float   mean_pt=0, mean_pt2=0, mean_eta=0 ;
  
  
  /* ===========================  Begin Executable Code  =============== */
  
  /* Initialize valid rows in dst_eventsummary table */
  if (!dst_event_summary_h->nok)
    dst_event_summary_h->nok = 1; 
  
  /* Initialize dst_eventsummary table */
  dst_eventsummary->n_event                   = 0;
  dst_eventsummary->bfc_run_id                  = 0;         
  dst_eventsummary->glb_trk_tot               = 0;
  dst_eventsummary->glb_trk_good              = 0;
  dst_eventsummary->glb_trk_prim              = 0;
  dst_eventsummary->glb_trk_plus              = 0;
  dst_eventsummary->glb_trk_minus             = 0;
  dst_eventsummary->glb_trk_exotic            = 0;
  dst_eventsummary->n_vert_total              = 0;
  for (i=0; i<5; i++) {
    dst_eventsummary->n_vert_type[i]          = 0;
  }
  dst_eventsummary->n_vert_pileup             = 0;
  dst_eventsummary->mean_pt                   = 0;
  dst_eventsummary->mean_pt2                  = 0;
  dst_eventsummary->mean_eta                  = 0;
  dst_eventsummary->rms_eta                   = 0;
  for (irange=0; irange<NRANGE; irange++) {
    dst_eventsummary->mult_eta[irange]        = 0;
    dst_eventsummary->mult_pt[irange]         = 0;
    dst_eventsummary->mult_phi[irange]        = 0;
    dst_eventsummary->energy_emc_eta[irange]  = 0;
    dst_eventsummary->energy_emc_phi[irange]  = 0;
  }
  nphirange = (float) NRANGE ;

  for (i=0; i<3; i++) {
    dst_eventsummary->prim_vrtx[i]          = 0.;
  }
  for (i=0; i<4; i++) {
    dst_eventsummary->mag_field[i]          = 0.;
  }
  if (!dst_track_h->nok){
    fprintf(stderr,"FILL_DST_EVENT_SUMMARY: Zero dst tracks...exiting.\n");
    return STAFCV_BAD;
  }
  

  /* 
     Get the min/max values for pt,eta,phi ranges from dst_summaryparam 
     table. 
     This allows user(s) to set desired min/max values for pt,eta,phi 
     multiplicity bin widths & ranges. 
  */
  pt_min  = 0.;
  pt_max  = dst_summaryparam->pt_bins[8];
  eta_min = 0.;
  eta_max = dst_summaryparam->eta_bins[8];

  /* Get double precision pi */
  pi    = acos(-1.);
  piov2 = pi/2;
  
  /* Initialize global track counters & sum variables  */
  glb_trk_good=glb_trk_prim=glb_trk_plus=glb_trk_minus=0;
  mean_pt=mean_pt2=mean_eta=rms_eta=0;
  
  /* Fill pt, eta & phi histograms  */
  for (itrk=0; itrk < dst_track_h->nok; itrk++) {/* begin global track loop */
    /* Calculate track multiplicities  */
    if ( dst_track[itrk].iflag <= 0 )
      continue;
    glb_trk_good++;       /*  good global tracks            */
    if ( dst_track[itrk].icharge > 0 )
      glb_trk_plus++;     /*  charge = 1                    */
    if ( dst_track[itrk].icharge < 0 )
      glb_trk_minus++;    /*  charge = -1                   */
    /*  Calculate kinematic varialbles for good tracks only */
    theta = piov2 - atan(dst_track[itrk].tanl);
    eta   = -log(tan(theta/2.));
    pt    = 1./dst_track[itrk].invpt;
    phi   = dst_track[itrk].psi;
    if (phi<0) phi += 360.;
    
  
  /* Fill pt, eta  range multiplicities  */
  /* Modified to allow user to define the ranges in pt,eta,phi via       */
  /* dst_summaryparam table                                              */
  /* Number of ranges(NRANGE) is fixed and hardwired. User cannot change */
  /* this.                                                               */

    /*    if(pt >= 0 && pt<pt
          dst_eventsummary->mult_pt[irange]++; */

    /* Sum pt, pt^2, eta, eta^2  for all good global charged tracks*/ 
    mean_pt  += pt;
    mean_pt2 += pt*pt;
    mean_eta += eta;
    rms_eta  += eta*eta;
  }/* end of global track loop  */
  
  /*  Fill track multiplicities  */
  dst_eventsummary->glb_trk_tot   = dst_track_h->nok;
  dst_eventsummary->glb_trk_good  = glb_trk_good;
  dst_eventsummary->glb_trk_plus  = glb_trk_plus;
  dst_eventsummary->glb_trk_minus = glb_trk_minus ;
  
  /* Fill mean eta, pt, pt^2 and rms_eta */
  dst_eventsummary->mean_pt  = mean_pt/(float)glb_trk_good;
  dst_eventsummary->mean_pt2 = mean_pt2/(float)glb_trk_good;
  dst_eventsummary->mean_eta = mean_eta/(float)glb_trk_good;
  dst_eventsummary->rms_eta  = sqrt(rms_eta/(float)glb_trk_good);
  

  /* Fill total # of vertices */
  dst_eventsummary->n_vert_total = dst_vertex_h->nok;  

  /* Count v0 candidates */
  for (ivtx=0;  ivtx<dst_vertex_h->nok; ivtx++)  { /* begin vertex loop */
    vtx_id = dst_vertex[ivtx].vtx_id;  /*  get vertex type */
    iflag  = dst_vertex[ivtx].iflag;

    if(vtx_id == 1 && iflag ==1 ) {
      /* Fill Primary vertex information */
      dst_eventsummary->prim_vrtx[0]    = dst_vertex[ivtx].x;
      dst_eventsummary->prim_vrtx[1]    = dst_vertex[ivtx].y;
      dst_eventsummary->prim_vrtx[2]    = dst_vertex[ivtx].z;
      dst_eventsummary->glb_trk_prim    = dst_vertex[ivtx].n_daughters;
    }
    
      dst_eventsummary->n_vert_type[vtx_id]++ ;

      if(vtx_id == 5)  dst_eventsummary->n_vert_pileup++ ;
      
 }  /* end of vertex loop  */

  
  /*  Fill event ID */
  dst_eventsummary->n_event = dst_eventheader->n_event;
  
  /* Fill DST production run ID */
  dst_eventsummary->bfc_run_id = dst_runheader->bfc_run_id;
  
  return STAFCV_OK;
}  /*  End of fill_dst_event_summary  */


float  mt_inverse_slope(double *mthisto,int ibegin, int istop)
{
  float mt_max, mt_min;
  float mtx, mt_binsize, invslope;
  float s=0, sx=0, sy=0, sxx=0, sxy=0, delta=0;
  int   imtbin, index, NBINS=10;

  mt_binsize  = (mt_max - mt_min)/NBINS;

  /*  Do a Linear Leat Square fit to  log(dN/mt*dy*dmt) = -mt/T  */
  for  (index=ibegin; index<istop;  index++) {
    if (!mthisto[index])
      continue;
    mtx  = mt_binsize*(float)index + mt_binsize/2.;
    sx  += mtx;
    sy  += log(mthisto[index]);
    sxx += mtx*mtx;
    sxy += log(mthisto[index])*mtx;
    s++;
  }
  delta    = s*sxx - sx*sx;
  invslope = fabs ((s*sxy - sx*sy)/delta);
  invslope = 1./invslope;
  if (DEBUG){
    for (imtbin=ibegin; imtbin<istop-1; imtbin++)
      fprintf (stderr, "%f ", mthisto[imtbin]);
    fprintf (stderr, "%f  \n", mthisto[istop-1]);
    fprintf (stderr, "s=%f,sx=%f,sy=%f,sxx=%f,sxy=%f\n",s,sx,sy,sxx,sxy);
    fprintf (stderr, "delta=%f,invslope=%f \n",delta,invslope);
  }
  return invslope;
} /* end of mt_inverse_slope */




