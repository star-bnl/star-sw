/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "asuAlloc.h"
/* ------- STAF/ROOT generated includes ------- */
#include "fill_dst_event_summary.h"

#define  PI_MASS     0.139569
#define  PT_MIN      0.
#define  PT_MAX      1.5
#define  MT_MIN      0.
#define  MT_MAX      1.5
#define  ETA_MIN    -2.0
#define  ETA_MAX     2.0
#define  PHI_MIN  -180.0
#define  PHI_MAX   180.0 
#define  NBINS      50
#define  NRANGE      5
#define  NPHIRANGE   8
#define  NETARANGE   3

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0

float  mt_inverse_slope(double *mt_histo,int iBegin, int iStop);

long  type_of_call fill_dst_event_summary_ (
  TABLE_HEAD_ST  *dst_run_header_h,    DST_RUN_HEADER_ST     *dst_runheader,
  TABLE_HEAD_ST  *dst_event_header_h,  DST_EVENT_HEADER_ST   *dst_eventheader,
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
   *:             dst_runheader         - DST run header table
   *:             dst_run_header_h      - Header Structure for dst_runheader  
   *:             dst_eventheader       - DST event header table
   *:             dst_event_header_h    - Header Structure for dst_eventheader
   *:             dst_track             - DST tracks table       
   *:             dst_track_h           - Header Structure for dst_track
   *:             dst_vertex            - DST vertex table
   *:             dst_vertex_h          - Header Structure for dst_vertex 
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
   *:
   *:>-------------------------------------------------------------------- 
   */

  /*  ==================  Local Variables  ======================== */
  int     irange, i;
  int     glb_trk_good, glb_trk_prim, glb_trk_plus, glb_trk_minus;
  int     itrk, ibin, iptbin, imtbin, ietabin, iphibin;
  int     minbin, maxbin, binrange;
  int     ivtx, vtx_id;
  double  pi, piov2;
  double  mt_histo[NBINS], pt_histo[NBINS], eta_histo[NBINS], phi_histo[NBINS];
  double  eta1_mt_histo[NBINS],eta2_mt_histo[NBINS],eta3_mt_histo[NBINS];
  float   mt_min, mt_max;
  float   pt_binsize, mt_binsize, eta_binsize, phi_binsize; 
  float   dmt, deta1, deta2, mtweight1, mtweight2;
  float   pt, mt, eta, rms_eta=0 ,phi, theta;
  float   mean_pt=0, mean_pt2=0, mean_eta=0, t_average=0;
  
  enum { PRIMVTX=1, K0=2, LAMBDA=3,  ALAMBDA=4, PILEUP=5};
  
  /* ===========================  Begin Executable Code  =============== */

  /* Initialize valid rows in dst_eventsummary table */
  dst_event_summary_h->nok = 0;
  
  /* Initialize dst_eventsummary table */
  dst_eventsummary->n_event[0]                = 0;
  dst_eventsummary->n_event[1]                = 0;
  dst_eventsummary->prod_run                  = 0;         
  dst_eventsummary->glb_trk_tot               = 0;
  dst_eventsummary->glb_trk_good              = 0;
  dst_eventsummary->glb_trk_prim              = 0;
  dst_eventsummary->glb_trk_plus              = 0;
  dst_eventsummary->glb_trk_minus             = 0;
  dst_eventsummary->n_vert_total              = 0;
  dst_eventsummary->n_vert_V0                 = 0;
  dst_eventsummary->n_vert_K0                 = 0;
  dst_eventsummary->n_vert_Lambda             = 0;
  dst_eventsummary->n_vert_ALambda            = 0;
  dst_eventsummary->n_vert_pileup             = 0;
  dst_eventsummary->mean_pt                   = 0;
  dst_eventsummary->mean_pt2                  = 0;
  dst_eventsummary->mean_eta                  = 0;
  dst_eventsummary->rms_eta                   = 0;
  dst_eventsummary->T_average                 = 0;
  dst_eventsummary->prim_vrtx_chisq           = 0;
  for (irange=0; irange<NRANGE; irange++) {
    dst_eventsummary->mult_eta[irange]        = 0;
    dst_eventsummary->mult_pt[irange]         = 0;
    dst_eventsummary->energy_emc_eta[irange]  = 0;
    dst_eventsummary->T_mt_bins[irange]       = 0;
  }
  for (irange=0; irange<NETARANGE; irange++) {
    dst_eventsummary->T_eta_bins[irange]      = 0;
  }
  for (irange=0; irange<NPHIRANGE; irange++) {
    dst_eventsummary->mult_phi[irange]        = 0;
    dst_eventsummary->energy_emc_phi[irange]  = 0;
  }
  for (i=0; i<10; i++) {
  dst_eventsummary->vrtx_qual[i]              = 0;
  if (i<6)
    dst_eventsummary->prim_vrtx_cov[i]        = 0;
  if (i<3)
    dst_eventsummary->prim_vrtx[i]            = 0;
}

  if (!dst_track_h->nok){
    fprintf(stderr,"FILL_DST_EVENT_SUMMARY: Zero dst tracks...exiting.\n");
    return STAFCV_BAD;
  }

  /* Reset pt, mt, eta & phi  histograms  */
  memset (&pt_histo,      0, sizeof(double)*NBINS);
  memset (&mt_histo,      0, sizeof(double)*NBINS);
  memset (&eta_histo,     0, sizeof(double)*NBINS);
  memset (&phi_histo,     0, sizeof(double)*NBINS);
  memset (&eta1_mt_histo, 0, sizeof(double)*NBINS);
  memset (&eta2_mt_histo, 0, sizeof(double)*NBINS);
  memset (&eta3_mt_histo, 0, sizeof(double)*NBINS);
  
  /* Claculate  histogram bin size */
  pt_binsize  = (PT_MAX  - PT_MIN )/NBINS;
  mt_binsize  = (MT_MAX  - MT_MIN )/NBINS;
  eta_binsize = (ETA_MAX - ETA_MIN)/NBINS;
  phi_binsize = (PHI_MAX - PHI_MIN)/NBINS;

  /*  Calculate  the mt bin weight  */
  deta1       = (ETA_MAX - ETA_MIN);
  deta2       = 1;
  dmt         = mt_binsize;
  mtweight1   = 1./(deta1*dmt);
  mtweight2   = 1./(deta2*dmt);

  /* Get double precision pi */
  pi = acos(-1.);
  piov2 = pi/2;

  /* Initialize global track counters & sum variables  */
  glb_trk_good=glb_trk_prim=glb_trk_plus=glb_trk_minus=0;
  mean_pt=mean_pt2=mean_eta=rms_eta=0;
  
  /* Fill pt, mt, eta & phi histograms  */
  for (itrk=0; itrk < dst_track_h->nok; itrk++) {/* begin global track loop */
    /* Calculate track multiplicities  */
    if ( dst_track[itrk].icharge > 0 )
      glb_trk_plus++;     /*  charge = 1             */
    if ( dst_track[itrk].icharge < 0 )
      glb_trk_minus++;    /*  charge = -1            */
    if ( dst_track[itrk].iflag < 0 )
      continue;
    glb_trk_good++;     /*  good tracks            */
    /*  Calculate kinematic varialbles for good tracks only */
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
    if (iphibin<NBINS)  
      phi_histo[iphibin]++;  /* phi histogram   */
    /*  weight the mt bin by  1/(mt*dy*dmt)     */
    if (imtbin<NBINS) {
       /* dN/mt*dy*dmt histogram */
      mt_histo[imtbin] += mtweight1/mt; 
      /*  Fill mt historgrams for three eta bins  */
      if ( -1.5 <= eta && eta < -0.5 )
	eta1_mt_histo[imtbin] += mtweight2/mt;
      if ( -0.5 <= eta && eta <  0.5 )
	eta2_mt_histo[imtbin] += mtweight2/mt;
      if (  0.5 <= eta && eta <  1.5 )
	eta3_mt_histo[imtbin] += mtweight2/mt;
    }
    /* Sum pt, pt^2, eta, eta^2  for all good global charged tracks*/ 
    mean_pt  += pt;
    mean_pt2 += pt*pt;
    mean_eta += eta;
    rms_eta  += eta*eta;
  }/* end of global track loop  */

  binrange = NBINS/NRANGE;  /* NBINS have to be a multiple of NRANGE */

  /* Fill pt, my, eta & phi  bin multiplicities  */
  for (irange=0; irange<NRANGE; irange++) { /* begin  looping over ranges  */
    minbin = binrange*irange;
    maxbin = minbin + binrange;
    for (ibin=minbin; ibin < maxbin; ibin++) {/* begin  looping over bins  */
      dst_eventsummary->mult_pt[irange] +=
	 pt_histo[ibin];         /* Fill pt  bin  multiplicities  */
      dst_eventsummary->mult_eta[irange]+=
	 eta_histo[ibin];        /* Fill eta bin  multiplicities  */
      dst_eventsummary->mult_phi[irange]+=
	 phi_histo[ibin];        /* Fill phi bin  multiplicities  */
    }/* end of looping over bins */

    /* Fill inverse slope for mt bins   */
    dst_eventsummary->T_mt_bins[irange] = 
      mt_inverse_slope(mt_histo, minbin, maxbin); /* mt inverse slope */

    /* Fill mt inverse slope for three eta bins  */
    switch(irange) {
    case 0:
      dst_eventsummary->T_eta_bins[irange] = 
	mt_inverse_slope(eta1_mt_histo,  0, NBINS ); /* mt inverse slope */
      break;
    case 1:
      dst_eventsummary->T_eta_bins[irange] = 
	mt_inverse_slope(eta2_mt_histo,  0, NBINS ); /* mt inverse slope */
      break;
    case 2:
      dst_eventsummary->T_eta_bins[irange] = 
	mt_inverse_slope(eta3_mt_histo,  0, NBINS ); /* mt inverse slope */
      break;
    }
  }  /* end of looping over ranges */

  
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
  
  /* Fill  average mt inverse slope */
  t_average = mt_inverse_slope(mt_histo, 0, NBINS ); /* use full mt_histo */
  dst_eventsummary->T_average = t_average;  /* average mt inverse slope */

  /* Fill total # of vertices */
  dst_eventsummary->n_vert_total = dst_vertex_h->nok;  

  /* Count v0 candidates */
  for (ivtx=0;  ivtx<dst_vertex_h->nok; ivtx++)  { /* begin vertex loop */
    vtx_id = dst_vertex[ivtx].vtx_id;  /*  get vertex type */
    if (vtx_id > 1)
      dst_eventsummary->n_vert_V0++;   /* count total number of V0s  */
    switch (vtx_id) {
    case PRIMVTX:
      /* Fill Primary vertex information */
      dst_eventsummary->prim_vrtx[0]    = dst_vertex[ivtx].x;
      dst_eventsummary->prim_vrtx[1]    = dst_vertex[ivtx].y;
      dst_eventsummary->prim_vrtx[2]    = dst_vertex[ivtx].z;
      dst_eventsummary->prim_vrtx_chisq = dst_vertex[ivtx].pchi2;
      dst_eventsummary->glb_trk_prim    = dst_vertex[ivtx].n_daughters;
      break;
      /* Count v0 candidates */
    case K0:
      dst_eventsummary->n_vert_K0++ ;
      break;
    case LAMBDA:
      dst_eventsummary->n_vert_Lambda++ ;
      break;
    case ALAMBDA:
      dst_eventsummary->n_vert_ALambda++ ;
      break;
    case PILEUP:
      dst_eventsummary->n_vert_pileup++ ;
      break;
    }
  }  /* end of vertex loop  */

  
  /*  Fill event ID */
  dst_eventsummary->n_event[0] = dst_eventheader->n_event[0];
  dst_eventsummary->n_event[1] = dst_eventheader->n_event[1];
  
  /* Fill DST production run ID */
  dst_eventsummary->prod_run = dst_runheader->run_id;
  
  dst_event_summary_h->nok = 1;
  return STAFCV_OK;
}  /*  End of fill_dst_event_summary  */


float  mt_inverse_slope(double *mthisto,int ibegin, int istop)
{
  
  float mtx, mt_min, mt_max, mt_binsize, invslope;
  float s=0, sx=0, sy=0, sxx=0, sxy=0, delta=0;
  int   imtbin, index;

  mt_binsize  = (MT_MAX - MT_MIN)/NBINS;

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
