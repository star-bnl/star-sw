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

float  mt_inverse_slope(double *mt_histo,int iBegin, int iStop);

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
  /*
  /*:>--------------------------------------------------------------------
  /*: ROUTINE:     fill_dst_run_summary_
  /*: DESCRIPTION: Fill run summary information based on DST global track, 
  /*:              auxiliary track, Tof, vertex and event_summary tables.
  /*:              
  /*:
  /*: AUTHOR:      Dhammika Weerasundara -- University of Washington
  /*:              dhammika@gibbs.npl.washington.edu
  /*:
  /*: ARGUMENTS:
  /*:          IN:
  /*:             dstRunHeader         - DST run header table
  /*:             dst_run_header_h     - Header Structure for dstRunHeader  
  /*:             dstEventHeader       - DST event header table
  /*:             dst_event_header_h   - Header Structure for dstEventHeader
  /*:             dstEventSummary      - DST event summary table 
  /*:             dst_event_summary_h  - Header Structure for dstEventSummary
  /*:             dstTof               - DST TOF table
  /*:             dst_tof_h            - Header Structure for dstTof       
  /*:             dstTrack             - DST tracks table       
  /*:             dst_track_h          - Header Structure for dstTrack
  /*:             dstTrackAux          - DST track auxiliary table
  /*:             dst_track_aux_h      - Header Structure for dstTrackAux   
  /*:             dstVertex            - DST vertex table
  /*:             dst_vertex_h         - Header Structure for dstVertex 
  /*:       INOUT:
  /*:         OUT:
  /*:             dstRunSummary      - DST run summary table 
  /*:             dst_run_summary_h  - Header Structure for dstRunSummary
  /*:             
  /*:
  /*: RETURNS:    STAF Condition Value
  /*:
  /*: HISTORY:    
  /*:      Aug 10, 1998       Dhammika W.        Original
  /*:
  /*:>--------------------------------------------------------------------
  
  /*  ==================  Local Variables  ======================== */
  int     irange;
  int     glb_trk_good, glb_trk_prim, glb_trk_plus, glb_trk_minus;
  int     itrk, ibin, iptBin, imtBin, ietaBin, iphiBin;
  int     minBin, maxBin, binRange;
  int     ivtx, vtx_id;
  double  pi, piov2;
  double  *mt_histo, *pt_histo, *eta_histo, *phi_histo;
  float   mt_min, mt_max;
  float   pt_binSize, mt_binSize, eta_binSize, phi_binSize; 
  float   dmt, deta, mtWeight;
  float   pt, mt, eta, phi, theta, mean_pt, mean_eta, T_average;
  
  enum { PRIM=0, K0=1, LAMBDA=2,  ALAMBDA=3, PILEUP=4};
  
  
  /* ===========================  Begin Executable Code  =============== */
  pi = acos(-1.);
  piov2 = pi/2;
  
  /* Allocate memory for histograms  */
  if( !(pt_histo  = (double *)malloc(sizeof(double) * NBINS)) )  {
    fprintf(stderr, "Unable to allocate pt_histo...exiting.\n");
    return STAFCV_BAD;
  }
  if( !(mt_histo  = (double *)malloc(sizeof(double) * NBINS)) )  {
    fprintf(stderr, "Unable to allocate mt_histo...exiting.\n");
    return STAFCV_BAD;
  }
  if( !(eta_histo  = (double *)malloc(sizeof(double) * NBINS)) )  {
    fprintf(stderr, "Unable to allocate eta_histo...exiting.\n");
    return STAFCV_BAD;
  }
  if( !(phi_histo  = (double *)malloc(sizeof(double) * NBINS)) )  {
    fprintf(stderr, "Unable to allocate phi_histo...exiting.\n");
    return STAFCV_BAD;
  }

    /*
      if( !(pt_histo  = new double[NBINS]) )  {
      fprintf(stderr, "Unable to allocate pt_histo...exiting.\n");
      return STAFCV_BAD;
      }
      if( !(mt_histo  = new double[NBINS]) )  {
      fprintf(stderr, "Unable to allocate mt_histo...exiting.\n");
      return STAFCV_BAD;
      }
      if( !(eta_histo = new double[NBINS]) )  {
      fprintf(stderr, "Unable to allocate eta_histo...exiting.\n");
      return STAFCV_BAD;
      }
      if( !(phi_histo = new double[NBINS]) )  {
      fprintf(stderr, "Unable to allocate phi_histo...exiting.\n");
      return STAFCV_BAD;
      }
    */

  /* Reset pt, mt, eta & phi  histograms  */
  memset (pt_histo,  0, sizeof(double)*NBINS);
  memset (mt_histo,  0, sizeof(double)*NBINS);
  memset (eta_histo, 0, sizeof(double)*NBINS);
  memset (phi_histo, 0, sizeof(double)*NBINS);
  
  /* Claculate  histogram bin size */
  mt_min      = sqrt(PT_MIN*PT_MIN + PI_MASS*PI_MASS);
  mt_max      = sqrt(PT_MAX*PT_MAX + PI_MASS*PI_MASS);
  pt_binSize  = (PT_MAX-PT_MIN)/NBINS;
  mt_binSize  = (mt_max-mt_min)/NBINS;
  eta_binSize = (ETA_MAX - ETA_MIN)/NBINS;
  phi_binSize = (PHI_MAX - PHI_MIN)/NBINS;

  /*  Calculate  the mt bin weight  */
  deta     = (ETA_MAX - ETA_MIN);
  dmt      = mt_binSize;
  mtWeight = 1./(deta*dmt);
  
  /* Initialize counters  */
  glb_trk_good=glb_trk_prim=glb_trk_plus=glb_trk_minus=0;
  
  /* Fill pt, mt, eta & phi histograms  */
  for (itrk=0; itrk < dst_track_h->nok; itrk++) {/* begin global track loop */
    /* Fill pt, my, eta & phi  bin multiplicities  */
    if ( dstTrack[itrk].icharge > 0 )
      glb_trk_plus++;
    if ( dstTrack[itrk].icharge < 0 )
      glb_trk_minus++;
    
    /*  Calculate kinematic varialbles */
    theta = piov2 - atan(dstTrack[itrk].tanl);
    eta   = -log(tan(theta/2.));
    pt    = 1./dstTrack[itrk].invpt;
    mt    = sqrt(pt*pt + PI_MASS*PI_MASS);
    phi   = dstTrack[itrk].psi;
    
    /*  Determine appropriate bin number */ 
    iptBin  = 1 + (int) ((pt - PT_MIN)/pt_binSize);
    imtBin  = 1 + (int) ((mt - mt_min)/mt_binSize);
    ietaBin = 1 + (int) ((eta - ETA_MIN)/eta_binSize);
    iphiBin = 1 + (int) ((phi - PHI_MIN)/phi_binSize);
    
    /*  Fill histograms.  Protect against going out of range. */
    if (iptBin<NBINS)
      phi_histo[iptBin]++;
    if (ietaBin<NBINS)
      eta_histo[ietaBin]++;
    if (iphiBin<NBINS)
      phi_histo[iphiBin]++;
    
    /*  weight the mt bin by  1/(mt*dy*dmt)  */
    if (imtBin<NBINS)
      mt_histo[imtBin] += log(mtWeight/mt) ;
    
    mean_pt  += pt;
    mean_eta += eta;
  }/* end of global track loop  */
  
  /* Reset  multiplicity counters  */
  for (irange=0; irange<NRANGE; irange++) {
    dstEventSummary->mult_eta[irange] = 0;
    dstEventSummary->mult_pt[irange]  = 0;
    dstEventSummary->mult_phi[irange] = 0;
  }
  
  binRange = NBINS/NRANGE;  /* NBINS have to be a multiple of NRANGE */
  /* Fill pt, my, eta & phi  bin multiplicities  */
  for (irange=0; irange<NRANGE; irange++) { /* begin  looping over ranges  */
    minBin = binRange*irange;
    maxBin = minBin + binRange;
    for (ibin=minBin+1; ibin < maxBin; ibin++) {/* begin  looping over bins  */
      dstEventSummary->mult_pt[irange] +=
	(int) pt_histo[ibin];         /* Fill pt  bin  multiplicities  */
      dstEventSummary->mult_eta[irange]+=
	(int) eta_histo[ibin];        /* Fill eta bin  multiplicities  */
      dstEventSummary->mult_phi[irange]+=
	(int) phi_histo[ibin];        /* Fill phi bin  multiplicities  */
    }/* end of looping over bins */
    dstEventSummary->T_bins[irange] = 
      mt_inverse_slope(mt_histo, minBin+1, maxBin); /* inverse slope */
  }  /* end of looping over ranges */
  
  /*  Fill track multiplicities  */
  dstEventSummary->glb_trk_tot   = dst_track_h->nok;
  dstEventSummary->glb_trk_good  = 0;
  dstEventSummary->glb_trk_prim  = 0;
  dstEventSummary->glb_trk_plus  = glb_trk_plus;
  dstEventSummary->glb_trk_minus = glb_trk_minus ;
  
  /* Fill mean eta and pt */
  dstEventSummary->mean_pt  = mean_pt/(float)dst_track_h->nok;
  dstEventSummary->mean_eta = mean_eta/(float)dst_track_h->nok;
  
  
  /*  Fill inverse slopes  */
  T_average = mt_inverse_slope(mt_histo, 1, 100); /* use full mt_histo */
  dstEventSummary->T_average = T_average;  /* average mt inverse slope */

  dstEventSummary->n_vert_total = dst_vertex_h->nok;  /* Total # of vertices */

  /* Count v0 candidates */
  for (ivtx=0;  ivtx<dst_vertex_h->nok; ivtx++)  { /* begin vertex loop */
    vtx_id = dstVertex[ivtx].vtx_id;  /*  get vertex type */
    if (!vtx_id)
      dst_vertex_h->nok++;   /* count total number of V0s  */
    switch (vtx_id) {
    case PRIM:
      /* Fill Primary vertex information */
      dstEventSummary->prim_vrtx[0]    = dstVertex[ivtx].x;
      dstEventSummary->prim_vrtx[1]    = dstVertex[ivtx].y;
      dstEventSummary->prim_vrtx[2]    = dstVertex[ivtx].z;
      dstEventSummary->prim_vrtx_chisq = dstVertex[ivtx].pchi2;
      break;
      /* Count v0 candidates */
    case K0:
      dstEventSummary->n_vert_K0++ ;
      break;
    case LAMBDA:
      dstEventSummary->n_vert_Lambda++ ;
      break;
    case ALAMBDA:
      dstEventSummary->n_vert_ALambda++ ;
      break;
    case PILEUP:
      dstEventSummary->n_vert_pileup++ ;
      break;
    }
  }  /* end of vertex loop  */

  
  /*  Fill event ID */
  dstEventSummary->n_event[0] = dstEventHeader->n_event[0];
  dstEventSummary->n_event[1] = dstEventHeader->n_event[1];
  
  /* Fill DST production run ID */
  dstEventSummary->prod_run = dstRunHeader->run_id;
  
  free (pt_histo);
  free (mt_histo);
  free (eta_histo);
  free (phi_histo);
  /* delete [] pt_histo;   
     delete [] mt_histo;   
     delete [] eta_histo;   
     delete [] phi_histo;  
  */
  return STAFCV_OK;
}  /*  End of fill_dst_event_summary  */


