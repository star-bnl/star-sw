
/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* ------- STAF/ROOT generated includes ------- */
#include "fill_dst_event_summary.h"
#include "StVertexDefinitions.h"
/* exteranl stuff  */
#include "global_prototypes.h"
#define  NRANGE     10

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0

/* Define global variables for defining ranges in pt, mt & eta  */
float  pt_min, pt_max,  eta_min, eta_max;

float  mt_inverse_slope(double *mt_histo,int iBegin, int iStop);

long  type_of_call fill_dst_event_summary_ (
  TABLE_HEAD_ST  *dstSummaryParam_h, DST_SUMMARY_PARAM_ST  *dstSummaryParam,
  TABLE_HEAD_ST  *dstRunHeader_h,    RUN_HEADER_ST     *dstRunHeader,
  TABLE_HEAD_ST  *dstEventHeader_h,  EVENT_HEADER_ST   *dstEventHeader,
  TABLE_HEAD_ST  *dstTrack_h,        DST_TRACK_ST      *dstTrack,
  TABLE_HEAD_ST  *dstVertex_h,       DST_VERTEX_ST     *dstVertex,
  TABLE_HEAD_ST  *dstEventSummary_h, DST_EVENT_SUMMARY_ST  *dstEventSummary)
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
   *:             dstRunHeader         - run header table
   *:             dst_dstRunHeader_h      - Header Structure for dstRunHeader  
   *:             dstEventHeader       - event header table
   *:             dst_dstEventHeader_h    - Header Structure for dstEventHeader
   *:             dstTrack             - DST tracks table       
   *:             dstTrack_h           - Header Structure for dstTrack
   *:             dstVertex            - DST vertex table
   *:             dstVertex_h          - Header Structure for dstVertex 
   *:             dstSummaryParam      - DST summary parameter table 
   *:             dstSummaryParam_h   - Header Structure for dstSummaryParam
   *:       INOUT:
   *:         OUT:
   *:             dstEventSummary      - DST event summary table 
   *:             dstEventSummary_h   - Header Structure for dstEventSummary
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
   *:                                       dstSummaryParam->n_phi_bins.
   *:      Sep 30, 1999       Margetis S.   Converted to new DST format
   *:      Jan 06, 2000       Lee Barnby    Remove unused variables.
   *:>-------------------------------------------------------------------- 
   */
  
  /*  ==================  Local Variables  ======================== */
  int     irange, i;
  int     glb_trk_good, glb_trk_prim, glb_trk_plus, glb_trk_minus;
  int     itrk, nphirange;
  int     ivtx, vtx_id, iflag;
  double  pi, piov2;
  float   pt, eta, rms_eta=0 ,phi, theta;
  float   mean_pt=0, mean_pt2=0, mean_eta=0 ;
  float   xlocal[3], bfield[3];
    
  
  /* ===========================  Begin Executable Code  =============== */
  
  /* Initialize valid rows in dstEventSummary table */
  if (!dstEventSummary_h->nok)
    dstEventSummary_h->nok = 1; 
  
  /* Initialize dstEventSummary table */
  dstEventSummary->n_event                   = 0;
  dstEventSummary->bfc_run_id                  = 0;         
  dstEventSummary->glb_trk_tot               = 0;
  dstEventSummary->glb_trk_good              = 0;
  dstEventSummary->glb_trk_prim              = 0;
  dstEventSummary->glb_trk_plus              = 0;
  dstEventSummary->glb_trk_minus             = 0;
  dstEventSummary->glb_trk_exotic            = 0;
  dstEventSummary->n_vert_total              = 0;
  for (i=0; i<5; i++) {
    dstEventSummary->n_vert_type[i]          = 0;
  }
  dstEventSummary->n_vert_pileup             = 0;
  dstEventSummary->mean_pt                   = 0;
  dstEventSummary->mean_pt2                  = 0;
  dstEventSummary->mean_eta                  = 0;
  dstEventSummary->rms_eta                   = 0;
  for (irange=0; irange<NRANGE; irange++) {
    dstEventSummary->mult_eta[irange]        = 0;
    dstEventSummary->mult_pt[irange]         = 0;
    dstEventSummary->mult_phi[irange]        = 0;
    dstEventSummary->energy_emc_eta[irange]  = 0;
    dstEventSummary->energy_emc_phi[irange]  = 0;
  }
  nphirange = (float) NRANGE ;

  for (i=0; i<3; i++) {
    dstEventSummary->prim_vrtx[i]          = 0.;
  }
  dstEventSummary->field         = 0.;
  if (!dstTrack_h->nok){
    fprintf(stderr,"FILL_DST_EVENT_SUMMARY: Zero dst tracks...exiting.\n");
    return STAFCV_BAD;
  }
  

  /* get magnetic field   */
  
  
  xlocal[0] =  xlocal[1] = xlocal[2] = 0.;
  gufld(xlocal,bfield);
  dstEventSummary->field = bfield[2];      


  /* 
     Get the min/max values for pt,eta,phi ranges from dstSummaryParam 
     table. 
     This allows user(s) to set desired min/max values for pt,eta,phi 
     multiplicity bin widths & ranges. 
  */
  pt_min  = 0.;
  pt_max  = dstSummaryParam->pt_bins[8];
  eta_min = 0.;
  eta_max = dstSummaryParam->eta_bins[8];

  /* Get double precision pi */
  pi    = acos(-1.);
  piov2 = pi/2;
  
  /* Initialize global track counters & sum variables  */
  glb_trk_good=glb_trk_prim=glb_trk_plus=glb_trk_minus=0;
  mean_pt=mean_pt2=mean_eta=rms_eta=0;
  
  /* Fill pt, eta & phi histograms  */
  for (itrk=0; itrk < dstTrack_h->nok; itrk++) {/* begin global track loop */
    /* Calculate track multiplicities  */
    if ( dstTrack[itrk].iflag <= 0 )
      continue;
    glb_trk_good++;       /*  good global tracks            */
    if ( dstTrack[itrk].icharge > 0 )
      glb_trk_plus++;     /*  charge = 1                    */
    if ( dstTrack[itrk].icharge < 0 )
      glb_trk_minus++;    /*  charge = -1                   */
    /*  Calculate kinematic varialbles for good tracks only */
    theta = piov2 - atan(dstTrack[itrk].tanl);
    eta   = -log(tan(theta/2.));
    pt    = 1./dstTrack[itrk].invpt;
    phi   = dstTrack[itrk].psi;
    if (phi<0) phi += 360.;
    
  
  /* Fill pt, eta  range multiplicities  */
  /* Modified to allow user to define the ranges in pt,eta,phi via       */
  /* dstSummaryParam table                                              */
  /* Number of ranges(NRANGE) is fixed and hardwired. User cannot change */
  /* this.                                                               */

    /*    if(pt >= 0 && pt<pt
          dstEventSummary->mult_pt[irange]++; */

    /* Sum pt, pt^2, eta, eta^2  for all good global charged tracks*/ 
    mean_pt  += pt;
    mean_pt2 += pt*pt;
    mean_eta += eta;
    rms_eta  += eta*eta;
  }/* end of global track loop  */
  
  /*  Fill track multiplicities  */
  dstEventSummary->glb_trk_tot   = dstTrack_h->nok;
  dstEventSummary->glb_trk_good  = glb_trk_good;
  dstEventSummary->glb_trk_plus  = glb_trk_plus;
  dstEventSummary->glb_trk_minus = glb_trk_minus ;
  
  /* Fill mean eta, pt, pt^2 and rms_eta */
  dstEventSummary->mean_pt  = mean_pt/(float)glb_trk_good;
  dstEventSummary->mean_pt2 = mean_pt2/(float)glb_trk_good;
  dstEventSummary->mean_eta = mean_eta/(float)glb_trk_good;
  dstEventSummary->rms_eta  = sqrt(rms_eta/(float)glb_trk_good);
  

  /* Fill total # of vertices */
  dstEventSummary->n_vert_total = dstVertex_h->nok;  

  /* Count v0 candidates */
  for (ivtx=0;  ivtx<dstVertex_h->nok; ivtx++)  { /* begin vertex loop */
    vtx_id = dstVertex[ivtx].vtx_id;  /*  get vertex type */
    iflag  = dstVertex[ivtx].iflag;

    if(vtx_id == 1 && iflag ==1 ) {
      /* Fill Primary vertex information */
      dstEventSummary->prim_vrtx[0]    = dstVertex[ivtx].x;
      dstEventSummary->prim_vrtx[1]    = dstVertex[ivtx].y;
      dstEventSummary->prim_vrtx[2]    = dstVertex[ivtx].z;
      dstEventSummary->glb_trk_prim    = dstVertex[ivtx].n_daughters;
    }
    if (vtx_id != kUndefinedVertexIdentifier && 
	vtx_id != kEventVertexIdentifier     &&
	vtx_id != kV0DecayIdentifier         &&
	vtx_id != kXiDecayIdentifier         &&
	vtx_id != kKinkDecayIdentifier) {
      printf ("Undefined vertex type = %d\n", vtx_id);
    }
    else  dstEventSummary->n_vert_type[vtx_id]++ ;

      if(vtx_id == 5)  dstEventSummary->n_vert_pileup++ ;
      
 }  /* end of vertex loop  */

  
  /*  Fill event ID */
  dstEventSummary->n_event = dstEventHeader->n_event;
  
  /* Fill DST production run ID */
  dstEventSummary->bfc_run_id = dstRunHeader->bfc_run_id;
  
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




