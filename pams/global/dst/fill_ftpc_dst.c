/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* ------- STAF/ROOT generated includes ------- */
#include "fill_ftpc_dst.h"

#define TWO4   16                    /* 2**4  */
#define TWO10  1024                  /* 2**10 */
#define TWO11  2048                  /* 2**11 */
#define TWO16  65536                 /* 2**16 */
#define TWO17  131072                /* 2**17 */
#define TWO20  1048576               /* 2**20 */
#define TWO21  2097152               /* 2**21 */
#define TWO25  33554432              /* 2**25 */

#define C_2_PI                        0.63661977236758134308

#define MAXHITS    10        /* Maximum number of hits on an FTPC track */

#define FTPC_FAC   2380.0    /* Multiplication fator to achieve 4 micron accuracy */
#define FTPC_MIN   -270.0    /* Minimum FTPC z-coordinate */
#define FTPC_MAX    270.0    /* Maximum FTPC z-coordinate */

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0


long  type_of_call fill_ftpc_dst_(TABLE_HEAD_ST *fptrack_h, FPT_FPTRACK_ST *fptrack,
  TABLE_HEAD_ST  *fppoint_h,           FCL_FPPOINT_ST        *fppoint,
  TABLE_HEAD_ST  *dst_track_h,         DST_TRACK_ST          *dst_track,
  TABLE_HEAD_ST  *dst_track_aux_h,     DST_TRACK_AUX_ST      *dst_track_aux,
  TABLE_HEAD_ST  *dst_point_h,         DST_POINT_ST          *dst_point,
  TABLE_HEAD_ST  *dst_vertex_h,        DST_VERTEX_ST         *dst_vertex,
  TABLE_HEAD_ST  *dst_dedx_h,          DST_DEDX_ST           *dst_dedx)
{
  /*
   *
   *:>-------------------------------------------------------------------- 
   *: ROUTINE:     fill_ftpc_dst_
   *: DESCRIPTION: Fill dst tables with FTPC results
   *:              
   *:
   *: AUTHOR:      Janet Seyboth   --  Max-Planck-Institute, Munich
   *:              jcs@mppmu.mpg.de
   *:
   *: ARGUMENTS:
   *:          IN:
   *:             fptrack               - FTPC track table 
   *:             fptrack_h             - Header Structure for FTPC track table
   *:             fppoint               - FTPC point table
   *:             fppoint_h             - Header Structure for FTPC point table
   *:       INOUT:
   *:       OUT:
   *:             dst_track             - DST tracks table       
   *:             dst_track_h           - Header Structure for dst_track
   *:             dst_track_aux         - DST auxillary tracks table       
   *:             dst_track_aux_h       - Header Structure for dst_track_aux
   *:             dst_point             - DST points table       
   *:             dst_point_h           - Header Structure for dst_point
   *:             dst_vertex            - DST vertex table
   *:             dst_vertex_h          - Header Structure for dst_vertex 
   *:             dst_dedx              _ DST dedx table
   *:             dst_dedx_h            _ Header structure for dst_dedx
   *:             
   *:
   *: RETURNS:    STAF Condition Value
   *:
   *: HISTORY:    
   *:      Dec 18, 1998    Janet Seyboth  Original
   *:
   *:>-------------------------------------------------------------------- 
   */
  
  /*  ==================  Local Variables  ======================== */
  int     i;
  int     ivtx_prim;
  int     itrk;
  int     iPoint;
  int     ftpcx, ftpcy, ftpcz;
  int     ftpcy10, ftpcy11;
  int     ihit;
  
  float   xsq, ysq;
  /* ===========================  Begin Executable Code  =============== */
  
  /* Locate primary vertex  */
  ivtx_prim = -1;
  for (i=0; i<dst_vertex_h->nok; i++) {
    if (dst_vertex[i].vtx_id = 1 ){
       ivtx_prim = i;
       break;
    } 
  }

  /* Loop over all tracks in FTPC track table */
  for (itrk=0; itrk<fptrack_h->nok; itrk++) {
    if (dst_track_h->nok>dst_track_h->maxlen-1) {
      fprintf(stderr,"FILL_FTPC_DST: Too many dst tracks...exiting.\n");
          return STAFCV_BAD;
    }
    else {
/*  Primary key */
    dst_track[dst_track_h->nok].id      = dst_track_h->nok + 1;
/* bitmask quality information */
    dst_track[dst_track_h->nok].iflag   = 0;
/*  Locate first hit on current track and determine detector id  */
     for (ihit=0; ihit<MAXHITS; ihit++) {
        if (fptrack[itrk].hitid[ihit] > -1){
           /* hitid array filled by FORTRAN routine, must -1 for C routine */
           iPoint =  fptrack[itrk].hitid[ihit] - 1;
           dst_track[dst_track_h->nok].x_first[0]    = 
                           fppoint[iPoint].x;
           dst_track[dst_track_h->nok].x_first[1]    = 
                           fppoint[iPoint].y;
           dst_track[dst_track_h->nok].x_first[2]    = 
                           fppoint[iPoint].z ;
           /*   Rows 1->10 FTPC West  det_id=4  */
           if (fppoint[iPoint].row >= 1 && fppoint[iPoint].row <= 10) {
               dst_track[dst_track_h->nok].det_id  = 4;    /* West */
           }
           /*   Rows 11->20 FTPC East  det_id=5  */
           else if (fppoint[iPoint].row >= 11 && fppoint[iPoint].row <=20) {
               dst_track[dst_track_h->nok].det_id  = 5;   /* East */
           }
           break;
        }
      }
/*  Number of points  */
    dst_track[dst_track_h->nok].n_point = fptrack[itrk].nrec;
/*  Maximum number of points */
    dst_track[dst_track_h->nok].n_max_point  = MAXHITS;
/*  Number of points used in fit */
    dst_track[dst_track_h->nok].n_fit_point  = fptrack[itrk].nfit;
/*  Charge */
    dst_track[dst_track_h->nok].icharge      = fptrack[itrk].q;
/*  x-coordinate at start of track  */
    dst_track[dst_track_h->nok].x0           = fptrack[itrk].v[0];
/*  y-coordinate at start of track  */
    dst_track[dst_track_h->nok].y0           = fptrack[itrk].v[1];
/*  z-coordinate at start of track  */
    dst_track[dst_track_h->nok].z0           = fptrack[itrk].v[2];
/*  momentum angle at start */
    dst_track[dst_track_h->nok].psi = 
        atan2(fptrack[itrk].p[2],fptrack[itrk].p[1]);
    if (dst_track[dst_track_h->nok].psi < 0.0) {
       dst_track[dst_track_h->nok].psi = 
                    dst_track[dst_track_h->nok].psi*C_2_PI;
    }
/*  tan(dip) = pz/pt at start  */
    dst_track[dst_track_h->nok].tanl  = fptrack[itrk].p[2]/
                   sqrt(fptrack[itrk].p[1]*fptrack[itrk].p[1]
                        + fptrack[itrk].p[3]*fptrack[itrk].p[3]);
/*  1/pt at start */
    dst_track[dst_track_h->nok].invpt =  
      1./sqrt(fptrack[itrk].p[1]*fptrack[itrk].p[1]
                +fptrack[itrk].p[2]*fptrack[itrk].p[2]);
/*  covariance matrix, diagonal elements */
    dst_track[dst_track_h->nok].covar_diag[0] = 0;
    dst_track[dst_track_h->nok].covar_diag[1] = 0;
    dst_track[dst_track_h->nok].covar_diag[2] = 0;
    dst_track[dst_track_h->nok].covar_diag[3] = 0;
    dst_track[dst_track_h->nok].covar_diag[4] = 0;
    dst_track[dst_track_h->nok].covar_diag[5] = 0;
/*  chi-sqare fit */
    dst_track[dst_track_h->nok].chisq[0]      = fptrack[itrk].chisq[0];
    dst_track[dst_track_h->nok].chisq[1]      = fptrack[itrk].chisq[1];
/*  Locate last hit on current track     */
     for (ihit=MAXHITS-1; ihit>=0; ihit--) {
        if (fptrack[itrk].hitid[ihit] > -1) {
           /* hitid array filled by FORTRAN routine, must -1 for C routine */
           iPoint =  fptrack[itrk].hitid[ihit] - 1;
           dst_track[dst_track_h->nok].x_last[0]     = 
                           fppoint[iPoint].x;
           dst_track[dst_track_h->nok].x_last[1]     =  
                           fppoint[iPoint].y;
           dst_track[dst_track_h->nok].x_last[2]     = 
                           fppoint[iPoint].z;
          break;
        }
     }
    xsq = (dst_track[dst_track_h->nok].x_last[0] - dst_track[dst_track_h->nok].x_first[0]) *
          (dst_track[dst_track_h->nok].x_last[0] - dst_track[dst_track_h->nok].x_first[0]);
    ysq = (dst_track[dst_track_h->nok].x_last[1] - dst_track[dst_track_h->nok].x_first[1]) *
          (dst_track[dst_track_h->nok].x_last[1] - dst_track[dst_track_h->nok].x_first[1]);
    dst_track[dst_track_h->nok].length  = sqrt(xsq + ysq);
    dst_track[dst_track_h->nok].impact  = 0;
    dst_track[dst_track_h->nok].ndegf   = 0;              
    dst_track[dst_track_h->nok].id_global_pid         = 0; 
    dst_track[dst_track_h->nok].id_hypo_pid           = 0;
/*  COMMENT OUT FOR MDC2   */
/* CURRENTLY treating ALL ftpc tracks as if they are primary tracks */
/*    if (ivtx_prim >= 0 ) {                               MDC2 */
/*       dst_track[dst_track_h->nok].id_start_vertex       =   MDC2 */
/*                    dst_vertex[ivtx_prim].id;            MDC2 */
/* Increase number of charged daughter tracks for primary vertex */
/*       dst_vertex[ivtx_prim].n_daughters = dst_vertex[ivtx_prim].n_daughters++;                                                          MDC2 */
/*    }                                                    MDC2 */
/*    else {                                               MDC2 */
        dst_track[dst_track_h->nok].id_start_vertex  = 0;
/*    }                                                    MDC2 */
/*  COMMENT OUT FOR MDC2                                   MDC2 */
    dst_track[dst_track_h->nok].id_stop_vertex        = 0;
    dst_track[dst_track_h->nok].id_emc                = 0;
    dst_track[dst_track_h->nok].id_smd                = 0;

/*  Fill dst auxillary track table */

    if (dst_track_aux_h->nok>dst_track_aux_h->maxlen-1) {
      fprintf(stderr,"FILL_FTPC_DST: Too many auxilliary dst tracks...exiting.\n");
          return STAFCV_BAD;
    }
    else {
      dst_track_aux[dst_track_aux_h->nok].id_track  = dst_track_h->nok + 1;
      dst_track_aux[dst_track_aux_h->nok].residuals[0] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].residuals[1] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[0] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[1] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[2] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[3] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[4] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[5] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[6] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[7] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[8] = 0.0;
      dst_track_aux[dst_track_aux_h->nok].covar_off_diag[9] = 0.0;
      dst_track_aux_h->nok++;
     }      /* End of filling dst auxillary track table */

/*  Fill dst point table for current track */

    for (ihit=0; ihit<MAXHITS; ihit++) {
       if (dst_point_h->nok>dst_point_h->maxlen-1) {
          fprintf(stderr,"FILL_FTPC_DST: Too many dst points...exiting.\n");
          return STAFCV_BAD;
       }
       else if (fptrack[itrk].hitid[ihit] > -1) {
         /* hitid array filled by FORTRAN routine, must -1 for C routine */
         iPoint =  fptrack[itrk].hitid[ihit] - 1;
/*       hw_position  (32 bits)                                             */
/*                    bits  0-3   det_id  (4 = FTPC West, 5 = FTPC East     */
/*                    bits 4-10   FTPC pad plane (1-20)                     */
/*                    bits 11-20  Sector number within pad-plane (1-6)      */
/*                    bits 21-24  number of pads in cluster                 */
/*                    bits 25-31  number of consecutive timebins in cluster */
         dst_point[dst_point_h->nok].hw_position = 
                      fppoint[iPoint].n_bins*TWO25
                    + fppoint[iPoint].n_pads*TWO21
                    + fppoint[iPoint].sector*TWO11
                    + fppoint[iPoint].row*TWO4
                    + dst_track[dst_track_h->nok].det_id;
       /*  Fill space point position coordinates  */
         if (fppoint[iPoint].x > FTPC_MIN && fppoint[iPoint].x < FTPC_MAX){
           ftpcx = FTPC_FAC*(fppoint[iPoint].x + FTPC_MAX);
         }
         else {
           ftpcx = 0;
         }
         if (fppoint[iPoint].y > FTPC_MIN && fppoint[iPoint].y < FTPC_MAX){
           ftpcy = FTPC_FAC*(fppoint[iPoint].y + FTPC_MAX);
         }
         else {
           ftpcy = 0;
         }
         if (fppoint[iPoint].z > FTPC_MIN && fppoint[iPoint].z < FTPC_MAX){
           ftpcz = FTPC_FAC*(fppoint[iPoint].z + FTPC_MAX);
         }
         else {
           ftpcz = 0;
         } 
         ftpcy10 = ftpcy/TWO10;
         ftpcy11 = ftpcy - TWO10*ftpcy10;
         dst_point[dst_point_h->nok].position[0] = ftpcx + TWO20*ftpcy11;
         dst_point[dst_point_h->nok].position[1] = ftpcy10 + TWO10*ftpcz;
       /*  Fill space point position errors (0.0<= error < 8.0) */
         if (fppoint[iPoint].x_err >= 0.0 && fppoint[iPoint].x_err < 8.0){
           ftpcx = TWO17*fppoint[iPoint].x_err;
         }
         else {
           ftpcx = 0;
         }
         if (fppoint[iPoint].y_err >= 0.0 && fppoint[iPoint].y_err < 8.0){
           ftpcy = TWO17*fppoint[iPoint].y_err;
         }
         else {
           ftpcy = 0;
         }
         if (fppoint[iPoint].z_err >= 0.0 && fppoint[iPoint].z_err < 8.0){
           ftpcz = TWO17*fppoint[iPoint].z_err;
         }
         else {
           ftpcz = 0;
         }
         ftpcy10 = ftpcy/TWO10;
         ftpcy11 = ftpcy - TWO10*ftpcy10;
         dst_point[dst_point_h->nok].pos_err[0] = ftpcx + TWO20*ftpcy11;
         dst_point[dst_point_h->nok].pos_err[1] = ftpcy10 + TWO10*ftpcz;
       /* Fill charge and maximum ADC value contained in cluster           */
       /*              bits 0-15    charge                                 */
       /*              bits 16-31   maximum ADC value contained in cluster */
         dst_point[dst_point_h->nok].charge  =  
                         fppoint[iPoint].max_adc*TWO16
                       + fppoint[iPoint].charge;
       /* Fill Foreign Key to dst_track table */
         dst_point[dst_point_h->nok].id_track    =  dst_track_h->nok + 1;
         dst_point_h->nok++;
       }
    }   /* End of filling dst point bank for current track */


/*  Fill dst_dedx table   */

    if (dst_dedx_h->nok>dst_dedx_h->maxlen-1) {
      fprintf(stderr,"FILL_FTPC_DST: Too many dst_dedx entries...exiting.\n");
          return STAFCV_BAD;
    }
    else {
      dst_dedx[dst_dedx_h->nok].id_track = dst_track[dst_track_h->nok].id;
      dst_dedx[dst_dedx_h->nok].det_id = dst_track[dst_track_h->nok].det_id;
      dst_dedx[dst_dedx_h->nok].iflag = 0;
      dst_dedx[dst_dedx_h->nok].ndedx = fptrack[itrk].ndedx;
      dst_dedx[dst_dedx_h->nok].dedx[0] = fptrack[itrk].dedx;
      dst_dedx[dst_dedx_h->nok].dedx[1] = 0;
      dst_dedx_h->nok++;
    }

    dst_track_h->nok++;

   }    /* End of processing current track */
  }     /* End of processing all FTPC tracks */
  return STAFCV_OK;
}
