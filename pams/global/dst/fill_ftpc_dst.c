/* ------- System includes -------------- */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* ------- STAF/ROOT generated includes ------- */
#include "fill_ftpc_dst.h"
#include "StDetectorId.h"
#include "StVertexId.h"
#include "math_constants.h"

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
   *:      July 27, 1999    Janet Seyboth  remove dst_track_aux table
   *:                                      use StVertexId.h and StDetectorId.h
   *:      Dec  18, 1998    Janet Seyboth  Original
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
    if (dst_vertex[i].vtx_id == kEventVtxId  && dst_vertex[i].iflag == 1){
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


/*  initialize map and det_id  */
    /*   primary vertex is always used for ftpc   */
    dst_track[dst_track_h->nok].map[0]   = 1;
    dst_track[dst_track_h->nok].map[1]   =  pow(2,31);
    dst_track[dst_track_h->nok].det_id   = 0;

/*  Loop over all hits on track       */
     for (ihit=0; ihit<MAXHITS; ihit++) {
printf(" fptrack[itrk].hitid[%d] = %d\n",ihit,fptrack[itrk].hitid[ihit]);
        if (fptrack[itrk].hitid[ihit] > -1){
           /* hitid array filled by FORTRAN routine, must -1 for C routine */
           iPoint =  fptrack[itrk].hitid[ihit] - 1;
           if (dst_track[dst_track_h->nok].det_id == 0 ) { 
              /*   Save first hit on current track and determine detector id  */
              dst_track[dst_track_h->nok].x_first[0]    = 
                           fppoint[iPoint].x;
              dst_track[dst_track_h->nok].x_first[1]    = 
                           fppoint[iPoint].y;
              dst_track[dst_track_h->nok].x_first[2]    = 
                           fppoint[iPoint].z ;
              /*   Rows 1->10 FTPC West  det_id= kFtpcWestId  */
              if (fppoint[iPoint].row >= 1 && fppoint[iPoint].row <= 10) {
                dst_track[dst_track_h->nok].det_id  = kFtpcWestId;    /* West */
              }
              /*   Rows 11->20 FTPC East  det_id=kFtpcEastId  */
              else if (fppoint[iPoint].row >= 11 && fppoint[iPoint].row <=20) {
                 dst_track[dst_track_h->nok].det_id  = kFtpcEastId;   /* East */
              }
           }
printf("fppoint[%d].row = %d\n",iPoint,fppoint[iPoint].row);
            dst_track[dst_track_h->nok].map[0] = 
                    dst_track[dst_track_h->nok].map[0] 
                  + pow(2,fppoint[iPoint].row);
        }
      }

/*  Track finding and track fitting method */
/*  (Currently undefined )                 */
    dst_track[dst_track_h->nok].method = 0;

/*  Geant particle ID number for mass hypothesis used in tracking */
/*   (Currently not set for FTPC)                                 */
    dst_track[dst_track_h->nok].pid    = 0;

/*  Number of points  */
    dst_track[dst_track_h->nok].n_point = fptrack[itrk].nrec;

/*  Maximum number of points */
    dst_track[dst_track_h->nok].n_max_point  = MAXHITS;

/*  Number of points used in fit */
    dst_track[dst_track_h->nok].n_fit_point  = fptrack[itrk].nfit;

/*  Charge */
    dst_track[dst_track_h->nok].icharge      = fptrack[itrk].q;

/*  Initialized to zero by FTPC dst filler module   */
    dst_track[dst_track_h->nok].id_start_vertex  = 0;


/*  radius at start of track (cm) */
    dst_track[dst_track_h->nok].r0   = 
           sqrt(fptrack[itrk].v[0]*fptrack[itrk].v[0] 
                 + fptrack[itrk].v[1]*fptrack[itrk].v[1]);

/*  azimuthal angle at start of track (deg) */
    dst_track[dst_track_h->nok].phi0 = 
                 atan2(fptrack[itrk].v[1],fptrack[itrk].v[0])
                     * C_DEG_PER_RAD;

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

/*  curvature */
    dst_track[dst_track_h->nok].curvature =  fptrack[itrk].curvature;

/*  covariance matrix */
/*  (currently not set for FTPC)  */
    dst_track[dst_track_h->nok].covar[0] = 0;
    dst_track[dst_track_h->nok].covar[1] = 0;
    dst_track[dst_track_h->nok].covar[2] = 0;
    dst_track[dst_track_h->nok].covar[3] = 0;
    dst_track[dst_track_h->nok].covar[4] = 0;
    dst_track[dst_track_h->nok].covar[5] = 0;
    dst_track[dst_track_h->nok].covar[6] = 0;
    dst_track[dst_track_h->nok].covar[7] = 0;
    dst_track[dst_track_h->nok].covar[8] = 0;
    dst_track[dst_track_h->nok].covar[9] = 0;
    dst_track[dst_track_h->nok].covar[10] = 0;
    dst_track[dst_track_h->nok].covar[11] = 0;
    dst_track[dst_track_h->nok].covar[12] = 0;
    dst_track[dst_track_h->nok].covar[13] = 0;
    dst_track[dst_track_h->nok].covar[14] = 0;

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


/*    DEBUG     JCS    */
printf("dst_track[dst_track_h->nok].id = %d\n",dst_track[dst_track_h->nok].id);
printf(".iflag = %x\n",dst_track[dst_track_h->nok].iflag);
printf(".det_id = %d\n",dst_track[dst_track_h->nok].det_id);
printf(".method = %d\n",dst_track[dst_track_h->nok].method);
printf(".pid = %d\n",dst_track[dst_track_h->nok].pid);
printf(".n_point = %d\n",dst_track[dst_track_h->nok].n_point);
printf(".n_max_point = %d\n",dst_track[dst_track_h->nok].n_max_point);
printf(".icharge = %d\n",dst_track[dst_track_h->nok].icharge);
printf(".id_start_vertex = %ld\n",dst_track[dst_track_h->nok].id_start_vertex);
printf(".map[0] = %lx\n",dst_track[dst_track_h->nok].map[0]);
printf(".map[1] = %lx\n",dst_track[dst_track_h->nok].map[1]);
printf(".r0 = %f\n",dst_track[dst_track_h->nok].r0);
printf("phi0 = %f\n",dst_track[dst_track_h->nok].phi0);
printf("z0 = %f\n",dst_track[dst_track_h->nok].z0);
printf("psi = %f\n",dst_track[dst_track_h->nok].psi);
printf("tanl = %f\n",dst_track[dst_track_h->nok].tanl);
printf("invpt = %f\n",dst_track[dst_track_h->nok].invpt);
printf("curvature = %f\n",dst_track[dst_track_h->nok].curvature);
printf("x_first[0] =  %f\n",dst_track[dst_track_h->nok].x_first[0]);
printf("x_first[1] =  %f\n",dst_track[dst_track_h->nok].x_first[1]);
printf("x_first[2] =  %f\n",dst_track[dst_track_h->nok].x_first[2]);
printf("x_last[0] =  %f\n",dst_track[dst_track_h->nok].x_last[0]);
printf("x_last[1] =  %f\n",dst_track[dst_track_h->nok].x_last[1]);
printf("x_last[2] =  %f\n",dst_track[dst_track_h->nok].x_last[2]);
printf("length =  %f\n",dst_track[dst_track_h->nok].length);
printf("impact  =  %f\n",dst_track[dst_track_h->nok].impact);

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
/*                    bits  0-3   det_id                                    */
/*                    bits 4-10   FTPC pad plane (1-20)                     */
/*                    bits 11-20  Sector number within pad-plane (1-6)      */
/*                    bits 21-24  number of pads in cluster                 */
/*                    bits 25-31  number of consecutive timebins in cluster */
         dst_point[dst_point_h->nok].hw_position = 
                      fppoint[iPoint].n_bins*pow(2,25)
                    + fppoint[iPoint].n_pads*pow(2,21)
                    + fppoint[iPoint].sector*pow(2,11)
                    + fppoint[iPoint].row*pow(2,4)
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
         ftpcy10 = ftpcy/pow(2,10);
         ftpcy11 = ftpcy - pow(2,10)*ftpcy10;
         dst_point[dst_point_h->nok].position[0] = ftpcx + pow(2,20)*ftpcy11;
         dst_point[dst_point_h->nok].position[1] = ftpcy10 + pow(2,10)*ftpcz;
       /*  Fill space point position errors (0.0<= error < 8.0) */
         if (fppoint[iPoint].x_err >= 0.0 && fppoint[iPoint].x_err < 8.0){
           ftpcx = pow(2,17)*fppoint[iPoint].x_err;
         }
         else {
           ftpcx = 0;
         }
         if (fppoint[iPoint].y_err >= 0.0 && fppoint[iPoint].y_err < 8.0){
           ftpcy = pow(2,17)*fppoint[iPoint].y_err;
         }
         else {
           ftpcy = 0;
         }
         if (fppoint[iPoint].z_err >= 0.0 && fppoint[iPoint].z_err < 8.0){
           ftpcz = pow(2,17)*fppoint[iPoint].z_err;
         }
         else {
           ftpcz = 0;
         }
         ftpcy10 = ftpcy/pow(2,10);
         ftpcy11 = ftpcy - pow(2,10)*ftpcy10;
         dst_point[dst_point_h->nok].pos_err[0] = ftpcx + pow(2,20)*ftpcy11;
         dst_point[dst_point_h->nok].pos_err[1] = ftpcy10 + pow(2,10)*ftpcz;
       /* Fill charge and maximum ADC value contained in cluster           */
       /*              bits 0-15    charge                                 */
       /*              bits 16-31   maximum ADC value contained in cluster */
         dst_point[dst_point_h->nok].charge  =  
                         fppoint[iPoint].max_adc*pow(2,16)
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
      dst_dedx[dst_dedx_h->nok].method = 0;
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
