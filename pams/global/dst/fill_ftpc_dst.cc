///////////////////////////////////////////////////////////////////////////
//
// $Id: fill_ftpc_dst.cc,v 1.1 2000/01/25 08:04:37 jcs Exp $
//
// $Log: fill_ftpc_dst.cc,v $
// Revision 1.1  2000/01/25 08:04:37  jcs
// convert c to c++
//
//
///////////////////////////////////////////////////////////////////////////
//    ROUTINE:     fill_ftpc_dst_
//    DESCRIPTION: Fill dst tables with FTPC results
//
//
//    AUTHOR:      Janet Seyboth   --  Max-Planck-Institute, Munich
//                 jcs@mppmu.mpg.de
//
//    ARGUMENTS:
//             IN:
//                fptrack               - FTPC track table
//                fptrack_h             - Header Structure for FTPC track table
//                fppoint               - FTPC point table
//                fppoint_h             - Header Structure for FTPC point table
//                fdepar                - FTPC fde parameter table
//                fdepar_h              - Header Structure for fde param table
//          INOUT:
//          OUT:
//                dst_track             - DST tracks table
//                dst_track_h           - Header Structure for dst_track
//                dst_point             - DST points table
//                dst_point_h           - Header Structure for dst_point
//                dst_dedx              _ DST dedx table
//                dst_dedx_h            _ Header structure for dst_dedx
//
//
//    RETURNS:    STAF Condition Value
//
//    HISTORY:
//         July 27, 1999    Janet Seyboth  remove dst_track_aux table
//                                         use StVertexId.h and StDetectorId.h
//         Dec  18, 1998    Janet Seyboth  Original
//
///////////////////////////////////////////////////////////////////////////
// ------- System includes --------------
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
// ------- STAF/ROOT generated includes ------- 
#include "fill_ftpc_dst.h"
#include "StDetectorId.h"
#include "StVertexId.h"
#include "StDedxMethod.h"
#include "math_constants.h"

const long MAXHITS = 10;       // Maximum number of hits on an FTPC track

const float FTPC_FAC = 2380.0; // Multiplication factor to achieve 4 micron accuracy
const float FTPC_MIN = -270.0;   // Minimum FTPC z-coordinate
const float FTPC_MAX =  270.0;   // Maximum FTPC z-coordinate

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 0


long  type_of_call fill_ftpc_dst_(TABLE_HEAD_ST *fptrack_h, FPT_FPTRACK_ST *fptrack,
  TABLE_HEAD_ST  *fppoint_h,           FCL_FPPOINT_ST        *fppoint,
  TABLE_HEAD_ST  *fdepar_h,            FDE_FDEPAR_ST         *fdepar,
  TABLE_HEAD_ST  *dst_track_h,         DST_TRACK_ST          *dst_track,
  TABLE_HEAD_ST  *dst_point_h,         DST_POINT_ST          *dst_point,
  TABLE_HEAD_ST  *dst_dedx_h,          DST_DEDX_ST           *dst_dedx)
{
  
  //  ==================  Local Variables  ======================== 
  int     itrk;
  int     iPoint;
  int     ftpcx, ftpcy, ftpcz;
  int     ftpcy10, ftpcy11;
  int     ihit;
  
  float   xsq, ysq, zsq;
  // ===========================  Begin Executable Code  =============== 
  

  // Loop over all tracks in FTPC track table
  for (itrk=0; itrk<fptrack_h->nok; itrk++) {
    if (dst_track_h->nok>dst_track_h->maxlen-1) {
      fprintf(stderr,"FILL_FTPC_DST: Too many dst tracks...exiting.\n");
          return STAFCV_BAD;
    }
    else {

//  Primary key
    dst_track[dst_track_h->nok].id      = dst_track_h->nok + 1;

//  initialize map and det_id 
//       primary vertex is always used for ftpc
    dst_track[dst_track_h->nok].map[0]   = 1;
    dst_track[dst_track_h->nok].map[1]   =  (int) pow(2,31);
    dst_track[dst_track_h->nok].det_id   = 0;

//  Loop over all hits on track 
     for (ihit=0; ihit<MAXHITS; ihit++) {
        if (fptrack[itrk].hitid[ihit] > -1){
//            hitid array filled by FORTRAN routine, must -1 for C routine
           iPoint =  fptrack[itrk].hitid[ihit] - 1;
           if (dst_track[dst_track_h->nok].det_id == 0 ) { 
//                 Save first hit on current track and determine detector id
              dst_track[dst_track_h->nok].x_first[0]    = 
                           fppoint[iPoint].x;
              dst_track[dst_track_h->nok].x_first[1]    = 
                           fppoint[iPoint].y;
              dst_track[dst_track_h->nok].x_first[2]    = 
                           fppoint[iPoint].z ;
//                 Rows 1->10 FTPC West  det_id= kFtpcWestId
              if (fppoint[iPoint].row >= 1 && fppoint[iPoint].row <= 10) {
                dst_track[dst_track_h->nok].det_id  = kFtpcWestId;    // West
              }
//                 Rows 11->20 FTPC East  det_id=kFtpcEastId  
              else if (fppoint[iPoint].row >= 11 && fppoint[iPoint].row <=20) {
                 dst_track[dst_track_h->nok].det_id  = kFtpcEastId;   // East
              }
           }
            dst_track[dst_track_h->nok].map[0] |= (int) pow(2,fppoint[iPoint].row);
        }
      }

//  Track finding and track fitting method 
//   (FTPC Current  set bit 11 )          
     dst_track[dst_track_h->nok].method = (int) pow(2,11);

//  Geant particle ID number for mass hypothesis used in tracking
//   (Currently not set for FTPC)                               
    dst_track[dst_track_h->nok].pid    = 0;

//  Number of points 
    dst_track[dst_track_h->nok].n_point = fptrack[itrk].nrec;

//  Number of points used in fit
    dst_track[dst_track_h->nok].n_fit_point  = fptrack[itrk].nfit;

//  Charge 
    dst_track[dst_track_h->nok].icharge      = fptrack[itrk].q;

//  Initialized to zero by FTPC dst filler module
    dst_track[dst_track_h->nok].id_start_vertex  = 0;


//  radius at start of track (cm) 
    dst_track[dst_track_h->nok].r0   = 
           sqrt(fptrack[itrk].v[0]*fptrack[itrk].v[0] 
                 + fptrack[itrk].v[1]*fptrack[itrk].v[1]);

//  azimuthal angle at start of track (deg)
    dst_track[dst_track_h->nok].phi0 = 
                 atan2(fptrack[itrk].v[1],fptrack[itrk].v[0])
                     * C_DEG_PER_RAD;

//  z-coordinate at start of track 
    dst_track[dst_track_h->nok].z0           = fptrack[itrk].v[2];

//  momentum angle at start 
    dst_track[dst_track_h->nok].psi = 
        atan2(fptrack[itrk].p[1],fptrack[itrk].p[0]);
    if (dst_track[dst_track_h->nok].psi < 0.0) {
       dst_track[dst_track_h->nok].psi = 
                    dst_track[dst_track_h->nok].psi + C_2PI;
    }
    dst_track[dst_track_h->nok].psi = 
        dst_track[dst_track_h->nok].psi * C_DEG_PER_RAD; 

//  1/pt at start 
    dst_track[dst_track_h->nok].invpt =  
      1./sqrt(fptrack[itrk].p[0]*fptrack[itrk].p[0]
                +fptrack[itrk].p[1]*fptrack[itrk].p[1]);

//  tan(dip) = pz/pt at start
    dst_track[dst_track_h->nok].tanl  = fptrack[itrk].p[2]  
                 *  dst_track[dst_track_h->nok].invpt;

//  curvature 
    dst_track[dst_track_h->nok].curvature =  fptrack[itrk].curvature;

//  covariance matrix 
//  (currently not set for FTPC) 
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

//  chi-sqare fit
    dst_track[dst_track_h->nok].chisq[0]      = fptrack[itrk].chisq[0]
          / (dst_track[dst_track_h->nok].n_fit_point - 3);
    dst_track[dst_track_h->nok].chisq[1]      = fptrack[itrk].chisq[1]
           / (dst_track[dst_track_h->nok].n_fit_point - 2);

//  Locate last hit on current track
     for (ihit=MAXHITS-1; ihit>=0; ihit--) {
        if (fptrack[itrk].hitid[ihit] > -1) {
//            hitid array filled by FORTRAN routine, must -1 for C routine
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
    zsq = (dst_track[dst_track_h->nok].x_last[2] - dst_track[dst_track_h->nok].x_first[2]) *
          (dst_track[dst_track_h->nok].x_last[2] - dst_track[dst_track_h->nok].x_first[2]);
    dst_track[dst_track_h->nok].length  = sqrt(xsq + ysq + zsq);


    dst_track[dst_track_h->nok].impact  = 0;

//  Maximum number of points 
    dst_track[dst_track_h->nok].n_max_point  = fptrack[itrk].nmax;

// bitmask quality information
    dst_track[dst_track_h->nok].iflag = 
      700 + fptrack[itrk].flag;
    if (fabs(dst_track[dst_track_h->nok].icharge) != 1. ) {
        dst_track[dst_track_h->nok].iflag   =  
             -dst_track[dst_track_h->nok].iflag + 20;
    }
    if (fabs(dst_track[dst_track_h->nok].invpt) >= 999999.)  {
        dst_track[dst_track_h->nok].iflag   = -799;
    }



//  Fill dst point table for current track 

    for (ihit=0; ihit<MAXHITS; ihit++) {
       if (dst_point_h->nok>dst_point_h->maxlen-1) {
          fprintf(stderr,"FILL_FTPC_DST: Too many dst points...exiting.\n");
          return STAFCV_BAD;
       }
       else if (fptrack[itrk].hitid[ihit] > -1) {
//          hitid array filled by FORTRAN routine, must -1 for C routine
         iPoint =  fptrack[itrk].hitid[ihit] - 1;
//       hw_position  (32 bits)             
//                    bits  0-3   det_id     
//                    bits 4-10   FTPC pad plane (1-20)                  
//                    bits 11-20  Sector number within pad-plane (1-6)    
//                    bits 21-24  number of pads in cluster                
//                    bits 25-31  number of consecutive timebins in cluster 
         dst_point[dst_point_h->nok].hw_position = 
                      fppoint[iPoint].n_bins*((int) pow(2,25))
                    + fppoint[iPoint].n_pads*((int) pow(2,21))
                    + fppoint[iPoint].sector*((int) pow(2,11))
                    + fppoint[iPoint].row*((int) pow(2,4))
                    + dst_track[dst_track_h->nok].det_id;
//         Fill space point position coordinates 
         if (fppoint[iPoint].x > FTPC_MIN && fppoint[iPoint].x < FTPC_MAX){
           ftpcx = (int) (FTPC_FAC*(fppoint[iPoint].x + FTPC_MAX));
         }
         else {
           ftpcx = 0;
         }
         if (fppoint[iPoint].y > FTPC_MIN && fppoint[iPoint].y < FTPC_MAX){
           ftpcy = (int) (FTPC_FAC*(fppoint[iPoint].y + FTPC_MAX));
         }
         else {
           ftpcy = 0;
         }
         if (fppoint[iPoint].z > FTPC_MIN && fppoint[iPoint].z < FTPC_MAX){
           ftpcz = (int) (FTPC_FAC*(fppoint[iPoint].z + FTPC_MAX));
         }
         else {
           ftpcz = 0;
         } 
         ftpcy10 = ftpcy/((int) pow(2,10));
         ftpcy11 = ftpcy - ((int) pow(2,10))*ftpcy10;
         dst_point[dst_point_h->nok].position[0] = ftpcx + ((int) pow(2,20))*ftpcy11;
         dst_point[dst_point_h->nok].position[1] = ftpcy10 + ((int) pow(2,10))*ftpcz;
//         Fill space point position errors (0.0<= error < 8.0)
         if (fppoint[iPoint].x_err >= 0.0 && fppoint[iPoint].x_err < 8.0){
           ftpcx = (int) (pow(2,17)*fppoint[iPoint].x_err);
         }
         else {
           ftpcx = 0;
         }
         if (fppoint[iPoint].y_err >= 0.0 && fppoint[iPoint].y_err < 8.0){
           ftpcy = (int) (pow(2,17)*fppoint[iPoint].y_err);
         }
         else {
           ftpcy = 0;
         }
         if (fppoint[iPoint].z_err >= 0.0 && fppoint[iPoint].z_err < 8.0){
           ftpcz =  (int) (pow(2,17)*fppoint[iPoint].z_err);
         }
         else {
           ftpcz = 0;
         }
         ftpcy10 = ftpcy/((int) pow(2,10));
         ftpcy11 = ftpcy - ((int) pow(2,10))*ftpcy10;
         dst_point[dst_point_h->nok].pos_err[0] = ftpcx + ((int) pow(2,20))*ftpcy11;
         dst_point[dst_point_h->nok].pos_err[1] = ftpcy10 + ((int) pow(2,10))*ftpcz;
//        Fill charge and maximum ADC value contained in cluster   
//                     bits 0-15    charge                        
//                     bits 16-31   maximum ADC value contained in cluster
         dst_point[dst_point_h->nok].charge  =  
                         fppoint[iPoint].max_adc*((int) pow(2,16))
                       + fppoint[iPoint].charge;
//        Fill Foreign Key to dst_track table 
         dst_point[dst_point_h->nok].id_track    =  dst_track_h->nok + 1;
         dst_point_h->nok++;
       }
    }   // End of filling dst point bank for current track


//  Fill dst_dedx table  

    if (dst_dedx_h->nok>dst_dedx_h->maxlen-1) {
      fprintf(stderr,"FILL_FTPC_DST: Too many dst_dedx entries...exiting.\n");
          return STAFCV_BAD;
    }
    else {
      dst_dedx[dst_dedx_h->nok].id_track = dst_track[dst_track_h->nok].id;
      dst_dedx[dst_dedx_h->nok].det_id = dst_track[dst_track_h->nok].det_id;

      if(fdepar->id_method == 0)
            dst_dedx[dst_dedx_h->nok].method = kTruncatedMeanId;
      else if (fdepar->id_method == 1)
            dst_dedx[dst_dedx_h->nok].method = kEnsembleTruncatedMeanId;
      else
            dst_dedx[dst_dedx_h->nok].method = kUndefinedMethodId;

      dst_dedx[dst_dedx_h->nok].ndedx = fptrack[itrk].ndedx;
      dst_dedx[dst_dedx_h->nok].dedx[0] = fptrack[itrk].dedx;
      dst_dedx[dst_dedx_h->nok].dedx[1] = 0;
      dst_dedx_h->nok++;
    }

    dst_track_h->nok++;

   }    // End of processing current track
  }     // End of processing all FTPC tracks
  return STAFCV_OK;
}
