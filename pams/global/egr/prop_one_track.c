#include <stdio.h>
#include <math.h>
#include "math_constants.h"
#include "phys_constants.h"

float prop_one_track( float gtrack[7] , float target[2] , float ptrack[3] )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    prop_one_track
**: DESCRIPTION: 
**:             Propagates one dst_track to a space point (x,y)
**:             or plane.
**:             
**: AUTHOR:     S. Margetis 
**: ARGUMENTS:
**:       IN:
**:            float target[2]     - Where to propagate the tracks (x,y)
**:            gtrack     - array of input track parameters
**: DEFINITONS:[0]=x0,[1]=y0,[2]=z0,[3]=psi,[4]=tanl,[5]=(float)charge,[6]=invpt
**:      OUT:
**:            ptrack    - output track parameters
**: DEFINITONS:[0]=x0,[1]=y0,[2]=z0
**:
**: RETURNS:    5=o.k, 1=failed
**:>------------------------------------------------------------------*/
  
  
  long  iflag;
  float bfld, xpr[2], curvf, gseed;
  float psi, pt, tanl, x0, y0, z0, xp[2], xout[2], p[3], xv[3], xx0[3];
  float trk[7], r1, r2, xc[2], xc1[2], xc2[2], x[2], y[2], cut, x1[3];
  float phi0, phi;
  float pStraight[3];
  float xlocal[3],bfield[3];
  

  /* get magnetic field   */
  xlocal[0] =  xlocal[1] = xlocal[2] = 0.;
  gufld_(xlocal,bfield);
  bfld = bfield[2]/10.;         /* Tesla */
  
  iflag=0;
  if(fabs(gtrack[6]) >= 0.001 )
    {
      pt    = 1./gtrack[6] ;
    }
  else
    {
      pt = 99999.;
      iflag=5;
    }

  if(iflag != 5)
    {
      trk[0] = gtrack[0];
      trk[1] = gtrack[1];
      trk[2] = gtrack[2];
      trk[3] = gtrack[3]*C_RAD_PER_DEG;
      trk[4] = gtrack[4];
      trk[5] = gtrack[5]; /* charge */
      trk[6] = pt;
      curvf  = (float) C_D_CURVATURE;
      r1     = pt/(curvf*bfld*10.);
      /*      r1     = fabs(r1); */
      if(r1 <= 0.)
        r1=-r1;
      
      xp[0]= target[0]; 
      xp[1]= target[1];
      prop_circle_param_(trk, xc, &r1); 

      ev0_project_track_(xc, &r1, xp, xpr); 

      /*      prop_update_track_param_(xc, &r1, xpr, trk, xout, &bfld); */

      gseed=sqrt((trk[0]-xpr[0])*(trk[0]-xpr[0])  +
                 (trk[1]-xpr[1])*(trk[1]-xpr[1]));

      xout[0] = trk[2] + (2.*r1*asin(gseed/(2.*r1)))*trk[4];

      ptrack[0] = xpr[0];
      ptrack[1] = xpr[1];
      ptrack[2] = xout[0];
      
    }
  else
    {
      x0   = gtrack[0];
      y0   = gtrack[1];
      z0   = gtrack[2];
      xv[0] = target[0];
      xv[1] = target[1];
      xv[2] = 0.;
      psi  = gtrack[3]*C_RAD_PER_DEG;
      tanl = gtrack[4]*C_RAD_PER_DEG;
      pStraight[0] = cos(psi);
      pStraight[1] = sin(psi);
      pStraight[2] = tanl;
      prop_fine_approach_(xv, x0, pStraight, &xx0); 
      ptrack[0] = xx0[0];
      ptrack[1] = xx0[1];
      ptrack[2] = xx0[2]; 
    }
  
  return 5.0;
}  

