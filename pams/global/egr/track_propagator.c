/*:>--------------------------------------------------------------------
**: FILE:       track_propagator.c.template
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.14 1998/08/28 21:22:28 fisyak Exp  
**:<------------------------------------------------------------------*/
#include "track_propagator.h"
#include <stdio.h>
#include <math.h>
#include "math_constants.h"
#include "phys_constants.h"

long type_of_call track_propagator_(
  TABLE_HEAD_ST         *gtrack_h,      DST_TRACK_ST           *gtrack ,
  TABLE_HEAD_ST         *target_h,  EGR_PROPAGATE_ST           *target ,
  TABLE_HEAD_ST         *ptrack_h,      DST_TRACK_ST           *ptrack )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    track_propagator_
**: DESCRIPTION: 
**:             Propagates instances of dst_track to points, cylinders
**:             or plane.
**:             
**: AUTHOR:     S. Margetis and W. Deng
**: ARGUMENTS:
**:       IN:
**:            target     - Where to propagate the tracks
**:            target_h   - header Structure for target
**:            gtrack     - input tracks
**:            gtrack_h   - header Structure for gtrack
**:      OUT:
**:             ptrack    - output tracks
**:            ptrack_h   - header Structure for gtrack
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
  
  
  long  i, iflag;
  float bfld, xpr[2];
  float psi, pt, tanl, x0, y0, z0, xp[2], xout[2], p[3], xv[3], xx0[3];
  float trk[7], r1, r2, xc[2], xc1[2], xc2[2], x[2], y[2], cut, x1[3];
  float phi0, phi, dphi, temppsi, test=1. ;
  float pStraight[3];
  long  psiftr;
  float xlocal[3],bfield[3];
  

  /* get magnetic field   */
  
  
  xlocal[0] =  xlocal[1] = xlocal[2] = 0.;
  gufld_(xlocal,bfield);
  printf("Propagator uses field of %f KG \n",bfield[2]);
  bfld = bfield[2]/10.;         /* Tesla */
  
  for(i=0; i<gtrack_h->nok; i++) 
    {
      if(fabs(gtrack[i].invpt) >= 0.001 )
        {
          pt    = 1./gtrack[i].invpt ;
        }
      else
        {
          /* message('Error, invpt); */
          pt = 99999.;
        }

      trk[0] = gtrack[i].r0*cos(gtrack[i].phi0*C_RAD_PER_DEG);
      trk[1] = gtrack[i].r0*sin(gtrack[i].phi0*C_RAD_PER_DEG);
      trk[2] = gtrack[i].z0;
      trk[3] = gtrack[i].psi*C_RAD_PER_DEG;
      trk[4] = gtrack[i].tanl;
      trk[5] = (float) gtrack[i].icharge;
      trk[6] = pt;
      r1     = (float) (pt/(C_D_CURVATURE*bfld*10.));
      /* we should take the absolute value of r1 */
      r1     = fabs(r1);
      
      if(target->iflag == 1 || target->iflag == 2)
	{
	  xp[0]= target->x[0]; 
	  xp[1]= target->x[1];
	  prop_circle_param_(trk, xc, &r1); 
	  ev0_project_track_(xc, &r1, xp, xpr); 
          prop_update_track_param_(xc, &r1, xpr, trk, xout, &bfld);
          /*	  printf("original, %f , %f , %f, r=%f, pt=%f\n",trk[0],trk[1],trk[2],r1,pt);
                  printf("target %f , %f \n",xp[0],xp[1]); 
                  printf("circle center %f , %f \n",xc[0],xc[1]);
                  printf("new pr points %f , %f \n",xpr[0],xpr[1]);
                  printf("new z,psi %f , %f \n",xout[0],xout[1]);  */
          /*	  prop_project_track_(&psi, &q, &pt, &tanl, &x0, &y0, &bfld, &z0, xp, xout); */
	  
	  ptrack[i].r0   = sqrt(xpr[0]*xpr[0]+xpr[1]*xpr[1]);
	  ptrack[i].phi0 = atan2(xpr[1],xpr[0]);
          if(ptrack[i].phi0 < 0. ) 
            ptrack[i].phi0 = ptrack[i].phi0 + C_2PI;
          ptrack[i].phi0 = ptrack[i].phi0*C_DEG_PER_RAD;
	  ptrack[i].z0   = xout[0];
	  ptrack[i].psi  = xout[1]*C_DEG_PER_RAD;
          
	  if (target->iflag == 2)
	    {
	      xv[0] = target->x[0];
	      xv[1] = target->x[1];
	      xv[2] = target->x[2];
	      x1[0] = xpr[0];
	      x1[1] = xpr[1];
	      x1[2] = xout[0];
	      trk[0] = xpr[0];
	      trk[1] = xpr[1];
	      trk[2] = xout[0];
	      trk[3] = xout[1];
	      prop_track_mom_(trk, p);  
	      prop_fine_approach_(xv, x1, p, &xx0); 
              ptrack[i].r0   = sqrt(xx0[0]*xx0[0]+xx0[1]*xx0[1]);
              ptrack[i].phi0 = atan2(xx0[1],xx0[0]);
              if(ptrack[i].phi0 < 0. ) 
                ptrack[i].phi0 = ptrack[i].phi0 + C_2PI;
              ptrack[i].phi0 = ptrack[i].phi0*C_DEG_PER_RAD;
	      ptrack[i].z0 = xx0[2];
	    }
	}
      
      if(target->iflag == 3)
	{
          x0   = gtrack[i].r0*cos(gtrack[i].phi0*C_RAD_PER_DEG) ;
          y0   = gtrack[i].r0*sin(gtrack[i].phi0*C_RAD_PER_DEG) ;
          z0   = gtrack[i].z0;
	  prop_circle_param_(trk, xc1, &r1); 
	  xc2[0] = xc2[1] = 0.;
	  r2 = target->r;
	  cut = 0.1;
	  prop_vzero_geom_(&cut, xc1, xc2, &r1, &r2, x, y, &iflag); 
	  if(iflag == 5)
	    {
	      /* message("Can't project to the circle target->r! "); */
	      continue;
	    }
	  else if (iflag == 3)
	    {
      	      xp[0] = x[0];
	      xp[1] = y[0];
	    }
	  else
	    {
	      if(  ((x0-x[0])*(x0-x[0]) + (y0-y[0])*(y0-y[0])) <
		   ((x0-x[1])*(x0-x[1]) + (y0-y[1])*(y0-y[1]))   )
		{ 
		  xp[0] = x[0];
		  xp[1] = y[0];
		}
	      else
		{
		  xp[0] = x[1];
		  xp[1] = y[1];
		} 
	    }
	  
	  prop_circle_param_(trk, xc, &r1); 
	  ev0_project_track_(xc, &r1, xp, xpr); 
          prop_update_track_param_(xc, &r1, xpr, trk, xout, &bfld);
	  
	  ptrack[i].r0   = sqrt(xpr[0]*xpr[0]+xpr[1]*xpr[1]);
	  ptrack[i].phi0 = atan2(xpr[1],xpr[0]);
          if(ptrack[i].phi0 < 0. ) 
            ptrack[i].phi0 = ptrack[i].phi0 + C_2PI;
          ptrack[i].phi0 = ptrack[i].phi0*C_DEG_PER_RAD;
	  ptrack[i].z0 = xout[0];
	  ptrack[i].psi= xout[1]*C_DEG_PER_RAD;
	}
      
      if(target->iflag == 4)
	{
          x0   = gtrack[i].r0*cos(gtrack[i].phi0*C_RAD_PER_DEG) ;
          y0   = gtrack[i].r0*sin(gtrack[i].phi0*C_RAD_PER_DEG) ;
          z0   = gtrack[i].z0;
	  phi0 = atan2(y0,x0);
          if(fabs(trk[4]) <= 0.001) trk[4]=0.001;
          dphi = - trk[5]*(target->z-z0)/(trk[4]*r1); 
	  phi  = phi0 + dphi;
	  xpr[0] = x0 + r1*(cos(phi)-cos(phi0));
	  xpr[1] = y0 + r1*(sin(phi)-sin(phi0));
	  ptrack[i].r0   = sqrt(xpr[0]*xpr[0]+xpr[1]*xpr[1]);
	  ptrack[i].phi0 = atan2(xpr[1],xpr[0]);
          if(ptrack[i].phi0 < 0. ) 
            ptrack[i].phi0 = ptrack[i].phi0 + C_2PI;
          ptrack[i].phi0 = ptrack[i].phi0*C_DEG_PER_RAD;
	  ptrack[i].z0 = target->z;
	  temppsi = trk[3]+dphi*C_DEG_PER_RAD;
	  if(temppsi<0.)
	    {
	      psiftr = (long)(temppsi/360.-1.);
	    }
	  else
	    {
	      psiftr = (long)(temppsi/360.);
	    }
	  ptrack[i].psi= temppsi - 360.* (float)psiftr; 
	  /*  Do we have to consider if psi is within (0, 360) degree? */ 
	}
      
      if(target->iflag == 5)
	{
          x0   = gtrack[i].r0*cos(gtrack[i].phi0*C_RAD_PER_DEG) ;
          y0   = gtrack[i].r0*sin(gtrack[i].phi0*C_RAD_PER_DEG) ;
          z0   = gtrack[i].z0;
	  xv[0] = target->x[0];
	  xv[1] = target->x[1];
	  xv[2] = target->x[2];
          psi  = gtrack[i].psi*C_RAD_PER_DEG;
          tanl = gtrack[i].tanl*C_RAD_PER_DEG;
	  pStraight[0] = cos(psi);
	  pStraight[1] = sin(psi);
	  pStraight[2] = tanl;
	  prop_fine_approach_(xv, x0, pStraight, &xx0);
          ptrack[i].r0   = sqrt(xx0[0]*xx0[0]+xx0[1]*xx0[1]);
          ptrack[i].phi0 = atan2(xx0[1],xx0[0]);
          if(ptrack[i].phi0 < 0. ) 
            ptrack[i].phi0 = ptrack[i].phi0 + C_2PI;
          ptrack[i].phi0 = ptrack[i].phi0*C_DEG_PER_RAD;
	  ptrack[i].z0 = xx0[2]; 
	}
    }
  return STAFCV_OK;
}  

