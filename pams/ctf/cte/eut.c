
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include "egr_globtrk.h"



#define MPI  3.141592654
#define todeg   (180./MPI)
/*
    Function declarations
*/ 
int eut_extra_r_cyl_ ( float b_field, EGR_GLOBTRK_ST *track,
                       float *r,  float* phi, float* z,
                       float *xc, float* yc,  float* rc ) ;

/*
    Global variables
*/ 
float b_fact = 0.0029980 ;
 

int eut_circles_x_ ( float *lx0, float *ly0, float *la0, float *lr0,
    	             float *lx1, float *ly1, float *lx2, float *ly2 )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Finds intersection between two circles      
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             x0,y0,a0        - First circle center and radius
**:             r0              - Second circle radius [center (0,0)] 
**:         OUT:
**:             x1,y1,x2,y2     - Intersection coordinates
**:
**: RETURNS:    number of solutions
**:>------------------------------------------------------------------*/
{
   int   nsol ;
   double x0, y0, a0, r0 ;
   double Bb, a, b, c ;
   double xs, ys, fs  ;
   double xf[5], yf[5] ; 
   double prec, dr ;
   float fact ;
/*---------------------------------------------------------------------
     Make pointers into variables
---------------------------------------------------------------------- */
   x0  = *lx0 ;
   y0  = *ly0 ;
   a0  = *la0 ;
   r0  = *lr0 ;

/*---------------------------------------------------------------------
     Set to zero some stuff now  
---------------------------------------------------------------------- */
   nsol      = 0  ;
   xf[1]     = 0. ;
   yf[1]     = 0. ;
   xf[2]     = 0. ;
   yf[2]     = 0. ;
   prec      = 1. ; /*   precision control     */
/*------------------------------------------------------------------------
    The solution is given by a second order equation
-------------------------------------------------------------------------*/

   Bb = 0.5 * ( x0*x0 + y0*y0 + r0*r0 - a0*a0 ) ;
      
   a  = y0*y0 + x0*x0 ;
   b  = - 2 * Bb * y0 ;
   c  = Bb*Bb - x0*x0 * r0*r0 ;

   fact = b*b - 4 * a * c ;
/*------------------------------------------------------------------------
          Check there is a solution
------------------------------------------------------------------------ */
   if ( fact > 0.) 
   {
      fact = sqrt(fact) ;
/*-------------------------------------------------------------------------
       Try the y positive solution
------------------------------------------------------------------------ */
      ys = ( -b + fact ) / ( 2. * a ) ;
      fs = r0*r0 - ys*ys ;
      if ( fs > 0. ) 
/*-------------------------------------------------------------------------
       Try the x positive solution
------------------------------------------------------------------------ */
      {
         xs =  sqrt(fs) ;
         dr = fabs((xs-x0)*(xs-x0)+(ys-y0)*(ys-y0)-a0*a0) ;
         if ( dr < prec ) 
         { 
            nsol++ ;
            xf[nsol] = xs ;
            yf[nsol] = ys ;
         }    
/*-------------------------------------------------------------------------
       Now the x negative solution
------------------------------------------------------------------------ */
         xs = - xs ;
         dr = fabs((xs-x0)*(xs-x0)+(ys-y0)*(ys-y0)-a0*a0) ;
         if ( dr < prec )
         {
            nsol++ ;
            xf[nsol] = xs ;
            yf[nsol] = ys ;
         }    
      }
/*-------------------------------------------------------------------------
       Try the y negative solution
------------------------------------------------------------------------ */
      ys = ( -b - fact ) / ( 2. * a ) ;
      fs = r0*r0 - ys*ys ;
      if ( fs > 0. )
/*-------------------------------------------------------------------------
       Try the x positive solution
------------------------------------------------------------------------ */
      {
         xs =  sqrt(fs) ;
         dr = fabs((xs-x0)*(xs-x0)+(ys-y0)*(ys-y0)-a0*a0) ;
         if ( dr < prec )
         {
            nsol++ ;
            xf[nsol] = xs ;
            yf[nsol] = ys ;
         }
/*-------------------------------------------------------------------------
       Now the x negative solution
------------------------------------------------------------------------ */
         xs = - xs ;
         dr = fabs((xs-x0)*(xs-x0)+(ys-y0)*(ys-y0)-a0*a0) ;
         if ( dr < prec )
         {
            nsol++ ;
            xf[nsol] = xs ;
            yf[nsol] = ys ;
         }
      }
/*-----------------------------------------------------------------------
     If there are no solution return 0
------------------------------------------------------------------------ */
     else return 0 ;
/*-----------------------------------------------------------------------
     Check not more than two solutions
------------------------------------------------------------------------ */
      if ( nsol > 2 ) 
      {
          printf ( "\n nsol: &d is too many solutions \n", nsol );
          nsol = 0 ;
          return nsol ;
      } 
/*-----------------------------------------------------------------------
     Copy solutions into arguments
------------------------------------------------------------------------ */
      *lx1 = xf[1] ;
      *ly1 = yf[1] ;

      *lx2 = xf[2] ;
      *ly2 = yf[2] ;
   }

   return nsol ;
}




int eut_clo_ ( float* xc, float* yc, float* rr, float* xp, float* yp )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Finds point of closest approach
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             xc, yc          - Track center
**:             rr              - Track radius
**:         OUT:
**:             xp,yp           - Point closest approach to center
**:
**: RETURNS:    1
**:>------------------------------------------------------------------*/
{
   float x1, y1, x2, y2, d1, d2, f1, f2, fact ;
/*----------------------------------------------------------
       Solve the equations 
----------------------------------------------------------*/          
   fact = *rr / sqrt ( *xc * *xc + *yc * *yc ) ;
   f1   = 1. + fact ;
   f2   = 1. - fact ;

   x1 = *xc * f1 ;
   y1 = *yc * f1 ;
   d1 = sqrt ( x1 * x1 + y1 * y1 ) ;

   x2 = *xc * f2 ;
   y2 = *yc * f2 ;
   d2 = sqrt ( x2 * x2 + y2 * y2 ) ;
/*---------------------------------------------------------------------
       Choose the closest
--------------------------------------------------------------------- */
   if ( d1 < d2 ) {
      *xp = x1 ;
      *yp = y1 ;
   }
   else {
      *xp = x2 ;
      *yp = y2 ;
   }

   return 1 ;
}



int eut_closest_ ( float b_field,
                   EGR_GLOBTRK_ST *track, float *lxp, float *lyp, float *lzp )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Finds point of closest approach
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             b_field         - Magnetic field in Tesla
**:             track           - Global track pointer
**:         OUT:
**:             xp,yp,zp        - Point closest approach to center
**:
**: RETURNS:    1
**:>------------------------------------------------------------------*/
{
   float phi0, x0, y0      ; /*  phi and coord. of first point in the track */
   float rr                ; /*  radius track                               */
   float xc, yc            ; /*  center track circle in x-y plane           */
   float stot              ; /*  trajectory length in xy                    */
   float angle, dangle     ;
   int   ok                ;


   float x1, y1, x2, y2, d1, d2, f1, f2 ;

/*----------------------------------------------------------
       Get track parameters
----------------------------------------------------------*/
   phi0 = track->psi / todeg + track->icharge * 0.5 * MPI ;
   if ( phi0 > 2.0 * MPI )
      phi0 = phi0 - 2.0 * MPI ;
   else if ( phi0 < 0. ) 
      phi0 = phi0 + 2.0 * MPI ;

   x0    = track->r0 * cos((track->phi0)/todeg) ;
   y0    = track->r0 * sin((track->phi0)/todeg) ;
   rr    = 1.0 / ( track->invpt * b_fact * b_field )  ;
   xc    = x0 - rr * cos(phi0) ;
   yc    = y0 - rr * sin(phi0) ;

/*----------------------------------------------------------
       Find point closest approach
----------------------------------------------------------*/          
   ok = eut_clo_ ( &xc, &yc, &rr, lxp, lyp ) ;
   if ( ok != 1 ) return 0 ;
/*---------------------------------------------------------------------
       Get the z coordinate
--------------------------------------------------------------------- */
   angle  = atan2 ( (*lyp-yc), (*lxp-xc) ) ;
   if ( angle < 0. )
      angle = angle + 2.0 * MPI ;

   dangle = angle  - phi0  ;
   dangle = fmod ( dangle, 2.0 * MPI ) ;
   if ( (track->icharge * dangle) < 0 )
      dangle = dangle + track->icharge * 2. * MPI ;
   
   stot   = fabs(dangle) * rr ;
   *lzp   = track->z0 - stot * track->tanl ;

   return 1 ;
}



int eut_extra_r_ ( float b_field, 
                   EGR_GLOBTRK_ST* track, float* r, float* x, float* y, float* z, 
                   float* xc, float* yc, float* rr )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Extrapolates track to cylinder with radius r
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             track           - Global track pointer
**:             r               - Cylinder radius     
**:         OUT:
**:             x,y,z           - Extrapolated point
**:             xc,yc,rr        - Center and radius track circle in x-y plane
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
{
   float phi ;
/*
   Default values 
*/

   *x = *y = *z = 0.F ;
/*
      If error return with error 
*/
   if ( !eut_extra_r_cyl_ ( b_field, track, r, &phi, z, xc, yc, rr ) ) return 1 ;
/*
     Otherwise get point in cartesian coordinates
*/
   *x = *r * cos(phi) ;
   *y = *r * sin(phi) ; 

   return 0 ;
}



int eut_extra_r_cyl_ ( float b_field, EGR_GLOBTRK_ST *track,
                       float *r,  float* phi, float* z, 
                       float *xc, float* yc,  float* rc )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Extrapolates track to cylinder with radius r
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             b_field         - Magnetic field in Tesla
**:             track           - Global track pointer
**:             r               - Cylinder radius
**:         OUT:
**:             phi,z           - Extrapolated point
**:             xc,yc,rc        - Center and radius track circle in x-y plane
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
{
   float q     ;
   float td, cosphi        ;
   float phi0, x0, y0      ; /*  phi and coord. of first point in the track */
   float fac1,sfac, fac2,deltat ;


/*----------------------------------------------------------
       Get track parameters
----------------------------------------------------------*/
   q    = (track->icharge)/fabs(track->icharge) ;
   phi0 = track->psi/todeg + q * 0.5 * MPI / fabs(q) ;

   x0    = track->r0 * cos((track->phi0)/todeg) ;
   y0    = track->r0 * sin((track->phi0)/todeg) ;
   *rc   = 1.0 / ( track->invpt * b_fact * b_field )  ;
   *xc   = x0 - *rc * cos(phi0) ;
   *yc   = y0 - *rc * sin(phi0) ;
/*  
       Check helix and cylinder intersect
*/ 
   fac1 = (*xc)*(*xc) + (*yc)*(*yc) ;
   sfac = sqrt( fac1 ) ;
/*  
    If they don't intersect return
*/  
   if ( fabs(sfac-(*rc)) > *r || fabs(sfac+(*rc)) < *r ) return  1 ;
/*  
       Find intersection
*/  
   fac2   = ( (*r)*(*r) + fac1 - (*rc)*(*rc)) / (2.00 * (*r) * sfac ) ;
   *phi   = atan2(*yc,*xc) + q*acos(fac2) ;
   td     = atan2((*r)*sin(*phi) - *yc,(*r)*cos(*phi) - *xc) ;
/*  
      cosphi = q*sin(td-*phi) / sqrt(1.00 + track->tanl*track->tanl) ;
      Intersection in z
*/  
   if ( td < 0 ) td = td + 2. * MPI ;
   deltat = fmod((-q*(td) + q*phi0),2*MPI) ;
   if ( deltat < 0. ) deltat = deltat + 2. * MPI ;
   *z = track->z0 + *rc * track->tanl * deltat ;
/* 
      That's it
*/ 
   return 0 ;
   
}



float eut_length_ ( float b_field, EGR_GLOBTRK_ST* track, 
                    float x1, float y1, float x2, float y2 )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Calculates trajectory length between two points on a track
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             track           - Track pointer
**:             x1, y1          - Point 1
**:             x2, y2          - Point 2
**:         OUT:
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
{
   float q, x0, y0, xc, yc, rc, phi0 ;
   float angle_1, angle_2, d_angle, sleng_xy, sleng ;
/*----------------------------------------------------------
       Get track parameters
----------------------------------------------------------*/
   q    = (track->icharge)/fabs(track->icharge) ;
   x0    = track->r0 * cos((track->phi0)/todeg) ;
   y0    = track->r0 * sin((track->phi0)/todeg) ;
   rc   = 1.0 / ( track->invpt * b_fact * b_field )  ;
   phi0 = track->psi/todeg + q * 0.5 * MPI / fabs(q) ;
   xc   = x0 - rc * cos(phi0) ;
   yc   = y0 - rc * sin(phi0) ;
/*
    Get angle difference 
*/
   angle_1  = atan2 ( (y1-yc), (x1-xc) ) ;
   if ( angle_1 < 0 ) angle_1 = angle_1 + 2. * MPI ;
   angle_2  = atan2 ( (y2-yc), (x2-xc) ) ;
   if ( angle_2 < 0 ) angle_2 = angle_2 + 2. * MPI ;
   d_angle  = q * ( angle_1 - angle_2 ) ;
   d_angle  = fmod ( d_angle, 2. * MPI ) ;
   if ( d_angle < 0 ) d_angle = d_angle + 2. * MPI ;
/*----------------------------------------------------------
       Get total angle and total trajectory
----------------------------------------------------------*/
   sleng_xy = fabs ( rc ) * d_angle ;
   sleng    = sleng_xy * sqrt ( 1.0 + track->tanl * track->tanl )  ;
   return sleng ;

}


int eut_x_r_ ( xc, yc, r, xf, yf, q, r_extra, x, y )
/*:>--------------------------------------------------------------------
**: ROUTINE:   Extrapolates track to cylinder with radius r
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:             xc, yc          - Track cylinder center
**:             r               - Track radius   
**:             xf, yf          - First point in track
**:             q               - Track charge
**:             rextra          - Radius of cylinder to extrapolate
**:         OUT:
**:             x, y            - Intersection
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
   float              *xc, *yc, *r, *xf, *yf, *r_extra, *x, *y ;
   int                *q ;
{
   int   nsol ;
   float x1, y1, x2, y2    ; /*  intersections both circles                 */
   float angle0, angle1, angle2, dangle1, dangle2 ;

/*----------------------------------------------------------
       Look for the intersection with cylinder
----------------------------------------------------------*/
   nsol = eut_circles_x_ ( xc, yc, r, r_extra, &x1, &y1, &x2, &y2 ) ;
   if ( nsol > 1 )
   {
/*----------------------------------------------------------
       Choose the closest solution
----------------------------------------------------------*/
      angle0  = atan2 ( (*yf-*yc), (*xf-*xc) ) ;
      angle1  = atan2 ( (y1-*yc) , (x1-*xc ) ) ;
      dangle1 = *q * ( angle0 - angle1 ) ;
      dangle1 = fmod ( dangle1, 2.* MPI ) ;
      if ( dangle1 < 0 ) dangle1 = dangle1 + 2. * MPI ; 

      angle2  = atan2 ( (y2-*yc), (x2-*xc) ) ;
      dangle2 = *q * ( angle0  - angle2 ) ;
      dangle2 = fmod ( dangle2, 2.* MPI ) ;
      if ( dangle2 < 0 ) dangle2 = dangle2 + 2. * MPI ; 

      if ( dangle1 < dangle2 )
      {
         *x      = x1 ;
         *y      = y1 ;
      }
      else
      {
         *x      = x2 ;
         *y      = y2 ;
      }
      return nsol ;
   }
   else
      return 0 ;
}
/***************************************************************

        End eut_x_r_

****************************************************************/
