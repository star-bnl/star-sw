//:>------------------------------------------------------------------
//: FILE:       gl3Tracks.h
//: HISTORY:
//:              6dec1999 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "daqFormats.h"

#ifndef GL3TRACKS
#define GL3TRACKS


class gl3Track {
public:
   short     id;          /* track id */
   short     nHits;       /* Number of points assigned to that track */
   long      primaryFlag; /* =1 if primary */
   float     chisq[2];    /* xy and dz chi2 */
   float     dedx;        /* dE/dx information */
   float     pt  ;        /* pt time charge */
   float     psi;         /* azimuthal angle of the momentum at (r,.. */
   float     tanl;        /* tg of the dip angle at (r,phi,z) */
   float     z0;          /* z coordinate of the first point */
   float     r0;          /* r coordinate of the first point */
   float     phi0;        /* phi coordinate of the first point */
   float     trackLength;
   float     dpt ;        /* pt error */
   float     dpsi;        /* psi error */
   float     dtanl ;      /* tanl error */
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   void print ( ) {
      float gl3ToDeg = 180./acos(-1.);
      printf ( "pt %f tanl %f psi %f r0 %f z0 %f phi0 %f nHits %d\n", 
                pt, tanl, psi*gl3ToDeg, r0, z0, phi0*gl3ToDeg, nHits ) ;
   }
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   void set ( type1_track* trk, float bField, float xVert, float yVert,
                                              float rVert, float phiVert ) {
   //
   //   Line parameters in xy
   //
      double ddXy  = trk->s11Xy * trk->s22Xy - trk->s12Xy * trk->s12Xy ;
      double a1Xy  = ( trk->g1Xy * trk->s22Xy - trk->g2Xy * trk->s12Xy ) / ddXy ;
      double a2Xy  = ( trk->g2Xy * trk->s11Xy - trk->g1Xy * trk->s12Xy ) / ddXy ;
   //
   //   Line parameters in sz
   //
      double ddSz  = trk->s11Sz * trk->s22Sz - trk->s12Sz * trk->s12Sz ;
      double a1Sz  = ( trk->g1Sz * trk->s22Sz - trk->g2Sz * trk->s12Sz ) / ddSz ;
      double a2Sz  = ( trk->g2Sz * trk->s11Sz - trk->g1Sz * trk->s12Sz ) / ddSz ;
      //
      double rc = sqrt ( a2Xy * a2Xy + 1 ) / ( 2 * fabs(a1Xy) ) ;
      //
      //   Get circle parameters
      //
      double xcp = - a2Xy / ( 2. * a1Xy ) ;
      double ycp = - 1.   / ( 2. * a1Xy ) ;
      double xc  = xcp + xVert ;
      double yc  = ycp + yVert ;
      //
      //   Get track parameters
      //
      double angle_vertex  = atan2 ( -ycp, -xcp ) ;
      if ( angle_vertex < 0. ) angle_vertex = angle_vertex + 2.*M_PI ;

      double dx_last    = trk->xLastHit - xc ;
      double dy_last    = trk->yLastHit - yc ;
      double angle_last = atan2 ( dy_last, dx_last ) ;
      if ( angle_last < 0. ) angle_last = angle_last + 2.*M_PI ;
      //
      //       Get the rotation
      //
      double d_angle = angle_last - angle_vertex ;

      if ( d_angle >  M_PI ) d_angle =   d_angle - 2.*M_PI  ;
      if ( d_angle < -M_PI ) d_angle =   d_angle + 2.*M_PI  ;
      short  q = ( ( d_angle < 0 ) ? 1 : -1 ) ;
//
      id    = id ;
      nHits = trk->nHits ; 
      pt    = (double)(2.9979e-3 * bField * rc ) * double(q) ;
      r0    = rVert ;
      phi0  = phiVert ;
      psi   = (double)(angle_vertex - q * 0.5F * M_PI) ;
      if ( psi < 0       ) psi = (double)(psi + 2.*M_PI );
      if ( psi > 2.*M_PI ) psi = (double)(psi - 2.*M_PI );
      //
      //      Get z parameters if needed
      //
      tanl  = -(double)a2Sz ;
      z0    =  (double)(a1Sz + a2Sz * ( trk->trackLength - rc * d_angle * q ) );
      trackLength = trk->trackLength ;
      //
      dpt = dpsi = dtanl = 0. ;
   }
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   void set ( type2_track* trk, float rVert, float phiVert ) {
      id          = trk->id ;
      nHits       = trk->nrec ;
      chisq[0]    = float(trk->xy_chisq)/10. ;
      chisq[1]    = float(trk->sz_chisq)/10. ;
      dedx        = trk->dedx ; 
      pt          = trk->pt ;
      psi         = trk->psi ;
      tanl        = trk->tanl ;
      z0          = trk->z0 ;
      trackLength = trk->trackLength ;
      r0          = rVert ;
      phi0          = phiVert ;
      dtanl = dpsi = dpt = 0. ;
   }
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   void set ( type3_track* trk ) {
      id          = trk->id ;
      nHits       = trk->nrec ;
      chisq[0]    = float(trk->xy_chisq)/10. ;
      chisq[1]    = float(trk->sz_chisq)/10. ;
      dedx        = trk->dedx ; 
      pt          = trk->pt ;
      psi         = trk->psi ;
      tanl        = trk->tanl ;
      z0          = trk->z0 ;
      trackLength = trk->trackLength ;
      r0          = trk->r0 ;
      phi0          = trk->phi0 ;
      dtanl = dpsi = dpt = 0. ;
   }
};

#endif
