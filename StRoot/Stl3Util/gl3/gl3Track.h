//:>------------------------------------------------------------------
//: FILE:       gl3Tracks.h
//: HISTORY:
//:              6dec1999 version 1.00
//:              2feb2000 add sector to add methods
//:             27jul2000 add methods to drop hits
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "Stl3Util/foreign/daqFormats.h"
#include "Stl3Util/ftf/FtfBaseTrack.h"
#include "Stl3Util/gl3/gl3Hit.h"

#ifndef GL3TRACK
#define GL3TRACK


class gl3Track: public FtfBaseTrack {
private:
   gl3Track* getNextTrack ( )    { return (gl3Track *)nextTrack ; } ;
public:
   void*     nextTrack ;
   int       sector ;
   inline virtual   void nextHit (){ currentHit = ((gl3Hit *)currentHit)->nextHit ; } ;

   int addTrack ( gl3Track* ) ;
   void      dropHits ( int rest, int rowMin, int rowMax ) ;
   gl3Track* merge ( FtfContainer *trackArea ) ;
   void      Print ( int level ) ;
   
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   float getRealEta ( ) {
      float theta = atan2(1.,(double)tanl);
      float rEta  = -1. * log (tan(theta/2.)) ;
      return rEta ;
   }
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   void print ( ) {
      float gl3ToDeg = 180./acos(-1.);
      printf ( "pt %f tanl %f psi %f r0 %f z0 %f phi0 %f nHits %d\n", 
                pt, tanl, psi, r0, z0, phi0*gl3ToDeg, nHits ) ;
   }
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
   void set ( short sectorIn, local_track* trk ) {
//    id          = sectorIn * 10000 + abs(trk->id) ;
      nHits       = trk->nHits ;
      nDedx       = trk->ndedx ;
      chi2[0]     = float(trk->xy_chisq)/10. ;
      chi2[1]     = float(trk->sz_chisq)/10. ;
      dedx        = trk->dedx ; 
      pt          = fabs(trk->pt) ;
      psi         = trk->psi ;
      tanl        = trk->tanl ;
      eta         = getRealEta();
      z0          = trk->z0 ;
      length      = trk->trackLength ;
      innerMostRow= trk->innerMostRow ;
      outerMostRow= trk->outerMostRow ;
      r0          = trk->r0 ;
      q           = (short )(trk->pt/fabs(trk->pt)) ;
      phi0        = trk->phi0 ;
      dpt         = float(trk->dpt)/32768. * pt ; 
      dpsi        = DecompressOver1(trk->dpsi,psi);
      dtanl       = DecompressOver1(trk->dtanl,tanl)/64.;
      dz0         = float(trk->dz0)/1024. ;
      nextTrack   = 0 ;
      firstHit    = 0 ;
      lastHit     = 0 ;
      //
      // Check errors are not zero
      //
      if ( dpt   == 0 ) dpt   = 1.e-5 * pt ;
      if ( dpsi  == 0 ) dpsi  = 1.e-5 ;
      if ( dtanl == 0 ) dtanl = 1.e-5 ;
      if ( dz0   == 0 ) dz0   = 1.e-3 ; 

   }
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//
//############################################################################
/*    void set ( FtfTrack* trk ) { */
/*       id          = abs(trk->id) ; */
/*       printf ( "set %d\n", id ) ; */
/*       nHits       = trk->nHits; */
/*       chi2[0]     = trk->chi2[0] ; */
/*       chi2[1]     = trk->chi2[1] ;  */
/*       dedx        = trk->dedx ;  */
/*       pt          = fabs(trk->pt) ; */
/*       psi         = trk->psi ; */
/*       tanl        = trk->tanl ; */
/*       z0          = trk->z0 ; */
/*       length      = trk->length ; */
/*       r0          = trk->r0 ; */
/*       phi0        = trk->phi0 ; */
/*       q           = trk->q ; */
/*       dtanl = dpsi = dpt = 0. ; */
/*    } */
};

#endif
