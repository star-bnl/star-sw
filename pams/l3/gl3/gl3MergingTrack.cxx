//:>------------------------------------------------------------------
//: FILE:       gl3MergingTrack.cc
//: HISTORY:
//:             18jan2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3MergingTrack.h"

//####################################################################
//
//####################################################################
void gl3MergingTrack::addTrack ( short sector, type1_track* track ) {
//
   id    = track->id + 10000 * sector   ;
   dedx  = track->dedx  ;
   nHits = track->nHits ;
   s11Xy = track->s11Xy ;
   s12Xy = track->s12Xy ;
   s22Xy = track->s22Xy ;
   g1Xy  = track->g1Xy ;
   g2Xy  = track->g2Xy ;
   s11Sz = track->s11Sz ;
   s12Sz = track->s12Sz ;
   s22Sz = track->s22Sz ;
   g1Sz  = track->g1Sz ;
   g2Sz  = track->g2Sz ;
   xRefHit    = track->xLastHit ;
   yRefHit    = track->yLastHit ;
   xLastHit   = track->xLastHit ;
   yLastHit   = track->yLastHit ;
   innerMostRow = track->innerMostRow ;
   outerMostRow = track->outerMostRow ;
   trackLength = track->trackLength ;
//
//   Calculate line parameters in conformal space
//   
   ddXy  = s11Xy * s22Xy - square ( s12Xy ) ;
   a1Xy  = ( g1Xy * s22Xy - g2Xy * s12Xy ) / ddXy ;
   a2Xy  = ( g2Xy * s11Xy - g1Xy * s12Xy ) / ddXy ;
   //
   ddSz  = s11Sz * s22Sz -  s12Sz * s12Sz ;
   a1Sz  = ( g1Sz * s22Sz - g2Sz * s12Sz ) / ddSz ;
   a2Sz  = ( g2Sz * s11Sz - g1Sz * s12Sz)  / ddSz ;
//
//   Calculate parameters in real space
//
   double xc, yc ;
   double rc   = sqrt ( a2Xy * a2Xy + 1 ) / ( 2 * fabs(a1Xy) ) ;
   pt          = (double)(2.9979e-3 * para->bField * rc );
   fillPrimary ( xc, yc, rc ) ;

}
