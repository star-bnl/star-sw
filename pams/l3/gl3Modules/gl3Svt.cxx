//:>------------------------------------------------------------------
//: FILE:       gl3Svt.cc
//: HISTORY:
//:             29feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Svt.h"


//####################################################################
//
//####################################################################
int gl3Svt::init ( l3List* histoList ) {
// printf ( "hello I am in gl3JPsi init   !!!\n");
   char hid[50] ;
   char title[100] ;
   //
   //   Book histos
   //
   strcpy ( hid, "svt:dx" ) ;
   strcpy ( title, "svt:dx" ) ;
   hx = new gl3Histo ( hid, title, 100, -10., 10. ) ;
   histoList->append ( (void *)hx ) ;

   strcpy ( hid, "svt:dy" ) ;
   strcpy ( title, "svt:dy" ) ;
   hy = new gl3Histo ( hid, title, 100, -10., 10. ) ;
   histoList->append ( (void *)hy ) ;

   strcpy ( hid, "svt:dz" ) ;
   strcpy ( title, "svt:dz" ) ;
   hz = new gl3Histo ( hid, title, 100, -10., 10. ) ;
   histoList->append ( (void *)hz ) ;
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Svt::process ( gl3Event* event ) {
   // printf ( "hello I am in gl3JPsi process!!!\n");
   int i ;
   short sector = 0 ;
   gl3Track* tTrack ;
   double radius, x,y,z;
   Ftf3DHit tHit, closestHit ;
   for ( i = 0 ; i < event->getNTracks() ; i++ ) {
      if ( i > 100 ) continue ;
      tTrack = event->getTrack(i);
      sector = tTrack->id / 10000 ;
      radius = tTrack->r0 + 0.000001 ;
      tHit        = tTrack->extraRadius ( radius ) ;
      closestHit = tTrack->closestApproach ( 0., 0. ) ;
      x = tHit.x ;
      y = tHit.y ;
      z = tHit.z ;

      hx->Fill(closestHit.x,1.);
      hy->Fill(closestHit.y,1.);
      hz->Fill(closestHit.z,1.);

      /*
      printf ( " Extra x y r z %f %f %f %f \n", tHit.x, tHit.y, 
                                                sqrt(x*x+y*y), tHit.z ) ;
      printf ( "Closest %f %f %f \n",closestHit.x,closestHit.y,closestHit.z );
      printf ( " Track x y r z %f %f %f %f \n", 
                   tTrack->r0*cos(tTrack->phi0),
		   tTrack->r0*sin(tTrack->phi0), 
		   tTrack->r0, 
		   tTrack->z0 ) ;                
*/
   }

   return 1 ;
}
