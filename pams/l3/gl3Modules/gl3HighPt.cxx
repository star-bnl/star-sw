//:>------------------------------------------------------------------
//: FILE:       gl3HighPt.cc
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3HighPt.h"


//####################################################################
//
//####################################################################
int gl3HighPt::init ( l3List* histos ) {
// printf ( "hello I am in gl3HighPt init   !!!\n");
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3HighPt::process ( gl3Event* event ) {
// printf ( "hello I am in gl3HighPt process!!!\n");
   gl3Hit* hitP ;
   gl3Track* trackP ;
/*
   for ( int i = 0 ; i < event->getNTracks() ; i++ ) {
      trackP = event->getTrack(i); 
      printf ( "q %d \n", trackP->q ) ;
   
      int counter = 0 ;
      for ( trackP->startLoop() ; trackP->done() ; trackP->nextHit() ) {
//        ((gl3Hit *)(trackP->currentHit))->print();
          int id = ((gl3Hit *)(trackP->currentHit))->getTrackId() ;
          if ( id != trackP->id ) {
              printf ( "trackId %d trkId from hit %d \n", trackP->id, id ) ; 
          }
          counter++ ;
      }
      if ( trackP->nHits != counter ) {
	 printf ( "** Hit mismatch: track %d has %d hits and %d hitList size \n", 
	       trackP->id, trackP->nHits, counter ) ;
	 for ( trackP->startLoop() ; trackP->done() ; trackP->nextHit() ) {
	    ((gl3Hit *)(trackP->currentHit))->print();
	    int id = ((gl3Hit *)(trackP->currentHit))->getTrackId() ;
	    printf ( "trackId %d trkId from hit %d \n", trackP->id, id ) ; 
	 }
      }
   }
*/
   return 1 ;
}
